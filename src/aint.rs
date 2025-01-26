//! Type analysis through abstract interpretation.

#![deny(clippy::wildcard_enum_match_arm)]

use crate::hir;
use slotmap::SecondaryMap;
use std::{fmt, ops};

#[derive(Debug)]
pub struct ComputedTypes {
    blocks: SecondaryMap<hir::BlockId, Lattice>,
    variables: SecondaryMap<hir::VariableId, Lattice>,
    lists: SecondaryMap<hir::ListId, Lattice>,
    parameters: SecondaryMap<hir::ParameterId, Lattice>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Lattice {
    string: bool,
    num: bool,
    bool: bool,
}

impl Lattice {
    const STRING: Self = Self {
        string: true,
        ..Self::BOTTOM
    };
    const NUM: Self = Self {
        num: true,
        ..Self::BOTTOM
    };
    const BOOL: Self = Self {
        bool: true,
        ..Self::BOTTOM
    };
    /// This is used for blocks that don't return anything.
    const BOTTOM: Self = Self {
        string: false,
        num: false,
        bool: false,
    };
}

impl fmt::Debug for Lattice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { string, num, bool } = self;
        f.write_str(match (string, num, bool) {
            (true, true, true) => "⊤",
            (true, true, false) => "𝕊∨ℕ",
            (true, false, true) => "𝕊∨𝔹",
            (true, false, false) => "𝕊",
            (false, true, true) => "ℕ∨𝔹",
            (false, true, false) => "ℕ",
            (false, false, true) => "𝔹",
            (false, false, false) => "⊥",
        })
    }
}

impl ops::BitOr for Lattice {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self {
            string: self.string | rhs.string,
            num: self.num | rhs.num,
            bool: self.bool | rhs.bool,
        }
    }
}

pub fn interpret(project: &hir::Project) -> ComputedTypes {
    let mut interpreter = Interpreter {
        blocks: SecondaryMap::new(),
        variables: SecondaryMap::new(),
        lists: SecondaryMap::new(),
        parameters: SecondaryMap::new(),
        project,
    };
    for hat in project.hats.values() {
        interpreter.interpret_sequence(&hat.body);
    }
    interpreter.resolve()
}

struct Interpreter<'a> {
    blocks: SecondaryMap<hir::BlockId, Thing>,
    variables: SecondaryMap<hir::VariableId, Thing>,
    lists: SecondaryMap<hir::ListId, Thing>,
    parameters: SecondaryMap<hir::ParameterId, Thing>,
    project: &'a hir::Project,
}

impl Interpreter<'_> {
    fn interpret_sequence(&mut self, body: &hir::Sequence) {
        for &block in &body.blocks {
            let thing = self.interpret_block(block);
            let _: Option<Thing> = self.blocks.insert(block, thing);
        }
    }

    fn interpret_block(&mut self, id: hir::BlockId) -> Thing {
        use hir::Block as B;
        let Some(block) = self.project.blocks.get(id) else {
            panic!("missing block: {id:?}");
        };
        match block {
            B::CallProcedure { arguments } => {
                for (parameter, argument) in arguments {
                    let argument = self.interpret_expression(argument);
                    *self
                        .parameters
                        .entry(parameter)
                        .unwrap()
                        .or_insert_with(|| Lattice::BOTTOM.into()) |= argument;
                }
                Lattice::BOTTOM.into()
            }
            B::SetVariable { variable, to } => {
                let to = self.interpret_expression(to);
                *self
                    .variables
                    .entry(*variable)
                    .unwrap()
                    .or_insert_with(|| Lattice::BOTTOM.into()) |= to;
                Lattice::BOTTOM.into()
            }
            B::ChangeVariable { variable, .. } => {
                *self
                    .variables
                    .entry(*variable)
                    .unwrap()
                    .or_insert_with(|| Lattice::BOTTOM.into()) |= Lattice::NUM.into();
                Lattice::BOTTOM.into()
            }
            B::AddToList { list, value } | B::ReplaceItemOfList { list, value, .. } => {
                let value = self.interpret_expression(value);
                *self
                    .lists
                    .entry(*list)
                    .unwrap()
                    .or_insert_with(|| Lattice::BOTTOM.into()) |= value;
                Lattice::BOTTOM.into()
            }
            B::If { then, else_, .. } => {
                self.interpret_sequence(then);
                self.interpret_sequence(else_);
                Lattice::BOTTOM.into()
            }
            B::For { body, .. }
            | B::Forever { body }
            | B::While { body, .. }
            | B::Until { body, .. } => {
                self.interpret_sequence(body);
                Lattice::BOTTOM.into()
            }
            B::StopAll
            | B::StopOtherScriptsInSprite
            | B::StopThisScript
            | B::BroadcastAndWait(_)
            | B::DeleteAllOfList(_)
            | B::DeleteItemOfList { .. }
            | B::Ask(_)
            | B::ChangeX { .. }
            | B::ChangeY { .. }
            | B::GoToXY { .. }
            | B::Hide
            | B::PenClear
            | B::PenStamp
            | B::SetCostume { .. }
            | B::SetSize { .. }
            | B::SetX { .. } => Lattice::BOTTOM.into(),
            B::Parameter(parameter) => Thing::from(*parameter),
            B::Variable(variable) => Thing::from(*variable),
            B::ItemOfList { list, .. } => Thing::from(*list),
            B::List(_) | B::Join(..) | B::LetterOf { .. } | B::Answer => Lattice::STRING.into(),
            B::LengthOfList(_)
            | B::Add(..)
            | B::Sub(..)
            | B::Mul(..)
            | B::Div(..)
            | B::Mod(..)
            | B::Abs(_)
            | B::Floor(_)
            | B::Ceiling(_)
            | B::Sqrt(_)
            | B::Sin(_)
            | B::Cos(_)
            | B::Tan(_)
            | B::Asin(_)
            | B::Acos(_)
            | B::Atan(_)
            | B::Ln(_)
            | B::Log(_)
            | B::Exp(_)
            | B::Exp10(_)
            | B::XPosition
            | B::StringLength(_) => Lattice::NUM.into(),
            B::Lt(..) | B::Eq(..) | B::Gt(..) | B::And(..) | B::Or(..) | B::Not(_) => {
                Lattice::BOOL.into()
            }
        }
    }

    fn interpret_expression(&self, expression: &hir::Expression) -> Thing {
        match *expression {
            hir::Expression::Block(block) => self.blocks[block].clone(),
            hir::Expression::Immediate(hir::Immediate::String(_)) => Lattice::STRING.into(),
            hir::Expression::Immediate(hir::Immediate::Number(_)) => Lattice::NUM.into(),
            hir::Expression::Immediate(hir::Immediate::Bool(_)) => Lattice::BOOL.into(),
        }
    }

    fn resolve(self) -> ComputedTypes {
        ComputedTypes {
            blocks: self
                .blocks
                .iter()
                .map(|(block, thing)| (block, self.resolve_one(thing)))
                .collect(),
            variables: self
                .variables
                .iter()
                .map(|(variable, thing)| (variable, self.resolve_one(thing)))
                .collect(),
            lists: self
                .lists
                .iter()
                .map(|(list, thing)| (list, self.resolve_one(thing)))
                .collect(),
            parameters: self
                .parameters
                .iter()
                .map(|(parameter, thing)| (parameter, self.resolve_one(thing)))
                .collect(),
        }
    }

    fn resolve_one(&self, thing: &Thing) -> Lattice {
        let variables = thing
            .variables
            .keys()
            .filter_map(|it| self.variables.get(it))
            .map(|it| self.resolve_one(it));
        let lists = thing
            .lists
            .keys()
            .filter_map(|it| self.lists.get(it))
            .map(|it| self.resolve_one(it));
        let parameters = thing
            .parameters
            .keys()
            .filter_map(|it| self.parameters.get(it))
            .map(|it| self.resolve_one(it));
        variables
            .chain(lists)
            .chain(parameters)
            .fold(thing.lattice, ops::BitOr::bitor)
    }
}

#[derive(Clone)]
struct Thing {
    lattice: Lattice,
    variables: SecondaryMap<hir::VariableId, ()>,
    lists: SecondaryMap<hir::ListId, ()>,
    parameters: SecondaryMap<hir::ParameterId, ()>,
}

impl From<Lattice> for Thing {
    fn from(value: Lattice) -> Self {
        Self {
            lattice: value,
            variables: SecondaryMap::new(),
            lists: SecondaryMap::new(),
            parameters: SecondaryMap::new(),
        }
    }
}

impl From<hir::VariableId> for Thing {
    fn from(value: hir::VariableId) -> Self {
        Self {
            lattice: Lattice::BOTTOM,
            variables: SecondaryMap::from_iter([(value, ())]),
            lists: SecondaryMap::new(),
            parameters: SecondaryMap::new(),
        }
    }
}

impl From<hir::ListId> for Thing {
    fn from(value: hir::ListId) -> Self {
        Self {
            lattice: Lattice::BOTTOM,
            variables: SecondaryMap::new(),
            lists: SecondaryMap::from_iter([(value, ())]),
            parameters: SecondaryMap::new(),
        }
    }
}

impl From<hir::ParameterId> for Thing {
    fn from(value: hir::ParameterId) -> Self {
        Self {
            lattice: Lattice::BOTTOM,
            variables: SecondaryMap::new(),
            lists: SecondaryMap::new(),
            parameters: SecondaryMap::from_iter([(value, ())]),
        }
    }
}

impl ops::BitOrAssign for Thing {
    fn bitor_assign(&mut self, rhs: Self) {
        let Self {
            lattice,
            variables,
            lists,
            parameters,
        } = rhs;
        self.lattice = self.lattice | lattice;
        self.variables.extend(variables);
        self.lists.extend(lists);
        self.parameters.extend(parameters);
    }
}
