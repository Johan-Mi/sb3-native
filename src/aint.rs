//! Type analysis through abstract interpretation.

#![deny(clippy::wildcard_enum_match_arm)]

use crate::hir;
use slotmap::SecondaryMap;
use std::{fmt, ops};

#[derive(Debug)]
pub struct ComputedTypes {
    ops: SecondaryMap<hir::OpId, Lattice>,
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
    /// This is used for ops that don't return anything.
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
        ops: SecondaryMap::new(),
        variables: SecondaryMap::new(),
        lists: SecondaryMap::new(),
        parameters: SecondaryMap::new(),
        project,
    };
    for basic_block in project.basic_blocks.values() {
        interpreter.interpret_basic_block(basic_block);
    }
    interpreter.resolve()
}

struct Interpreter<'a> {
    ops: SecondaryMap<hir::OpId, Thing>,
    variables: SecondaryMap<hir::VariableId, Thing>,
    lists: SecondaryMap<hir::ListId, Thing>,
    parameters: SecondaryMap<hir::ParameterId, Thing>,
    project: &'a hir::Project,
}

impl Interpreter<'_> {
    fn interpret_basic_block(&mut self, body: &hir::BasicBlock) {
        for &op in &body.ops {
            let thing = self.interpret_op(op);
            let _: Option<Thing> = self.ops.insert(op, thing);
        }
    }

    fn interpret_op(&mut self, id: hir::OpId) -> Thing {
        use hir::Op;
        let Some(op) = self.project.ops.get(id) else {
            panic!("missing op: {id:?}");
        };
        match op {
            Op::CallProcedure { arguments } => {
                for (parameter, argument) in arguments {
                    let argument = self.interpret_value(argument);
                    *self
                        .parameters
                        .entry(parameter)
                        .unwrap()
                        .or_insert_with(|| Lattice::BOTTOM.into()) |= argument;
                }
                Lattice::BOTTOM.into()
            }
            Op::SetVariable { variable, to } => {
                let to = self.interpret_value(to);
                *self
                    .variables
                    .entry(*variable)
                    .unwrap()
                    .or_insert_with(|| Lattice::BOTTOM.into()) |= to;
                Lattice::BOTTOM.into()
            }
            Op::ChangeVariable { variable, .. } => {
                *self
                    .variables
                    .entry(*variable)
                    .unwrap()
                    .or_insert_with(|| Lattice::BOTTOM.into()) |= Lattice::NUM.into();
                Lattice::BOTTOM.into()
            }
            Op::AddToList { list, value } | Op::ReplaceItemOfList { list, value, .. } => {
                let value = self.interpret_value(value);
                *self
                    .lists
                    .entry(*list)
                    .unwrap()
                    .or_insert_with(|| Lattice::BOTTOM.into()) |= value;
                Lattice::BOTTOM.into()
            }
            Op::If { .. }
            | Op::For { .. }
            | Op::Forever { .. }
            | Op::While { .. }
            | Op::Until { .. }
            | Op::StopAll
            | Op::StopOtherScriptsInSprite
            | Op::StopThisScript
            | Op::BroadcastAndWait(_)
            | Op::DeleteAllOfList(_)
            | Op::DeleteItemOfList { .. }
            | Op::Ask(_)
            | Op::ChangeX { .. }
            | Op::ChangeY { .. }
            | Op::GoToXY { .. }
            | Op::Hide
            | Op::PenClear
            | Op::PenStamp
            | Op::SetCostume { .. }
            | Op::SetSize { .. }
            | Op::SetX { .. } => Lattice::BOTTOM.into(),
            Op::Parameter(parameter) => Thing::from(*parameter),
            Op::Variable(variable) => Thing::from(*variable),
            Op::ItemOfList { list, .. } => Thing::from(*list),
            Op::List(_) | Op::Join(..) | Op::LetterOf { .. } | Op::Answer => Lattice::STRING.into(),
            Op::LengthOfList(_)
            | Op::Add(..)
            | Op::Sub(..)
            | Op::Mul(..)
            | Op::Div(..)
            | Op::Mod(..)
            | Op::Abs(_)
            | Op::Floor(_)
            | Op::Ceiling(_)
            | Op::Sqrt(_)
            | Op::Sin(_)
            | Op::Cos(_)
            | Op::Tan(_)
            | Op::Asin(_)
            | Op::Acos(_)
            | Op::Atan(_)
            | Op::Ln(_)
            | Op::Log(_)
            | Op::Exp(_)
            | Op::Exp10(_)
            | Op::XPosition
            | Op::StringLength(_) => Lattice::NUM.into(),
            Op::Lt(..) | Op::Eq(..) | Op::Gt(..) | Op::And(..) | Op::Or(..) | Op::Not(_) => {
                Lattice::BOOL.into()
            }
        }
    }

    fn interpret_value(&self, value: &hir::Value) -> Thing {
        match *value {
            hir::Value::Op(op) => self.ops[op].clone(),
            hir::Value::Immediate(hir::Immediate::String(_)) => Lattice::STRING.into(),
            hir::Value::Immediate(hir::Immediate::Number(_)) => Lattice::NUM.into(),
            hir::Value::Immediate(hir::Immediate::Bool(_)) => Lattice::BOOL.into(),
        }
    }

    fn resolve(self) -> ComputedTypes {
        ComputedTypes {
            ops: self
                .ops
                .iter()
                .map(|(op, thing)| (op, self.resolve_one(thing)))
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
