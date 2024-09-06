//! Type analysis through abstract interpretation.

// TODO: Rewrite this with `(Lattice, Vec<BlockId>)`

#![deny(clippy::wildcard_enum_match_arm)]

use crate::hir;
use std::{collections::BTreeMap, fmt, ops};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Lattice {
    string: bool,
    num: bool,
    bool: bool,
}

impl Lattice {
    const TOP: Self = Self {
        string: true,
        num: true,
        bool: true,
    };
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

impl ops::BitOrAssign for Lattice {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

pub fn interpret(project: &hir::Project) -> BTreeMap<hir::BlockId, Lattice> {
    let mut interpreter = Interpreter {
        blocks: BTreeMap::new(),
        variables: BTreeMap::new(),
        lists: BTreeMap::new(),
        parameters: BTreeMap::new(),
        project,
    };
    for hat in project.hats.values() {
        interpreter.interpret_sequence(&hat.body);
    }
    interpreter.blocks
}

struct Interpreter<'a> {
    blocks: BTreeMap<hir::BlockId, Lattice>,
    variables: BTreeMap<hir::VariableId, Lattice>,
    lists: BTreeMap<hir::ListId, Lattice>,
    parameters: BTreeMap<hir::ParameterId, Lattice>,
    project: &'a hir::Project,
}

impl Interpreter<'_> {
    fn interpret_sequence(&mut self, body: &hir::Sequence) {
        for &block in &body.blocks {
            self.interpret_block(block);
        }
    }

    fn interpret_block(&mut self, id: hir::BlockId) {
        use hir::Block as B;
        let Some(block) = self.project.blocks.get(&id) else {
            panic!("missing block: {id:?}");
        };
        let lattice = match block {
            B::CallProcedure { arguments } => {
                for (&parameter, argument) in arguments {
                    let argument = self.interpret_expression(argument);
                    *self
                        .parameters
                        .entry(parameter)
                        .or_insert(Lattice::BOTTOM) |= argument;
                }
                Lattice::BOTTOM
            }
            B::SetVariable { variable, to } => {
                let to = self.interpret_expression(to);
                *self.variables.entry(*variable).or_insert(Lattice::BOTTOM) |=
                    to;
                Lattice::BOTTOM
            }
            B::ChangeVariable { variable, .. } => {
                *self.variables.entry(*variable).or_insert(Lattice::BOTTOM) |=
                    Lattice::NUM;
                Lattice::BOTTOM
            }
            B::AddToList { list, value }
            | B::ReplaceItemOfList { list, value, .. } => {
                let value = self.interpret_expression(value);
                *self.lists.entry(*list).or_insert(Lattice::BOTTOM) |= value;
                Lattice::BOTTOM
            }
            B::If { then, else_, .. } => {
                self.interpret_sequence(then);
                self.interpret_sequence(else_);
                Lattice::BOTTOM
            }
            B::For { body, .. }
            | B::Forever { body }
            | B::While { body, .. }
            | B::Until { body, .. } => {
                self.interpret_sequence(body);
                Lattice::BOTTOM
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
            | B::SetX { .. } => Lattice::BOTTOM,
            // TODO: Analyse parameter, variable and list types
            B::Parameter(_) | B::Variable(_) | B::ItemOfList { .. } => {
                Lattice::TOP
            }
            B::List(_) | B::Join(..) | B::LetterOf { .. } | B::Answer => {
                Lattice::STRING
            }
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
            | B::StringLength(_) => Lattice::NUM,
            B::Lt(..)
            | B::Eq(..)
            | B::Gt(..)
            | B::And(..)
            | B::Or(..)
            | B::Not(_) => Lattice::BOOL,
        };
        self.blocks.insert(id, lattice);
    }

    fn interpret_expression(&self, expression: &hir::Expression) -> Lattice {
        match expression {
            hir::Expression::Block(block) => self.blocks[block],
            hir::Expression::Immediate(hir::Immediate::String(_)) => {
                Lattice::STRING
            }
            hir::Expression::Immediate(hir::Immediate::Number(_)) => {
                Lattice::NUM
            }
            hir::Expression::Immediate(hir::Immediate::Bool(_)) => {
                Lattice::BOOL
            }
        }
    }
}
