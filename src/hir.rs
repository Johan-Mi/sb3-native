mod lowering;

use crate::de;
use beach_map::{BeachMap, Id};
use std::{collections::HashMap, fmt};

pub use de::RawValue as Immediate;

pub struct Project {
    pub hats: Vec<Hat>,
    pub basic_blocks: BeachMap<BasicBlock>,
    pub ops: BeachMap<Op>,
}

#[derive(Debug)]
struct Target(usize);

#[derive(Debug)]
pub struct Hat {
    owner: Target,
    kind: HatKind,
    pub body: Id<BasicBlock>,
}

#[derive(Debug)]
enum HatKind {
    WhenFlagClicked,
    WhenReceived { broadcast_name: String },
    Procedure,
}

#[derive(Debug, Default)]
pub struct BasicBlock {
    pub ops: Vec<Id<Op>>,
}

impl From<Id<Op>> for BasicBlock {
    fn from(value: Id<Op>) -> Self {
        Self {
            ops: Vec::from([value]),
        }
    }
}

impl From<Option<Id<Op>>> for BasicBlock {
    fn from(value: Option<Id<Op>>) -> Self {
        value.map_or_else(Self::default, Self::from)
    }
}

#[derive(Debug)]
pub enum Op {
    If {
        condition: Value,
        then: Id<BasicBlock>,
        else_: Id<BasicBlock>,
    },
    For {
        variable: Option<Id<Variable>>,
        times: Value,
        body: Id<BasicBlock>,
    },
    Forever {
        body: Id<BasicBlock>,
    },
    While {
        condition: Value,
        body: Id<BasicBlock>,
    },
    Until {
        condition: Value,
        body: Id<BasicBlock>,
    },

    CallProcedure {
        arguments: HashMap<Id<Parameter>, Value>,
    },
    Parameter(Id<Parameter>),

    StopAll,
    StopOtherScriptsInSprite,
    StopThisScript,

    BroadcastAndWait(Value),

    Variable(Id<Variable>),
    SetVariable {
        variable: Id<Variable>,
        to: Value,
    },
    ChangeVariable {
        variable: Id<Variable>,
        by: Value,
    },
    List(Id<List>),
    AddToList {
        list: Id<List>,
        value: Value,
    },
    DeleteAllOfList(Id<List>),
    DeleteItemOfList {
        list: Id<List>,
        index: Value,
    },
    ItemOfList {
        list: Id<List>,
        index: Value,
    },
    LengthOfList(Id<List>),
    ReplaceItemOfList {
        list: Id<List>,
        index: Value,
        value: Value,
    },

    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Mod(Value, Value),

    Lt(Value, Value),
    Eq(Value, Value),
    Gt(Value, Value),

    And(Value, Value),
    Or(Value, Value),
    Not(Value),

    Join(Value, Value),
    StringLength(Value),
    LetterOf {
        index: Value,
        string: Value,
    },

    Abs(Value),
    Floor(Value),
    Ceiling(Value),
    Sqrt(Value),
    Sin(Value),
    Cos(Value),
    Tan(Value),
    Asin(Value),
    Acos(Value),
    Atan(Value),
    Ln(Value),
    Log(Value),
    Exp(Value),
    Exp10(Value),

    Answer,
    XPosition,

    Ask(Value),
    ChangeX {
        by: Value,
    },
    ChangeY {
        by: Value,
    },
    GoToXY {
        x: Value,
        y: Value,
    },
    Hide,
    PenClear,
    PenStamp,
    SetCostume {
        to: Value,
    },
    SetSize {
        to: Value,
    },
    SetX {
        to: Value,
    },
}

pub enum Value {
    Op(Id<Op>),
    Immediate(Immediate),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Op(op) => op.fmt(f),
            Self::Immediate(immediate) => immediate.fmt(f),
        }
    }
}

pub struct Variable;

pub struct List;

pub struct Parameter;
