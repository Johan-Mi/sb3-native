mod lowering;

use crate::de;
use slotmap::{SecondaryMap, SlotMap};
use std::fmt;

pub use de::RawValue as Immediate;

#[derive(Debug)]
pub struct Project {
    targets: Vec<Target>,
    pub hats: SlotMap<HatId, Hat>,
    pub basic_blocks: SlotMap<BasicBlockId, BasicBlock>,
    pub ops: SlotMap<OpId, Op>,
}

#[derive(Debug)]
struct Target {
    hats: SecondaryMap<HatId, ()>,
}

#[derive(Debug)]
pub struct Hat {
    kind: HatKind,
    pub body: BasicBlockId,
}

#[derive(Debug)]
enum HatKind {
    WhenFlagClicked,
    WhenReceived { broadcast_name: String },
    Procedure,
}

#[derive(Debug, Default)]
pub struct BasicBlock {
    pub ops: Vec<OpId>,
}

impl From<OpId> for BasicBlock {
    fn from(value: OpId) -> Self {
        Self {
            ops: Vec::from([value]),
        }
    }
}

impl From<Option<OpId>> for BasicBlock {
    fn from(value: Option<OpId>) -> Self {
        value.map_or_else(Self::default, Self::from)
    }
}

#[derive(Debug)]
pub enum Op {
    If {
        condition: Value,
        then: BasicBlockId,
        else_: BasicBlockId,
    },
    For {
        variable: Option<VariableId>,
        times: Value,
        body: BasicBlockId,
    },
    Forever {
        body: BasicBlockId,
    },
    While {
        condition: Value,
        body: BasicBlockId,
    },
    Until {
        condition: Value,
        body: BasicBlockId,
    },

    CallProcedure {
        arguments: SecondaryMap<ParameterId, Value>,
    },
    Parameter(ParameterId),

    StopAll,
    StopOtherScriptsInSprite,
    StopThisScript,

    BroadcastAndWait(Value),

    Variable(VariableId),
    SetVariable {
        variable: VariableId,
        to: Value,
    },
    ChangeVariable {
        variable: VariableId,
        by: Value,
    },
    List(ListId),
    AddToList {
        list: ListId,
        value: Value,
    },
    DeleteAllOfList(ListId),
    DeleteItemOfList {
        list: ListId,
        index: Value,
    },
    ItemOfList {
        list: ListId,
        index: Value,
    },
    LengthOfList(ListId),
    ReplaceItemOfList {
        list: ListId,
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
    Op(OpId),
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

slotmap::new_key_type! {
    pub struct HatId;

    pub struct BasicBlockId;

    pub struct OpId;

    pub struct VariableId;

    pub struct ListId;

    pub struct ParameterId;
}
