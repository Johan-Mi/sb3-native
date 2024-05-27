pub use crate::de::RawValue as Immediate;
use std::{collections::BTreeMap, num::NonZeroU32};

struct Project {
    hats: BTreeMap<BlockId, Hat>,
    blocks: BTreeMap<BlockId, Block>,
}

struct Hat {
    kind: HatKind,
    body: Sequence,
}

enum HatKind {
    WhenFlagClicked,
}

struct Sequence {
    blocks: Vec<BlockId>,
}

enum Block {
    If {
        condition: Expresssion,
        then: Sequence,
        else_: Sequence,
    },
    For {
        variable: Option<VariableId>,
        times: Expresssion,
        body: Sequence,
    },
    Forever {
        body: Sequence,
    },
    While {
        condition: Expresssion,
        body: Sequence,
    },
    Until {
        condition: Expresssion,
        body: Sequence,
    },
}

enum Expresssion {
    Block(BlockId),
    Immediate(Immediate),
}

struct BlockId(NonZeroU32);

struct VariableId(NonZeroU32);

struct ListId(NonZeroU32);
