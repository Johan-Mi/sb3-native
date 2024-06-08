use crate::de;
use anyhow::{bail, Result};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    num::NonZeroU32,
};

pub use de::RawValue as Immediate;

pub struct Project {
    targets: Vec<Target>,
    hats: BTreeMap<BlockId, Hat>,
    blocks: BTreeMap<BlockId, Block>,
}

impl Project {
    pub fn lower(de: de::Project) -> Result<Self> {
        let mut hats = BTreeMap::new();
        let blocks = BTreeMap::new();

        let mut generator = Generator::default();
        let mut block_ids = HashMap::new();
        let mut t = |id: de::BlockId| {
            *block_ids
                .entry(id)
                .or_insert_with(|| generator.new_block_id())
        };

        let targets = de
            .targets
            .into_iter()
            .map(|target| {
                let mut my_hats = BTreeSet::new();

                for (id, block) in target.blocks {
                    let id = t(id);

                    match &*block.opcode {
                        "argument_reporter_string_number" => {
                            todo!("argument_reporter_string_number")
                        }
                        "control_for_each" => todo!("control_for_each"),
                        "control_if" => todo!("control_if"),
                        "control_if_else" => todo!("control_if_else"),
                        "control_repeat" => todo!("control_repeat"),
                        "control_repeat_until" => todo!("control_repeat_until"),
                        "control_stop" => todo!("control_stop"),
                        "data_addtolist" => todo!("data_addtolist"),
                        "data_changevariableby" => {
                            todo!("data_changevariableby")
                        }
                        "data_deletealloflist" => todo!("data_deletealloflist"),
                        "data_deleteoflist" => todo!("data_deleteoflist"),
                        "data_itemoflist" => todo!("data_itemoflist"),
                        "data_lengthoflist" => todo!("data_lengthoflist"),
                        "data_replaceitemoflist" => {
                            todo!("data_replaceitemoflist")
                        }
                        "data_setvariableto" => todo!("data_setvariableto"),
                        "event_broadcastandwait" => {
                            todo!("event_broadcastandwait")
                        }
                        "event_whenbroadcastreceived" => {
                            todo!("event_whenbroadcastreceived")
                        }
                        "event_when_flag_clicked" => {
                            hats.insert(
                                id,
                                Hat {
                                    kind: HatKind::WhenFlagClicked,
                                    body: Sequence { blocks: Vec::new() },
                                },
                            );
                            my_hats.insert(id);
                        }
                        "looks_hide" => todo!("looks_hide"),
                        "looks_setsizeto" => todo!("looks_setsizeto"),
                        "looks_switchcostumeto" => {
                            todo!("looks_switchcostumeto")
                        }
                        "motion_changexby" => todo!("motion_changexby"),
                        "motion_changeyby" => todo!("motion_changeyby"),
                        "motion_gotoxy" => todo!("motion_gotoxy"),
                        "motion_setx" => todo!("motion_setx"),
                        "motion_xposition" => todo!("motion_xposition"),
                        "operator_add" => todo!("operator_add"),
                        "operator_and" => todo!("operator_and"),
                        "operator_divide" => todo!("operator_divide"),
                        "operator_equals" => todo!("operator_equals"),
                        "operator_gt" => todo!("operator_gt"),
                        "operator_join" => todo!("operator_join"),
                        "operator_length" => todo!("operator_length"),
                        "operator_letter_of" => todo!("operator_letter_of"),
                        "operator_lt" => todo!("operator_lt"),
                        "operator_mathop" => todo!("operator_mathop"),
                        "operator_mod" => todo!("operator_mod"),
                        "operator_multiply" => todo!("operator_multiply"),
                        "operator_not" => todo!("operator_not"),
                        "operator_or" => todo!("operator_or"),
                        "operator_subtract" => todo!("operator_subtract"),
                        "pen_clear" => todo!("pen_clear"),
                        "pen_stamp" => todo!("pen_stamp"),
                        "procedures_call" => todo!("procedures_call"),
                        "procedures_definition" => {
                            todo!("procedures_definition")
                        }
                        "procedures_prototype" => todo!("procedures_prototype"),
                        "sensing_answer" => todo!("sensing_answer"),
                        "sensing_askandwait" => todo!("sensing_askandwait"),
                        opcode => bail!("invalid opcode: {opcode:?}"),
                    }
                }

                Ok(Target { hats: my_hats })
            })
            .collect::<Result<_>>()?;

        Ok(Self {
            targets,
            hats,
            blocks,
        })
    }
}

struct Target {
    hats: BTreeSet<BlockId>,
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BlockId(NonZeroU32);

struct VariableId(NonZeroU32);

struct ListId(NonZeroU32);

struct Generator(NonZeroU32);

impl Default for Generator {
    fn default() -> Self {
        Self(NonZeroU32::MIN)
    }
}

impl Generator {
    fn new_raw(&mut self) -> NonZeroU32 {
        let n = self.0;
        self.0 = self.0.checked_add(1).unwrap();
        n
    }

    fn new_block_id(&mut self) -> BlockId {
        BlockId(self.new_raw())
    }
}
