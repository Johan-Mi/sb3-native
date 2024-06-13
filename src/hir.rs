use crate::de;
use anyhow::{bail, Context, Result};
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
        let mut cx = LoweringContext::default();
        let mut hats = BTreeMap::new();
        let mut blocks = BTreeMap::new();

        let targets = de
            .targets
            .into_iter()
            .map(|target| {
                let mut my_hats = BTreeSet::new();

                for (id, mut block) in target.blocks {
                    let id = cx.block_id(id);

                    let block = match &*block.opcode {
                        "argument_reporter_string_number" => {
                            todo!("argument_reporter_string_number")
                        }
                        "control_for_each" => todo!("control_for_each"),
                        "control_forever" => {
                            let body = cx.substack(&mut block, "SUBSTACK")?;
                            Some(Block::Forever { body: body.into() })
                        }
                        "control_if" => {
                            let condition =
                                cx.input(&mut block, "CONDITION")?;
                            let then = cx.substack(&mut block, "SUBSTACK")?;
                            Some(Block::If {
                                condition,
                                then: then.into(),
                                else_: Sequence::default(),
                            })
                        }
                        "control_if_else" => {
                            let condition =
                                cx.input(&mut block, "CONDITION")?;
                            let then = cx.substack(&mut block, "SUBSTACK")?;
                            let else_ = cx.substack(&mut block, "SUBSTACK2")?;
                            Some(Block::If {
                                condition,
                                then: then.into(),
                                else_: else_.into(),
                            })
                        }
                        "control_repeat" => {
                            let times = cx.input(&mut block, "TIMES")?;
                            let body = cx.substack(&mut block, "SUBSTACK")?;
                            Some(Block::Repeat {
                                times,
                                body: body.into(),
                            })
                        }
                        "control_repeat_until" => {
                            let condition =
                                cx.input(&mut block, "CONDITION")?;
                            let body = cx.substack(&mut block, "SUBSTACK")?;
                            Some(Block::Until {
                                condition,
                                body: body.into(),
                            })
                        }
                        "control_stop" => todo!("control_stop"),
                        "control_while" => {
                            let condition =
                                cx.input(&mut block, "CONDITION")?;
                            let body = cx.substack(&mut block, "SUBSTACK")?;
                            Some(Block::While {
                                condition,
                                body: body.into(),
                            })
                        }
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
                                    body: Sequence::default(),
                                },
                            );
                            my_hats.insert(id);
                            None
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
                        "operator_add" => {
                            let lhs = cx.input(&mut block, "NUM1")?;
                            let rhs = cx.input(&mut block, "NUM2")?;
                            Some(Block::Add(lhs, rhs))
                        }
                        "operator_and" => todo!("operator_and"),
                        "operator_divide" => {
                            let lhs = cx.input(&mut block, "NUM1")?;
                            let rhs = cx.input(&mut block, "NUM2")?;
                            Some(Block::Div(lhs, rhs))
                        }
                        "operator_equals" => {
                            let lhs = cx.input(&mut block, "OPERAND1")?;
                            let rhs = cx.input(&mut block, "OPERAND2")?;
                            Some(Block::Eq(lhs, rhs))
                        }
                        "operator_gt" => {
                            let lhs = cx.input(&mut block, "OPERAND1")?;
                            let rhs = cx.input(&mut block, "OPERAND2")?;
                            Some(Block::Gt(lhs, rhs))
                        }
                        "operator_join" => todo!("operator_join"),
                        "operator_length" => todo!("operator_length"),
                        "operator_letter_of" => todo!("operator_letter_of"),
                        "operator_lt" => {
                            let lhs = cx.input(&mut block, "OPERAND1")?;
                            let rhs = cx.input(&mut block, "OPERAND2")?;
                            Some(Block::Lt(lhs, rhs))
                        }
                        "operator_mathop" => todo!("operator_mathop"),
                        "operator_mod" => {
                            let lhs = cx.input(&mut block, "NUM1")?;
                            let rhs = cx.input(&mut block, "NUM2")?;
                            Some(Block::Mod(lhs, rhs))
                        }
                        "operator_multiply" => {
                            let lhs = cx.input(&mut block, "NUM1")?;
                            let rhs = cx.input(&mut block, "NUM2")?;
                            Some(Block::Mul(lhs, rhs))
                        }
                        "operator_not" => todo!("operator_not"),
                        "operator_or" => todo!("operator_or"),
                        "operator_subtract" => {
                            let lhs = cx.input(&mut block, "NUM1")?;
                            let rhs = cx.input(&mut block, "NUM2")?;
                            Some(Block::Sub(lhs, rhs))
                        }
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
                    };

                    if let Some(block) = block {
                        blocks.insert(id, block);
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

#[derive(Default)]
struct Sequence {
    blocks: Vec<BlockId>,
}

impl From<BlockId> for Sequence {
    fn from(value: BlockId) -> Self {
        Self {
            blocks: Vec::from([value]),
        }
    }
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
    Repeat {
        times: Expresssion,
        body: Sequence,
    },

    Add(Expresssion, Expresssion),
    Sub(Expresssion, Expresssion),
    Mul(Expresssion, Expresssion),
    Div(Expresssion, Expresssion),
    Mod(Expresssion, Expresssion),

    Lt(Expresssion, Expresssion),
    Eq(Expresssion, Expresssion),
    Gt(Expresssion, Expresssion),
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

#[derive(Default)]
struct LoweringContext {
    generator: Generator,
    block_ids: HashMap<de::BlockId, BlockId>,
}

impl LoweringContext {
    fn block_id(&mut self, id: de::BlockId) -> BlockId {
        *self
            .block_ids
            .entry(id)
            .or_insert_with(|| self.generator.new_block_id())
    }

    fn input(
        &mut self,
        block: &mut de::Block,
        name: &str,
    ) -> Result<Expresssion> {
        let input = block
            .inputs
            .remove(name)
            .with_context(|| format!("missing block input: {name:?}"))?;
        Ok(match input {
            de::Input::Block(block) => Expresssion::Block(self.block_id(block)),
            de::Input::Number(n) => {
                Expresssion::Immediate(Immediate::Number(n))
            }
            de::Input::String(s) => {
                Expresssion::Immediate(Immediate::String(s))
            }
            de::Input::Broadcast(_) => todo!(),
            de::Input::Variable(_) => todo!(),
            de::Input::List(_) => todo!(),
        })
    }

    fn substack(
        &mut self,
        block: &mut de::Block,
        name: &str,
    ) -> Result<BlockId> {
        match block.inputs.remove(name) {
            None => bail!("missing substack: {name:?}"),
            Some(de::Input::Block(block)) => Ok(self.block_id(block)),
            Some(_) => bail!("substack {name:?} must be a block ID"),
        }
    }
}
