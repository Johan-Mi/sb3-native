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

                for (id, block) in target.blocks {
                    let id = cx.block_id(id);
                    if let Some(block) = lower_block(
                        block,
                        id,
                        &mut hats,
                        &mut my_hats,
                        &mut cx,
                    )? {
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

fn lower_block(
    mut block: de::Block,
    id: BlockId,
    hats: &mut BTreeMap<BlockId, Hat>,
    my_hats: &mut BTreeSet<BlockId>,
    cx: &mut LoweringContext,
) -> Result<Option<Block>, anyhow::Error> {
    Ok(Some(match &*block.opcode {
        "argument_reporter_string_number" => {
            todo!("argument_reporter_string_number")
        }
        "control_for_each" => todo!("control_for_each"),
        "control_forever" => {
            let body = cx.substack(&mut block, "SUBSTACK")?;
            Block::Forever { body: body.into() }
        }
        "control_if" => Block::If {
            condition: cx.input(&mut block, "CONDITION")?,
            then: cx.substack(&mut block, "SUBSTACK")?.into(),
            else_: Sequence::default(),
        },
        "control_if_else" => Block::If {
            condition: cx.input(&mut block, "CONDITION")?,
            then: cx.substack(&mut block, "SUBSTACK")?.into(),
            else_: cx.substack(&mut block, "SUBSTACK2")?.into(),
        },
        "control_repeat" => Block::For {
            variable: None,
            times: cx.input(&mut block, "TIMES")?,
            body: cx.substack(&mut block, "SUBSTACK")?.into(),
        },
        "control_repeat_until" => Block::Until {
            condition: cx.input(&mut block, "CONDITION")?,
            body: cx.substack(&mut block, "SUBSTACK")?.into(),
        },
        "control_stop" => todo!("control_stop"),
        "control_while" => Block::While {
            condition: cx.input(&mut block, "CONDITION")?,
            body: cx.substack(&mut block, "SUBSTACK")?.into(),
        },
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
            return Ok(None);
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
        "operator_add" => Block::Add(
            cx.input(&mut block, "NUM1")?,
            cx.input(&mut block, "NUM2")?,
        ),
        "operator_and" => Block::And(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_divide" => Block::Div(
            cx.input(&mut block, "NUM1")?,
            cx.input(&mut block, "NUM2")?,
        ),
        "operator_equals" => Block::Eq(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_gt" => Block::Gt(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_join" => Block::Join(
            cx.input(&mut block, "STRING1")?,
            cx.input(&mut block, "STRING2")?,
        ),
        "operator_length" => {
            Block::StringLength(cx.input(&mut block, "STRING")?)
        }
        "operator_letter_of" => Block::LetterOf {
            index: cx.input(&mut block, "LETTER")?,
            string: cx.input(&mut block, "STRING")?,
        },
        "operator_lt" => Block::Lt(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_mathop" => {
            let num = cx.input(&mut block, "NUM")?;
            let operator = block
                .fields
                .operator
                .context("missing field: \"OPERATOR\"")?
                .0;
            match &*operator {
                "abs" => Block::Abs(num),
                "floor" => Block::Floor(num),
                "ceiling" => Block::Ceiling(num),
                "sqrt" => Block::Sqrt(num),
                "sin" => Block::Sin(num),
                "cos" => Block::Cos(num),
                "tan" => Block::Tan(num),
                "asin" => Block::Asin(num),
                "acos" => Block::Acos(num),
                "atan" => Block::Atan(num),
                "ln" => Block::Ln(num),
                "log" => Block::Log(num),
                "e ^" => Block::Exp(num),
                "10 ^" => Block::Exp10(num),
                _ => bail!("invalid mathop: {operator:?}"),
            }
        }
        "operator_mod" => Block::Mod(
            cx.input(&mut block, "NUM1")?,
            cx.input(&mut block, "NUM2")?,
        ),
        "operator_multiply" => Block::Mul(
            cx.input(&mut block, "NUM1")?,
            cx.input(&mut block, "NUM2")?,
        ),
        "operator_not" => Block::Not(cx.input(&mut block, "OPERAND")?),
        "operator_or" => Block::Or(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_subtract" => Block::Sub(
            cx.input(&mut block, "NUM1")?,
            cx.input(&mut block, "NUM2")?,
        ),
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
    }))
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
        condition: Expression,
        then: Sequence,
        else_: Sequence,
    },
    For {
        variable: Option<VariableId>,
        times: Expression,
        body: Sequence,
    },
    Forever {
        body: Sequence,
    },
    While {
        condition: Expression,
        body: Sequence,
    },
    Until {
        condition: Expression,
        body: Sequence,
    },

    Add(Expression, Expression),
    Sub(Expression, Expression),
    Mul(Expression, Expression),
    Div(Expression, Expression),
    Mod(Expression, Expression),

    Lt(Expression, Expression),
    Eq(Expression, Expression),
    Gt(Expression, Expression),

    And(Expression, Expression),
    Or(Expression, Expression),
    Not(Expression),

    Join(Expression, Expression),
    StringLength(Expression),
    LetterOf {
        index: Expression,
        string: Expression,
    },

    Abs(Expression),
    Floor(Expression),
    Ceiling(Expression),
    Sqrt(Expression),
    Sin(Expression),
    Cos(Expression),
    Tan(Expression),
    Asin(Expression),
    Acos(Expression),
    Atan(Expression),
    Ln(Expression),
    Log(Expression),
    Exp(Expression),
    Exp10(Expression),
}

enum Expression {
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
    ) -> Result<Expression> {
        let input = block
            .inputs
            .remove(name)
            .with_context(|| format!("missing block input: {name:?}"))?;
        Ok(match input {
            de::Input::Block(block) => Expression::Block(self.block_id(block)),
            de::Input::Number(n) => Expression::Immediate(Immediate::Number(n)),
            de::Input::String(s) => Expression::Immediate(Immediate::String(s)),
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
