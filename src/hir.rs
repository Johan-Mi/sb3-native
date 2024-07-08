use crate::de;
use anyhow::{bail, Context, Result};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt,
    num::NonZeroU32,
};

pub use de::RawValue as Immediate;

#[derive(Debug)]
pub struct Project {
    targets: Vec<Target>,
    hats: BTreeMap<BlockId, Hat>,
    blocks: BTreeMap<BlockId, Block>,
}

impl Project {
    pub fn lower(de: de::Project) -> Result<Self> {
        let mut cx = LoweringContext::default();

        let mut nexts = BTreeMap::new();
        let predecessors = de
            .targets
            .iter()
            .flat_map(|it| &it.blocks)
            .filter_map(|(id, block)| {
                let id = cx.block_id(id.clone());
                let p = block
                    .inputs
                    .values()
                    .filter_map(|it| {
                        if let de::Input::Block(input) = it {
                            Some(cx.block_id(input.clone()))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                if let Some(next) = &block.next {
                    nexts.insert(id, cx.block_id(next.clone()));
                }
                (!p.is_empty()).then_some((id, p))
            })
            .collect::<BTreeMap<_, _>>();

        if std::env::var_os("DUMP_HIR_CFG").is_some() {
            eprintln!("{predecessors:#?}");
            eprintln!("{nexts:#?}");
        }

        let mut hats = BTreeMap::new();

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
                        cx.blocks.insert(id, block);
                    }
                }

                Ok(Target { hats: my_hats })
            })
            .collect::<Result<_>>()?;

        for block in cx.blocks.values_mut() {
            match block {
                Block::If { then, else_, .. } => {
                    fill_sequence(then, &predecessors, &nexts);
                    fill_sequence(else_, &predecessors, &nexts);
                }
                Block::For { body, .. }
                | Block::Forever { body }
                | Block::While { body, .. }
                | Block::Until { body, .. } => {
                    fill_sequence(body, &predecessors, &nexts);
                }
                _ => {}
            }
        }

        Ok(Self {
            targets,
            hats,
            blocks: cx.blocks,
        })
    }
}

fn fill_sequence(
    sequence: &mut Sequence,
    predecessors: &BTreeMap<BlockId, Vec<BlockId>>,
    nexts: &BTreeMap<BlockId, BlockId>,
) {
    let mut next = sequence.blocks.pop();
    while let Some(block) = next {
        append_predecessors(sequence, block, predecessors);
        next = nexts.get(&block).copied();
    }
}

fn append_predecessors(
    sequence: &mut Sequence,
    block: BlockId,
    predecessors: &BTreeMap<BlockId, Vec<BlockId>>,
) {
    for &predecessor in predecessors.get(&block).into_iter().flatten() {
        append_predecessors(sequence, predecessor, predecessors);
    }
    sequence.blocks.push(block);
}

fn lower_block(
    mut block: de::Block,
    id: BlockId,
    hats: &mut BTreeMap<BlockId, Hat>,
    my_hats: &mut BTreeSet<BlockId>,
    cx: &mut LoweringContext,
) -> Result<Option<Block>, anyhow::Error> {
    Ok(Some(match &*block.opcode {
        "argument_reporter_string_number" => Block::Parameter,
        "control_for_each" => {
            let times = cx.input(&mut block, "VALUE")?;
            let body = cx.substack(&mut block, "SUBSTACK")?;
            Block::For {
                variable: Some(
                    cx.variable_id(
                        block
                            .fields
                            .variable
                            .context("missing field: \"VARIABLE\"")?
                            .1,
                    ),
                ),
                times,
                body,
            }
        }
        "control_forever" => {
            let body = cx.substack(&mut block, "SUBSTACK")?;
            Block::Forever { body }
        }
        "control_if" => Block::If {
            condition: cx.input(&mut block, "CONDITION")?,
            then: cx.substack(&mut block, "SUBSTACK")?,
            else_: Sequence::default(),
        },
        "control_if_else" => Block::If {
            condition: cx.input(&mut block, "CONDITION")?,
            then: cx.substack(&mut block, "SUBSTACK")?,
            else_: cx.substack(&mut block, "SUBSTACK2")?,
        },
        "control_repeat" => Block::For {
            variable: None,
            times: cx.input(&mut block, "TIMES")?,
            body: cx.substack(&mut block, "SUBSTACK")?,
        },
        "control_repeat_until" => Block::Until {
            condition: cx.input(&mut block, "CONDITION")?,
            body: cx.substack(&mut block, "SUBSTACK")?,
        },
        "control_stop" => {
            let stop_option = &*block
                .fields
                .stop_option
                .context("missing field: \"STOP_OPTION\"")?
                .0;
            match stop_option {
                "all" => Block::StopAll,
                "other scripts in sprite" | "other scripts in stage" => {
                    Block::StopOtherScriptsInSprite
                }
                "this script" => Block::StopThisScript,
                _ => bail!("invalid stop option: {stop_option:?}"),
            }
        }
        "control_while" => Block::While {
            condition: cx.input(&mut block, "CONDITION")?,
            body: cx.substack(&mut block, "SUBSTACK")?,
        },
        "data_addtolist" => {
            let value = cx.input(&mut block, "ITEM")?;
            let list = cx.list_id(
                block.fields.list.context("missing field: \"LIST\"")?.1,
            );
            Block::AddToList { list, value }
        }
        "data_changevariableby" => {
            let by = cx.input(&mut block, "VALUE")?;
            let variable = cx.variable_id(
                block
                    .fields
                    .variable
                    .context("missing field: \"VARIABLE\"")?
                    .1,
            );
            Block::ChangeVariable { variable, by }
        }
        "data_deletealloflist" => {
            let list = cx.list_id(
                block.fields.list.context("missing field: \"LIST\"")?.1,
            );
            Block::DeleteAllOfList(list)
        }
        "data_deleteoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let list = cx.list_id(
                block.fields.list.context("missing field: \"LIST\"")?.1,
            );
            Block::DeleteItemOfList { list, index }
        }
        "data_itemoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let list = cx.list_id(
                block.fields.list.context("missing field: \"LIST\"")?.1,
            );
            Block::ItemOfList { list, index }
        }
        "data_lengthoflist" => {
            let list = cx.list_id(
                block.fields.list.context("missing field: \"LIST\"")?.1,
            );
            Block::LengthOfList(list)
        }
        "data_replaceitemoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let value = cx.input(&mut block, "ITEM")?;
            let list = cx.list_id(
                block.fields.list.context("missing field: \"LIST\"")?.1,
            );
            Block::ReplaceItemOfList { list, index, value }
        }
        "data_setvariableto" => {
            let to = cx.input(&mut block, "VALUE")?;
            let variable = cx.variable_id(
                block
                    .fields
                    .variable
                    .context("missing field: \"VARIABLE\"")?
                    .1,
            );
            Block::SetVariable { variable, to }
        }
        "event_broadcastandwait" => {
            Block::BroadcastAndWait(cx.input(&mut block, "BROADCAST_INPUT")?)
        }
        "event_whenbroadcastreceived" => {
            let broadcast_name = block
                .fields
                .broadcast_option
                .context("missing field: \"BROADCAST_OPTION\"")?
                .0;
            hats.insert(
                id,
                Hat {
                    kind: HatKind::WhenReceived { broadcast_name },
                    body: Sequence::default(),
                },
            );
            my_hats.insert(id);
            return Ok(None);
        }
        "event_whenflagclicked" => {
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
        "looks_hide" => Block::Hide,
        "looks_setsizeto" => Block::SetSize {
            to: cx.input(&mut block, "SIZE")?,
        },
        "looks_switchcostumeto" => Block::SetCostume {
            to: cx.input(&mut block, "COSTUME")?,
        },
        "motion_changexby" => Block::ChangeX {
            by: cx.input(&mut block, "DX")?,
        },
        "motion_changeyby" => Block::ChangeY {
            by: cx.input(&mut block, "DY")?,
        },
        "motion_gotoxy" => Block::GoToXY {
            x: cx.input(&mut block, "X")?,
            y: cx.input(&mut block, "Y")?,
        },
        "motion_setx" => Block::SetX {
            to: cx.input(&mut block, "X")?,
        },
        "motion_xposition" => Block::XPosition,
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
        "pen_clear" => Block::PenClear,
        "pen_stamp" => Block::PenStamp,
        "procedures_call" => Block::CallProcedure,
        "procedures_definition" => {
            hats.insert(
                id,
                Hat {
                    kind: HatKind::Procedure,
                    body: Sequence::default(),
                },
            );
            my_hats.insert(id);
            return Ok(None);
        }
        "procedures_prototype" => return Ok(None),
        "sensing_answer" => Block::Answer,
        "sensing_askandwait" => Block::Ask(cx.input(&mut block, "QUESTION")?),
        opcode => bail!("invalid opcode: {opcode:?}"),
    }))
}

#[derive(Debug)]
struct Target {
    hats: BTreeSet<BlockId>,
}

#[derive(Debug)]
struct Hat {
    kind: HatKind,
    body: Sequence,
}

#[derive(Debug)]
enum HatKind {
    WhenFlagClicked,
    WhenReceived { broadcast_name: String },
    Procedure,
}

#[derive(Debug, Default)]
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

#[derive(Debug)]
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

    CallProcedure,
    Parameter,

    StopAll,
    StopOtherScriptsInSprite,
    StopThisScript,

    BroadcastAndWait(Expression),

    Variable(VariableId),
    SetVariable {
        variable: VariableId,
        to: Expression,
    },
    ChangeVariable {
        variable: VariableId,
        by: Expression,
    },
    List(ListId),
    AddToList {
        list: ListId,
        value: Expression,
    },
    DeleteAllOfList(ListId),
    DeleteItemOfList {
        list: ListId,
        index: Expression,
    },
    ItemOfList {
        list: ListId,
        index: Expression,
    },
    LengthOfList(ListId),
    ReplaceItemOfList {
        list: ListId,
        index: Expression,
        value: Expression,
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

    Answer,
    XPosition,

    Ask(Expression),
    ChangeX {
        by: Expression,
    },
    ChangeY {
        by: Expression,
    },
    GoToXY {
        x: Expression,
        y: Expression,
    },
    Hide,
    PenClear,
    PenStamp,
    SetCostume {
        to: Expression,
    },
    SetSize {
        to: Expression,
    },
    SetX {
        to: Expression,
    },
}

enum Expression {
    Block(BlockId),
    Immediate(Immediate),
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Block(block) => block.fmt(f),
            Self::Immediate(immediate) => immediate.fmt(f),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BlockId(NonZeroU32);

impl fmt::Debug for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{}", self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct VariableId(NonZeroU32);

impl fmt::Debug for VariableId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ListId(NonZeroU32);

impl fmt::Debug for ListId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "l{}", self.0)
    }
}

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

    fn new_variable_id(&mut self) -> VariableId {
        VariableId(self.new_raw())
    }

    fn new_list_id(&mut self) -> ListId {
        ListId(self.new_raw())
    }
}

#[derive(Default)]
struct LoweringContext {
    generator: Generator,
    blocks: BTreeMap<BlockId, Block>,
    block_ids: HashMap<de::BlockId, BlockId>,
    variable_ids: HashMap<de::VariableId, VariableId>,
    list_ids: HashMap<de::ListId, ListId>,
}

impl LoweringContext {
    fn block_id(&mut self, id: de::BlockId) -> BlockId {
        *self
            .block_ids
            .entry(id)
            .or_insert_with(|| self.generator.new_block_id())
    }

    fn variable_id(&mut self, id: de::VariableId) -> VariableId {
        *self
            .variable_ids
            .entry(id)
            .or_insert_with(|| self.generator.new_variable_id())
    }

    fn list_id(&mut self, id: de::ListId) -> ListId {
        *self
            .list_ids
            .entry(id)
            .or_insert_with(|| self.generator.new_list_id())
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
            de::Input::Variable(id) => {
                let variable_block_id = self.generator.new_block_id();
                let variable_id = self.variable_id(id);
                self.blocks
                    .insert(variable_block_id, Block::Variable(variable_id));
                Expression::Block(variable_block_id)
            }
            de::Input::List(id) => {
                let list_block_id = self.generator.new_block_id();
                let list_id = self.list_id(id);
                self.blocks.insert(list_block_id, Block::List(list_id));
                Expression::Block(list_block_id)
            }
        })
    }

    fn substack(
        &mut self,
        block: &mut de::Block,
        name: &str,
    ) -> Result<Sequence> {
        match block.inputs.remove(name) {
            None => bail!("missing substack: {name:?}"),
            Some(de::Input::Block(block)) => Ok(self.block_id(block).into()),
            Some(_) => bail!("substack {name:?} must be a block ID"),
        }
    }
}
