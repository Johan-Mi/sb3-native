use crate::de;
use anyhow::{bail, Context, Result};
use slotmap::{SecondaryMap, SlotMap};
use std::{collections::HashMap, fmt};

pub use de::RawValue as Immediate;

#[derive(Debug)]
pub struct Project {
    targets: Vec<Target>,
    pub hats: SlotMap<HatId, Hat>,
    pub blocks: SlotMap<BlockId, Block>,
}

impl Project {
    pub fn lower(de: de::Project) -> Result<Self> {
        let mut cx = LoweringContext::new(&de);

        let mut nexts = SecondaryMap::new();
        let predecessors = de
            .targets
            .iter()
            .flat_map(|it| &it.blocks)
            .filter_map(|(id, block)| {
                let id = cx.block_ids[id];
                let p = block
                    .inputs
                    .values()
                    .filter_map(|it| match it {
                        de::Input::Block(input) => Some(cx.block_ids[input]),
                        de::Input::Variable(id) => {
                            let variable_block_id =
                                cx.blocks.insert(Block::Variable(cx.variable_ids[id]));
                            assert!(cx.pseudos.insert(it, variable_block_id).is_none());
                            Some(variable_block_id)
                        }
                        de::Input::List(id) => {
                            let list_block_id = cx.blocks.insert(Block::List(cx.list_ids[id]));
                            assert!(cx.pseudos.insert(it, list_block_id).is_none());
                            Some(list_block_id)
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                if let Some(next) = &block.next {
                    assert!(nexts.insert(id, cx.block_ids[next]).is_none());
                }
                (!p.is_empty()).then_some((id, p))
            })
            .collect::<SecondaryMap<_, _>>();

        if std::env::var_os("DUMP_HIR_CFG").is_some() {
            eprintln!("{predecessors:#?}");
            eprintln!("{nexts:#?}");
        }

        let mut hats = SlotMap::with_key();

        let targets = de
            .targets
            .into_iter()
            .map(|target| {
                let mut my_hats = SecondaryMap::new();

                for (id, block) in target.blocks {
                    let id = cx.block_ids[&id];
                    if let Some(block) = lower_block(block, &mut hats, &mut my_hats, &cx)? {
                        cx.blocks[id] = block;
                    }
                }

                Ok(Target { hats: my_hats })
            })
            .collect::<Result<_>>()?;

        for hat in hats.values_mut() {
            fill_sequence(&mut hat.body, &predecessors, &nexts);
        }

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

impl LoweringContext {
    fn new(de: &de::Project) -> Self {
        let mut blocks = SlotMap::with_key();
        let mut variables = SlotMap::with_key();
        let mut lists = SlotMap::with_key();
        let mut parameters = SlotMap::with_key();

        Self {
            block_ids: de
                .targets
                .iter()
                .flat_map(|it| it.blocks.keys())
                .map(|it| (it.clone(), blocks.insert(Block::StopAll))) // Dummy block
                .collect(),
            blocks,
            variable_ids: de
                .targets
                .iter()
                .flat_map(|it| it.variables.keys())
                .map(|it| (it.clone(), variables.insert(())))
                .collect(),
            list_ids: de
                .targets
                .iter()
                .flat_map(|it| it.lists.keys())
                .map(|it| (it.clone(), lists.insert(())))
                .collect(),
            parameter_ids: de
                .targets
                .iter()
                .flat_map(|it| it.blocks.values())
                .filter(|it| it.opcode == "procedures_prototype")
                .flat_map(|it| it.inputs.keys())
                .map(|it| (it.clone(), parameters.insert(())))
                .collect(),
            pseudos: HashMap::new(),
        }
    }
}

fn fill_sequence(
    sequence: &mut Sequence,
    predecessors: &SecondaryMap<BlockId, Vec<BlockId>>,
    nexts: &SecondaryMap<BlockId, BlockId>,
) {
    let mut next = sequence.blocks.pop();
    while let Some(block) = next {
        append_predecessors(sequence, block, predecessors);
        next = nexts.get(block).copied();
    }
}

fn append_predecessors(
    sequence: &mut Sequence,
    block: BlockId,
    predecessors: &SecondaryMap<BlockId, Vec<BlockId>>,
) {
    for &predecessor in predecessors.get(block).into_iter().flatten() {
        append_predecessors(sequence, predecessor, predecessors);
    }
    sequence.blocks.push(block);
}

fn lower_block(
    mut block: de::Block,
    hats: &mut SlotMap<HatId, Hat>,
    my_hats: &mut SecondaryMap<HatId, ()>,
    cx: &LoweringContext,
) -> Result<Option<Block>, anyhow::Error> {
    Ok(Some(match &*block.opcode {
        "argument_reporter_string_number" => todo!("look up parameters by user-visible name"),
        "control_for_each" => {
            let times = cx.input(&mut block, "VALUE")?;
            let body = cx.substack(&mut block, "SUBSTACK")?;
            Block::For {
                variable: Some(
                    cx.variable_ids[&block
                        .fields
                        .variable
                        .context("missing field: \"VARIABLE\"")?
                        .1],
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
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Block::AddToList { list, value }
        }
        "data_changevariableby" => {
            let by = cx.input(&mut block, "VALUE")?;
            let variable = cx.variable_ids[&block
                .fields
                .variable
                .context("missing field: \"VARIABLE\"")?
                .1];
            Block::ChangeVariable { variable, by }
        }
        "data_deletealloflist" => {
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Block::DeleteAllOfList(list)
        }
        "data_deleteoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Block::DeleteItemOfList { list, index }
        }
        "data_itemoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Block::ItemOfList { list, index }
        }
        "data_lengthoflist" => {
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Block::LengthOfList(list)
        }
        "data_replaceitemoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let value = cx.input(&mut block, "ITEM")?;
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Block::ReplaceItemOfList { list, index, value }
        }
        "data_setvariableto" => {
            let to = cx.input(&mut block, "VALUE")?;
            let variable = cx.variable_ids[&block
                .fields
                .variable
                .context("missing field: \"VARIABLE\"")?
                .1];
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
            let hat = hats.insert(Hat {
                kind: HatKind::WhenReceived { broadcast_name },
                body: Sequence::from(block.next.map(|it| cx.block_ids[&it])),
            });
            assert!(my_hats.insert(hat, ()).is_none());
            return Ok(None);
        }
        "event_whenflagclicked" => {
            let hat = hats.insert(Hat {
                kind: HatKind::WhenFlagClicked,
                body: Sequence::from(block.next.map(|it| cx.block_ids[&it])),
            });
            assert!(my_hats.insert(hat, ()).is_none());
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
        "operator_add" => Block::Add(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?),
        "operator_and" => Block::And(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_divide" => {
            Block::Div(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?)
        }
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
        "operator_length" => Block::StringLength(cx.input(&mut block, "STRING")?),
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
        "operator_mod" => Block::Mod(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?),
        "operator_multiply" => {
            Block::Mul(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?)
        }
        "operator_not" => Block::Not(cx.input(&mut block, "OPERAND")?),
        "operator_or" => Block::Or(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_subtract" => {
            Block::Sub(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?)
        }
        "pen_clear" => Block::PenClear,
        "pen_stamp" => Block::PenStamp,
        "procedures_call" => Block::CallProcedure {
            arguments: block
                .inputs
                .iter()
                .map(|(id, argument)| {
                    (
                        cx.parameter_ids[id],
                        cx.just_input(argument.clone(), argument),
                    )
                })
                .collect(),
        },
        "procedures_definition" => {
            let hat = hats.insert(Hat {
                kind: HatKind::Procedure,
                body: Sequence::from(block.next.map(|it| cx.block_ids[&it])),
            });
            assert!(my_hats.insert(hat, ()).is_none());
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
    hats: SecondaryMap<HatId, ()>,
}

#[derive(Debug)]
pub struct Hat {
    kind: HatKind,
    pub body: Sequence,
}

#[derive(Debug)]
enum HatKind {
    WhenFlagClicked,
    WhenReceived { broadcast_name: String },
    Procedure,
}

#[derive(Debug, Default)]
pub struct Sequence {
    pub blocks: Vec<BlockId>,
}

impl From<BlockId> for Sequence {
    fn from(value: BlockId) -> Self {
        Self {
            blocks: Vec::from([value]),
        }
    }
}

impl From<Option<BlockId>> for Sequence {
    fn from(value: Option<BlockId>) -> Self {
        value.map_or_else(Self::default, Self::from)
    }
}

#[derive(Debug)]
pub enum Block {
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

    CallProcedure {
        arguments: SecondaryMap<ParameterId, Expression>,
    },
    Parameter(ParameterId),

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

pub enum Expression {
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

slotmap::new_key_type! {
    pub struct HatId;

    pub struct BlockId;

    pub struct VariableId;

    pub struct ListId;

    pub struct ParameterId;
}

struct LoweringContext {
    blocks: SlotMap<BlockId, Block>,
    block_ids: HashMap<de::BlockId, BlockId>,
    variable_ids: HashMap<de::VariableId, VariableId>,
    list_ids: HashMap<de::ListId, ListId>,
    parameter_ids: HashMap<String, ParameterId>,
    pseudos: HashMap<*const de::Input, BlockId>,
}

impl LoweringContext {
    fn input(&self, block: &mut de::Block, name: &str) -> Result<Expression> {
        let ptr = block
            .inputs
            .get(name)
            .with_context(|| format!("missing block input: {name:?}"))? as _;
        let input = block.inputs.remove(name).unwrap();
        Ok(self.just_input(input, ptr))
    }

    fn just_input(&self, input: de::Input, ptr: *const de::Input) -> Expression {
        match input {
            de::Input::Block(block) => Expression::Block(self.block_ids[&block]),
            de::Input::Number(n) => Expression::Immediate(Immediate::Number(n)),
            de::Input::String(s) => Expression::Immediate(Immediate::String(s)),
            de::Input::Broadcast(_) => todo!(),
            de::Input::Variable(_) | de::Input::List(_) => Expression::Block(self.pseudos[&ptr]),
        }
    }

    fn substack(&self, block: &mut de::Block, name: &str) -> Result<Sequence> {
        match block.inputs.remove(name) {
            None => bail!("missing substack: {name:?}"),
            Some(de::Input::Block(block)) => Ok(self.block_ids[&block].into()),
            Some(_) => bail!("substack {name:?} must be a block ID"),
        }
    }
}
