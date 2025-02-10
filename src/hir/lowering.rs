use super::{
    BasicBlock, Hat, HatKind, Immediate, List, Op, Parameter, Project, Target, Value, Variable,
};
use crate::de;
use anyhow::{bail, Context, Result};
use beach_map::{BeachMap, Id};
use std::collections::HashMap;

impl Project {
    pub fn lower(de: de::Project) -> Result<Self> {
        let mut cx = LoweringContext::new(&de);

        let mut nexts = HashMap::new();
        let predecessors = de
            .targets
            .iter()
            .flat_map(|it| &it.blocks)
            .filter_map(|(id, block)| {
                let id = cx.op_ids[id];
                let p = block
                    .inputs
                    .values()
                    .filter_map(|it| match it {
                        de::Input::Block(input) => Some(cx.op_ids[input]),
                        de::Input::Variable(id) => {
                            let variable_op_id = cx.ops.insert(Op::Variable(cx.variable_ids[id]));
                            assert!(cx.pseudos.insert(it, variable_op_id).is_none());
                            Some(variable_op_id)
                        }
                        de::Input::List(id) => {
                            let list_op_id = cx.ops.insert(Op::List(cx.list_ids[id]));
                            assert!(cx.pseudos.insert(it, list_op_id).is_none());
                            Some(list_op_id)
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                if let Some(next) = &block.next {
                    assert!(nexts.insert(id, cx.op_ids[next]).is_none());
                }
                (!p.is_empty()).then_some((id, p))
            })
            .collect::<HashMap<_, _>>();

        if std::env::var_os("DUMP_HIR_CFG").is_some() {
            eprintln!("{predecessors:#?}");
            eprintln!("{nexts:#?}");
        }

        let mut hats = Vec::new();
        let mut basic_blocks = BeachMap::new();

        for (target_id, target) in de.targets.into_iter().enumerate() {
            for (id, block) in target.blocks {
                let id = cx.op_ids[&id];
                if let Some(op) =
                    lower_block(block, &mut hats, Target(target_id), &mut basic_blocks, &cx)?
                {
                    cx.ops[id] = op;
                }
            }
        }

        for basic_block in &mut basic_blocks {
            fill_basic_block(basic_block, &predecessors, &nexts);
        }

        Ok(Self {
            hats,
            basic_blocks,
            ops: cx.ops,
        })
    }
}

struct LoweringContext {
    ops: BeachMap<Op>,
    op_ids: HashMap<de::BlockId, Id<Op>>,
    variable_ids: HashMap<de::VariableId, Id<Variable>>,
    list_ids: HashMap<de::ListId, Id<List>>,
    parameter_ids: HashMap<String, Id<Parameter>>,
    pseudos: HashMap<*const de::Input, Id<Op>>,
}

impl LoweringContext {
    fn new(de: &de::Project) -> Self {
        let mut ops = BeachMap::new();
        let mut variables = BeachMap::new();
        let mut lists = BeachMap::new();
        let mut parameters = BeachMap::new();

        Self {
            op_ids: de
                .targets
                .iter()
                .flat_map(|it| it.blocks.keys())
                .map(|it| (it.clone(), ops.insert(Op::StopAll))) // Dummy op
                .collect(),
            ops,
            variable_ids: de
                .targets
                .iter()
                .flat_map(|it| it.variables.keys())
                .map(|it| (it.clone(), variables.insert(Variable)))
                .collect(),
            list_ids: de
                .targets
                .iter()
                .flat_map(|it| it.lists.keys())
                .map(|it| (it.clone(), lists.insert(List)))
                .collect(),
            parameter_ids: de
                .targets
                .iter()
                .flat_map(|it| it.blocks.values())
                .filter(|it| it.opcode == "procedures_prototype")
                .flat_map(|it| it.inputs.keys())
                .map(|it| (it.clone(), parameters.insert(Parameter)))
                .collect(),
            pseudos: HashMap::new(),
        }
    }

    fn input(&self, block: &mut de::Block, name: &str) -> Result<Value> {
        let ptr = block
            .inputs
            .get(name)
            .with_context(|| format!("missing block input: {name:?}"))? as _;
        let input = block.inputs.remove(name).unwrap();
        Ok(self.just_input(input, ptr))
    }

    fn just_input(&self, input: de::Input, ptr: *const de::Input) -> Value {
        match input {
            de::Input::Block(block) => Value::Op(self.op_ids[&block]),
            de::Input::Number(n) => Value::Immediate(Immediate::Number(n)),
            de::Input::String(s) => Value::Immediate(Immediate::String(s)),
            de::Input::Broadcast(_) => todo!(),
            de::Input::Variable(_) | de::Input::List(_) => Value::Op(self.pseudos[&ptr]),
        }
    }

    fn substack(&self, block: &mut de::Block, name: &str) -> Result<BasicBlock> {
        match block.inputs.remove(name) {
            None => bail!("missing substack: {name:?}"),
            Some(de::Input::Block(block)) => Ok(self.op_ids[&block].into()),
            Some(_) => bail!("substack {name:?} must be a block ID"),
        }
    }
}

fn fill_basic_block(
    basic_block: &mut BasicBlock,
    predecessors: &HashMap<Id<Op>, Vec<Id<Op>>>,
    nexts: &HashMap<Id<Op>, Id<Op>>,
) {
    let mut next = basic_block.ops.pop();
    while let Some(op) = next {
        append_predecessors(basic_block, op, predecessors);
        next = nexts.get(&op).copied();
    }
}

fn append_predecessors(
    basic_block: &mut BasicBlock,
    op: Id<Op>,
    predecessors: &HashMap<Id<Op>, Vec<Id<Op>>>,
) {
    for &predecessor in predecessors.get(&op).into_iter().flatten() {
        append_predecessors(basic_block, predecessor, predecessors);
    }
    basic_block.ops.push(op);
}

fn lower_block(
    mut block: de::Block,
    hats: &mut Vec<Hat>,
    owner: Target,
    basic_blocks: &mut BeachMap<BasicBlock>,
    cx: &LoweringContext,
) -> Result<Option<Op>, anyhow::Error> {
    Ok(Some(match &*block.opcode {
        "argument_reporter_string_number" => todo!("look up parameters by user-visible name"),
        "control_for_each" => {
            let times = cx.input(&mut block, "VALUE")?;
            let body = basic_blocks.insert(cx.substack(&mut block, "SUBSTACK")?);
            Op::For {
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
            let body = basic_blocks.insert(cx.substack(&mut block, "SUBSTACK")?);
            Op::Forever { body }
        }
        "control_if" => Op::If {
            condition: cx.input(&mut block, "CONDITION")?,
            then: basic_blocks.insert(cx.substack(&mut block, "SUBSTACK")?),
            else_: basic_blocks.insert(BasicBlock::default()),
        },
        "control_if_else" => Op::If {
            condition: cx.input(&mut block, "CONDITION")?,
            then: basic_blocks.insert(cx.substack(&mut block, "SUBSTACK")?),
            else_: basic_blocks.insert(cx.substack(&mut block, "SUBSTACK2")?),
        },
        "control_repeat" => Op::For {
            variable: None,
            times: cx.input(&mut block, "TIMES")?,
            body: basic_blocks.insert(cx.substack(&mut block, "SUBSTACK")?),
        },
        "control_repeat_until" => Op::Until {
            condition: cx.input(&mut block, "CONDITION")?,
            body: basic_blocks.insert(cx.substack(&mut block, "SUBSTACK")?),
        },
        "control_stop" => {
            let stop_option = &*block
                .fields
                .stop_option
                .context("missing field: \"STOP_OPTION\"")?
                .0;
            match stop_option {
                "all" => Op::StopAll,
                "other scripts in sprite" | "other scripts in stage" => {
                    Op::StopOtherScriptsInSprite
                }
                "this script" => Op::StopThisScript,
                _ => bail!("invalid stop option: {stop_option:?}"),
            }
        }
        "control_while" => Op::While {
            condition: cx.input(&mut block, "CONDITION")?,
            body: basic_blocks.insert(cx.substack(&mut block, "SUBSTACK")?),
        },
        "data_addtolist" => {
            let value = cx.input(&mut block, "ITEM")?;
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Op::AddToList { list, value }
        }
        "data_changevariableby" => {
            let by = cx.input(&mut block, "VALUE")?;
            let variable = cx.variable_ids[&block
                .fields
                .variable
                .context("missing field: \"VARIABLE\"")?
                .1];
            Op::ChangeVariable { variable, by }
        }
        "data_deletealloflist" => {
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Op::DeleteAllOfList(list)
        }
        "data_deleteoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Op::DeleteItemOfList { list, index }
        }
        "data_itemoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Op::ItemOfList { list, index }
        }
        "data_lengthoflist" => {
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Op::LengthOfList(list)
        }
        "data_replaceitemoflist" => {
            let index = cx.input(&mut block, "INDEX")?;
            let value = cx.input(&mut block, "ITEM")?;
            let list = cx.list_ids[&block.fields.list.context("missing field: \"LIST\"")?.1];
            Op::ReplaceItemOfList { list, index, value }
        }
        "data_setvariableto" => {
            let to = cx.input(&mut block, "VALUE")?;
            let variable = cx.variable_ids[&block
                .fields
                .variable
                .context("missing field: \"VARIABLE\"")?
                .1];
            Op::SetVariable { variable, to }
        }
        "event_broadcastandwait" => Op::BroadcastAndWait(cx.input(&mut block, "BROADCAST_INPUT")?),
        "event_whenbroadcastreceived" => {
            let broadcast_name = block
                .fields
                .broadcast_option
                .context("missing field: \"BROADCAST_OPTION\"")?
                .0;
            hats.push(Hat {
                owner,
                kind: HatKind::WhenReceived { broadcast_name },
                body: basic_blocks.insert(BasicBlock::from(block.next.map(|it| cx.op_ids[&it]))),
            });
            return Ok(None);
        }
        "event_whenflagclicked" => {
            hats.push(Hat {
                owner,
                kind: HatKind::WhenFlagClicked,
                body: basic_blocks.insert(BasicBlock::from(block.next.map(|it| cx.op_ids[&it]))),
            });
            return Ok(None);
        }
        "looks_hide" => Op::Hide,
        "looks_setsizeto" => Op::SetSize {
            to: cx.input(&mut block, "SIZE")?,
        },
        "looks_switchcostumeto" => Op::SetCostume {
            to: cx.input(&mut block, "COSTUME")?,
        },
        "motion_changexby" => Op::ChangeX {
            by: cx.input(&mut block, "DX")?,
        },
        "motion_changeyby" => Op::ChangeY {
            by: cx.input(&mut block, "DY")?,
        },
        "motion_gotoxy" => Op::GoToXY {
            x: cx.input(&mut block, "X")?,
            y: cx.input(&mut block, "Y")?,
        },
        "motion_setx" => Op::SetX {
            to: cx.input(&mut block, "X")?,
        },
        "motion_xposition" => Op::XPosition,
        "operator_add" => Op::Add(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?),
        "operator_and" => Op::And(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_divide" => Op::Div(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?),
        "operator_equals" => Op::Eq(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_gt" => Op::Gt(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_join" => Op::Join(
            cx.input(&mut block, "STRING1")?,
            cx.input(&mut block, "STRING2")?,
        ),
        "operator_length" => Op::StringLength(cx.input(&mut block, "STRING")?),
        "operator_letter_of" => Op::LetterOf {
            index: cx.input(&mut block, "LETTER")?,
            string: cx.input(&mut block, "STRING")?,
        },
        "operator_lt" => Op::Lt(
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
                "abs" => Op::Abs(num),
                "floor" => Op::Floor(num),
                "ceiling" => Op::Ceiling(num),
                "sqrt" => Op::Sqrt(num),
                "sin" => Op::Sin(num),
                "cos" => Op::Cos(num),
                "tan" => Op::Tan(num),
                "asin" => Op::Asin(num),
                "acos" => Op::Acos(num),
                "atan" => Op::Atan(num),
                "ln" => Op::Ln(num),
                "log" => Op::Log(num),
                "e ^" => Op::Exp(num),
                "10 ^" => Op::Exp10(num),
                _ => bail!("invalid mathop: {operator:?}"),
            }
        }
        "operator_mod" => Op::Mod(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?),
        "operator_multiply" => {
            Op::Mul(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?)
        }
        "operator_not" => Op::Not(cx.input(&mut block, "OPERAND")?),
        "operator_or" => Op::Or(
            cx.input(&mut block, "OPERAND1")?,
            cx.input(&mut block, "OPERAND2")?,
        ),
        "operator_subtract" => {
            Op::Sub(cx.input(&mut block, "NUM1")?, cx.input(&mut block, "NUM2")?)
        }
        "pen_clear" => Op::PenClear,
        "pen_stamp" => Op::PenStamp,
        "procedures_call" => Op::CallProcedure {
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
            hats.push(Hat {
                owner,
                kind: HatKind::Procedure,
                body: basic_blocks.insert(BasicBlock::from(block.next.map(|it| cx.op_ids[&it]))),
            });
            return Ok(None);
        }
        "procedures_prototype" => return Ok(None),
        "sensing_answer" => Op::Answer,
        "sensing_askandwait" => Op::Ask(cx.input(&mut block, "QUESTION")?),
        opcode => bail!("invalid opcode: {opcode:?}"),
    }))
}
