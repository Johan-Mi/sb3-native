use anyhow::Result;
use serde::{de::Visitor, Deserialize};
use std::{collections::HashMap, fs::File, path::Path};

#[derive(Debug, Deserialize)]
pub struct Project {
    targets: Vec<Target>,
}

impl Project {
    pub fn load(path: &Path) -> Result<Self> {
        let file = File::open(path)?;
        let mut zip = zip::read::ZipArchive::new(file)?;
        let project_json = zip.by_name("project.json")?;
        Ok(serde_json::from_reader(project_json)?)
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Target {
    #[serde(default)]
    is_stage: bool,
    name: String,
    variables: HashMap<String, Variable>,
    lists: HashMap<String, List>,
    blocks: HashMap<String, Block>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum RawValue {
    String(String),
    Number(f64),
    Bool(bool),
}

#[derive(Debug, Deserialize)]
struct Variable(String, RawValue);

#[derive(Debug, Deserialize)]
struct List(String, Vec<RawValue>);

#[derive(Debug, Deserialize)]
struct Block {
    opcode: String,
    next: Option<String>,
    #[serde(default)]
    inputs: HashMap<String, Input>,
}

#[derive(Debug)]
enum Input {
    Block(String),
    // Also includes positive numbers, positive integers, integers and angles.
    Number(f64),
    // Also includes colors.
    String(String),
    Broadcast { id: String },
    Variable { id: String },
    List { id: String },
}

impl<'de> Deserialize<'de> for Input {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(InputVisitor)
    }
}

struct InputVisitor;

impl<'de> Visitor<'de> for InputVisitor {
    type Value = Input;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        formatter.write_str("an input")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        _ = seq.next_element::<u8>();
        let input = seq
            .next_element::<InnerInput>()?
            .ok_or_else(|| {
                serde::de::Error::custom("unexpected end of block input")
            })?
            .0;
        _ = seq.next_element::<Input>();
        Ok(input)
    }
}

struct InnerInput(Input);

impl<'de> Deserialize<'de> for InnerInput {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(InnerInputVisitor)
    }
}

struct InnerInputVisitor;

impl<'de> Visitor<'de> for InnerInputVisitor {
    type Value = InnerInput;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        formatter.write_str("an input")
    }

    fn visit_str<E>(self, v: &str) -> std::prelude::v1::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_string(v.to_owned())
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(InnerInput(Input::Block(v)))
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        match seq.next_element::<u8>()? {
            Some(4..=8) => {
                let n = seq
                    .next_element::<NumberOrNumericString>()?
                    .ok_or_else(|| {
                        serde::de::Error::custom(
                            "unexpected end of numeric block input",
                        )
                    })?;
                Ok(InnerInput(Input::Number(n.0)))
            }
            Some(9..=10) => {
                let s = seq.next_element()?.ok_or_else(|| {
                    serde::de::Error::custom(
                        "unexpected end of string block input",
                    )
                })?;
                Ok(InnerInput(Input::String(s)))
            }
            Some(11) => {
                let _name = seq.next_element::<String>()?.ok_or_else(|| {
                    serde::de::Error::custom(
                        "unexpected end of broadcast block input",
                    )
                })?;
                let id = seq.next_element()?.ok_or_else(|| {
                    serde::de::Error::custom(
                        "unexpected end of broadcast block input",
                    )
                })?;
                Ok(InnerInput(Input::Broadcast { id }))
            }
            Some(12) => {
                let _name = seq.next_element::<String>()?.ok_or_else(|| {
                    serde::de::Error::custom(
                        "unexpected end of variable block input",
                    )
                })?;
                let id = seq.next_element()?.ok_or_else(|| {
                    serde::de::Error::custom(
                        "unexpected end of variable block input",
                    )
                })?;
                Ok(InnerInput(Input::Variable { id }))
            }
            Some(13) => {
                let _name = seq.next_element::<String>()?.ok_or_else(|| {
                    serde::de::Error::custom(
                        "unexpected end of list block input",
                    )
                })?;
                let id = seq.next_element()?.ok_or_else(|| {
                    serde::de::Error::custom(
                        "unexpected end of list block input",
                    )
                })?;
                Ok(InnerInput(Input::List { id }))
            }
            _ => Err(serde::de::Error::custom("invalid block input tag")),
        }
    }
}

struct NumberOrNumericString(f64);

impl<'de> Deserialize<'de> for NumberOrNumericString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(NumberOrNumericStringVisitor)
    }
}

struct NumberOrNumericStringVisitor;

impl<'de> Visitor<'de> for NumberOrNumericStringVisitor {
    type Value = NumberOrNumericString;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        formatter.write_str("a number or a numeric string")
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(NumberOrNumericString(v))
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(NumberOrNumericString(v.parse().map_err(|_| {
            serde::de::Error::custom("expected string literal to be numeric")
        })?))
    }
}
