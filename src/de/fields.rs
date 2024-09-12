#![allow(
    clippy::manual_non_exhaustive,
    reason = "unit fields are required because the schema contains `null`s"
)]

use super::{ListId, VariableId};
use serde::Deserialize;

#[derive(Deserialize, Debug)]
pub struct Variable(String, pub VariableId);
#[derive(Deserialize, Debug)]
pub struct List(String, pub ListId);
#[derive(Deserialize, Debug)]
pub struct Value(pub String, ());
#[derive(Deserialize, Debug)]
pub struct Operator(pub String, ());
#[derive(Deserialize, Debug)]
pub struct KeyOption(String, ());
#[derive(Deserialize, Debug)]
pub struct BroadcastOption(pub String, ());
#[derive(Deserialize, Debug)]
pub struct StopOption(pub String, ());
#[derive(Deserialize, Debug)]
pub struct CloneOption(String, ());
