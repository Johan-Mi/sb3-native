use super::{ListId, VariableId};
use serde::Deserialize;

#[derive(Deserialize, Debug)]
pub struct Variable(String, VariableId);
#[derive(Deserialize, Debug)]
pub struct List(String, ListId);
#[derive(Deserialize, Debug)]
pub struct Value(String, ());
#[derive(Deserialize, Debug)]
pub struct Operator(pub String, ());
#[derive(Deserialize, Debug)]
pub struct KeyOption(String, ());
#[derive(Deserialize, Debug)]
pub struct BroadcastOption(String, ());
#[derive(Deserialize, Debug)]
pub struct StopOption(String, ());
#[derive(Deserialize, Debug)]
pub struct CloneOption(String, ());
