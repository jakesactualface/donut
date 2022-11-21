#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(usize),
    Boolean(bool),
    Null,
}
