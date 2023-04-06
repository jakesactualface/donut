pub mod app;
pub mod object;
pub mod parse;
pub mod token;
pub mod ui;

use std::{
    fs::File,
    io::{BufRead, BufReader, Lines, Result},
    path::Path,
};

fn read_lines<P>(filename: P) -> Result<Lines<BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(BufReader::new(file).lines())
}
