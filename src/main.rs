use std::{
    fs::File,
    io::{BufReader, Read},
};

use anyhow::Result;
use clap::Parser;
use lexer::tokens;
use parser::parse_file;

mod common;
mod lexer;
mod parser;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    file: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let file = File::open(&cli.file)?;
    let reader = BufReader::new(file);
    let iter = reader
        .bytes()
        .map(|x| x.expect("invalid byte in file") as char);

    let program = parse_file(tokens(iter, &cli.file));
    println!("{:#?}", program);

    return Ok(());
}
