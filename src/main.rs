pub mod day12;
use std::fs;

#[macro_use]
extern crate lazy_static;

fn main() {
    let contents =
        fs::read_to_string("./src/day12/input").expect("Something went wrong reading the file");
    day12::main(contents.trim());
}
