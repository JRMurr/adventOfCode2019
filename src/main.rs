pub mod day13;
use std::fs;

#[allow(unused_imports)]
#[macro_use]
extern crate lazy_static;

fn main() {
    let contents =
        fs::read_to_string("./src/day13/input").expect("Something went wrong reading the file");
    day13::main(contents.trim());
}
