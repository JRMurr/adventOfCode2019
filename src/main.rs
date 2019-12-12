pub mod day11;
use std::fs;

fn main() {
    let contents =
        fs::read_to_string("./src/day11/input").expect("Something went wrong reading the file");
    day11::main(contents.trim());
}
