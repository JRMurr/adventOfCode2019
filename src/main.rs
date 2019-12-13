pub mod day10;
use std::fs;

fn main() {
    let contents =
        fs::read_to_string("./src/day10/input").expect("Something went wrong reading the file");
    day10::main(contents.trim());
}
