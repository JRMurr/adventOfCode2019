pub mod day9;
use std::fs;

fn main() {
    let contents =
        fs::read_to_string("./src/day9/input").expect("Something went wrong reading the file");
    day9::main(contents.trim());
}
