pub mod day8;
use std::fs;

fn main() {
    let contents =
        fs::read_to_string("./src/day8/input").expect("Something went wrong reading the file");
    day8::main(contents.trim());
}
