pub mod day6;
use std::fs;

fn main() {
    let contents =
        fs::read_to_string("./src/day6/input").expect("Something went wrong reading the file");
    day6::main(contents);
}
