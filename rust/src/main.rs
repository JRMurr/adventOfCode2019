pub mod day5;
use std::fs;

fn main() {
    let contents =
        fs::read_to_string("./src/day5/input").expect("Something went wrong reading the file");
    day5::main(contents);
}
