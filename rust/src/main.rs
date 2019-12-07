pub mod day7;
use std::fs;

fn main() {
    let contents =
        fs::read_to_string("./src/day7/input").expect("Something went wrong reading the file");
    day7::main(contents);
}
