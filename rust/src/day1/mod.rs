use std::fs;
pub fn day1() {
    let contents =
        fs::read_to_string("./src/day1/input").expect("Something went wrong reading the file");
    let total = contents
        .split_whitespace()
        .fold(0, |acc, x| acc + get_fuel(x.parse().unwrap()));
    println!("total fuel: {}", total)
}

fn get_fuel(mass: i32) -> i32 {
    let num = (mass / 3) - 2; // rust rounds down for integer division
    match num {
        num if num > 0 => num + get_fuel(num),
        _ => 0,
    }
}
