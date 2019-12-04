pub fn main() {
    // input 382345-843167
    let min = 382345;
    let max = 843167;
    let num_passed = (min..max + 1).filter(|num| check_pass(*num)).count();
    println!("num passed: {}", num_passed);
    // println!("{}", check_pass(111122));
}

fn check_pass(pass: usize) -> bool {
    let mut pass_vec: Vec<u32> = pass
        .to_string()
        .chars()
        .map(|x| x.to_digit(10).unwrap())
        .collect();
    let mut has_pair = false;
    let mut last_num = pass_vec.remove(0);
    let mut pair_count = 0;
    for num in pass_vec {
        if num < last_num {
            return false;
        }
        if num == last_num {
            pair_count += 1;
        } else {
            if pair_count == 1 {
                has_pair = true;
            }
            pair_count = 0;
        }
        last_num = num;
    }
    if pair_count == 1 {
        has_pair = true;
    }
    has_pair
}
