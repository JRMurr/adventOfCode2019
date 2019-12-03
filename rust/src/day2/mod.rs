use std::fs;
pub fn day2() {
    let contents =
        fs::read_to_string("./src/day2/input").expect("Something went wrong reading the file");
    let prog: Vec<usize> = contents.split(",").map(|x| x.parse().unwrap()).collect();
    // part 1 would be
    // println!("output: {}", run_with_inital_vals(12, 2, &prog));
    let (noun, verb) = find_desired_initals(&prog, 19690720).unwrap();
    println!(
        "noun: {}, verb: {}, 100 * noun + verb: {}",
        noun,
        verb,
        100 * noun + verb
    );
}

fn run_with_inital_vals(noun: usize, verb: usize, prog: &Vec<usize>) -> usize {
    // println!("noun: {}, verb: {},", noun, verb);
    let mut new_prog = prog.to_vec();
    new_prog[1] = noun;
    new_prog[2] = verb;
    run_prog(0, &mut new_prog);
    new_prog[0]
}

fn run_prog(index: usize, prog: &mut Vec<usize>) {
    let opcode = prog[index];
    match opcode {
        1 => {
            let (p1, p2, output) = get_params(index, prog);
            prog[output] = prog[p1] + prog[p2];
        }
        2 => {
            let (p1, p2, output) = get_params(index, prog);
            prog[output] = prog[p1] * prog[p2];
        }
        99 => return,
        _ => panic!("bad op code"),
    }
    if opcode != 99 {
        run_prog(index + 4, prog)
    }
}

// returns the (noun, verb) that gives the desired ouput
fn find_desired_initals(
    prog: &Vec<usize>,
    desired_output: usize,
) -> Result<(usize, usize), &'static str> {
    for verb in 1..100 {
        for noun in 1..100 {
            let output = run_with_inital_vals(noun, verb, prog);
            if output == desired_output {
                return Ok((noun, verb));
            }
        }
    }
    Err("Bad desired_output")
}

// returns param1, prarm2, output
fn get_params(index: usize, prog: &Vec<usize>) -> (usize, usize, usize) {
    return (prog[index + 1], prog[index + 2], prog[index + 3]);
}
