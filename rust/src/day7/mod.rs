type LangVal = isize;
use itertools::Itertools;
pub fn main(contents: String) {
    let prog: Vec<LangVal> = contents
        .split(",")
        .map(|x| x.trim().parse().unwrap())
        .collect();
    // let out_buf = run_amps(&prog, (1, 0, 4, 3, 2));
    let out_buf = find_amps_part2(&prog);
    println!("out_buf: {:?}", out_buf);
    // let (noun, verb) = find_desired_initals(&prog, 19690720).unwrap();
    // println!(
    //     "noun: {}, verb: {}, 100 * noun + verb: {}",
    //     noun,
    //     verb,
    //     100 * noun + verb
    // );
}
#[derive(Debug)]
enum Opcode {
    // true if in immediate mode for that arg
    Add(bool, bool),
    Mul(bool, bool),
    JmpT(bool, bool),
    JmpF(bool, bool),
    Lt(bool, bool),
    Equal(bool, bool),
    Set,
    Output,
    Halt,
}
fn run_with_input(prog: &Vec<LangVal>, input_buf: &mut Vec<LangVal>) -> LangVal {
    // println!("noun: {}, verb: {},", noun, verb);
    let mut new_prog = prog.to_vec();
    let mut out_buf = vec![];
    run_prog(0, &mut new_prog, input_buf, &mut out_buf);
    out_buf[0]
}

// fn run_amps_part1(
//     prog: &Vec<LangVal>,
//     amp_settings: (LangVal, LangVal, LangVal, LangVal, LangVal),
// ) -> LangVal {
//     let (a1, a2, a3, a4, a5) = amp_settings;
//     let out = run_with_input(prog, &mut vec![a1.clone(), 0]);
//     let out = run_with_input(prog, &mut vec![a2.clone(), out.clone()]);
//     let out = run_with_input(prog, &mut vec![a3.clone(), out.clone()]);
//     let out = run_with_input(prog, &mut vec![a4.clone(), out.clone()]);
//     let out = run_with_input(prog, &mut vec![a5.clone(), out.clone()]);
//     out
// }

// fn find_amps_part1(prog: &Vec<LangVal>) -> LangVal {
//     (0..5)
//         .permutations(5)
//         .map(|perm| {
//             run_amps_part1(
//                 &prog.to_vec(),
//                 (perm[0], perm[1], perm[2], perm[3], perm[4]),
//             )
//         })
//         .max()
//         .unwrap()
// }

fn setup_with_amp(prog: &Vec<LangVal>, amp: LangVal) -> (Vec<LangVal>, Vec<LangVal>, Vec<LangVal>) {
    let new_prog = prog.to_vec();
    let out_buf = vec![];
    let input_buf = vec![amp];
    (new_prog, input_buf, out_buf)
}

fn run_amps_part2(
    prog: &Vec<LangVal>,
    amp_settings: (LangVal, LangVal, LangVal, LangVal, LangVal),
) -> LangVal {
    let (a1, a2, a3, a4, a5) = amp_settings;
    let (mut prog1, mut in_1, mut out_1) = setup_with_amp(prog, a1);
    let (mut prog2, mut in_2, mut out_2) = setup_with_amp(prog, a2);
    let (mut prog3, mut in_3, mut out_3) = setup_with_amp(prog, a3);
    let (mut prog4, mut in_4, mut out_4) = setup_with_amp(prog, a4);
    let (mut prog5, mut in_5, mut out_5) = setup_with_amp(prog, a5);
    let (mut pc_1, mut pc_2, mut pc_3, mut pc_4, mut pc_5) =
        (Some(0), Some(0), Some(0), Some(0), Some(0));
    in_1.push(0);
    let mut out = vec![];
    loop {
        // todo: chain outputs together
        pc_1 = run_prog(pc_1.unwrap(), &mut prog1, &mut in_1, &mut out_1);
        if pc_1.is_some() {
            in_2.push(out_1.remove(0));
        }
        pc_2 = run_prog(pc_2.unwrap(), &mut prog2, &mut in_2, &mut out_2);
        if pc_2.is_some() {
            in_3.push(out_2.remove(0));
        }
        pc_3 = run_prog(pc_3.unwrap(), &mut prog3, &mut in_3, &mut out_3);
        if pc_3.is_some() {
            in_4.push(out_3.remove(0));
        }
        pc_4 = run_prog(pc_4.unwrap(), &mut prog4, &mut in_4, &mut out_4);
        if pc_4.is_some() {
            in_5.push(out_4.remove(0));
        }
        pc_5 = run_prog(pc_5.unwrap(), &mut prog5, &mut in_5, &mut out_5);
        // println!("pc_5: {:?}", pc_5);
        if pc_5.is_none() {
            // println!("out: {:?}", out);
            return out.pop().unwrap();
        } else {
            let val = out_5.remove(0);
            in_1.push(val);
            out.push(val);
        }
    }
}
fn find_amps_part2(prog: &Vec<LangVal>) -> LangVal {
    (5..10)
        .permutations(5)
        .map(|perm| {
            run_amps_part2(
                &prog.to_vec(),
                (perm[0], perm[1], perm[2], perm[3], perm[4]),
            )
        })
        .max()
        .unwrap()
}

// returns (rest of code, digits)
fn parse_digits(code: usize, num_digits: u32) -> (usize, usize) {
    let power = (10 as usize).pow(num_digits);
    let rest = code / power;
    let digits = code % power;
    (rest, digits)
}

// returns opcode + jmp amount
fn get_op(code: usize) -> (Opcode, usize) {
    use Opcode::*;
    // println!("start code: {}", code);
    let (code, op) = parse_digits(code, 2);
    match op {
        1 => {
            let (code, p1_mode) = parse_digits(code, 1);
            let (_, p2_mode) = parse_digits(code, 1);
            (Add(p1_mode == 1, p2_mode == 1), 4)
        }
        2 => {
            let (code, p1_mode) = parse_digits(code, 1);
            let (_, p2_mode) = parse_digits(code, 1);
            (Mul(p1_mode == 1, p2_mode == 1), 4)
        }
        3 => {
            // let (_, out_mode) = parse_digits(code, 1);
            (Set, 2)
        }
        4 => {
            // let (_, out_mode) = parse_digits(code, 1);
            (Output, 2)
        }
        5 => {
            let (code, p1_mode) = parse_digits(code, 1);
            let (_, p2_mode) = parse_digits(code, 1);
            (JmpT(p1_mode == 1, p2_mode == 1), 3)
        }
        6 => {
            let (code, p1_mode) = parse_digits(code, 1);
            let (_, p2_mode) = parse_digits(code, 1);
            (JmpF(p1_mode == 1, p2_mode == 1), 3)
        }
        7 => {
            let (code, p1_mode) = parse_digits(code, 1);
            let (_, p2_mode) = parse_digits(code, 1);
            (Lt(p1_mode == 1, p2_mode == 1), 4)
        }
        8 => {
            let (code, p1_mode) = parse_digits(code, 1);
            let (_, p2_mode) = parse_digits(code, 1);
            (Equal(p1_mode == 1, p2_mode == 1), 4)
        }
        99 => (Halt, 0),
        _ => panic!("bad op code: {}", op),
    }
}

// returns Some(pc) if it did not halt, None if it halted
fn run_prog(
    index: usize,
    prog: &mut Vec<LangVal>,
    input_buf: &mut Vec<LangVal>,
    out_buf: &mut Vec<LangVal>,
) -> Option<usize> {
    use Opcode::*;
    let (opcode, jmp_amt) = get_op(prog[index] as usize);
    // println!("get op: {:?}, from: {}", opcode, prog[index]);
    match opcode {
        Add(p1_mode, p2_mode) => {
            let (params, output_loc) = get_var_params(index, vec![p1_mode, p2_mode], prog);
            prog[output_loc] = params.get(0).unwrap() + params.get(1).unwrap();
        }
        Mul(p1_mode, p2_mode) => {
            let (params, output_loc) = get_var_params(index, vec![p1_mode, p2_mode], prog);
            prog[output_loc] = params.get(0).unwrap() * params.get(1).unwrap();
        }
        JmpT(p1_mode, p2_mode) => {
            let (params, _) = get_var_params(index, vec![p1_mode, p2_mode], prog);
            if *params.get(0).unwrap() != 0 {
                return run_prog(*params.get(1).unwrap() as usize, prog, input_buf, out_buf);
            }
        }
        JmpF(p1_mode, p2_mode) => {
            let (params, _) = get_var_params(index, vec![p1_mode, p2_mode], prog);
            if *params.get(0).unwrap() == 0 {
                return run_prog(*params.get(1).unwrap() as usize, prog, input_buf, out_buf);
            }
        }
        Lt(p1_mode, p2_mode) => {
            let (params, output_loc) = get_var_params(index, vec![p1_mode, p2_mode], prog);
            let to_store = if params.get(0).unwrap() < params.get(1).unwrap() {
                1
            } else {
                0
            };
            prog[output_loc] = to_store;
        }
        Equal(p1_mode, p2_mode) => {
            let (params, output_loc) = get_var_params(index, vec![p1_mode, p2_mode], prog);
            let to_store = if params.get(0).unwrap() == params.get(1).unwrap() {
                1
            } else {
                0
            };
            prog[output_loc] = to_store;
        }
        Set => {
            let output_loc = get_output_loc(index, prog);
            prog[output_loc] = input_buf.remove(0);
        }
        Output => {
            let input_loc = get_output_loc(index, prog);
            out_buf.push(prog[input_loc as usize]);
            return Some(index + jmp_amt);
            // println!("out_buf: {:?}", out_buf);
        }
        Halt => return None,
    }
    run_prog(index + jmp_amt, prog, input_buf, out_buf)
}

fn get_output_loc(index: usize, prog: &Vec<LangVal>) -> usize {
    prog[index + 1] as usize
}

fn convert_param(is_immediate: bool, mut param: LangVal, prog: &Vec<LangVal>) -> LangVal {
    if !is_immediate {
        param = prog[param as usize];
    }
    param
}

// will return (params, out_loc)
fn get_var_params(index: usize, modes: Vec<bool>, prog: &Vec<LangVal>) -> (Vec<LangVal>, usize) {
    let start = index + 1;
    let end = start + modes.len();
    let converted_params = modes
        .iter()
        .zip(prog[start..end].iter())
        .map(|(mode, param)| convert_param(*mode, *param, prog))
        .collect();
    (converted_params, prog[end] as usize)
}
