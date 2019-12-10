type LangVal = isize;
pub fn main(contents: String) {
    let prog: Vec<LangVal> = contents
        .split(",")
        .map(|x| x.trim().parse().unwrap())
        .collect();
    let out_buf = run_with_inital_vals(&prog, &mut vec![5 as LangVal]);
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
fn run_with_inital_vals(prog: &Vec<LangVal>, input_buf: &mut Vec<LangVal>) -> Vec<LangVal> {
    // println!("noun: {}, verb: {},", noun, verb);
    let mut new_prog = prog.to_vec();
    let mut out_buf = vec![];
    run_prog(0, &mut new_prog, input_buf, &mut out_buf);
    out_buf
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

fn run_prog(
    index: usize,
    prog: &mut Vec<LangVal>,
    input_buf: &mut Vec<LangVal>,
    out_buf: &mut Vec<LangVal>,
) {
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
            prog[output_loc] = *input_buf.get(0).unwrap();
        }
        Output => {
            let input_loc = get_output_loc(index, prog);
            out_buf.push(prog[input_loc as usize]);
            // println!("out_buf: {:?}", out_buf);
        }
        Halt => return,
    }
    if jmp_amt != 0 {
        run_prog(index + jmp_amt, prog, input_buf, out_buf)
    }
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
