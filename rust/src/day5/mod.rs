type LangVal = isize;
pub fn main(contents: String) {
    let prog: Vec<LangVal> = contents
        .split(",")
        .map(|x| x.trim().parse().unwrap())
        .collect();
    let out_buf = run_with_inital_vals(&prog, &mut vec![1 as LangVal]);
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
        99 => (Halt, 0),
        _ => panic!("bad op code: {}", op),
    }
}

fn conver_params(
    param_tuple: (LangVal, LangVal, LangVal),
    mode_tuple: (bool, bool, bool),
    prog: &mut Vec<LangVal>,
) -> (LangVal, LangVal, usize) {
    // println!(
    //     "mode_tuple: {:?}, param_tuple: {:?}",
    //     mode_tuple, param_tuple
    // );
    let (p1_mode, p2_mode, _) = mode_tuple;
    let (mut p1, mut p2, output_loc) = param_tuple;
    if !p1_mode {
        p1 = prog[p1 as usize];
    }
    if !p2_mode {
        p2 = prog[p2 as usize];
    }
    (p1, p2, output_loc as usize)
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
            let (p1, p2, output_loc) =
                conver_params(get_params(index, prog), (p1_mode, p2_mode, true), prog);
            prog[output_loc] = p1 + p2;
        }
        Mul(p1_mode, p2_mode) => {
            let (p1, p2, output_loc) =
                conver_params(get_params(index, prog), (p1_mode, p2_mode, true), prog);
            prog[output_loc] = p1 * p2;
        }
        Set => {
            let output_loc = get_param(index, prog);
            prog[output_loc] = *input_buf.get(0).unwrap();
        }
        Output => {
            let input_loc = get_param(index, prog);
            out_buf.push(prog[input_loc as usize]);
            // println!("out_buf: {:?}", out_buf);
        }
        Halt => return,
    }
    if jmp_amt != 0 {
        run_prog(index + jmp_amt, prog, input_buf, out_buf)
    }
}

// returns param1
fn get_param(index: usize, prog: &Vec<LangVal>) -> usize {
    prog[index + 1] as usize
}

// returns param1, prarm2, output_loc
fn get_params(index: usize, prog: &Vec<LangVal>) -> (LangVal, LangVal, LangVal) {
    return (prog[index + 1], prog[index + 2], prog[index + 3]);
}
