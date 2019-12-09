type LangVal = isize;
use std::collections::HashMap;
// use itertools::Itertools;
pub fn main(contents: &str) {
    let prog: Vec<LangVal> = contents
        .split(",")
        .map(|x| x.trim().parse().unwrap())
        .collect();
    println!("out: {}", run_with_input(&prog, &vec![5]));
}

#[derive(Debug, Copy, Clone)]
enum ParamMode {
    Position,
    Immediate,
    Relative,
}

#[derive(Debug)]
enum Opcode {
    Add(ParamMode, ParamMode),
    Mul(ParamMode, ParamMode),
    JmpT(ParamMode, ParamMode),
    JmpF(ParamMode, ParamMode),
    Lt(ParamMode, ParamMode),
    Equal(ParamMode, ParamMode),
    AdjustRel(ParamMode),
    Set,
    Output,
    Halt,
}

fn run_with_input(prog: &Vec<LangVal>, input_buf: &Vec<LangVal>) -> LangVal {
    // println!("noun: {}, verb: {},", noun, verb);
    let new_prog = prog.to_vec();
    let mut int_code = IntCode::new(new_prog, input_buf.to_vec());
    int_code.run_till_halt();
    int_code.out_buf[0]
}

// returns (rest of code, digits)
fn parse_digits(code: usize, num_digits: u32) -> (usize, usize) {
    let power = (10 as usize).pow(num_digits);
    let rest = code / power;
    let digits = code % power;
    (rest, digits)
}

fn get_mode(digit: usize) -> ParamMode {
    use ParamMode::*;
    match digit {
        2 => Relative,
        1 => Immediate,
        _ => Position,
    }
}

fn get_modes(code: usize) -> (ParamMode, ParamMode) {
    let (code, d1) = parse_digits(code, 1);
    let (_, d2) = parse_digits(code, 1);
    (get_mode(d1), get_mode(d2))
}

// returns opcode + jmp amount
fn get_op(code: usize) -> (Opcode, usize) {
    use Opcode::*;
    // println!("start code: {}", code);
    let (code, op) = parse_digits(code, 2);
    match op {
        1 => {
            let (p1_mode, p2_mode) = get_modes(code);
            (Add(p1_mode, p2_mode), 4)
        }
        2 => {
            let (p1_mode, p2_mode) = get_modes(code);
            (Mul(p1_mode, p2_mode), 4)
        }
        3 => (Set, 2),
        4 => (Output, 2),
        5 => {
            let (p1_mode, p2_mode) = get_modes(code);
            (JmpT(p1_mode, p2_mode), 3)
        }
        6 => {
            let (p1_mode, p2_mode) = get_modes(code);
            (JmpF(p1_mode, p2_mode), 3)
        }
        7 => {
            let (p1_mode, p2_mode) = get_modes(code);
            (Lt(p1_mode, p2_mode), 4)
        }
        8 => {
            let (p1_mode, p2_mode) = get_modes(code);
            (Equal(p1_mode, p2_mode), 4)
        }
        9 => {
            let (_, p1_mode) = parse_digits(code, 1);
            (AdjustRel(get_mode(p1_mode)), 2)
        }
        99 => (Halt, 0),
        _ => panic!("bad op code: {}", op),
    }
}

struct IntCode {
    prog: Vec<LangVal>,
    input_buf: Vec<LangVal>,
    pub out_buf: Vec<LangVal>,
    relative_base: LangVal,
    memory: HashMap<usize, LangVal>,
}

impl IntCode {
    pub fn new(prog: Vec<LangVal>, input_buf: Vec<LangVal>) -> IntCode {
        IntCode {
            prog,
            input_buf,
            out_buf: vec![],
            relative_base: 0,
            memory: HashMap::new(),
        }
    }

    fn get_loc(&self, index: usize) -> LangVal {
        if index < self.prog.len() {
            return self.prog[index];
        }
        match self.memory.get(&index) {
            Some(v) => *v,
            None => 0,
        }
    }

    fn get_range(&self, start: usize, end: usize) -> Vec<LangVal> {
        let mut out = vec![];
        for i in start..end {
            let val = self.get_loc(i);
            out.push(val);
        }
        out
    }

    fn set_loc(&mut self, index: usize, value: LangVal) {
        if index < self.prog.len() {
            self.prog[index] = value;
        } else {
            self.memory.insert(index, value);
        }
    }

    pub fn run_prog(&mut self, index: usize) -> Option<usize> {
        use Opcode::*;
        let (opcode, jmp_amt) = get_op(self.get_loc(index) as usize);
        // println!("get op: {:?}, from: {}", opcode, prog[index]);
        match opcode {
            Add(p1_mode, p2_mode) => {
                let (params, output_loc) = self.get_var_params(index, vec![p1_mode, p2_mode]);
                self.set_loc(output_loc, params[0] + params[1]);
            }
            Mul(p1_mode, p2_mode) => {
                let (params, output_loc) = self.get_var_params(index, vec![p1_mode, p2_mode]);
                self.set_loc(output_loc, params[0] * params[1]);
            }
            JmpT(p1_mode, p2_mode) => {
                let (params, _) = self.get_var_params(index, vec![p1_mode, p2_mode]);
                if params[0] != 0 {
                    return self.run_prog(params[1] as usize);
                }
            }
            JmpF(p1_mode, p2_mode) => {
                let (params, _) = self.get_var_params(index, vec![p1_mode, p2_mode]);
                if params[0] == 0 {
                    return self.run_prog(params[1] as usize);
                }
            }
            Lt(p1_mode, p2_mode) => {
                let (params, output_loc) = self.get_var_params(index, vec![p1_mode, p2_mode]);
                let to_store = if params[0] < params[1] { 1 } else { 0 };
                self.set_loc(output_loc, to_store);
            }
            Equal(p1_mode, p2_mode) => {
                let (params, output_loc) = self.get_var_params(index, vec![p1_mode, p2_mode]);
                let to_store = if params[0] == params[1] { 1 } else { 0 };
                self.set_loc(output_loc, to_store);
            }
            AdjustRel(mode) => {
                let (params, _) = self.get_var_params(index, vec![mode]);
                self.relative_base = self.relative_base + params[0];
            }
            Set => {
                let output_loc = self.get_output_loc(index);
                let val = self.input_buf.remove(0);
                self.set_loc(output_loc, val);
            }
            Output => {
                let input_loc = self.get_output_loc(index);
                self.out_buf.push(self.get_loc(input_loc as usize));
                return Some(index + jmp_amt);
            }
            Halt => return None,
        }
        self.run_prog(index + jmp_amt)
    }

    pub fn run_till_halt(&mut self) {
        let pc = self.run_prog(0);
        loop {
            if pc.is_some() {
                self.run_prog(pc.unwrap());
            } else {
                return;
            }
        }
    }

    fn convert_param(&mut self, mode: ParamMode, param: LangVal) -> LangVal {
        use ParamMode::*;
        match mode {
            Position => self.get_loc(param as usize),
            Relative => {
                let param = self.relative_base + param;
                self.get_loc(param as usize)
            }
            Immediate => param,
        }
    }
    // will return (params, out_loc)
    fn get_var_params(&mut self, index: usize, modes: Vec<ParamMode>) -> (Vec<LangVal>, usize) {
        let start = index + 1;
        let end = start + modes.len();
        let converted_params = modes
            .iter()
            .zip(self.get_range(start, end).iter())
            .map(|(mode, param)| self.convert_param(*mode, *param))
            .collect();
        (converted_params, self.get_loc(end) as usize)
    }

    fn get_output_loc(&mut self, index: usize) -> usize {
        self.get_loc(index + 1) as usize
    }
}
