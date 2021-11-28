pub type LangVal = i64;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq)]
enum ParamMode {
    Position,
    Immediate,
    Relative,
}

#[derive(Debug)]
enum Opcode {
    Add(ParamMode, ParamMode, ParamMode),
    Mul(ParamMode, ParamMode, ParamMode),
    JmpT(ParamMode, ParamMode, ParamMode),
    JmpF(ParamMode, ParamMode, ParamMode),
    Lt(ParamMode, ParamMode, ParamMode),
    Equal(ParamMode, ParamMode, ParamMode),
    AdjustRel(ParamMode),
    Set(ParamMode),
    Output(ParamMode),
    Halt,
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

fn get_modes(code: usize) -> (ParamMode, ParamMode, ParamMode) {
    let (code, d1) = parse_digits(code, 1);
    let (code, d2) = parse_digits(code, 1);
    let (_, d3) = parse_digits(code, 1);
    (get_mode(d1), get_mode(d2), get_mode(d3))
}

// returns opcode + jmp amount
fn get_op(code: usize) -> (Opcode, usize) {
    use Opcode::*;
    let (code, op) = parse_digits(code, 2);
    match op {
        1 => {
            let (p1_mode, p2_mode, p3_mode) = get_modes(code);
            (Add(p1_mode, p2_mode, p3_mode), 4)
        }
        2 => {
            let (p1_mode, p2_mode, p3_mode) = get_modes(code);
            (Mul(p1_mode, p2_mode, p3_mode), 4)
        }
        3 => {
            let (_, p1_mode) = parse_digits(code, 1);
            (Set(get_mode(p1_mode)), 2)
        }
        4 => {
            let (_, p1_mode) = parse_digits(code, 1);
            (Output(get_mode(p1_mode)), 2)
        }
        5 => {
            let (p1_mode, p2_mode, p3_mode) = get_modes(code);
            (JmpT(p1_mode, p2_mode, p3_mode), 3)
        }
        6 => {
            let (p1_mode, p2_mode, p3_mode) = get_modes(code);
            (JmpF(p1_mode, p2_mode, p3_mode), 3)
        }
        7 => {
            let (p1_mode, p2_mode, p3_mode) = get_modes(code);
            (Lt(p1_mode, p2_mode, p3_mode), 4)
        }
        8 => {
            let (p1_mode, p2_mode, p3_mode) = get_modes(code);
            (Equal(p1_mode, p2_mode, p3_mode), 4)
        }
        9 => {
            let (_, p1_mode) = parse_digits(code, 1);
            (AdjustRel(get_mode(p1_mode)), 2)
        }
        99 => (Halt, 0),
        _ => panic!("bad op code: {}", op),
    }
}

pub struct IntCode {
    prog: Vec<LangVal>,
    input_buf: Vec<LangVal>,
    pub out_buf: Vec<LangVal>,
    relative_base: LangVal,
    memory: HashMap<usize, LangVal>,
}

impl IntCode {
    pub fn new(prog: Vec<LangVal>) -> IntCode {
        IntCode {
            prog,
            input_buf: vec![],
            out_buf: vec![],
            relative_base: 0,
            memory: HashMap::new(),
        }
    }

    // pub fn run_with_input(prog: &Vec<LangVal>, input_buf: &mut Vec<LangVal>) -> Vec<LangVal> {
    //     let new_prog = prog.to_vec();
    //     let mut int_code = IntCode::new(new_prog);
    //     int_code.add_inputs(input_buf);
    //     int_code.run_till_halt();
    //     int_code.out_buf
    // }

    pub fn add_input(&mut self, val: LangVal) {
        self.input_buf.push(val);
    }

    // pub fn add_inputs(&mut self, vals: &mut Vec<LangVal>) {
    //     self.input_buf.append(vals);
    // }

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
        // println!("setting: {}, to be: {}", index, value);
        if index < self.prog.len() {
            self.prog[index] = value;
        } else {
            self.memory.insert(index, value);
        }
    }

    pub fn run_prog(&mut self, mut index: usize) -> Option<usize> {
        use Opcode::*;
        loop {
            let (opcode, jmp_amt) = get_op(self.get_loc(index) as usize);
            // println!(
            //     "---------------\nget op: {:?}, from: {}",
            //     opcode,
            //     self.get_loc(index)
            // );
            match opcode {
                Add(p1_mode, p2_mode, p3_mode) => {
                    let (params, output_loc) =
                        self.get_var_params(index, vec![p1_mode, p2_mode, p3_mode]);
                    self.set_loc(output_loc, params[0] + params[1]);
                }
                Mul(p1_mode, p2_mode, p3_mode) => {
                    let (params, output_loc) =
                        self.get_var_params(index, vec![p1_mode, p2_mode, p3_mode]);
                    self.set_loc(output_loc, params[0] * params[1]);
                }
                JmpT(p1_mode, p2_mode, p3_mode) => {
                    let (params, _) = self.get_var_params(index, vec![p1_mode, p2_mode, p3_mode]);
                    if params[0] != 0 {
                        index = params[1] as usize;
                        continue;
                    }
                }
                JmpF(p1_mode, p2_mode, p3_mode) => {
                    let (params, _) = self.get_var_params(index, vec![p1_mode, p2_mode, p3_mode]);
                    if params[0] == 0 {
                        index = params[1] as usize;
                        continue;
                    }
                }
                Lt(p1_mode, p2_mode, p3_mode) => {
                    let (params, output_loc) =
                        self.get_var_params(index, vec![p1_mode, p2_mode, p3_mode]);
                    let to_store = if params[0] < params[1] { 1 } else { 0 };
                    self.set_loc(output_loc, to_store);
                }
                Equal(p1_mode, p2_mode, p3_mode) => {
                    let (params, output_loc) =
                        self.get_var_params(index, vec![p1_mode, p2_mode, p3_mode]);
                    let to_store = if params[0] == params[1] { 1 } else { 0 };
                    self.set_loc(output_loc, to_store);
                }
                AdjustRel(mode) => {
                    let val = self.convert_param(mode, self.get_loc(index + 1));
                    self.relative_base = self.relative_base + val;
                }
                Set(mode) => {
                    let output_loc = self.convert_output_param(mode, self.get_loc(index + 1));
                    let val = self.input_buf.remove(0);
                    self.set_loc(output_loc as usize, val);
                }
                Output(mode) => {
                    let to_out = self.convert_param(mode, self.get_loc(index + 1));
                    self.out_buf.push(to_out);
                    return Some(index + jmp_amt);
                }
                Halt => return None,
            }
            index = index + jmp_amt;
        }
    }

    // pub fn run_till_halt(&mut self) {
    //     let mut pc = self.run_prog(0);
    //     loop {
    //         if pc.is_some() {
    //             pc = self.run_prog(pc.unwrap());
    //         } else {
    //             return;
    //         }
    //     }
    // }

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

    fn convert_output_param(&mut self, mode: ParamMode, param: LangVal) -> usize {
        use ParamMode::*;
        match mode {
            Immediate => panic!("should not be immediate mode for output"),
            Position => param as usize,
            Relative => (self.relative_base + param) as usize,
        }
    }
    // will return (params, out_loc)
    fn get_var_params(&mut self, index: usize, modes: Vec<ParamMode>) -> (Vec<LangVal>, usize) {
        let start = index + 1;
        let end = start + (modes.len() - 1);
        let converted_params = modes
            .iter()
            .zip(self.get_range(start, end).iter())
            .map(|(mode, param)| self.convert_param(*mode, *param))
            .collect();
        (
            converted_params,
            self.convert_output_param(*modes.last().unwrap(), self.get_loc(end)),
        )
    }

    // fn get_output_loc(&mut self, index: usize, mode: ParamMode) -> LangVal {
    //     use ParamMode::*;
    //     match mode {
    //         Position => self.get_loc(index + 1),
    //         Immediate =>
    //     }
    //     self.convert_param(mode, self.get_loc(index + 1))
    // }
}
