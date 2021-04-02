use super::*;

#[derive(Debug, Clone)]
pub struct IntProgram {
    index: isize,
    relative_base: isize,
    pub mem: Vec<isize>,
    pub inputs: Vec<isize>,
    default: Option<isize>,
}

impl IntProgram {
    pub fn new(input: &str, inputs: Vec<isize>) -> Self {
        IntProgram {
            index: 0,
            relative_base: 0,
            mem: input.split(',').map(parse).collect(),
            inputs,
            default: None,
        }
    }
    pub fn with_default(input: &str, default: isize) -> Self {
        IntProgram {
            index: 0,
            relative_base: 0,
            mem: input.split(',').map(parse).collect(),
            inputs: vec![],
            default: Some(default),
        }
    }
    pub fn get(&self, index: isize) -> isize {
        self.mem.get(index as usize).copied().unwrap_or(0)
    }
    pub fn get_mut(&mut self, index: isize) -> &mut isize {
        let index = index as usize;
        if index >= self.mem.len() {
            self.mem.resize(index + 1, 0);
        }
        &mut self.mem[index]
    }
}

pub enum IntResult {
    Finished,
    Output(isize),
    Step,
    Default,
}

pub fn int_code(code: &mut IntProgram, return_on_output: bool) -> Option<isize> {
    loop {
        match step_int_code(code, return_on_output) {
            IntResult::Finished => return None,
            IntResult::Output(o) => return Some(o),
            _ => {}
        }
    }
}

pub fn step_int_code(code: &mut IntProgram, return_on_output: bool) -> IntResult {
    let param_map = |op| match op {
        99 => &[][..],
        1 => &[1, 1, 0],
        2 => &[1, 1, 0],
        3 => &[0],
        4 => &[1],
        5 => &[1, 1],
        6 => &[1, 1],
        7 => &[1, 1, 0],
        8 => &[1, 1, 0],
        9 => &[1],
        op => panic!("Unknown op: {}", op),
    };

    let mut inst = code.get(code.index);
    let op = inst % 100;
    inst /= 100;
    let param_vars = param_map(op);
    let p = param_vars
        .iter()
        .enumerate()
        .map(|(i, io)| {
            let mode = inst % 10;
            inst /= 10;
            let pos = code.index + 1 + i as isize;
            if *io == 0 {
                match mode {
                    0 => code.get(pos),
                    1 => panic!("Output in immediate mode"),
                    2 => code.get(pos) + code.relative_base,
                    m => panic!("Invalid parameter mode {}", m),
                }
            } else {
                match mode {
                    0 => code.get(code.get(pos)),
                    1 => code.get(pos),
                    2 => code.get(code.get(pos) + code.relative_base),
                    m => panic!("Invalid parameter mode {}", m),
                }
            }
        })
        .to_vec();

    match op {
        1 => {
            *code.get_mut(p[2]) = p[0] + p[1];
        }
        2 => {
            *code.get_mut(p[2]) = p[0] * p[1];
        }
        3 => {
            if code.inputs.is_empty() {
                if let Some(input) = code.default {
                    *code.get_mut(p[0]) = input;
                    code.index += p.len() as isize + 1;
                    return IntResult::Default;
                } else {
                    panic!("Out of Input");
                }
            } else {
                *code.get_mut(p[0]) = code.inputs.remove(0);
            }
        }
        4 => {
            if return_on_output {
                code.index += p.len() as isize + 1;
                return IntResult::Output(p[0]);
            } else {
                println!("{}", p[0]);
            }
        }
        5 => {
            if p[0] != 0 {
                code.index = p[1];
                return IntResult::Step;
            }
        }
        6 => {
            if p[0] == 0 {
                code.index = p[1];
                return IntResult::Step;
            }
        }
        7 => {
            *code.get_mut(p[2]) = (p[0] < p[1]) as isize;
        }
        8 => {
            *code.get_mut(p[2]) = (p[0] == p[1]) as isize;
        }
        9 => {
            code.relative_base += p[0];
        }
        99 => return IntResult::Finished,
        op => panic!("invalid opcode: {}", op),
    }
    code.index += p.len() as isize + 1;
    IntResult::Step
}
