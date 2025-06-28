use csa_lab4::isa::{self, CodeWord, Instr, Slot};
use log::{info, LevelFilter};
use simplelog::*;
use std::fs::File;
use std::io::Write;
use std::io::{self, Read};
use std::str::FromStr;

const DATA_WORD_BITSIZE: i32 = 24;
const WORD_MAX: i32 = (1 << (DATA_WORD_BITSIZE - 1)) - 1; //  8_388_607
const WORD_MOD: i32 = 1 << DATA_WORD_BITSIZE; // 16_777_216

pub struct DataPath {
    data_memory: Vec<i32>,
    data_stack: Vec<i32>,
    t_reg: i32,
    ar_reg: i32,
    io_reg: i32,
    c_flag: i32,
}

impl DataPath {
    pub fn new(data_memory: Vec<i32>) -> Self {
        DataPath {
            data_memory,
            data_stack: vec![],
            t_reg: 0,
            ar_reg: 0,
            io_reg: 0,
            c_flag: 0,
        }
    }

    fn data_stack(&self) -> i32 {
        match self.data_stack.last() {
            Some(&val) => val,
            _ => 0,
        }
    }

    fn signal_data_push(&mut self) {
        self.data_stack.push(self.t_reg);
    }

    fn signal_data_pop(&mut self) {
        self.data_stack.pop();
    }

    fn check_t_overflow(&mut self) {
        if self.t_reg > WORD_MAX {
            self.t_reg -= WORD_MOD;
            self.c_flag = 1;
        } else if self.t_reg < -WORD_MAX - 1 {
            self.t_reg += WORD_MOD;
            self.c_flag = 1;
        } else {
            self.c_flag = 0;
        }
    }

    fn signal_latch_t_push(&mut self, instr: Instr, arg: Option<i32>, eam: u8) {
        match instr {
            // from data memory
            Instr::Load(_) | Instr::LoadIndirect => {
                self.t_reg = self.data_memory[self.ar_reg as usize];
            }

            // from CU
            Instr::PushNum(_) | Instr::PushAddr(_) => {
                self.t_reg = arg.unwrap();
            }

            // from IO register
            Instr::In(_) => {
                self.t_reg = self.io_reg;
            }

            // from ALU
            Instr::Add => {
                self.t_reg += self.data_stack();

                if eam == 1 {
                    self.t_reg += self.c_flag
                };
                self.check_t_overflow();
            }
            Instr::Sub => {
                self.t_reg -= self.data_stack();

                if eam == 1 {
                    self.t_reg -= self.c_flag // t - (d+carry)
                };
                self.check_t_overflow();
            }
            Instr::And => {
                self.t_reg &= &self.data_stack();
                self.c_flag = 0;
            }
            Instr::Not => {
                self.t_reg = !self.t_reg;
                self.c_flag = 0;
            }
            Instr::Neg => {
                self.t_reg = -self.t_reg;
                self.c_flag = 0;
            }
            Instr::Xor => {
                self.t_reg ^= self.data_stack();
                self.c_flag = 0;
            }
            Instr::Divide => {
                self.t_reg /= self.data_stack();
                self.c_flag = 0;
            }
            Instr::Remainder => {
                self.t_reg %= self.data_stack();
                self.c_flag = 0;
            }
            Instr::MultiplyLow => {
                // self.t_reg = (self.t_reg * self.data_stack()) & 0xFFFFFF;
                self.t_reg *= self.data_stack();
                self.check_t_overflow();
            }
            Instr::MultiplyHigh => {
                self.t_reg = (self.t_reg * self.data_stack()) >> 24;
                self.c_flag = if self.t_reg > 0 { 1 } else { 0 };
            }

            _ => {
                unreachable!();
            }
        }
    }

    fn signal_latch_t_pop(&mut self) {
        self.t_reg = self.data_stack();
    }

    fn signal_latch_ar(&mut self, instr: Instr, arg: Option<i32>) {
        match instr {
            // from stack
            Instr::LoadIndirect | Instr::StoreIndirect => {
                self.ar_reg = self.t_reg;
            }
            // from CU
            Instr::Load(_) | Instr::Store(_) => {
                self.ar_reg = arg.unwrap();
            }

            _ => {
                unreachable!();
            }
        }
    }

    fn signal_latch_io(&mut self, instr: Instr, arg: Option<i32>) {
        match instr {
            Instr::Out(_) => {
                self.io_reg = self.t_reg;
            }
            Instr::In(_) => {
                self.io_reg = arg.unwrap();
            }

            _ => {
                unreachable!();
            }
        }
    }

    fn signal_dm_write(&mut self) {
        self.data_memory[self.ar_reg as usize] = self.t_reg;
    }

    pub fn flag_zero(&self) -> bool {
        self.t_reg == 0
    }

    pub fn flag_sign(&self) -> bool {
        self.t_reg < 0
    }
}

pub struct ControlUnit {
    instruction_memory: Vec<CodeWord>,
    return_stack: Vec<i32>,
    pc_reg: i32,
    ir_reg: CodeWord,
    eam_reg: u8,
    ei_reg: u8,
    slc_reg: u8,
    stc_reg: u8,
    tick: i32,
    dp: DataPath,
    io_devices: Vec<IoDevice>,
    int_depth: u8,
}

enum PcMuxOptions {
    PC,
    ReturnStack,
    Interrupt,
    IR,
}

enum RStackMuxOptions {
    Increment,
    Regular,
}

impl ControlUnit {
    fn new(instruction_memory: Vec<CodeWord>, dp: DataPath, io_devices: Vec<IoDevice>) -> Self {
        ControlUnit {
            instruction_memory,
            return_stack: vec![],
            pc_reg: 0,
            ir_reg: CodeWord {
                label: None,
                slots: vec![],
            },
            eam_reg: 0,
            ei_reg: 0,
            slc_reg: 0,
            stc_reg: 0,
            tick: 0,
            dp,
            io_devices,
            int_depth: 0,
        }
    }

    fn tick(&mut self) {
        self.tick += 1;
    }

    fn fetch_instruction(&self) -> Option<Slot> {
        match self.slc_reg {
            0 => None,
            _ => self.ir_reg.slots.get((self.slc_reg - 1) as usize).cloned(),
        }
    }

    fn return_stack(&self) -> i32 {
        match self.return_stack.last() {
            Some(&val) => val,
            _ => 0,
        }
    }

    fn intreq(&self) -> bool {
        let mut res = false;
        for device in &self.io_devices {
            res |= device.intrq;
        }
        res
    }

    fn signal_intack(&mut self) -> i32 {
        for device in &mut self.io_devices {
            if let Some(device_number) = device.signal_intack() {
                return device_number;
            }
        }
        unreachable!("intack without requesting devices");
    }

    fn signal_write(&mut self, port: i32) {
        self.io_devices
            .iter_mut()
            .for_each(|device| device.signal_write(port, self.dp.io_reg));
    }

    fn signal_latch_pc(&mut self, sel: PcMuxOptions, arg: Option<i32>) {
        match sel {
            PcMuxOptions::PC => {
                self.pc_reg += 1;
            }
            PcMuxOptions::Interrupt => {
                self.pc_reg = arg.unwrap();
            }
            PcMuxOptions::ReturnStack => {
                self.pc_reg = self.return_stack();
            }
            PcMuxOptions::IR => {
                self.pc_reg = arg.unwrap();
            }
        }
    }

    fn signal_return_push(&mut self, sel: RStackMuxOptions) {
        match sel {
            RStackMuxOptions::Regular => {
                self.return_stack.push(self.pc_reg);
            }
            RStackMuxOptions::Increment => {
                self.return_stack.push(self.pc_reg + 1);
            }
        }
    }

    fn signal_return_pop(&mut self) {
        self.return_stack.pop();
    }

    fn signal_latch_ir(&mut self) {
        self.ir_reg = self.instruction_memory[self.pc_reg as usize].clone();
    }

    fn signal_disable_eam(&mut self) {
        self.eam_reg = 0;
    }

    fn signal_enable_eam(&mut self) {
        self.eam_reg = 1;
    }

    fn signal_ei(&mut self) {
        self.ei_reg = 1;
    }

    fn signal_di(&mut self) {
        self.ei_reg = 0;
    }

    fn signal_slc_next(&mut self) {
        self.slc_reg += 1;
    }

    fn signal_slc_reset(&mut self) {
        self.slc_reg = 0;
    }

    fn signal_stc_next(&mut self) {
        self.stc_reg += 1;
    }

    fn signal_stc_reset(&mut self) {
        self.stc_reg = 0;
    }

    pub fn process_tick(&mut self) -> bool {
        // returns true to continue, false to halt

        let slot = self.fetch_instruction();

        let (instr, arg) = match &slot {
            Some(Slot::Basic(instr)) => (instr.to_string(), "".to_string()),
            Some(Slot::Arg(instr, arg)) => (instr.to_string(), format!("({arg})")),
            _ => ("---".to_string(), "".to_string()),
        };

        info!(
            "{} PC/SLOT/STEP:{}/{}/{} EI/EAM:{}/{} Idepth:{} {}{} T:{} Dst:{} Rst:{} AR:{} IO:{}",
            self.tick,
            self.pc_reg,
            self.slc_reg,
            self.stc_reg,
            self.ei_reg,
            self.eam_reg,
            self.int_depth,
            instr,
            arg,
            self.dp.t_reg,
            format_stack(&self.dp.data_stack, 4),
            format_stack(&self.return_stack, 4),
            self.dp.ar_reg,
            self.dp.io_reg,
        );

        for iodevice in &mut self.io_devices {
            iodevice.check_schedule(self.tick);
        }

        if self.slc_reg == 0 && self.ei_reg == 1 && self.intreq() {
            let device_num = self.signal_intack();
            self.signal_return_push(RStackMuxOptions::Regular);
            self.signal_latch_pc(PcMuxOptions::Interrupt, Some(device_num));
            self.int_depth += 1;
        }

        if self.slc_reg == 0 {
            self.signal_latch_ir();
            self.signal_slc_next();

            self.tick();
            return true;
        }

        if self.slc_reg == 6 {
            self.signal_latch_pc(PcMuxOptions::PC, None);
        }

        if let Some(instr) = slot {
            match instr {
                Slot::Basic(Instr::Halt) => {
                    return false;
                }

                Slot::Basic(Instr::Nop) => {
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::LoadIndirect) => {
                    if self.stc_reg == 0 {
                        self.dp.signal_latch_ar(Instr::LoadIndirect, None);
                        self.dp.signal_latch_t_pop();
                        self.dp.signal_data_pop();
                        self.signal_stc_next();
                    } else if self.stc_reg == 1 {
                        self.dp.signal_data_push();
                        self.dp
                            .signal_latch_t_push(Instr::LoadIndirect, None, self.eam_reg);
                        self.signal_stc_reset();
                        self.signal_slc_next();
                    }
                }

                Slot::Basic(Instr::StoreIndirect) => {
                    if self.stc_reg == 0 {
                        self.dp.signal_latch_ar(Instr::LoadIndirect, None);
                        self.dp.signal_latch_t_pop();
                        self.dp.signal_data_pop();
                        self.signal_stc_next();
                    } else if self.stc_reg == 1 {
                        self.dp.signal_dm_write();
                        self.dp.signal_latch_t_pop();
                        self.dp.signal_data_pop();
                        self.signal_stc_reset();
                        self.signal_slc_next();
                    }
                }

                Slot::Basic(Instr::Return) => {
                    self.signal_latch_pc(PcMuxOptions::ReturnStack, None);
                    self.signal_return_pop();
                    self.signal_slc_reset();

                    self.int_depth = self.int_depth.saturating_sub(1);
                }

                Slot::Basic(Instr::Add) => {
                    self.dp.signal_latch_t_push(Instr::Add, None, self.eam_reg);
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::Sub) => {
                    self.dp.signal_latch_t_push(Instr::Sub, None, self.eam_reg);
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::MultiplyHigh) => {
                    self.dp
                        .signal_latch_t_push(Instr::MultiplyHigh, None, self.eam_reg);
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::MultiplyLow) => {
                    self.dp
                        .signal_latch_t_push(Instr::MultiplyLow, None, self.eam_reg);
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::Divide) => {
                    self.dp
                        .signal_latch_t_push(Instr::Divide, None, self.eam_reg);
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::Remainder) => {
                    self.dp
                        .signal_latch_t_push(Instr::Remainder, None, self.eam_reg);
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::Not) => {
                    self.dp.signal_latch_t_push(Instr::Not, None, self.eam_reg);
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::Neg) => {
                    self.dp.signal_latch_t_push(Instr::Neg, None, self.eam_reg);
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::And) => {
                    self.dp.signal_latch_t_push(Instr::And, None, self.eam_reg);
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::Xor) => {
                    self.dp.signal_latch_t_push(Instr::Xor, None, self.eam_reg);
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::Dup) => {
                    self.dp.signal_data_push();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::Drop) => {
                    self.dp.signal_latch_t_pop();
                    self.dp.signal_data_pop();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::EnableEam) => {
                    self.signal_enable_eam();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::DisableEam) => {
                    self.signal_disable_eam();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::EnableInterrupts) => {
                    self.signal_ei();
                    self.signal_slc_next();
                }

                Slot::Basic(Instr::DisableInterrupts) => {
                    self.signal_di();
                    self.signal_slc_next();
                }

                Slot::Arg(Instr::Jump(_), arg) => {
                    self.signal_latch_pc(PcMuxOptions::IR, Some(arg));
                    self.signal_slc_reset();
                }

                Slot::Arg(Instr::Call(_), arg) => {
                    self.signal_return_push(RStackMuxOptions::Increment);
                    self.signal_latch_pc(PcMuxOptions::IR, Some(arg));
                    self.signal_slc_reset();
                }

                Slot::Arg(Instr::BranchZero(_), arg) => {
                    if self.dp.flag_zero() {
                        self.signal_latch_pc(PcMuxOptions::IR, Some(arg));
                    } else {
                        self.signal_latch_pc(PcMuxOptions::PC, None);
                    }
                    self.signal_slc_reset();
                }

                Slot::Arg(Instr::BranchPositive(_), arg) => {
                    if !self.dp.flag_sign() && !self.dp.flag_zero() {
                        self.signal_latch_pc(PcMuxOptions::IR, Some(arg));
                    } else {
                        self.signal_latch_pc(PcMuxOptions::PC, None);
                    }
                    self.signal_slc_reset();
                }

                Slot::Arg(Instr::Load(_), arg) => {
                    if self.stc_reg == 0 {
                        self.dp
                            .signal_latch_ar(Instr::Load("".to_string()), Some(arg));
                        self.signal_stc_next();
                    } else if self.stc_reg == 1 {
                        self.dp.signal_data_push();
                        self.dp.signal_latch_t_push(
                            Instr::Load("".to_string()),
                            None,
                            self.eam_reg,
                        );
                        self.signal_latch_pc(PcMuxOptions::PC, None);
                        self.signal_stc_reset();
                        self.signal_slc_reset();
                    }
                }

                Slot::Arg(Instr::Store(_), arg) => {
                    if self.stc_reg == 0 {
                        self.dp
                            .signal_latch_ar(Instr::Store("".to_string()), Some(arg));
                        self.signal_stc_next();
                    } else if self.stc_reg == 1 {
                        self.dp.signal_dm_write();
                        self.dp.signal_latch_t_pop();
                        self.dp.signal_data_pop();
                        self.signal_latch_pc(PcMuxOptions::PC, None);
                        self.signal_stc_reset();
                        self.signal_slc_reset();
                    }
                }

                Slot::Arg(Instr::PushNum(_), arg) | Slot::Arg(Instr::PushAddr(_), arg) => {
                    self.dp.signal_data_push();
                    self.dp
                        .signal_latch_t_push(Instr::PushNum(0), Some(arg), self.eam_reg);
                    self.signal_latch_pc(PcMuxOptions::PC, None);
                    self.signal_slc_reset();
                }

                Slot::Arg(Instr::In(_), port) => {
                    if self.stc_reg == 0 {
                        self.dp.signal_latch_io(
                            Instr::In(0),
                            Some(self.io_devices[(port - 1) as usize].data),
                        );
                        self.signal_stc_next();
                    } else if self.stc_reg == 1 {
                        self.dp.signal_data_push();
                        self.dp
                            .signal_latch_t_push(Instr::In(0), None, self.eam_reg);
                        self.signal_latch_pc(PcMuxOptions::PC, None);
                        self.signal_stc_reset();
                        self.signal_slc_reset();
                    }
                }

                Slot::Arg(Instr::Out(_), port) => {
                    if self.stc_reg == 0 {
                        self.dp.signal_latch_io(Instr::Out(0), None);
                        self.dp.signal_latch_t_pop();
                        self.dp.signal_data_pop();
                        self.signal_stc_next();
                    } else if self.stc_reg == 1 {
                        self.signal_write(port);
                        self.signal_latch_pc(PcMuxOptions::PC, None);
                        self.signal_stc_reset();
                        self.signal_slc_reset();
                    }
                }

                _ => {
                    unreachable!("instruction not matched")
                }
            }

            if self.slc_reg == 7 {
                // unfortunatelly, I don't know how to make it better
                self.signal_slc_reset();
            }
            self.tick();

            return true;
        }
        unreachable!("current slot is None")
    }

    pub fn take_output(&self) -> Vec<String> {
        self.io_devices
            .iter()
            .map(|device| {
                let port_num = device.num_port;
                let formatted_output = device
                    .output_buffer
                    .iter()
                    .map(|&num| {
                        // Convert number to character if it's a printable ASCII, otherwise show the number
                        if (32..=126).contains(&num) {
                            // Printable ASCII range
                            let ch = num as u8 as char;
                            if ch.is_ascii_graphic() || ch == ' ' {
                                format!("{num}({ch})")
                            } else {
                                // Control characters in ASCII range (like newlines, tabs)
                                format!("{num}({ch:?})")
                            }
                        } else {
                            // Non-ASCII or unprintable
                            format!("{num}(??)")
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("IO{port_num}: [{formatted_output}]")
            })
            .collect()
    }

    pub fn ticks(&self) -> i32 {
        self.tick
    }
}

struct IoDevice {
    num_port: i32,
    intrq: bool,
    data: i32,
    input_schedule: Vec<(i32, i32)>, // (tick, byte)
    output_buffer: Vec<i32>,
}

impl IoDevice {
    fn new(num_port: i32, input_schedule: Vec<(i32, i32)>) -> Self {
        IoDevice {
            num_port,
            intrq: false,
            data: 0,
            input_schedule,
            output_buffer: vec![],
        }
    }

    fn check_schedule(&mut self, tick: i32) {
        let (next_input_time, next_data) = *self.input_schedule.first().unwrap_or(&(i32::MAX, 0));

        if next_input_time <= tick {
            self.data = next_data;
            self.intrq = true;
            info!(
                "IO DEVICE {} INPUT:{}",
                self.num_port,
                self.input_schedule.remove(0).1
            );
        }
    }

    fn signal_intack(&mut self) -> Option<i32> {
        if self.intrq {
            self.intrq = false;
            return Some(self.num_port);
        }

        None
    }

    fn signal_write(&mut self, num_port: i32, data: i32) {
        if num_port == self.num_port {
            self.data = data;
            self.output_buffer.push(data);
            info!("IO DEVICE {num_port} <- {data}");
        }
    }
}

pub fn simulate(
    prog: Vec<CodeWord>,
    data: Vec<i32>,
    input: Vec<Vec<(i32, i32)>>,
    tick_limit: i32,
) -> io::Result<(String, i32, Vec<i32>)> {
    let io_devices: Vec<IoDevice> = input
        .iter()
        .enumerate()
        .map(|(index, schedule)| IoDevice::new((index + 1) as i32, schedule.clone()))
        .collect();
    let dp = DataPath::new(data);
    let mut cu = ControlUnit::new(prog, dp, io_devices);

    while cu.ticks() < tick_limit {
        if !cu.process_tick() {
            break;
        }
    }

    let out = cu.take_output();
    Ok((out.join("\n"), cu.ticks(), cu.dp.data_memory))
}

fn main() -> io::Result<()> {
    let code_filepath = std::env::args()
        .nth(1)
        .expect("Usage: machine <code.bin> <data.bin> <input-schedule> [tick-limit]");
    let data_filepath = std::env::args()
        .nth(2)
        .expect("Usage: machine <code.bin> <data.bin> <input-schedule> [tick-limit]");
    let input_filepath = std::env::args()
        .nth(3)
        .expect("Usage: machine <code.bin> <data.bin> <input-schedule> [tick-limit]");
    let tick_limit = std::env::args()
        .nth(4)
        .unwrap_or("5000".to_string())
        .parse()
        .unwrap();

    let program = read_code_binary(code_filepath).unwrap();
    let data = read_data_binary(data_filepath).unwrap();
    let input = read_input_file(input_filepath).unwrap();

    SimpleLogger::init(
        LevelFilter::Info,
        ConfigBuilder::new()
            .set_time_level(LevelFilter::Off)
            .build(),
    )
    .unwrap();

    let (output, ticks, data_memory) = simulate(program, data, input, tick_limit)?;

    println!("{output}");
    println!("ticks: {ticks}");

    let _ = dump_memory(data_memory, "result-data.bin");

    Ok(())
}

fn format_stack(v: &Vec<i32>, n: usize) -> String {
    let len = v.len();

    if len == 0 {
        "[]".to_string()
    } else if len <= n {
        return format!("{v:?}");
    } else {
        let remaining_count = len - n;
        let last_n_elements = &v[len - n..];
        return format!("[({remaining_count})..., {last_n_elements:?}]");
    }
}

fn read_input_file(filename: String) -> io::Result<Vec<Vec<(i32, i32)>>> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let result = contents
        .lines()
        .map(|line| line.trim())
        .map(parse_input_line)
        .collect();

    Ok(result)
}

fn parse_input_line(line: &str) -> Vec<(i32, i32)> {
    if line.is_empty() {
        return vec![];
    }
    line.split("), (")
        .map(|pair| {
            let cleaned = pair.trim_matches(|c| c == '(' || c == ')' || c == ' ');
            let mut parts = cleaned.split(',');
            let first = parts.next().unwrap().trim();
            let second = parts.next().unwrap().trim();

            let first_num = i32::from_str(first).unwrap();

            let second_num = if second.starts_with('\'') && second.ends_with('\'') {
                // character
                let ch = second.trim_matches('\'');
                if ch.len() == 1 {
                    ch.chars().next().unwrap() as i32
                } else {
                    panic!("Invalid character literal: {second}");
                }
            } else {
                // number
                i32::from_str(second).unwrap()
            };

            (first_num, second_num)
        })
        .collect()
}

fn dump_memory(vec: Vec<i32>, filename: &str) -> std::io::Result<()> {
    let mut file = File::create(filename)?;

    for num in vec {
        let three_bytes = num & 0x00FFFFFF;

        file.write_all(&[
            ((three_bytes >> 16) & 0xFF) as u8,
            ((three_bytes >> 8) & 0xFF) as u8,
            (three_bytes & 0xFF) as u8,
        ])?;
    }

    Ok(())
}

fn read_code_binary(path: String) -> std::io::Result<Vec<CodeWord>> {
    let mut file = File::open(path)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;

    let mut out = Vec::new();
    let bytes_per_word = isa::INSTR_WORD_BITS / 8;
    let mut idx = 0;

    while idx + bytes_per_word <= buf.len() {
        let mut w32 = [0u8; 4];
        w32.copy_from_slice(&buf[idx..idx + 4]);
        idx += 4;
        let word = u64::from(u32::from_be_bytes(w32));

        let opcode = ((word >> (isa::INSTR_WORD_BITS - 5)) & 0x1F) as u8;
        let instr = isa::u8_to_instr(opcode);

        if isa::is_arg_instr(&instr) {
            let n = isa::arg_bitcount(&instr);
            let imm = (word & ((1u64 << n) - 1)) as i32;
            out.push(isa::CodeWord {
                label: None,
                slots: vec![isa::Slot::Arg(instr, imm)],
            });
        } else {
            let mut slots = Vec::new();
            for i in 0..isa::SLOTS_PER_WORD {
                let shift = (isa::INSTR_WORD_BITS - 5 * (i + 1)) as u64;
                let code = ((word >> shift) & 0x1F) as u8;
                let op = isa::u8_to_instr(code);
                slots.push(isa::Slot::Basic(op));
            }
            out.push(isa::CodeWord { label: None, slots });
        }
    }

    Ok(out)
}

fn read_data_binary(path: String) -> io::Result<Vec<i32>> {
    let mut f = File::open(path)?;
    let mut buf = Vec::new();
    f.read_to_end(&mut buf)?;
    let mut words = Vec::new();
    let mut i = 0;
    while i + 3 <= buf.len() {
        let hi = buf[i] as i32;
        let mid = buf[i + 1] as i32;
        let lo = buf[i + 2] as i32;
        let w = (hi << 16) | (mid << 8) | lo;
        words.push(w);
        i += 3;
    }
    Ok(words)
}
