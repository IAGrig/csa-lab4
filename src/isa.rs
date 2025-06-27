pub const INSTR_WORD_BITS: usize = 32;

/// number of 5‐bit slots we can pack
pub const SLOTS_PER_WORD: usize = INSTR_WORD_BITS / 5;

#[derive(Debug, Clone)]
pub enum Instr {
    Nop,
    Jump(String),
    Call(String),
    BranchZero(String),
    BranchPositive(String),
    Load(String),
    Store(String),
    PushAddr(String),
    PushNum(i32), // same ISA instruction as PushAddr, both compiled to one opcode
    LoadIndirect,
    StoreIndirect,
    Return,
    Add,
    Sub,
    MultiplyLow,
    MultiplyHigh,
    Divide,
    Remainder,
    Not,
    Neg,
    And,
    Xor,
    Dup,
    Drop,
    Out(i32),
    In(i32),
    EnableEam,
    DisableEam,
    EnableInterrupts,
    DisableInterrupts,
    Halt,
}

#[derive(Debug, Clone)]
pub enum Slot {
    Basic(Instr),
    Arg(Instr, i32),
}

#[derive(Clone, Debug)]
pub struct CodeWord {
    pub label: Option<String>,
    pub slots: Vec<Slot>,
}

pub fn instr_to_u8(i: &Instr) -> u8 {
    match i {
        Instr::Nop => 0,
        Instr::Jump(_) => 1,
        Instr::Call(_) => 2,
        Instr::BranchZero(_) => 3,
        Instr::BranchPositive(_) => 4,
        Instr::Load(_) => 5,
        Instr::Store(_) => 6,
        Instr::PushAddr(_) => 7,
        Instr::PushNum(_) => 7, // same as PushAddr
        Instr::Return => 8,
        Instr::Add => 9,
        Instr::Sub => 10,
        Instr::MultiplyHigh => 11,
        Instr::MultiplyLow => 12,
        Instr::Divide => 13,
        Instr::Remainder => 14,
        Instr::Not => 15,
        Instr::Neg => 16,
        Instr::And => 17,
        Instr::Xor => 18,
        Instr::Dup => 19,
        Instr::Drop => 20,
        Instr::LoadIndirect => 21,
        Instr::StoreIndirect => 22,
        Instr::Out(_) => 23,
        Instr::In(_) => 24,
        Instr::EnableInterrupts => 25,
        Instr::DisableInterrupts => 26,
        Instr::EnableEam => 27,
        Instr::DisableEam => 28,
        Instr::Halt => 29,
    }
}

pub fn u8_to_instr(x: u8) -> Instr {
    match x {
        0 => Instr::Nop,
        1 => Instr::Jump("".to_string()),
        2 => Instr::Call("".to_string()),
        3 => Instr::BranchZero("".to_string()),
        4 => Instr::BranchPositive("".to_string()),
        5 => Instr::Load("".to_string()),
        6 => Instr::Store("".to_string()),
        7 => Instr::PushNum(0), // or PushAddr
        8 => Instr::Return,
        9 => Instr::Add,
        10 => Instr::Sub,
        11 => Instr::MultiplyHigh,
        12 => Instr::MultiplyLow,
        13 => Instr::Divide,
        14 => Instr::Remainder,
        15 => Instr::Not,
        16 => Instr::Neg,
        17 => Instr::And,
        18 => Instr::Xor,
        19 => Instr::Dup,
        20 => Instr::Drop,
        21 => Instr::LoadIndirect,
        22 => Instr::StoreIndirect,
        23 => Instr::Out(0),
        24 => Instr::In(0),
        25 => Instr::EnableInterrupts,
        26 => Instr::DisableInterrupts,
        27 => Instr::EnableEam,
        28 => Instr::DisableEam,
        29 => Instr::Halt,
        _ => Instr::Nop,
    }
}

pub fn is_arg_instr(i: &Instr) -> bool {
    matches!(
        i,
        Instr::Jump(_)
            | Instr::Call(_)
            | Instr::BranchZero(_)
            | Instr::BranchPositive(_)
            | Instr::Load(_)
            | Instr::Store(_)
            | Instr::PushAddr(_)
            | Instr::PushNum(_)
            | Instr::Out(_)
            | Instr::In(_)
    )
}

pub fn arg_bitcount(i: &Instr) -> usize {
    match i {
        // 20‐bit jump/call/branches
        Instr::Jump(_) | Instr::Call(_) | Instr::BranchZero(_) | Instr::BranchPositive(_) => 20,

        // 24‐bit data addresses / pushes
        Instr::Load(_) | Instr::Store(_) | Instr::PushAddr(_) | Instr::PushNum(_) => 24,

        // 8‐bit I/O port
        Instr::Out(_) | Instr::In(_) => 8,

        _ => 0,
    }
}

use std::fmt;

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instr::Nop => write!(f, "NOP"),
            Instr::Jump(_) => write!(f, "JUMP"),
            Instr::Call(_) => write!(f, "CALL"),
            Instr::BranchZero(_) => write!(f, "BZ"),
            Instr::BranchPositive(_) => write!(f, "BPOS"),
            Instr::Load(_) => write!(f, "LD"),
            Instr::Store(_) => write!(f, "ST"),
            Instr::PushAddr(_) => write!(f, "PUSH"),
            Instr::PushNum(_) => write!(f, "PUSH"),
            Instr::LoadIndirect => write!(f, "LDI"),
            Instr::StoreIndirect => write!(f, "STI"),
            Instr::Return => write!(f, "RET"),
            Instr::Add => write!(f, "ADD"),
            Instr::Sub => write!(f, "SUB"),
            Instr::MultiplyLow => write!(f, "MULL"),
            Instr::MultiplyHigh => write!(f, "MULH"),
            Instr::Divide => write!(f, "DIV"),
            Instr::Remainder => write!(f, "REM"),
            Instr::Not => write!(f, "NOT"),
            Instr::Neg => write!(f, "NEG"),
            Instr::And => write!(f, "AND"),
            Instr::Xor => write!(f, "XOR"),
            Instr::Dup => write!(f, "DUP"),
            Instr::Drop => write!(f, "DROP"),
            Instr::In(_) => write!(f, "IN"),
            Instr::Out(_) => write!(f, "OUT"),
            Instr::EnableEam => write!(f, "ENEAM"),
            Instr::DisableEam => write!(f, "DISEAM"),
            Instr::EnableInterrupts => write!(f, "ENINT"),
            Instr::DisableInterrupts => write!(f, "DISINT"),
            Instr::Halt => write!(f, "HALT"),
        }
    }
}
