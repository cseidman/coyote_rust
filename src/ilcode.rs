/*
Intermediate code
*/

use crate::chunk::OpCode;
use OpCode::* ;
use std::ops::Add;

pub fn generateByteCode(ilcode: &IlCode) {
    for i in 0 .. ilcode.code.len() {
        let instr = ilcode.code[i] ;
        match instr {
            Instruction::SingleInstruction {},
            Instruction::CompoundInstruction {}
        }

    }
}

#[derive(Clone, Copy)]
pub enum Instruction {

    SingleInstruction {
        opcode: OpCode,
        byte: usize
    },

    CompoundInstruction {
        opcode: OpCode,
        operand: u16,
        byte: usize
    },

    StartLabel,
    EndLabel,
    LoopLabel

}

pub struct IlCode {
    code: Vec<Instruction>,
    comments: Vec<String>,
    lines: usize,
    byteLocation: usize
}

impl IlCode {

    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            comments: Vec::new(),
            lines: 0,
            byteLocation: 0
        }
    }

    pub fn AddSingle(&mut self, opcode: OpCode) {

        let instr = Instruction::SingleInstruction {
            opcode,
            byte: self.byteLocation
        };
        self.Add(instr);
        self.byteLocation+=1 ;

    }

    pub fn AddCompound(&mut self, opcode: OpCode, operand: u16) {

        let instr = Instruction::CompoundInstruction {
            opcode,
            operand,
            byte: self.byteLocation
        };
        self.Add(instr);
        self.byteLocation+=3 ;

    }

    fn Add(&mut self, instr: Instruction) {

        self.code.push(instr);
        self.comments.push("# ".to_string());
        self.lines+=1 ;
    }

    pub fn AddComment(&mut self, comment: &str) {
        let line = self.lines ;
        self.comments[line] = format!("# {}", comment);
    }

}