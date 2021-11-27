use crate::chunk::{Chunk,OpCode};
use crate::value::{printValue};
use crate::common::{BytesToU64};
use std::convert::Into;

fn simpleInstruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn valueInstruction (name: &str,chunk: &Chunk, offset: usize) -> usize {
    let offset = offset+1 ;
    let constant = BytesToU64(&chunk.code[offset..(offset+8)]) ;
    print!("{:16} {} '", name, constant);
    print!("{}",constant) ;
    println!("'") ;
    offset + 1
}

fn stringInstruction (name: &str,chunk: &Chunk, offset: usize) -> usize {
    let offset = offset+1 ;
    let constant = BytesToU64(&chunk.code[offset..(offset+8)]) ;
    let value = chunk.strings.getValue(constant as usize) ;
    print!("{:16} {} ", name, constant);
    println!("{}",value) ;
    offset + 1
}

fn constantInstruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code[offset] ;
    print!("{:16} {} '", name, constant);
    let val = chunk.constants.values[constant as usize] ;
    printValue(val) ;
    println!("'") ;
    offset +1
}

pub fn disassembleInstruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset) ;
    if offset > 0 && chunk.lines[offset] == chunk.lines[offset-9] {
        print!("   | ") ;
    } else {
        print!("{:4} ",chunk.lines[offset]) ;
    }

    let instruction: OpCode =  chunk.code[offset].into() ;

    match instruction {
        OpCode::OP_RETURN => simpleInstruction("OP_RETURN", offset),
        OpCode::OP_EQUAL => simpleInstruction("OP_EQUAL", offset),
        OpCode::OP_GREATER => simpleInstruction("OP_GREATER", offset),
        OpCode::OP_LESS => simpleInstruction("OP_LESS", offset),
        OpCode::OP_IADD => simpleInstruction("OP_IADD", offset),
        OpCode::OP_ISUBTRACT => simpleInstruction("OP_ISUBTRACT", offset),
        OpCode::OP_IMULTIPLY => simpleInstruction("OP_IMULTIPLY", offset),
        OpCode::OP_IDIVIDE => simpleInstruction("OP_IDIVIDE", offset),
        OpCode::OP_FADD => simpleInstruction("OP_FADD", offset),
        OpCode::OP_FSUBTRACT => simpleInstruction("OP_FSUBTRACT", offset),
        OpCode::OP_FMULTIPLY => simpleInstruction("OP_FMULTIPLY", offset),
        OpCode::OP_FDIVIDE => simpleInstruction("OP_FDIVIDE", offset),
        OpCode::OP_NOT => simpleInstruction("OP_NOT", offset),
        OpCode::OP_INEGATE => simpleInstruction("OP_INEGATE", offset),
        OpCode::OP_CONSTANT => constantInstruction("OP_CONSTANT", chunk, offset),
        OpCode::OP_NIL => simpleInstruction("OP_NIL", offset),
        OpCode::OP_TRUE => simpleInstruction("OP_TRUE", offset),
        OpCode::OP_FALSE => simpleInstruction("OP_FALSE", offset),
        OpCode::OP_PUSH => valueInstruction("OP_PUSH",  chunk, offset),
        OpCode::OP_PRINT => simpleInstruction("OP_PRINT", offset),
        _ => {
            println!("Unknown opcode {}", instruction as u8) ;
            offset
        }
    }
}

pub fn disassembleChunk(chunk: &Chunk, name: &str) {
    println!() ;
    println!("== {} ==", name);
    let mut offset  = 0 ;
    loop {
        offset = disassembleInstruction(chunk, offset) ;
        if offset >= chunk.code.len() {
            break ;
        }
    }
}



