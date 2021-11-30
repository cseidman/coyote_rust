use crate::chunk::{Chunk,OpCode};
use crate::value::{printValue};
use crate::common::{BytesTof64};
use std::convert::Into;

fn simpleInstruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn valueInstruction (name: &str,chunk: &Chunk, offset: usize) -> usize {
    let offset = offset+1 ;
    let constant = BytesTof64(&chunk.code[offset..(offset+8)]) ;
    print!("{:16} {} '", name, constant);
    print!("{}",constant) ;
    println!("'") ;
    offset + 1
}

fn stringInstruction (name: &str,chunk: &Chunk, offset: usize) -> usize {
    let offset = offset+1 ;
    let constant = BytesTof64(&chunk.code[offset..(offset+8)]) ;
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
    macro_rules! display {
        ($opcode:expr) => {
            {
                format!("{:?}",$opcode).as_str()
            }
        };
    }

    print!("{:04} ", offset) ;
    if offset > 0 && chunk.lines[offset] == chunk.lines[offset-9] {
        print!("   | ") ;
    } else {
        print!("{:4} ",chunk.lines[offset]) ;
    }

    let instruction: OpCode =  chunk.code[offset].into() ;

    match instruction {
        OpCode::OP_RETURN
        | OpCode::OP_EQUAL
        | OpCode::OP_GREATER
        | OpCode::OP_LESS
        | OpCode::OP_IADD
        | OpCode::OP_ISUBTRACT
        | OpCode::OP_IMULTIPLY
        | OpCode::OP_IDIVIDE
        | OpCode::OP_FADD
        | OpCode::OP_FSUBTRACT
        | OpCode::OP_FMULTIPLY
        | OpCode::OP_FDIVIDE
        | OpCode::OP_NOT
        | OpCode::OP_INEGATE
        | OpCode::OP_NIL
        | OpCode::OP_TRUE
        | OpCode::OP_FALSE
        | OpCode::OP_PRINT => simpleInstruction(display!(instruction), offset),
        OpCode::OP_PUSH => valueInstruction(display!(instruction),  chunk, offset),
        OpCode::OP_CONSTANT => constantInstruction(display!(instruction), chunk, offset),
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



