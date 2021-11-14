use crate::chunk::{Chunk,OpCode};
use crate::value::{Value, printValue};
use std::convert::Into;

fn simpleInstruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn constantInstruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code[offset+1] ;
    print!("{:16} {} '", name, constant);
    printValue(&chunk.constants.values[constant as usize]) ;
    println!("'") ;
    offset + 2
}

pub fn disassembleInstruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset) ;
    if offset > 0 && chunk.lines[offset] == chunk.lines[offset-1] {
        print!("   | ") ;
    } else {
        print!("{:4} ",chunk.lines[offset]) ;
    }

    let instruction: OpCode =  chunk.code[offset].into() ;

    match instruction {
        OpCode::OP_RETURN => simpleInstruction("OP_RETURN", offset),
        OpCode::OP_ADD => simpleInstruction("OP_ADD", offset),
        OpCode::OP_SUBTRACT => simpleInstruction("OP_SUBTRACT", offset),
        OpCode::OP_MULTIPLY => simpleInstruction("OP_MULTIPLY", offset),
        OpCode::OP_DIVIDE => simpleInstruction("OP_DIVIDE", offset),
        OpCode::OP_NEGATE => simpleInstruction("OP_NEGATE", offset),
        OpCode::OP_CONSTANT => constantInstruction("OP_CONSTANT", chunk, offset),
        _ => {
            println!("Unknown opcode {}", instruction as u8) ;
            offset+1
        }
    }
}

pub fn disassembleChunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset  = 0 ;
    loop {
        offset = disassembleInstruction(chunk, offset) ;
        if offset >= chunk.code.len() {
            break ;
        }
    }
}



