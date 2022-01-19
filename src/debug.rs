use crate::chunk::{Chunk,OpCode};
use crate::value::{printValue};
use crate::common::{BytesToU16};
use std::convert::Into;

fn simpleInstruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn valueInstruction (name: &str,chunk: &Chunk, offset: usize) -> usize {
    let offset = offset+1 ;
    let constant = BytesToU16(&chunk.code[offset..offset+2]) ;
    println!("{:16} {}", name, constant);
    offset + 2
}

fn constantInstruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code[offset+1] as usize;
    print!("{:16} {} '", name, constant);
    let val = chunk.constants.values[constant] ;
    printValue(val) ;
    println!("'") ;
    offset +3
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
    if offset > 0 && chunk.lines[offset] == chunk.lines[offset-2] {
        print!("   | ") ;
    } else {
        print!("{:4} ",chunk.lines[offset]) ;
    }

    let instruction: OpCode =  chunk.code[offset].into() ;

    match instruction {
        OpCode::OP_RETURN
        | OpCode::OP_IEQ
        | OpCode::OP_FEQ
        | OpCode::OP_SEQ
        | OpCode::OP_INEQ
        | OpCode::OP_FNEQ
        | OpCode::OP_SNEQ
        | OpCode::OP_IGT
        | OpCode::OP_FGT
        | OpCode::OP_SGT
        | OpCode::OP_ILT
        | OpCode::OP_FLT
        | OpCode::OP_SLT
        | OpCode::OP_IGTEQ
        | OpCode::OP_FGTEQ
        | OpCode::OP_SGTEQ
        | OpCode::OP_ILTEQ
        | OpCode::OP_FLTEQ
        | OpCode::OP_SLTEQ
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
        | OpCode::OP_NOP
        | OpCode::OP_POP
        | OpCode::OP_FALSE
        | OpCode::OP_SPRINT
        | OpCode::OP_PRINT => simpleInstruction(display!(instruction), offset),
        OpCode::OP_PUSH
        | OpCode::OP_LOOP
        | OpCode::OP_JUMP_IF_FALSE
        | OpCode::OP_JUMP_IF_FALSE_NOPOP
        | OpCode::OP_JUMP
        | OpCode::OP_LOADVAR
        | OpCode::OP_SETVAR => valueInstruction(display!(instruction),  chunk, offset),
        OpCode::OP_SCONSTANT
        | OpCode::OP_CONSTANT => constantInstruction(display!(instruction), chunk, offset),
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



