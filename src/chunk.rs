
use crate::chunk::OpCode::*;
use crate::value::{ValueArray, writeValueArray, Value};

#[derive(Copy, Clone)]
pub enum OpCode {
    OP_RETURN,
    OP_CONSTANT,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NEGATE,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_UNKNOWN
}
impl From<u8> for OpCode {
    fn from(orig: u8) -> Self {
        match orig {
            0 => OP_RETURN,
            1 => OP_CONSTANT,
            2 => OP_ADD,
            3 => OP_SUBTRACT,
            4 => OP_MULTIPLY,
            5 => OP_DIVIDE,
            6 => OP_NEGATE,
            7 => OP_NIL,
            8 => OP_TRUE,
            9 => OP_FALSE,
            _ => OP_UNKNOWN,
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: ValueArray,
    pub lines: Vec<usize>
}

impl Chunk {

    pub fn new() -> Self {
        Chunk{
            code: Vec::new(),
            constants: ValueArray::new(),
            lines: Vec::new()
        }
    }
}

pub fn initChunk(chunk: &mut Chunk) {
    chunk.code = Vec::new() ;
}

pub fn freeChunk(chunk: &mut Chunk) {
    initChunk(chunk) ;
}

pub fn writeChunk(chunk: &mut Chunk, byte: u8, line: usize) {
    chunk.code.push(byte) ;
    chunk.lines.push(line) ;
}

pub fn addConstant(chunk: &mut Chunk, value: Value) -> usize {
    writeValueArray(&mut chunk.constants, value) ;
    chunk.constants.values.len()-1
}

