
use crate::chunk::OpCode::*;
use crate::value::{ValueArray, writeValueArray};
use crate::strings::{StringPool};

#[derive(Copy, Clone)]
pub enum OpCode {
    OP_RETURN,
    OP_CONSTANT,
    OP_IADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_INEGATE,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_PUSH,
    OP_IPOP,
    OP_FPOP,
    OP_SPOP,
    OP_PRINT,
    OP_FNEGATE,
    OP_UNKNOWN
}
impl From<u8> for OpCode {
    fn from(orig: u8) -> Self {
        match orig {
            0   => OP_RETURN,
            1   => OP_CONSTANT,
            2   => OP_IADD,
            3   => OP_SUBTRACT,
            4   => OP_MULTIPLY,
            5   => OP_DIVIDE,
            6   => OP_INEGATE,
            7   => OP_NIL,
            8   => OP_TRUE,
            9   => OP_FALSE,
            10  => OP_NOT,
            11  => OP_EQUAL,
            12  => OP_GREATER,
            13  => OP_LESS,
            14  => OP_PUSH,
            15  => OP_IPOP,
            16  => OP_FPOP,
            17  => OP_SPOP,
            18  => OP_PRINT,
            19  => OP_FNEGATE,
            _ => OP_UNKNOWN,
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: ValueArray,
    pub strings: StringPool,
    pub lines: Vec<usize>
}

impl Chunk {

    pub fn new() -> Self {
        Chunk{
            code: Vec::new(),
            constants: ValueArray::new(),
            strings: StringPool::new(),
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

pub fn writeU64Chunk(chunk: &mut Chunk, data: u64, line: usize) {
    for b in u64::to_be_bytes(data) {
        chunk.code.push(b);
        chunk.lines.push(line) ;
    }
}

pub fn addConstant(chunk: &mut Chunk, value: u64) -> usize {
    writeValueArray(&mut chunk.constants, value) ;
    chunk.constants.values.len()-1
}

pub fn addStringConstant(chunk: &mut Chunk, s: String) -> usize {
    let stringIndex = chunk.strings.store(s) ;
    writeValueArray(&mut chunk.constants, stringIndex as u64) ;
    chunk.constants.values.len()-1
}

