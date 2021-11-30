use crate::chunk::OpCode::*;
use crate::value::{ValueArray, writeValueArray};
use crate::strings::{StringPool};
use std::borrow::Borrow;

#[derive(Copy, Clone, Debug)]
pub enum OpCode {
    OP_RETURN,
    OP_CONSTANT,
    OP_IADD,
    OP_ISUBTRACT,
    OP_IMULTIPLY,
    OP_IDIVIDE,
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
    OP_DEFINE_IGLOBAL,
    OP_GET_IGLOBAL,
    OP_DEFINE_FGLOBAL,
    OP_GET_FGLOBAL,
    OP_DEFINE_BGLOBAL,
    OP_GET_BGLOBAL,
    OP_DEFINE_SGLOBAL,
    OP_GET_SGLOBAL,
    OP_DEFINE_ILOCAL,
    OP_GET_ILOCAL,
    OP_DEFINE_FLOCAL,
    OP_GET_FLOCAL,
    OP_DEFINE_BLOCAL,
    OP_GET_BLOCAL,
    OP_DEFINE_SLOCAL,
    OP_GET_SLOCAL,
    OP_FADD,
    OP_FSUBTRACT,
    OP_FMULTIPLY,
    OP_FDIVIDE,
    OP_UNKNOWN
}
impl From<u8> for OpCode {
    fn from(orig: u8) -> Self {
        match orig {
            0   => OP_RETURN,
            1   => OP_CONSTANT,
            2   => OP_IADD,
            3   => OP_ISUBTRACT,
            4   => OP_IMULTIPLY,
            5   => OP_IDIVIDE,
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
            20  => OP_DEFINE_IGLOBAL,
            21  => OP_GET_IGLOBAL,
            22  => OP_DEFINE_FGLOBAL,
            23  => OP_GET_FGLOBAL,
            24  => OP_DEFINE_BGLOBAL,
            25  => OP_GET_BGLOBAL,
            26  => OP_DEFINE_SGLOBAL,
            27  => OP_GET_SGLOBAL,
            28  => OP_DEFINE_ILOCAL,
            29  => OP_GET_ILOCAL,
            30  => OP_DEFINE_FLOCAL,
            31  => OP_GET_FLOCAL,
            32  => OP_DEFINE_BLOCAL,
            33  => OP_GET_BLOCAL,
            34  => OP_DEFINE_SLOCAL,
            35  => OP_GET_SLOCAL,
            36  => OP_FADD,
            37  => OP_FSUBTRACT,
            38  => OP_FMULTIPLY,
            39  => OP_FDIVIDE,
            _   => OP_UNKNOWN,
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

pub fn writef64Chunk(chunk: &mut Chunk, data: f64, line: usize) {
    for b in f64::to_be_bytes(data) {
        chunk.code.push(b);
        chunk.lines.push(line) ;
    }
}

pub fn addConstant(chunk: &mut Chunk, value: f64) -> usize {
    writeValueArray(&mut chunk.constants, value) ;
    chunk.constants.values.len()-1
}

pub fn addStringConstant(chunk: &mut Chunk, s: String) -> usize {
    let stringIndex = chunk.strings.store(s) ;
    writeValueArray(&mut chunk.constants, stringIndex as f64) ;
    chunk.constants.values.len()-1
}

