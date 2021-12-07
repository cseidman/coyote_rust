use crate::chunk::OpCode::*;
use crate::value::{ValueArray, writeValueArray, Value};
use crate::heapvalue::{MemPool, HeapValue, HeapValueArray};
/*
const OP_UNKNOWN:u8 = 0;
const OP_RETURN:u8 = 1;
const OP_CONSTANT:u8 = 2;
const OP_IADD:u8 = 3;
const OP_ISUBTRACT:u8 = 4;
const OP_IMULTIPLY:u8 = 5;
const OP_IDIVIDE:u8 = 6;
const OP_INEGATE:u8 = 7
const OP_NIL:u8 = 8;
const OP_TRUE:u8 = 9;
const OP_FALSE:u8 = 10 ;
const OP_NOT:u8 = 11;
const OP_EQUAL:u8 = 12;
const OP_GREATER:u8 = 14;
const OP_LESS:u8 = 15;
const OP_PUSH:u8 = 16;
const OP_IPOP:u8 = 17;
const OP_FPOP:u8 = 18;
const OP_SPOP:u8 = 19;
const OP_PRINT:u8 = 20;
const OP_FNEGATE:u8 = 21;
const OP_FADD:u8 = 22;
const OP_FSUBTRACT:u8 = 23;
const OP_FMULTIPLY:u8 = 24;
const OP_FDIVIDE:u8 = 25;
const OP_SCONSTANT:u8 = 26;
*/
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
    OP_FADD,
    OP_FSUBTRACT,
    OP_FMULTIPLY,
    OP_FDIVIDE,
    OP_SCONSTANT,
    OP_LOADVAR,
    OP_SETVAR,
    OP_JUMP_IF_FALSE,
    OP_JUMP,
    OP_SPRINT,
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
            20  => OP_FADD,
            21  => OP_FSUBTRACT,
            22  => OP_FMULTIPLY,
            23  => OP_FDIVIDE,
            24  => OP_SCONSTANT,
            25  => OP_LOADVAR,
            26  => OP_SETVAR,
            27  => OP_JUMP_IF_FALSE,
            28  => OP_JUMP,
            29  => OP_SPRINT,
            _   => OP_UNKNOWN,
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: ValueArray,
    pub heapConstants: HeapValueArray,
    pub lines: Vec<usize>
}

impl Chunk {
    pub fn new() -> Self {
        Chunk{
            code: Vec::new(),
            constants: ValueArray::new(),
            heapConstants: HeapValueArray::new() ,
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

pub fn writeu16Chunk(chunk: &mut Chunk, data: u16, line: usize) {
    for b in u16::to_le_bytes(data) {
        chunk.code.push(b);
        chunk.lines.push(line) ;
    }
}

pub fn addConstant(chunk: &mut Chunk, value: Value) -> u16 {
    writeValueArray(&mut chunk.constants, value) ;
    (chunk.constants.values.len()-1) as u16
}

pub fn addStringConstant(chunk: &mut Chunk, s: String) -> u16 {
    // Get the index of the heap constant to use as a pointer
    let curIndex = chunk.heapConstants.len() as u16;
    // Add the this pointer to the constants
    addConstant(chunk, Value::integer(curIndex as i64));
    chunk.heapConstants.push(HeapValue::string(s)) ;
    curIndex
}

pub fn getStringConstant(chunk: &mut Chunk,s: String) -> u16 {
    for i in 0..(chunk.heapConstants.len()-1) {
        let label = chunk.heapConstants[i].clone().getString() ;
        if label == s {
            return i as u16;
        }
    }
    0_u16
}
