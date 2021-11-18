#[macro_use]
use crate::chunk::{Chunk, OpCode};
use crate::vm::InterpretResult::{INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR};
use crate::value::{printValue, Value} ;
use crate::debug::* ;
use crate::compiler::{Compiler};

use OpCode::* ;
use std::borrow::Borrow;
use std::rc::Rc;
use std::io::{stderr, Write} ;

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
}

pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: [u64;8000],
    stackTop: usize
}

impl VM {

    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack:[0;8000],
            stackTop: 0
        }
    }

    pub fn push(&mut self, value:u64) {
        self.stack[self.stackTop] = value;
        self.stackTop+=1 ;
    }

    pub fn peek(&mut self, distance: usize) -> u64 {
        let index = self.stackTop-1-distance ;
        self.stack[index]
    }

    pub fn pop(&mut self) -> u64 {
        self.stackTop-=1 ;
        self.stack[self.stackTop]
    }

    pub fn reset(&mut self) {
        self.stackTop = 0 ;
    }

    pub fn Compile(&mut self, source: String) -> InterpretResult {
        let mut compiler = Compiler::new(source, &mut self.chunk);

        let res = compiler.compile();
        if !res {
            return INTERPRET_COMPILE_ERROR;
        }
        INTERPRET_OK
    }

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        self.Compile(source) ;
        self.run()
    }

    pub fn debug(&self) {
        if self.stackTop > 0 {
            print!("          ");
            for slot in 0..self.stackTop {
                print!("[ ");
                printValue(&self.stack[slot]);
                print!(" ]");
            }
            println!();
            disassembleInstruction(&self.chunk, self.ip) ;
        }

    }

    pub fn runtimeError(&self, message: &'static str) {
        let instruction = self.chunk.code[self.ip-1] ;
        let line = self.chunk.lines[self.ip-1];
        let _ = stderr().write_fmt(format_args!("[line {}] in script\n", line));

    }

    pub fn run(&mut self) -> InterpretResult {

        macro_rules! READ_OPERAND {
            () => {
                {
                 let val:[u8;8] = [
                        self.chunk.code[self.ip] ,
                        self.chunk.code[self.ip+1] ,
                        self.chunk.code[self.ip+2] ,
                        self.chunk.code[self.ip+3] ,
                        self.chunk.code[self.ip+4] ,
                        self.chunk.code[self.ip+5] ,
                        self.chunk.code[self.ip+6] ,
                        self.chunk.code[self.ip+7]
                    ];
                    self.ip+=8;
                    u64::from_be_bytes(val)
                }
            };
        }

        macro_rules! READ_BYTE {
            () => {
                {
                    self.ip+=1 ;
                    self.chunk.code[self.ip-1]
                }
            };
        }

        macro_rules! BINOP {
            ($binop:tt) => {
                {
                    let rt = self.pop() ;
                    let lt = self.pop() ;
                    let val = lt $binop rt ;
                    self.push(val);
                }
            };
        }

        macro_rules! CMPOP {
            ($binop:tt) => {
                {
                    let rt = self.pop() ;
                    let lt = self.pop() ;
                    let val = lt $binop rt ;
                    self.push(val as u64);
                }
            };
        }
        
        loop {

            self.debug();
            let instruction:OpCode = READ_BYTE!().into();

            match instruction {
                OP_RETURN => {
                    self.pop();
                    return INTERPRET_OK
                },
                OP_CONSTANT => {
                    let constIndex = READ_BYTE!() as usize;
                    let constant = self.chunk.constants.values[constIndex] ;
                    self.push(constant as u64) ;
                },
                OP_IPUSH => {
                    let val = READ_OPERAND!() ;
                    self.push(val) ;
                },
                OP_STRING => {
                    let val = READ_OPERAND!() ;
                    self.push(val) ;
                }
                OP_NIL => { self.push(0)},
                OP_TRUE => { self.push(1)},
                OP_FALSE => { self.push(0)},
                OP_EQUAL => {
                    let a = self.pop() ;
                    let b = self.pop() ;
                    self.push((a==b) as u64);
                },
                OP_GREATER => {CMPOP!(>)},
                OP_LESS => {CMPOP!(<)},
                OP_ADD => { BINOP!(+)},
                OP_SUBTRACT => { BINOP!(-)},
                OP_MULTIPLY => { BINOP!(*)},
                OP_DIVIDE => {BINOP!(/)},
                OP_NOT => {
                  let value = self.pop() ;
                  self.push(!value) ;
                },
                OP_NEGATE => {
                    // TODO: Make sure to set this to integer and float values
                    let mut value = self.pop() as i64;
                    self.push(-value as u64);
                },
                OP_PRINT => {
                    let data = self.pop() ;
                    println!("{}", data) ;
                }
                _ => {return INTERPRET_OK}
            }

        }

    }
}

