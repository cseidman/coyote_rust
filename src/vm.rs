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
    stack: [Value;8000],
    stackTop: usize
}

impl VM {

    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack:[NUMBER_VAL!(0.0);8000],
            stackTop: 0
        }
    }

    pub fn push(&mut self, value:Value) {
        self.stack[self.stackTop] = value;
        self.stackTop+=1 ;
    }

    pub fn peek(&mut self, distance: usize) -> Value {
        let index = self.stackTop-1-distance ;
        self.stack[index]
    }

    pub fn pop(&mut self) -> Value {
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
        }
        disassembleInstruction(&self.chunk, self.ip) ;
    }

    pub fn runtimeError(&self, message: &'static str) {
        let instruction = self.chunk.code[self.ip-1] ;
        let line = self.chunk.lines[self.ip-1];
        let _ = stderr().write_fmt(format_args!("[line {}] in script\n", line));
    }

    pub fn run(&mut self) -> InterpretResult {

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
                    self.push(BOOL_VAL!(val));
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
                    self.push(constant) ;
                },
                OP_NIL => { self.push(NIL_VAL!()) },
                OP_TRUE => { self.push(BOOL_VAL!(true)) },
                OP_FALSE => { self.push(BOOL_VAL!(false)) },
                OP_EQUAL => {
                    let a = self.pop() ;
                    let b = self.pop() ;
                    self.push(BOOL_VAL!(a==b));
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
                    let peeked_value = self.peek(0) ;
                    match peeked_value {
                        Value::Number(_) => {
                            self.pop() ;
                            self.push(-peeked_value);
                        } ,
                        _ => {
                            self.runtimeError("Only numbers can be negated");
                            return INTERPRET_RUNTIME_ERROR ;
                        }
                    }
                },
                _ => {return INTERPRET_OK}
            }

        }

    }
}

