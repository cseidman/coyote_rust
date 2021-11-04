use crate::chunk::{Chunk, OpCode};
use crate::vm::InterpretResult::INTERPRET_OK;
use crate::value::{printValue, Value} ;
use crate::debug::* ;

use OpCode::* ;

pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
}

pub struct VM <'a>{
    chunk: Option<&'a Chunk>,
    ip: usize,
    stack: [Value;8000],
    stackTop: usize
}

impl<'a> VM<'a> {

    pub fn new() -> Self {
        VM {
            chunk: None,
            ip: 0,
            stack:[0.0;8000],
            stackTop: 0
        }
    }

    pub fn push(&mut self, value:Value) {
        self.stack[self.stackTop] = value;
        self.stackTop+=1 ;
    }

    pub fn pop(&mut self) -> Value {
        self.stackTop-=1 ;
        self.stack[self.stackTop]
    }

    pub fn reset(&mut self) {
        self.stackTop = 0 ;
    }

    pub fn interpret(&mut self, chunk: &'a Chunk) -> InterpretResult {

        self.chunk = Some(chunk) ;
        return self.run() ;


    }

    pub fn debug(&self) {
        if self.stackTop > 0 {
            print!("          ");
            for slot in 0..self.stackTop {
                print!("[ ");
                printValue(self.stack[slot]);
                print!(" ]");
            }
            println!();
        }
        disassembleInstruction(self.chunk.unwrap(), self.ip) ;
    }


    pub fn run(&mut self) -> InterpretResult {

        let chunk = self.chunk.unwrap() ;

        macro_rules! READ_BYTE {
            () => {
                {
                    self.ip+=1 ;
                    chunk.code[self.ip-1]
                }
            };
        }

        macro_rules! BINOP {
            ($binop:tt) => {
                {
                    let rt = self.pop() ;
                    let lt = self.pop() ;
                    self.push(lt $binop rt);
                }
            };
        }
        
        loop {

            self.debug();
            let instruction:OpCode = READ_BYTE!().into();

            match instruction {
                OP_RETURN => {
                    return INTERPRET_OK
                },
                OP_CONSTANT => {
                    let constIndex = READ_BYTE!() as usize;
                    let constant = chunk.constants.values[constIndex] ;
                    self.push(constant) ;
                },
                OP_ADD => { BINOP!(+)},
                OP_SUBTRACT => { BINOP!(-)},
                OP_MULTIPLY => { BINOP!(*)},
                OP_DIVIDE => {BINOP!(/)},
                OP_NEGATE => {
                    let val = -self.pop();
                    self.push(val);
                },
                _ => {return INTERPRET_OK}
            }

        }

    }
}