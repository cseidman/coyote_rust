use crate::chunk::{Chunk, OpCode};
use crate::vm::InterpretResult::{INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR};
use crate::value::{printValue, Value} ;
use crate::debug::* ;
use crate::compiler::{Compiler};
use crate::common::{boolAsf64};

use OpCode::* ;
use std::borrow::Borrow;
use std::collections::HashMap;
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
    stackTop: usize,
    globals: HashMap<String, Value>
}

impl VM {

    pub fn new() -> Self {

        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack:[Value::nil;8000],
            stackTop: 0,
            globals: HashMap::new()
        }
    }

    pub fn push(&mut self, value:Value) {
        self.stack[self.stackTop] = value;
        self.stackTop+=1 ;
    }

    pub fn peek(&mut self, distance: usize) -> Value {
        let index = self.stackTop-1-distance ;
        self.stack[index].clone()
    }

    pub fn pop(&mut self) -> Value {
        self.stackTop-=1 ;
        let idx = self.stackTop ;
        self.stack[idx]
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

        print!("          ");
        for slot in 0..self.stackTop {
            print!("[ ");
            let val = self.stack[slot] ;
            printValue(val);
            print!(" ]");
        }
        println!();
        disassembleInstruction(&self.chunk, self.ip) ;

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
                  let mut val:[u8;2] = Default::default();
                  val.copy_from_slice(&self.chunk.code[self.ip..(self.ip+2)]) ;
                  self.ip+=2;
                  u16::from_le_bytes(val)
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

        macro_rules! IBINOP {
            ($binop:tt) => {
                {
                    let rh = self.pop().get_integer() ;
                    let lh = self.pop().get_integer() ;
                    self.push(Value::integer(lh $binop rh));
                }
            };
        }

        macro_rules! FBINOP {
            ($binop:tt) => {
                {
                    let rh = self.pop().get_float() ;
                    let lh = self.pop().get_float() ;
                    self.push(Value::float(lh $binop rh));
                }
            };
        }

        macro_rules! CMPOP {
            ($binop:tt) => {
                {
                    let rt = self.pop().get_bool() ;
                    let lt = self.pop().get_bool() ;
                    let val = Value::logical(lt $binop rt) ;
                    self.push(val);
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
                    let constIndex = READ_OPERAND!() as usize;
                    let constant = self.chunk.constants.values[constIndex].clone() ;
                    self.push(constant) ;
                },
                OP_SCONSTANT => {
                    let constIndex = READ_OPERAND!() as usize;
                    let constant = self.chunk.constants.values[constIndex].clone() ;
                    self.push(constant) ;
                },
                OP_NIL => { self.push(Value::nil)},
                OP_TRUE => { self.push(Value::logical(true))},
                OP_FALSE => { self.push(Value::logical(false))},
                OP_EQUAL => {
                    let a = self.pop() ;
                    let b = self.pop() ;
                    self.push(Value::logical(a==b));
                },
                OP_GREATER => {CMPOP!(>)},
                OP_LESS => {CMPOP!(<)},
                OP_IADD => {IBINOP!(+)},
                OP_FADD => {FBINOP!(+)},
                OP_ISUBTRACT => {IBINOP!(-)},
                OP_FSUBTRACT => {FBINOP!(-)},
                OP_IMULTIPLY => {IBINOP!(*)},
                OP_FMULTIPLY => {FBINOP!(*)},
                OP_IDIVIDE => {IBINOP!(*)},
                OP_FDIVIDE => {FBINOP!(/)},

                OP_NOT => {
                  let value = !self.pop().get_bool();
                  self.push(Value::logical(value)) ;
                },
                OP_INEGATE => {
                    let value = -self.pop().get_integer();
                    self.push(Value::integer(value));
                },
                OP_FNEGATE => {
                    let value = -self.pop().get_float();
                    self.push(Value::float(value));
                }
                OP_PRINT => {
                    let data = self.pop() ;
                    println!("{}", data) ;
                },
                OP_DEFINE_IGLOBAL=> {
                    //let name = READ_STRING!().clone() ;
                    //let value = self.pop().clone() ;
                    //self.globals.insert(name, value) ;
                },
                OP_GET_IGLOBAL => {
                    //let name = READ_STRING!().clone() ;
                    //let k = self.globals.get(&name).unwrap().clone() ;
                    //self.push(k) ;
                }
                _ => {return INTERPRET_OK}
            }

        }

    }
}

