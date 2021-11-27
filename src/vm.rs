use crate::chunk::{Chunk, OpCode};
use crate::vm::InterpretResult::{INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR};
use crate::value::{printValue} ;
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
    stack: [f64;8000],
    stackTop: usize,
    globals: HashMap<String, f64>
}

impl VM {

    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack:[0.0;8000],
            stackTop: 0,
            globals: HashMap::new()
        }
    }

    pub fn push(&mut self, value:f64) {
        self.stack[self.stackTop] = value;
        self.stackTop+=1 ;
    }

    pub fn peek(&mut self, distance: usize) -> f64 {
        let index = self.stackTop-1-distance ;
        self.stack[index]
    }

    pub fn pop(&mut self) -> f64 {
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
                  let mut val:[u8;8] = Default::default();
                  val.copy_from_slice(&self.chunk.code[self.ip..(self.ip+8)]) ;
                  self.ip+=8;
                  f64::from_be_bytes(val)
                }
            };
        }

        macro_rules! READ_STRING {
            () => {
                {
                    let idx = READ_OPERAND!() ;
                    self.chunk.strings.getValue(idx as usize)
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
            ($binop:tt, $type:ty) => {
                {
                    let rt = self.pop() as $type ;
                    let lt = self.pop() as $type;
                    let val = lt $binop rt ;
                    self.push(val as f64);
                }
            };
        }

        macro_rules! CMPOP {
            ($binop:tt) => {
                {
                    let rt = self.pop() ;
                    let lt = self.pop() ;
                    let val = lt $binop rt ;
                    self.push(boolAsf64(val));
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
                    self.push(constant as f64) ;
                },
                OP_PUSH => {
                    let val = READ_OPERAND!() ;
                    self.push(val) ;
                },
                OP_SPOP => {
                    let val = READ_OPERAND!() ;
                    self.push(val) ;
                }
                OP_NIL => { self.push(0.0)},
                OP_TRUE => { self.push(1.0)},
                OP_FALSE => { self.push(0.0)},
                OP_EQUAL => {
                    let a = self.pop() ;
                    let b = self.pop() ;
                    self.push(boolAsf64(a==b));
                },
                OP_GREATER => {CMPOP!(>)},
                OP_LESS => {CMPOP!(<)},

                OP_IADD => { BINOP!(+ ,i64)},
                OP_ISUBTRACT => { BINOP!(- ,i64)},
                OP_IMULTIPLY => { BINOP!(*, i64)},
                OP_IDIVIDE => {BINOP!(/, i64)},

                OP_FADD => { BINOP!(+ ,f64)},
                OP_FSUBTRACT => { BINOP!(- ,f64)},
                OP_FMULTIPLY => { BINOP!(*, f64)},
                OP_FDIVIDE => {BINOP!(/, f64)},

                OP_NOT => {
                  let value = self.pop() != 0.0;
                  self.push(boolAsf64(value)) ;
                },
                OP_INEGATE => {
                    let value = -(self.pop() as i64);
                    self.push(value as f64);
                },
                OP_FNEGATE => {
                    let value = -self.pop();
                    self.push(value);
                }
                OP_PRINT => {
                    let data = self.pop() ;
                    println!("{}", data) ;
                },
                OP_DEFINE_IGLOBAL=> {
                    let name = READ_STRING!().clone() ;
                    let value = self.pop() ;
                    self.globals.insert(name, value) ;
                },
                OP_GET_IGLOBAL => {
                    let name = READ_STRING!().clone() ;
                    let k = *self.globals.get(&name).unwrap() ;
                    self.push(k) ;
                }
                _ => {return INTERPRET_OK}
            }

        }

    }
}

