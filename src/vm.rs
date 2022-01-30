use crate::chunk::{Chunk, OpCode};
use crate::vm::InterpretResult::{INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR};
use crate::value::{printValue, Value} ;
use crate::debug::* ;
use crate::compiler::{Compiler};
use crate::common::{boolAsf64};
use crate::errors::{InterpretResult} ;
use InterpretResult::* ;

use OpCode::* ;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;
use std::io::{stderr, Write,self,stdin,stdout} ;
use std::ops::Deref;

pub struct Frame<'a> {
    slots: &'a [Value],
    ip: usize
}

pub struct VM<'a> {
    chunk: Chunk,
    ip: usize,
    
    stack: Vec<Value>,
    stackTop: usize,
    
    frames: Vec<Frame<'a>>,

}

impl<'a> VM<'a> {

    pub fn new() -> Self {

        let mut vm = VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(16000000),
            stackTop: 64000, // Start at an arbitrary position
            frames: Vec::new(),
        } ;

        for _ in 0 .. 64000 {
            vm.stack.push(Value::empty);
        }
        vm
    }

    pub fn push(&mut self, value:Value) {
        //self.stack[self.stackTop] = value;
        self.stack.push(value);
        self.stackTop+=1 ;
    }

    pub fn peek(&mut self, distance: usize) -> Value {
        let index = self.stackTop-1-distance ;
        self.stack[index].clone()
    }

    pub fn pop(&mut self) -> Value {
        self.stackTop-=1 ;
        //let idx = self.stackTop ;
        //let value = self.stack[idx].clone() ;
        self.stack.pop().unwrap()

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
        let compileResult = self.Compile(source) ;
        if compileResult == INTERPRET_OK {
            return self.run();
        }
        compileResult
    }

    pub fn debug(&self) {

        print!("          ");
        for slot in 64000..self.stackTop {

            let val = self.stack[slot].clone() ;
            if val.isEmpty() {
                continue;
            }

            print!("[ ");
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

        macro_rules! CPIPOP {
            ($binop:tt) => {
                {
                    let rt = self.pop().get_integer() ;
                    let lt = self.pop().get_integer() ;
                    let val = Value::logical(lt $binop rt) ;
                    self.push(val);
                }
            };
        }

        macro_rules! CPFPOP {
            ($binop:tt) => {
                {
                    let rt = self.pop().get_float() ;
                    let lt = self.pop().get_float() ;
                    let val = Value::logical(lt $binop rt) ;
                    self.push(val);
                }
            };
        }

        macro_rules! CPSPOP {
            ($binop:tt) => {
                {
                    let rt = self.pop().get_string();
                    let lt = self.pop().get_string() ;
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
                    //self.pop();
                    return INTERPRET_OK
                },
                OP_CONSTANT => {
                    let constIndex = READ_OPERAND!() as usize;
                    let constant = self.chunk.constants.values[constIndex].clone() ;
                    self.push(constant) ;
                },
                OP_SCONSTANT => {
                    let constIndex = READ_OPERAND!() as usize;
                    // This is the pointer to the string in the heapValue array
                    let stringValue = self.chunk.constants.values[constIndex].clone() ;
                    self.push(stringValue) ;
                },
                OP_NIL => { self.push(Value::nil)},
                OP_TRUE => { self.push(Value::logical(true))},
                OP_FALSE => { self.push(Value::logical(false))},
                OP_EQUAL => {
                    let a = self.pop() ;
                    let b = self.pop() ;
                    self.push(Value::logical(a==b));
                },
                OP_GREATER => {CPIPOP!(>)},
                OP_LESS => {CPIPOP!(<)},

                OP_IADD => {IBINOP!(+)},
                OP_FADD => {FBINOP!(+)},

                OP_ISUBTRACT => {IBINOP!(-)},
                OP_FSUBTRACT => {FBINOP!(-)},

                OP_IMULTIPLY => {IBINOP!(*)},
                OP_FMULTIPLY => {FBINOP!(*)},

                OP_IDIVIDE => {IBINOP!(/)},
                OP_FDIVIDE => {FBINOP!(/)},

                OP_IEQ => {CPIPOP!(==)},
                OP_FEQ => {CPFPOP!(==)},
                OP_SEQ => {CPSPOP!(==)},

                OP_INEQ => {CPIPOP!(!=)},
                OP_FNEQ => {CPFPOP!(!=)},
                OP_SNEQ => {CPSPOP!(!=)},

                OP_IGT => {CPIPOP!(>)},
                OP_FGT => {CPFPOP!(>)},
                OP_SGT => {CPSPOP!(>)},

                OP_ILT => {CPIPOP!(<)},
                OP_FLT => {CPFPOP!(<)},
                OP_SLT => {CPSPOP!(<)},

                OP_IGTEQ => {CPIPOP!(>=)},
                OP_FGTEQ => {CPFPOP!(>=)},
                OP_SGTEQ => {CPSPOP!(>=)},

                OP_ILTEQ => {CPIPOP!(<=)},
                OP_FLTEQ => {CPFPOP!(<=)},
                OP_SLTEQ => {CPSPOP!(<=)},

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

                OP_LOADVAR => {
                    let slot = READ_OPERAND!() as usize;
                    self.push(self.stack[slot].clone()) ;
                }

                OP_SETVAR => {
                    let slot = READ_OPERAND!() as usize;
                    self.stack[slot] = self.pop() ;
                 }

                OP_PRINT => {
                    let data = self.pop() ;
                    println!("{}", data) ;
                },

                OP_SPRINT => {
                    let s = self.pop().get_string();
                    println!("{}", s) ;
                },

                OP_JUMP_IF_FALSE => {

                    let logicalResult = self.pop().get_bool() ;
                    let jumpto = READ_OPERAND!() as usize;

                    if !logicalResult {
                        self.ip += jumpto ;
                    }
                },

                OP_JUMP_IF_FALSE_NOPOP => {

                    let logicalResult = self.peek(0).get_bool() ;
                    let jumpto = READ_OPERAND!() as usize;

                    if !logicalResult {
                        self.ip += jumpto ;
                    }

                },

                OP_JUMP =>{
                    self.ip += READ_OPERAND!() as usize;
                },

                OP_LOOP => {
                    self.ip -= READ_OPERAND!() as usize
                },

                OP_NOP => {},
                OP_POP => {
                    self.pop();
                },

                OP_NEWARRAY => {
                    let arity = self.pop().get_integer() as usize;
                    let aValue:Vec<Value> = Vec::with_capacity(arity) ;
                }
                _ => {return INTERPRET_OK}
            }

        }

    }
}

