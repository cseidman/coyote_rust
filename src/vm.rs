use crate::chunk::{Chunk, OpCode};
use crate::vm::InterpretResult::{INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR};
use crate::value::{printValue, Value, Array, Dict, HKey};
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
use std::cell::RefCell;
use crate::ast::DataType;

pub struct Frame<'a> {
    pub chunk: &'a mut Chunk,
    pub slotPtr: usize,
    pub data: &'a mut [Value],
    pub ip: &'a mut usize
}

pub struct VM {
    chunk: Chunk,
    ip: usize,
    
    stack: Vec<Value>,
    stackTop: usize,
}

impl<'a> VM {

    pub fn new() -> Self {

        let mut vm = VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(1024000),
            stackTop: 8092, // Start at an arbitrary position

        } ;

        for _ in 0 .. 1024000 {
            vm.stack.push(Value::empty);
        }
        vm
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

    pub fn runtimeError(&self, message: &'static str) {
        let instruction = self.chunk.code[self.ip-1] ;
        let line = self.chunk.lines[self.ip-1];
        let _ = stderr().write_fmt(format_args!("[line {}] in script\n", line));
    }

    pub fn run(&'a mut self) -> InterpretResult {

        let f = Frame {
            chunk: &mut self.chunk,
            ip: &mut self.ip,
            slotPtr: 0,
            data: &mut self.stack[self.stackTop..]
        } ;

        let mut frames: Vec<Frame<'a>> = Vec::new() ;
        frames.push(f) ;
        let mut fPtr:usize = 0 ;
       
        macro_rules! READ_OPERAND {
            () => {
                {
                  let mut val:[u8;2] = Default::default();
                  let ip = *frames[fPtr].ip ;

                  val.copy_from_slice(&frames[fPtr]
                    .chunk
                    .code[ip..(ip+2)]) ;

                  *frames[fPtr].ip+=2;
                  u16::from_le_bytes(val)
                }
            };
        }

        macro_rules! READ_BYTE {
            () => {{
                let p = *frames[fPtr].ip;
                *frames[fPtr].ip+=1;
                frames[fPtr].chunk.code[p]
            }};
        }

        macro_rules! push {
            ($value:expr) => {
                let ptr = frames[fPtr].slotPtr ;
                frames[fPtr].data[ptr] = $value ;
                frames[fPtr].slotPtr+=1 ;
            };
        }

        macro_rules! pop {
            () => {{
                frames[fPtr].slotPtr-=1 ;
                frames[fPtr].data[frames[fPtr].slotPtr].clone()
            }};
        }

        macro_rules! peek {
            ($distance:expr) => {{
                frames[fPtr].data[frames[fPtr].slotPtr-$distance-1].clone()
            }};
        }

        macro_rules! IBINOP {
            ($binop:tt) => {{
                let rh = pop!().get_integer() ;
                let lh = pop!().get_integer() ;
                push!(Value::integer(lh $binop rh));
            }};
        }

        macro_rules! FBINOP {
            ($binop:tt) => {
                {
                    let rh = pop!().get_float() ;
                    let lh = pop!().get_float() ;
                    push!(Value::float(lh $binop rh));
                }
            };
        }

        macro_rules! CPIPOP {
            ($binop:tt) => {
                {
                    let rt = pop!().get_integer() ;
                    let lt = pop!().get_integer() ;
                    let val = Value::logical(lt $binop rt) ;
                    push!(val);
                }
            };
        }

        macro_rules! CPFPOP {
            ($binop:tt) => {
                {
                    let rt = pop!().get_float() ;
                    let lt = pop!().get_float() ;
                    let val = Value::logical(lt $binop rt) ;
                    push!(val);
                }
            };
        }

        macro_rules! CPSPOP {
            ($binop:tt) => {
                {
                    let rt = pop!().get_string();
                    let lt = pop!().get_string() ;
                    let val = Value::logical(lt $binop rt) ;
                    push!(val);
                }
            };
        }

        macro_rules! get_function {
            ($loc:expr) => {
                {
                    let f  = frames[fPtr]
                        .chunk
                        .functionStore[$loc as usize]
                        .clone() ;
                    f.get_function().clone()
                }
            };
        }

        macro_rules! setArrayElement {
            ($datatype:expr, $slot:expr) => {
                let mut array = frames[fPtr].data[$slot].clone();

                let index = pop!().get_integer() as usize;
                let value = pop!();

                let ar = array.get_array_mut();

                let arrayDataType = ar.getDataType();
                if arrayDataType != $datatype {
                    panic!("Incompatible data types for array") ;
                }

                ar.put(index, value);
                frames[fPtr].data[$slot] = array;
            };
        }

        loop {

            /*** Debug ***/
            print!("          ");
            for slot in 0..frames[fPtr].slotPtr {

                let val = frames[fPtr].data[slot].clone() ;
                if val.isEmpty() {
                    continue;
                }

                print!("[ ");
                printValue(val);
                print!(" ]");
            }
            println!();
            disassembleInstruction(&frames[fPtr].chunk, *frames[fPtr].ip) ;
            /*** Debug ***/

            let instruction:OpCode = READ_BYTE!().into();

            match instruction {

                OP_PUSH => {
                    let value = READ_OPERAND!() as i64;
                    push!(Value::integer(value)) ;
                },
                OP_CONSTANT => {
                    let constIndex = READ_OPERAND!() as usize;

                    let constant = frames[fPtr].chunk.constants.values[constIndex].clone() ;
                    push!(constant) ;
                },
                OP_SCONSTANT => {
                    let constIndex = READ_OPERAND!() as usize;
                    // This is the pointer to the string in the heapValue array
                    let stringValue = frames[fPtr].chunk.constants.values[constIndex].clone() ;
                    push!(stringValue) ;
                },
                OP_NIL => { push!(Value::nil);},
                OP_TRUE => { push!(Value::logical(true));},
                OP_FALSE => { push!(Value::logical(false));},
                OP_EQUAL => {
                    let a = pop!() ;
                    let b = pop!() ;
                    push!(Value::logical(a==b));
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
                    let value = !pop!().get_bool();
                    push!(Value::logical(value)) ;
                },

                OP_INEGATE => {
                    let value = -pop!().get_integer();
                    push!(Value::integer(value));
                },

                OP_FNEGATE => {
                    let value = -pop!().get_float();
                    push!(Value::float(value));
                }

                OP_LOADVAR => {
                    let slot = READ_OPERAND!() as usize;
                    let val = frames[fPtr].data[slot+8092].clone() ;
                    push!(val) ;
                }

                OP_SETVAR => {
                    let slot = READ_OPERAND!() as usize;
                    frames[fPtr].data[slot+8092] = pop!() ;
                 }

                OP_PRINT => {
                    let data = pop!() ;
                    println!("{}", data) ;
                },

                OP_SPRINT => {
                    let s = pop!().get_string();
                    println!("{}", s) ;
                },

                OP_JUMP_IF_FALSE => {

                    let logicalResult = pop!().get_bool() ;
                    let jumpto = READ_OPERAND!() as usize;

                    if !logicalResult {
                        *frames[fPtr].ip += jumpto ;
                    }
                },

                OP_JUMP_IF_FALSE_NOPOP => {

                    let logicalResult = peek!(0).get_bool() ;
                    let jumpto = READ_OPERAND!() as usize;

                    if !logicalResult {
                        *frames[fPtr].ip += jumpto ;
                    }

                },

                OP_JUMP =>{
                    *frames[fPtr].ip += READ_OPERAND!() as usize;
                },

                OP_LOOP => {
                    *frames[fPtr].ip -= READ_OPERAND!() as usize
                },

                OP_NOP => {},
                OP_POP => {
                    pop!();
                },

                OP_IGETAELEMENT
                | OP_SGETAELEMENT
                | OP_BGETAELEMENT
                | OP_FGETAELEMENT => {
                    let index = pop!().get_integer() as usize;

                    let slot = READ_OPERAND!() as usize;
                    let array = frames[fPtr].data[slot+8092].clone().get_array() ;

                    push!(array.get(index));

                },

                OP_SETHELEMENT => {

                    let slot = READ_OPERAND!() as usize;
                    let mut hash = frames[fPtr].data[slot].clone() ;

                    let key = HKey::new(pop!());
                    let value = pop!() ;

                    let h = hash.get_dict_mut();

                    h.insert(key, value);
                    frames[fPtr].data[slot] = hash ;
                },

                OP_ISETAELEMENT => {
                    let index = READ_OPERAND!() as usize;
                    setArrayElement!(DataType::Integer, index);
                },
                OP_SSETAELEMENT => {
                    let slot = READ_OPERAND!() as usize;
                    setArrayElement!(DataType::String, slot);
                },
                OP_BSETAELEMENT => {
                    let slot = READ_OPERAND!() as usize;
                    setArrayElement!(DataType::Bool, slot);
                },
                OP_FSETAELEMENT => {
                    let slot = READ_OPERAND!() as usize;
                    setArrayElement!(DataType::Float, slot);
                },

                OP_NEWARRAY => {

                    let datatype = DataType::from_operand(pop!().get_integer() as u16) ;

                    let arity = pop!().get_integer() as usize;
                    let mut array = Array::new(arity, datatype) ;

                    for _ in 0..arity {
                        array.insert(pop!().clone()) ;
                    }

                    push!(Value::array(array));

                }

                OP_GETHELEMENT => {
                    let key = pop!().clone();

                    let k = HKey::new(key) ;

                    let slot = READ_OPERAND!() as usize;
                    let dict = frames[fPtr].data[slot].clone().get_dict() ;
                    let value = dict.get(k).unwrap_or(&Value::nil).clone();
                    push!(value);

                },

                OP_NEWDICT => {
                    let mut dict = Value::hash(Dict::new());
                    let h = dict.get_dict_mut();

                    let arity = pop!().get_integer() as usize;

                    for _ in 0..arity {

                        let val = pop!().clone();
                        let key = pop!().clone();

                        let k = HKey::new(key) ;

                        h.insert(k,val) ;
                    }

                    push!(dict) ;
                },
                OP_RETURN => {
                    frames.pop();
                    return INTERPRET_OK
                },
                OP_CALL => {

                    let args = READ_OPERAND!() ;

                    let fnc = get_function!(args) ;

                },

                _ => {return INTERPRET_OK}
            }

        }

    }

}

