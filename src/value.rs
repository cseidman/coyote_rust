#![macro_use]
use crate::heapvalue::*;


use std::fmt ;
use std::fmt::{Formatter, Display};
use std::io::{stderr, Write} ;
use std::rc::Rc;
use std::ops;
use std::cmp ;
use std::cmp::Ordering;
use rust_decimal::prelude::*;
use crate::ast::DataType;
use std::collections::{HashMap};
use std::ops::{Add, Sub};
use blake2::{Blake2b, Digest};
use blake2::digest::generic_array::{GenericArray};

pub type hashkey = GenericArray<u8, u64> ;

pub fn printValue(value: Value) {
    print!("{}",value ) ;
}

#[derive(Debug,Clone)]
pub struct Array {
    avalue: Vec<Value>
}

#[derive(Debug,Clone)]
pub struct Dict {
    hvalue: HashMap<Value,Value>
}

pub struct ValueArray{
    pub values: Vec<Value>
}

impl ValueArray {
    pub fn new() -> Self {
        ValueArray{
            values: Vec::new()
        }
    }
}

pub fn writeValueArray(array: &mut ValueArray, value: Value) {
    array.values.push(value) ;
}

#[derive(Debug,Clone,Copy, PartialOrd)]
pub enum Value {
    integer(i64),
    float(f64),
    i128(i128),
    decimal(Decimal),
    logical(bool),
    string(u16),
    dict(u16),
    array(u16),
    hash(u16),
    nil
}

impl Display for Value {

    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::logical(x) =>
                if *x {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                },
            Value::nil => write!(f, "nil"),
            //Value::array(_) => write!(f, "array"),
            //Value::dict(_) => write!(f, "dict"),
            Value::integer(x) => write!(f,"{}",x),
            Value::float(x) => write!(f,"{}",x),
            //Value::string(x) => write!(f,"{}",x),
            _ => write!(f,"(Unknown)"),
        }

    }
}

impl Value {

    pub fn get_integer(self) -> i64 {
        match self {
            Value::integer(x) => x,
            Value::float(x) => x as i64,
            _ => {
                panic!("Value is not a number") ;
            }
        }
    }

    pub fn get_float(self) -> f64 {
        match self {
            Value::float(x) => x,
            Value::integer(x) => x as f64,
            _ => {
                panic!("Value is not a number") ;
            }
        }
    }

    pub fn get_bool(self) -> bool {
        if let Value::logical(x) = self {
            x
        } else {
            panic!("Not a bool");
        }
    }

    pub fn get_string_pointer(self) -> u16 {
        if let Value::string(x) = self {
            x
        } else {
            panic!("Not a string pointer");
        }
    }

}

impl PartialEq for Value {

    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::integer(x) => {
                if let Value::integer(y) = other {
                    x==y
                } else {
                    panic!("Mismatch when trying to sub integers");
                }
            },
            Value::float(x) => {
                if let Value::float(y) = other {
                    x==y
                } else {
                    panic!("Mismatch when trying to sub floats");
                }
            },
            Value::i128(x) => {
                if let Value::i128(y) = other {
                    *x==*y
                } else {
                    panic!("Mismatch when trying to sub 128 bit integers");
                }
            },
            Value::decimal(x) => {
                if let Value::decimal(y) = other {
                    *x==*y
                } else {
                    panic!("Mismatch when trying to sub decimals");
                }
            },
            _=>panic!("Unable to subtract types")
        }

    }

}

impl Sub for Value {

    type Output = Value;


    fn sub(self, rhs: Self) -> Value {
        match self {
            Value::integer(x) => {
                if let Value::integer(y) = rhs {
                    Value::integer(x-y)
                } else {
                    panic!("Mismatch when trying to sub integers");
                }
            },
            Value::float(x) => {
                if let Value::float(y) = rhs {
                    Value::float(x-y)
                } else {
                    panic!("Mismatch when trying to sub floats");
                }
            },
            Value::i128(x) => {
                if let Value::i128(y) = rhs {
                    Value::i128(x-y)
                } else {
                    panic!("Mismatch when trying to sub 128 bit integers");
                }
            },
            Value::decimal(x) => {
                if let Value::decimal(y) = rhs {
                    Value::decimal(x-y)
                } else {
                    panic!("Mismatch when trying to sub decimals");
                }
            },
            _=>panic!("Unable to subtract types")
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Value {
        match self {
            Value::integer(x) => {
                if let Value::integer(y) = rhs {
                    Value::integer(x+y)
                } else {
                    panic!("Mismatch when trying to add integers");
                }
            },
            Value::float(x) => {
                if let Value::float(y) = rhs {
                    Value::float(x+y)
                } else {
                    panic!("Mismatch when trying to add floats");
                }
            },
            /*
            Value::string(mut x) => {
                if let Value::string(y) = rhs {
                    x.push_str(y.as_str());
                    Value::string(x)
                } else {
                    panic!("Mismatch when trying to add strings");
                }
            },
            */
            Value::i128(x) => {
                if let Value::i128(y) = rhs {
                    Value::i128(x+y)
                } else {
                    panic!("Mismatch when trying to add 128 bit integers");
                }
            },
            Value::decimal(x) => {
                if let Value::decimal(y) = rhs {
                    Value::decimal(x+y)
                } else {
                    panic!("Mismatch when trying to add decimals");
                }
            },
            _=>panic!("Unable to add types")
        }
    }


}

#[cfg(test)]
mod test {

    use super::* ;
    use std::collections::hash_map::Entry::Vacant;

    #[test]
    pub fn test_integer() {

        let lhVal = Value::integer(5) ;
        let rhVal = Value::integer(10) ;

        let result = lhVal + rhVal ;

        if let Value::integer(x) = result {
            println!("Result of 5 + 10 = {:?}", result) ;
            assert_eq!(x, 15);
        } else {
            assert!(false) ;
        }

    }
    #[test]
    pub fn test_float() {

        let lhVal = Value::float(5.0) ;
        let rhVal = Value::float(10.0) ;

        let result = lhVal + rhVal ;

        if let Value::float(x) = result {
            println!("Result of 5.0 + 10.0 = {:?}", result) ;
            assert_eq!(x, 15.0);
        } else {
            assert!(false) ;
        }
    }

}