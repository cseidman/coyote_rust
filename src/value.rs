#![macro_use]
use crate::strings::*;

use std::fmt ;
use std::fmt::Formatter;
use std::rc::Rc;
use std::ops;
use std::cmp ;
use std::cmp::Ordering;

#[derive(Clone, Copy)]
pub enum Value {
  Number(f64),
  Pointer(u64),
  Bool(bool),
  Nil
}

impl cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        todo!()
    }

    fn lt(&self, other: &Self) -> bool {
       match self {
           Value::Number(x) => *x < {
               match other {
                   Value::Number(y) => *y,
                   _ => 0.0
               }
           },
           _ => false
       }
    }

    fn le(&self, other: &Self) -> bool {
        match self {
            Value::Number(x) => *x <= {
                match other {
                    Value::Number(y) => *y,
                    _ => 0.0
                }
            },
            _ => false
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match self {
            Value::Number(x) => *x > {
                match other {
                    Value::Number(y) => *y,
                    _ => 0.0
                }
            },
            _ => false
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match self {
            Value::Number(x) => *x >= {
                match other {
                    Value::Number(y) => *y,
                    _ => 0.0
                }
            },
            _ => false
        }
    }
}

impl cmp::PartialEq<Self> for Value {
    fn eq(&self, other: &Self) -> bool {

        match self {
            Value::Number(x) => *x == {
                match other {
                    Value::Number(y) => *y,
                    _ => 0.0
                }
            },
            Value::Bool(x) => *x == {
                match other {
                    Value::Bool(y) => *y,
                    _ => false
                }
            },
            Value::Nil => false,
            // Todo: Handle string
            _=> false
        }
    }
}

impl cmp::Eq for Value {}

impl ops::Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(x) => {
                Value::Bool(!x)
            },
            _ => { panic!("Can only NOT booleans!") }
        }
    }
}

impl ops::Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(x) => {
                Value::Number(-x)
            },
            _ => { panic!("Can only negate numbers!") }
        }
    }
}

impl ops::Sub for Value {
    type Output = Value;

    fn sub(self, _rhs: Value) -> Value {

        let rhVal = match _rhs {
            Value::Number(y) => y ,
            _ => 0.0
        };

        match self {
            Value::Number(x) => {
                Value::Number(x - rhVal)
            },
            _ => Value::Number(0.0)
        }
    }
}


impl ops::Mul for Value {
    type Output = Value;

    fn mul(self, _rhs: Value) -> Value {

        let rhVal = match _rhs {
            Value::Number(y) => y ,
            _ => 0.0
        };

        match self {
            Value::Number(x) => {
                Value::Number(x * rhVal)
            },
            _ => Value::Number(0.0)
        }
    }
}


impl ops::Add for Value {
    type Output = Value;

    fn add(self, _rhs: Value) -> Value {

        let rhVal = match _rhs {
            Value::Number(y) => y ,
            _ => 0.0
        };

        match self {
            Value::Number(x) => {
                Value::Number(x + rhVal)
            },
            _ => Value::Number(0.0)
        }
    }
}

impl ops::Div for Value {
    type Output = Value;

    fn div(self, _rhs: Value) -> Value {

        let rhVal = match _rhs {
            Value::Number(y) => y ,
            _ => 0.0
        };

        match self {
            Value::Number(x) => {
                Value::Number(x / rhVal)
            },
            _ => Value::Number(0.0)
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(x) => {write!(f, "{}",x)},
            Value::Bool(x) => {
                let mut bool_string = "true" ;
                if !x {
                    bool_string = "false" ;
                }
                write!(f, "{}",bool_string)
            }
            Value::Nil => {write!(f, "Nil")},
            Value::Pointer(x) => {
                write!(f, "Pointer: {}", x)
            },
            _ => {write!(f, "{}","Unknown value")},
        }
    }
}

pub fn printValue(number: &u64) {
    print!("{}",&number) ;
}

pub struct ValueArray{
    pub values: Vec<u64>
}

impl ValueArray {
    pub fn new() -> Self {
        ValueArray{
            values: Vec::new()
        }
    }
}

pub fn writeValueArray(array: &mut ValueArray, value: u64) {
    array.values.push(value) ;
}
