#![macro_use]

use std::fmt ;
use std::fmt::Formatter;
use std::rc::Rc;
use std::ops;
use std::ops::Neg;

macro_rules! NUMBER_VAL {($value:expr) => {Value::Number($value)};}
macro_rules! AS_NUMBER {
    ($value:expr) => {{
         match $value {
             Value::Number(x) => x ,
             _=> 0.0
         }
    }};
}

macro_rules! STRING_VAL {($value:expr) => {Value::String($value)};}
macro_rules! AS_STRING {($value:expr) => {if let $value = Value::String(x) {x}};}

macro_rules! BOOL_VAL {($value:expr) => {Value::Bool($value)};}
macro_rules! AS_BOOL {($value:expr) => {if let $value = Value::Bool(x) {x}};}


#[derive(Clone, Copy)]
pub enum Value {
  Number(f64),
  String(&'static str),
  Bool(bool)
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
            Value::String(x) => {write!(f, "{}",x)},
            Value::Bool(x) => {write!(f, "{}",x)}
        }
    }
}

pub fn printValue(value: &Value) {
    print!("{}",AS_NUMBER!(*value)) ;
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
