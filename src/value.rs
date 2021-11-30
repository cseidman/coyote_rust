#![macro_use]
use crate::strings::*;

use std::fmt ;
use std::fmt::Formatter;
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

pub fn printValue(number: f64) {
    print!("{}",number ) ;
}

pub struct ValueArray{
    pub values: Vec<f64>
}

impl ValueArray {
    pub fn new() -> Self {
        ValueArray{
            values: Vec::new()
        }
    }
}

pub fn writeValueArray(array: &mut ValueArray, value: f64) {
    array.values.push(value) ;
}

#[derive(Debug)]
pub enum Value {
    integer(i64),
    float(f64),
    i128(Box<i128>),
    decimal(Box<Decimal>),
    logical(bool),
    string(Box<String>),
    dict(Box<HashMap<Value,Value>>),
    array(Vec<Value>),
    //hash(Box<hashkey>),
    nil
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
            Value::string(mut x) => {
                if let Value::string(y) = rhs {
                    x.push_str(y.as_str());
                    Value::string(x)
                } else {
                    panic!("Mismatch when trying to add strings");
                }
            },
            Value::i128(x) => {
                if let Value::i128(y) = rhs {
                    Value::i128(Box::new(*x+*y))
                } else {
                    panic!("Mismatch when trying to add 128 bit integers");
                }
            },
            Value::decimal(x) => {
                if let Value::decimal(y) = rhs {
                    Value::decimal(Box::new(*x+*y))
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

    #[test]
    pub fn test_string() {

        let lhVal = Value::string(Box::new("Hey".to_string())) ;
        let rhVal = Value::string(Box::new(" hey!".to_string())) ;

        let result = lhVal + rhVal ;

        if let Value::string(x) = &result {
            println!("Result of 'Hey' + ' hey!' = {:?}", result) ;
            assert_eq!(**x, "Hey hey!".to_string());
        } else {
            assert!(false) ;
        }

    }

}