#![macro_use]
use crate::strings::*;

use std::fmt ;
use std::fmt::Formatter;
use std::rc::Rc;
use std::ops;
use std::cmp ;
use std::cmp::Ordering;
use crate::ast::DataType;

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

pub enum ValueType {
    integer(i64),
    float(f64),
    logical(bool),
    string(Box<String>)
}

pub struct Value {
    valuetype: ValueType
}

