#![macro_use]
use crate::strings::*;

use std::fmt ;
use std::fmt::Formatter;
use std::rc::Rc;
use std::ops;
use std::cmp ;
use std::cmp::Ordering;

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
