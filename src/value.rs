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
