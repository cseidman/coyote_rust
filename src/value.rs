#![macro_use]

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
use std::cell::RefCell;

pub type hashkey = GenericArray<u8, u64> ;

pub fn printValue(value: Value) {
    print!("{}",value ) ;
}

// This is used for constants
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
#[derive(Debug,Clone, PartialOrd, PartialEq, Eq, Hash)]
pub enum HKey {
    HString(String),
    HInteger(i64)
}

impl HKey {
    pub fn new(value: Value) -> Self {
        match value {
            Value::string(x) => {
                HKey::HString(x)
            },
            Value::integer(x) => {
                HKey::HInteger(x)
            },
            _ => {
                panic!("Hash key must be a string or an integer!") ;
            }
        }

    }
}

impl Display for HKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HKey::HString(s) => {
                write!(f, "{}", s)
            },
            HKey::HInteger(i) => {
                write!(f, "{}", i)
            },
        }
    }
}
// Hash type
#[derive(Debug,Clone)]
pub struct Dict {
    hashVal: HashMap<HKey, Value>
}

impl Dict {
    pub fn new() -> Self {
        Self {
            hashVal: HashMap::new()
        }
    }

    pub fn insert(&mut self, key: HKey, value: Value) {
        self.hashVal.insert(key, value) ;
    }

    pub fn get(&self, key: HKey) -> Option<&Value> {
        self.hashVal.get(&key)
    }

}

impl PartialEq for Dict {
    fn eq(&self, other: &Self) -> bool {
        false
    }

    fn ne(&self, other: &Self) -> bool {
        false
    }
}

impl PartialOrd for Dict {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ordering::Equal)
    }

    fn lt(&self, other: &Self) -> bool {
        false
    }

    fn le(&self, other: &Self) -> bool {
        false
    }

    fn gt(&self, other: &Self) -> bool {
        false
    }

    fn ge(&self, other: &Self) -> bool {
        false
    }
}

// Array type
#[derive(Debug,Clone, PartialOrd, PartialEq)]
pub struct Array {
    size: usize,
    values: Vec<Value>,
    datatype: DataType
}

impl Array {

    pub fn new(size: usize, datatype: DataType) -> Self {
        Self {
            size,
            values: Vec::with_capacity(size),
            datatype
        }
    }

    pub fn getDataType(&self) -> DataType {
        self.datatype
    }

    pub fn check_size(&self, index: usize) {
        if index >= self.size {
            panic!("Can't address index {} in an array of size {}", index, self.size) ;
        }
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn put(&mut self, index: usize, value: Value) {
        self.check_size(index) ;
        self.values[index] = value ;
    }

    pub fn append(&mut self, value: Value) {
        self.values.push(value) ;
    }

    pub fn insert(&mut self, value: Value) {
        self.values.insert(0,value) ;
    }

    pub fn put_array(&mut self, value: Value) {

    }

    pub fn get(&self, index: usize) -> Value {
        self.check_size(index) ;
        self.values[index].clone()
    }

}
#[derive(Debug,Clone, PartialOrd)]
pub enum Visibilty {
    private,
    public
}

impl PartialEq for Visibilty {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug,Clone,PartialOrd)]
pub struct Property {
    pub name: String,
    pub visibility: Visibilty,
    pub propertyType: DataType
}

impl PartialEq for Property {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

pub fn writeValueArray(array: &mut ValueArray, value: Value) {
    array.values.push(value) ;
}

#[derive(Debug,Clone, PartialOrd)]
pub struct Class {
    pub properties: Vec<Property>
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialOrd)]
pub enum Value {
    integer(i64),
    float(f64),
    logical(bool),
    string(String),
    array(Array),
    class(Class),
    hash(Dict),
    nil,
    empty
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
            //Value::empty => write!(f, "(empty)"),
            Value::array(x) => {

                let mut counter = 0 ;
                write!(f, "[").unwrap();
                for e in &x.values {
                    if counter > 0 {
                        write!(f, ", ").unwrap();
                    }
                    counter += 1 ;
                    write!(f, "{}", e).unwrap();
                    if counter >= 10 {
                        write!(f, ", ..({} more elements)", x.size-10).unwrap();
                        break ;
                    }
                }
                write!(f, "]")
            },
            Value::hash(x) => {
                let mut counter = 0 ;
                write!(f, "@[").unwrap();
                for k in x.hashVal.keys() {
                    if counter > 0 {
                        write!(f, ", ").unwrap();
                    }
                    write!(f,"{}={}" ,k,x.hashVal[k]).unwrap();
                    counter+=1 ;
                }
                write!(f, "]")
            },
            Value::integer(x) => write!(f,"{}",x),
            Value::float(x) => write!(f,"{}",x),
            Value::string(x) => write!(f,"{}",x),
            _ => write!(f,"(Unknown)"),
        }

    }
}

impl Value {

    pub fn getDataType(&self) -> DataType {
        match self {
            Value::integer(..) => DataType::Integer,
            Value::float(..) => DataType::Float,
            Value::string(..) => DataType::String,
            Value::array(x) => x.datatype,
            Value::hash(..) => DataType::Dict,
            Value::logical(..) => DataType::Bool,
            Value::nil => DataType::Nil,
            Value::empty => DataType::None,
            Value::class(..) => DataType::Object,
        }
    }

    pub fn isNil(&self) -> bool {
        if let Value::nil = self {
           return true
        }
        false
    }

    pub fn isEmpty(&self) -> bool {
        if let Value::empty = self {
            return true
        }
        false
    }

    pub fn get_array(self) -> Array {
        if let Value::array(x) = self {
            return x;
        }
        panic!("Value {:?} is not an array", self) ;
    }

    pub fn get_array_mut(&mut self) -> &mut Array {
        if let Value::array(x) = self {
            return x;
        }
        panic!("Value {:?} is not an array", self) ;
    }

    pub fn get_dict(self) -> Dict {
        if let Value::hash(x) = self {
            return x;
        }
        panic!("Value {:?} is not a hash", self) ;
    }

    pub fn get_dict_mut(&mut self) -> &mut Dict {
        if let Value::hash(x) = self {
            return x;
        }
        panic!("Value {:?} is not a hash", self) ;
    }

    pub fn get_integer(self) -> i64 {
        match self {
            Value::integer(x) => x,
            Value::float(x) => x as i64,
            _ => {
                panic!("Value {:?} is not a number", self) ;
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

    pub fn get_string(self) -> String {
        if let Value::string(x) = self {
            x.to_string()
        } else {
            panic!("Not a string");
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
                    panic!("Mismatch when trying to equate {:?} and {:?} instead of integers", self, other);
                }
            },
            Value::float(x) => {
                if let Value::float(y) = other {
                    x==y
                } else {
                    panic!("Mismatch when trying to equate floats");
                }
            },
            Value::hash(x) => {
                false
            },
            Value::class(x) => {
                false
            },
            Value::array(x) => {
                if let Value::array(y) = other {

                    if y.size != x.size {
                        return false ;
                    }

                    for i in 0..x.size {
                        if x.values[i] != y.values[i] {
                            return false;
                        }
                    }
                    return true

                }
                panic!("Mismatch when trying to equate arrays");
            }
            _=>panic!("Unable to equate type {:?}", self )
        }

    }

}

impl Eq for Value {}

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