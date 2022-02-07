use crate::scanner::{TokenType, Token} ;
use TokenType::* ;
use crate::chunk::{Chunk, writeChunk, backPatch, OpCode, writeu16Chunk, addConstant, currentLocation};
use crate::chunk::OpCode::*;
use crate::compiler::* ;
use crate::value::{Value};
use std::env::var;
use std::str::{FromStr};

use crate::errors::{InterpretResult, ReportError};
use InterpretResult::* ;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialOrd, PartialEq)]
pub enum JumpType {
    jumpIfFalse,
    Jump
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum JumpPop {
    POP,
    NOPOP
}

#[derive(PartialEq, Copy, Clone)]
pub enum OpType {
    Literal,
    Binop,
    Unary,
    Statement,
    Root
}

// This struct is used to determine to what extent
// types are compatible for binary operations
#[derive(PartialEq, PartialOrd, Copy, Clone, Debug)]
pub enum DataType {
    Integer,
    Float,
    String,
    Bool,
    IArray,
    FArray,
    SArray,
    BArray,
    Dict,
    Object,
    Nil,
    None
}

impl DataType {

    pub fn to_operand(&self) -> u16 {
        match self {
            DataType::Integer => 10,
            DataType::Float => 20,
            DataType::String => 30,
            DataType::Bool => 40,
            DataType::IArray => 50,
            DataType::FArray => 60,
            DataType::SArray => 70,
            DataType::BArray => 80,
            DataType::Dict => 90,
            DataType::Object => 100,
            DataType::Nil => 110,
            DataType::None => 0
        }

    }

    pub fn from_operand(d: u16) -> DataType {
        match d {
            10 => DataType::Integer ,
            20 => DataType::Float ,
            30 => DataType::String,
            40 => DataType::Bool,
            50 => DataType::IArray,
            60 => DataType::FArray,
            70 => DataType::SArray,
            80 => DataType::BArray,
            90 => DataType::Dict,
            100 => DataType::Object,
            110 => DataType::Nil,
            0 => DataType::None,
            _ => DataType::None,
        }

    }

    pub fn emit(&self) -> String {
        match self {
            DataType::Integer => "I",
            DataType::Float => "F",
            DataType::String => "S",
            DataType::Bool => "B",
            _ => "_"
        }.to_string()
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
    Eq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    NEq
}
impl Operator {
    pub fn emit(&self) -> String {
        match self {
            Operator::Plus  => "ADD",
            Operator::Minus => "SUB",
            Operator::Div   => "DIV",
            Operator::Mul   => "MUL",
            Operator::Eq    => "EQ",
            Operator::Gt    => "GT",
            Operator::Lt    => "LT",
            Operator::GtEq  => "GTEQ",
            Operator::LtEq  => "LTEQ",
            Operator::NEq   => "NEQ"
        }.to_string()
    }
}

#[derive(Clone, Debug, PartialOrd, PartialEq)]
pub enum Node {
    Value {
        line: usize,
        label: String,
        value: Value,
        dataType: DataType
    },
    UnaryExpr {
        line: usize,
        op: Operator,
        child: Box<Node>,
    },
    BinaryExpr {
        line: usize,
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Logical {
        line: usize,
        expr: Vec<Node>
    },

    Array {
        line: usize,
        arity: usize,
        valueType: DataType,
        elementType: DataType,
        values: Vec<Node>
    },

    Dict {
        line: usize,
        arity: usize,
        keys: Vec<Node>,
        values: Vec<Node>
    },

    VarDecl {
        line: usize,
        name: String ,
        assigned: bool,
        varExpr: Box<Node>
    },
    setVar {
        line: usize,
        name: String ,
        datatype: DataType,
        child: Box<Node>
    },
    namedVar {
        line: usize,
        name: String
    },
    setArray {
        line: usize,
        name: String,
        index: Box<Node>,
        child: Box<Node>
    },

    setHash {
        line: usize,
        name: String,
        key: Box<Node>,
        child: Box<Node>
    },

    namedArray {
        line: usize,
        name: String,
        index: Box<Node>
    },

    class {
        line: usize,
        propertyCount: usize,
        properties: Vec<Node>
    },

    namedHash {
        line: usize,
        name: String,
        key: Box<Node>
    },

    Return {
        line: usize,
        returnVal: Box<Node>
    },
    Root {
        children: Vec<Node>
    },
    Print {
        line: usize,
        printExpr: Box<Node>
    },

    Block,
    EndBlock,

    And {
        line: usize,
        expr: Box<Node>,
    },
    Or {
        line: usize,
        expr: Box<Node>,
    },
    //Loop,
    EndWhile {
        line: usize,
        condition: Vec<Node>,
        statements: Vec<Node>
    },
    If,
    Endif {
        hasElse: bool,
        condition: Vec<Node>,
        statements: Vec<Node>,
        elsenode: Vec<Node>,
    },
    While,
    Break,
    Continue,

}

impl Node {
    pub fn getDataType(self) -> DataType {
        match self {
            Node::Value {
                line,
                label,
                value,
                dataType } => dataType,
            _ => DataType::Nil
        }
    }
}