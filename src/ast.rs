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

#[derive(Clone, Debug)]
pub enum JumpType {
    jumpIfFalse,
    Jump
}

#[derive(Clone, Debug, PartialEq)]
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
#[derive(PartialEq, Copy, Clone, Debug)]
pub enum DataType {
    Integer,
    Float,
    String,
    Bool,
    Nil,
    None
}
impl DataType {
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

#[derive(Debug, Clone)]
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

#[derive(Clone, Debug)]
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
    Statement {
        tokenType: TokenType
    },
    VarDecl {
        name: String ,
        assigned: bool,
        varExpr: Box<Node>
    },
    setVar {
        name: String ,
        datatype: DataType,
        child: Box<Node>
    },
    namedVar {
        name: String
    },
    Return {
        returnVal: Box<Node>
    },
    Root {
        children: Vec<Node>
    },
    Print {
        printExpr: Box<Node>
    },

    Block,
    EndBlock,
    jumpIfFalse {
        popType: JumpPop
    } ,
    jump ,
    backpatch {
        jumpType: JumpType
    },
    pop,
    Loop,
    EndWhile {
        condition: Box<Node>,
        jump: Box<Node>,
        statements: Vec<Node>
    },
    While,
    Break,
    Continue
}

