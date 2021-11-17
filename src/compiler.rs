#[macro_use]

use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{Chunk, writeChunk, OpCode, addConstant, addStringConstant};
use crate::value::{Value} ;
use crate::scanner::*;
use TokenType::* ;
use std::io::{stderr, Write} ;
use OpCode::*;
use std::str::{FromStr};

const PREC_NONE: u8 = 0 ;
const PREC_ASSIGNMENT: u8 = 1 ; // =
const PREC_OR: u8 = 2 ;         // or
const PREC_AND: u8 = 3 ;        // and
const PREC_EQUALITY: u8 = 4 ;   // == !=
const PREC_COMPARISON: u8 = 5 ; // < > <= >=
const PREC_TERM: u8 = 6 ;       // + -
const PREC_FACTOR: u8 = 7 ;     // * /
const PREC_UNARY: u8 = 8 ;      // ! -
const PREC_CALL: u8 = 9 ;       // . ()
const PREC_PRIMARY: u8 = 10 ;

#[derive(PartialEq)]
enum expFunctions {
    NONE,
    BINARY,
    UNARY,
    NUMBER,
    GROUPING,
    LITERAL,
    STRING
}

use expFunctions::* ;
use crate::debug::{disassembleInstruction, disassembleChunk};

pub struct Compiler<'a>  {
    chunk: &'a mut Chunk,
    scanner: Scanner ,
    parser: Parser
}

pub struct Parser {
    tokens: Vec<Token>,
    token_ptr: usize,
    hadError: bool,
    panicMode: bool
}

impl Parser {

    pub fn new() -> Self {
        Parser {
            tokens: Vec::new(),
            token_ptr: 0,
            hadError: false,
            panicMode: false
        }
    }

    pub fn getToken(&mut self, pointer: usize) -> Token {
        // Nobody likes 'clone', but in this case, the reward/effort
        // ratio of using RC or tuning lifetimes in a way to allow
        // the Token struct to implement 'Copy' was too low. This only
        // happens during the compilation phase so the impact should be minimal
        self.tokens[pointer-1].clone()
    }

    pub fn advance(&mut self, token: Token) {
        self.tokens.push(token) ;
        self.token_ptr+=1 ;
    }

    pub fn current(&mut self) -> Token {
        let ptr = self.token_ptr ;
        self.getToken(ptr)
    }

    pub fn previous(&mut self) -> Token  {
        let ptr = self.token_ptr-1 ;
        self.getToken(ptr)
    }
}

macro_rules! currentChunk {
    ($self:ident) => {
        &mut $self.chunk
    };
}

impl<'a> Compiler<'a> {

    pub fn new(source: String, chunk: &'a mut Chunk) -> Self {
        Compiler {
            chunk,
            scanner: Scanner::new(source),
            parser: Parser::new()
        }
    }

    pub fn error(&mut self, message: &str) {
        let token = self.parser.previous() ;
        self.errorAt(token, message);
    }

    pub fn errorAt(&mut self, token: Token, message: &str) {
        if self.parser.panicMode {
            return ;
        }
        self.parser.panicMode = true ;
        let _ = stderr().write_fmt(format_args!("[line {}] Error", token.line)) ;

        match token.tokenType {
            TOKEN_EOF => {
                let _ = stderr().write(" at end".as_bytes()).unwrap() ;
            },
            //TOKEN_ERROR => {},
            _ => {
                let _ = stderr().write_fmt(format_args!(" at '{}'", token.name));
            }
        }
        let _ = stderr().write_fmt(format_args!(" {}\n", message)) ;
        self.parser.hadError = true ;
    }

    pub fn errorAtCurrent(&mut self, message: &str) {
        let token = self.parser.current() ;
        self.errorAt(token, message);
    }

    pub fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.parser.current().tokenType == token_type {
            self.advance() ;
        } else {
            self.errorAtCurrent(message) ;
        }
    }

    pub fn advance(&mut self) {
        loop {
            let tok = self.scanner.scanToken();
            self.parser.advance(tok);

            if self.parser.current().tokenType != TOKEN_ERROR {
                break ;
            }
        }
    }

    pub fn expression(&mut self) {
        self.parsePrecedence(PREC_ASSIGNMENT) ;
    }

    fn emitOp(&mut self, opcode: OpCode)  {
        self.emitByte(opcode as u8) ;
    }

    fn emitByte(&mut self, byte: u8) {
        let line = self.parser.previous().line ;
        let chunk = currentChunk!(self) ;
        writeChunk(chunk, byte, line);
    }

    fn emitBytes(&mut self, opcode: OpCode, byte: u8) {
        self.emitOp(opcode) ;
        self.emitByte(byte) ;
    }

    fn emitReturn(&mut self) {
        self.emitOp(OP_RETURN) ;
    }

    fn emitConstant(&mut self, value: Value) {
        let byte = self.makeConstant(value) as u8 ;
        self.emitBytes(OP_CONSTANT, byte) ;
    }

    fn makeConstant(&mut self, value: Value) -> usize {
        let index = addConstant(currentChunk!(self), value) ;
        if index > u8::MAX as usize {
            self.error("Too many constants in one chunk") ;
            return 0 ;
        }
        index
    }

    fn endCompiler(&mut self) {
        self.emitReturn() ;

        if self.parser.hadError {
            disassembleChunk(self.chunk, "code") ;
        }
    }

    fn literal(&mut self) {
        match self.parser.previous().tokenType {
            TOKEN_FALSE => self.emitOp(OP_FALSE),
            TOKEN_TRUE => self.emitOp(OP_TRUE),
            _ => self.emitOp(OP_NIL),
        }
    }

    fn text(&mut self) {
        let strval = self.parser.previous().name ;
        let pointer = addStringConstant(&mut self.chunk, strval) as u64;
        let value = Value::Pointer(pointer) ;
        self.emitConstant(value) ;
    }

    fn number(&mut self) {
        let number = f64::from_str(self.parser.previous().name.as_str()).unwrap() ;
        self.emitConstant(NUMBER_VAL!(number)) ;
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression") ;
    }

    fn unary(&mut self) {
        let operatorType = self.parser.previous().tokenType ;
        self.parsePrecedence(PREC_UNARY);

        match operatorType {
            TOKEN_BANG  => self.emitOp(OP_NOT),
            TOKEN_MINUS => self.emitOp(OP_NEGATE),
            _ => {}
        }
    }

    fn binary(&mut self) {
        let operatorType = self.parser.previous().tokenType ;
        let rule = self.getRule(operatorType) ;
        let prec = rule.2 + 1 ;
        self.parsePrecedence(prec) ;

        match operatorType {
            TOKEN_BANG_EQUAL  => {
                self.emitOp(OP_EQUAL) ;
                self.emitOp(OP_NOT) ;
            },
            TOKEN_EQUAL_EQUAL => self.emitOp(OP_EQUAL),
            TOKEN_GREATER     => self.emitOp(OP_GREATER),
            TOKEN_GREATER_EQUAL => {
                self.emitOp(OP_LESS) ;
                self.emitOp(OP_NOT) ;
            },
            TOKEN_LESS => self.emitOp(OP_LESS),
            TOKEN_LESS_EQUAL => {
                self.emitOp(OP_GREATER) ;
                self.emitOp(OP_NOT) ;
            },
            TOKEN_PLUS  => self.emitOp(OP_ADD),
            TOKEN_MINUS => self.emitOp(OP_SUBTRACT),
            TOKEN_STAR  => self.emitOp(OP_MULTIPLY),
            TOKEN_SLASH => self.emitOp(OP_DIVIDE),
            _ => {}
        }
    }

    fn getRule(&mut self, tokenType: TokenType) -> (expFunctions, expFunctions, u8) {
        match tokenType {
            TOKEN_LEFT_PAREN    => (GROUPING, NONE, PREC_NONE),
            TOKEN_MINUS
            | TOKEN_PLUS          => (UNARY, BINARY , PREC_TERM),
            TOKEN_SLASH
            | TOKEN_STAR        => (NONE, BINARY, PREC_FACTOR),
            TOKEN_NUMBER        => (NUMBER, NONE, PREC_NONE),
            TOKEN_STRING        => (STRING, NONE, PREC_NONE),
            TOKEN_FALSE
            | TOKEN_TRUE
            | TOKEN_NIL         => (LITERAL, NONE, PREC_NONE),
            TOKEN_BANG          => (UNARY, NONE, PREC_NONE),
            TOKEN_EQUAL_EQUAL   => (NONE, BINARY, PREC_EQUALITY),
            | TOKEN_GREATER
            | TOKEN_GREATER_EQUAL
            | TOKEN_LESS
            | TOKEN_LESS_EQUAL  => (NONE, BINARY, PREC_COMPARISON),
            _                   => (NONE, NONE, PREC_NONE )
        }
    }

    fn execExpression(&mut self, func: expFunctions) {
        match func {
            BINARY => self.binary(),
            UNARY => self.unary(),
            GROUPING => self.grouping(),
            NUMBER => self.number(),
            LITERAL => self.literal(),
            STRING => self.text(),
            _ => {}
         }
    }

    fn parsePrecedence(&mut self, precedence: u8) {
        self.advance() ;
        let token_type = self.parser.previous().tokenType ;
        let prefixRule = self.getRule(token_type).0 ;
        if prefixRule == NONE {
            self.error("Expect expression") ;
            return ;
        }
        self.execExpression(prefixRule) ;

        loop {
            let current = self.parser.current().tokenType ;
            let nextPrec = self.getRule(current).2  ;
            if precedence > nextPrec{
                break ;
            }
            self.advance() ;
            let previous = self.parser.previous().tokenType ;
            let infixRule = self.getRule(previous).1 ;
            self.execExpression(infixRule) ;
        }

    }

    pub fn compile(&mut self) -> bool {
        self.advance() ;
        self.expression() ;
        self.consume(TOKEN_EOF, "Expect end of expression");

        self.endCompiler() ;


        !self.parser.hadError
    }

}