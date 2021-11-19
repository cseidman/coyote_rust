use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{Chunk, writeChunk, OpCode, addConstant, addStringConstant, writeU64Chunk};
use crate::value;
use crate::scanner::*;
use TokenType::* ;
use std::io::{stderr, Write} ;
use OpCode::*;
use std::str::{FromStr};
use crate::ast::* ;

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
enum ExpFunctions {
    NONE,
    BINARY,
    UNARY,
    NUMBER,
    INTEGER,
    FLOAT,
    BIGINT,
    DOUBLE,
    GROUPING,
    LITERAL,
    STRING
}

use ExpFunctions::* ;
use crate::debug::{disassembleInstruction, disassembleChunk};

struct Rule {
    prefix: ExpFunctions,
    infix: ExpFunctions,
    prec: u8
}

impl Rule {
    pub fn new(prefix: ExpFunctions, infix: ExpFunctions, prec: u8) -> Rule {
        Rule {
            prefix,
            infix,
            prec
        }
    }
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

pub struct Compiler<'a>  {
    chunk: &'a mut Chunk,
    scanner: Scanner ,
    parser: Parser,
    ast: Vec<Ast>
}

impl<'a> Compiler<'a> {

    pub fn new(source: String, chunk: &'a mut Chunk) -> Self {
        Compiler {
            chunk,
            scanner: Scanner::new(source),
            parser: Parser::new(),
            ast: Vec::new()
        }
    }

    fn astPush(&mut self, tokenType: TokenType, opType: OpType, label: &str, value: u64 ) {

        let dataType = match  tokenType {
            TOKEN_INTEGER => DataType::Integer,
            TOKEN_FLOAT => DataType::Float,
            TOKEN_STRING => DataType::String,
            TOKEN_BOOL => DataType::Bool,
            _ => DataType::None
        } ;

        self.ast.push(
            Ast {
                tokenType,
                opType,
                dataType,
                label: label.to_string(),
                value
            }
        );
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

    fn expression(&mut self) {
        self.parsePrecedence(PREC_ASSIGNMENT) ;
    }

    fn check(&mut self, tokenType: TokenType) -> bool {
        self.parser.current().tokenType == tokenType
    }

    fn t_match(&mut self, tokenType: TokenType) -> bool{
        if !self.check(tokenType) {
            return false;
        }
        self.advance();
        true
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

    fn emitU64(&mut self, number: u64 ) {
        let line = self.parser.previous().line ;
        let chunk = currentChunk!(self) ;
        writeU64Chunk(chunk, number, line);
    }

    fn emitReturn(&mut self) {
        self.emitOp(OP_RETURN) ;
    }

    fn emitConstant(&mut self, value: u64) {
        let byte = self.makeConstant("df".to_string()) as u8 ;
        self.emitBytes(OP_CONSTANT, byte) ;
    }

    fn makeConstant(&mut self, value: String) -> usize {
        let index = addConstant(currentChunk!(self), 0) ;
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
            TOKEN_FALSE => self.astPush(TOKEN_FALSE, OpType::Literal, "False", 0),
            TOKEN_TRUE => self.astPush(TOKEN_TRUE, OpType::Literal, "True", 1),
            _ => self.astPush(TOKEN_NIL, OpType::Literal, "Nil", 0)
        }
    }

    fn text(&mut self) {
        let strVal = self.parser.previous().name ;
        let pointer = addStringConstant(&mut self.chunk, strVal.clone()) as u64;
        let s = strVal.as_str();
        self.astPush(TokenType::TOKEN_STRING, OpType::Literal, s, pointer as u64);
    }

    fn integer(&mut self) {
        let strVal = self.parser.previous().name ;
        let integer = i64::from_str(strVal.as_str()).unwrap() ;
        self.astPush(TokenType::TOKEN_INTEGER, OpType::Literal, strVal.as_str(), integer as u64);
    }

    fn float(&mut self) {
        let strVal = self.parser.previous().name ;
        let float = f64::from_str(strVal.as_str()).unwrap() ;
        self.astPush(TokenType::TOKEN_FLOAT, OpType::Literal,strVal.as_str(), float as u64);
    }

     fn grouping(&mut self) {
        self.expression();
        self.consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression") ;
    }

    fn unary(&mut self) {
        let operatorType = self.parser.previous().tokenType ;
        self.parsePrecedence(PREC_UNARY);

        match operatorType {
            TOKEN_BANG  => self.astPush(TokenType::TOKEN_BANG, OpType::Unary,"!", 0),
            TOKEN_MINUS => self.astPush(TokenType::TOKEN_MINUS, OpType::Unary,"-", 0),
            _ => {}
        }
    }

    fn binary(&mut self) {
        let token = self.parser.previous() ;
        let operatorType = token.tokenType ;
        let rule = self.getRule(operatorType) ;
        let prec = rule.prec + 1 ;
        self.parsePrecedence(prec) ;

        self.astPush(operatorType, OpType::Binop,token.name.as_str(), 0);

    }

    fn getRule(&mut self, tokenType: TokenType) -> Rule {
        match tokenType {
            TOKEN_LEFT_PAREN    => Rule::new(GROUPING, NONE, PREC_NONE),
            TOKEN_MINUS
            | TOKEN_PLUS        => Rule::new(UNARY, BINARY , PREC_TERM),
            TOKEN_SLASH
            | TOKEN_STAR        => Rule::new(NONE, BINARY, PREC_FACTOR),
            TOKEN_NUMBER        => Rule::new(NUMBER, NONE, PREC_NONE),
            TOKEN_INTEGER       => Rule::new(INTEGER, NONE, PREC_NONE),
            TOKEN_FLOAT         => Rule::new(FLOAT, NONE, PREC_NONE),
            TOKEN_STRING        => Rule::new(STRING, NONE, PREC_NONE),
            TOKEN_FALSE
            | TOKEN_TRUE
            | TOKEN_NIL         => Rule::new(LITERAL, NONE, PREC_NONE),
            TOKEN_BANG          => Rule::new(UNARY, NONE, PREC_NONE),
            TOKEN_EQUAL_EQUAL   => Rule::new(NONE, BINARY, PREC_EQUALITY),
            | TOKEN_GREATER
            | TOKEN_GREATER_EQUAL
            | TOKEN_LESS
            | TOKEN_LESS_EQUAL  => Rule::new(NONE, BINARY, PREC_COMPARISON),
            _                   => Rule::new(NONE, NONE, PREC_NONE )
        }
    }

    fn execExpression(&mut self, func:ExpFunctions) {
        match func {
            BINARY => self.binary(),
            UNARY => self.unary(),
            GROUPING => self.grouping(),
            LITERAL => self.literal(),
            STRING => self.text(),
            INTEGER => self.integer(),
            FLOAT => self.float(),
            _ => {}
         }
    }

    fn parsePrecedence(&mut self, precedence: u8) {
        self.advance() ;
        let token_type = self.parser.previous().tokenType ;
        let prefixRule = self.getRule(token_type).prefix ;
        if prefixRule == NONE {
            self.error("Expect expression") ;
            return ;
        }
        self.execExpression(prefixRule) ;

        loop {
            let current = self.parser.current().tokenType ;
            let nextPrec = self.getRule(current).prec  ;
            if precedence > nextPrec{
                break ;
            }
            self.advance() ;
            let previous = self.parser.previous().tokenType ;
            let infixRule = self.getRule(previous).infix ;
            self.execExpression(infixRule) ;
        }

    }

    fn printStatement(&mut self) {
        self.expression();
        self.consume(TOKEN_SEMICOLON, "Expect ';' after value.");
        self.astPush(TOKEN_PRINT, OpType::Statement, "print", 0);
    }

    fn statement(&mut self) {
        if self.t_match(TOKEN_PRINT) {
            self.printStatement();
        }
    }

    fn declaration(&mut self) {
        self.statement() ;
    }

    pub fn compile(&mut self) -> bool {
        self.advance() ;

        loop {
            if self.t_match(TOKEN_EOF) {
                break ;
            }
            self.declaration();
        }

        self.endCompiler() ;

        for i in &self.ast {
            println!("{}",i.label);
        }

        let tree = buildTree(&self.ast) ;
        walkTree(tree, 1) ;

        !self.parser.hadError
    }

}