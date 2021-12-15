use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{Chunk, writeChunk, OpCode, addConstant, writeU64Chunk, addStringConstant, getStringConstant, backPatch};
use crate::value::{Value};
use crate::scanner::*;
use TokenType::* ;
use std::io::{stderr, Write} ;
use OpCode::*;
use std::str::{FromStr};
use crate::ast::* ;
use crate::symbol::{SymbolTable, Symbol};

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
    STRING,
    VARIABLE,
    BLOCKING,
    AND,
    OR
}

use ExpFunctions::* ;
use crate::debug::{disassembleInstruction, disassembleChunk};
use std::env::var;
use std::collections::HashMap;
use crate::ast::JumpType::Jump;
use crate::ast::JumpPop::{NOPOP, POP};

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
    pub chunk: &'a mut Chunk,
    scanner: Scanner ,
    parser: Parser,
    pub ast: Vec<Ast>,

    pub localCount: usize,
    pub scopeDepth: usize,

    pub symbTable: SymbolTable,

    pub jumpifFalse: Vec<usize>,
    pub jump: Vec<usize>
}

impl<'a> Compiler<'a> {

    pub fn new(source: String, chunk: &'a mut Chunk) -> Self {
        Compiler {
            chunk,
            scanner: Scanner::new(source),
            parser: Parser::new(),
            ast: Vec::new(),
            localCount: 0,
            scopeDepth: 0,
            symbTable: SymbolTable::new(),
            jumpifFalse: Vec::new(),
            jump: Vec::new()
        }
    }


    // Symbol table operations
    pub fn addVariable(&mut self, varname: String, datatype: DataType) -> usize {
        self.symbTable.addSymbol(varname, datatype)
    }

    pub fn getVariable(&mut self, varname: String) -> Symbol {
        let symb = self.symbTable.getSymbol(varname.clone()) ;
        if symb.is_err() {
            panic!("Cannot find variable {}", varname.clone()) ;
        } else {
            symb.unwrap()
        }
    }

    fn astPush(&mut self, ast: Ast ) {
        self.ast.push(ast);
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

    pub fn makeConstant(&mut self, value: Value) -> u16 {
        addConstant(currentChunk!(self), value)
    }

    fn endCompiler(&mut self) {

        self.astPush(Ast::ret) ;

        if self.parser.hadError {
            disassembleChunk(self.chunk, "code") ;
        }
    }


    fn literal(&mut self) {
        match self.parser.previous().tokenType {
            TOKEN_FALSE => self.astPush(Ast::literal {
                label: "False".to_string(),
                value: Value::logical(false),
                dataType: DataType::Bool
            }),
            TOKEN_TRUE => self.astPush(Ast::literal {
                label: "True".to_string(),
                value: Value::logical(true),
                dataType: DataType::Bool
            }),
            _ => self.astPush(Ast::literal {
                label: "Nil".to_string(),
                value: Value::nil,
                dataType: DataType::Bool
            })
        }
    }

    fn text(&mut self) {
        let strVal = self.parser.previous().name ;
        let value = addStringConstant(self.chunk, strVal.clone()) as i64;
        self.astPush(Ast::literal {
            label: strVal,
            value: Value::integer(value) ,
            dataType: DataType::String
        });
    }

    fn integer(&mut self) {
        let strVal = self.parser.previous().name ;
        let integer = i64::from_str(strVal.as_str()).unwrap() ;
        self.astPush(Ast::literal {
            label: strVal,
            value: Value::integer(integer) ,
            dataType: DataType::Integer
        });
    }

    fn float(&mut self) {
        let strVal = self.parser.previous().name ;
        let float = f64::from_str(strVal.as_str()).unwrap() ;

        self.astPush(Ast::literal {
            label: strVal,
            value: Value::float(float) ,
            dataType: DataType::Float
        });
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression") ;
    }

    fn block(&mut self) {
        while !self.check(TOKEN_RIGHT_BRACE) && !self.check(TOKEN_EOF) {
            self.declaration()
        }
        self.consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    }

    fn unary(&mut self) {
        let token = self.parser.previous() ;
        let operatorType = token.tokenType ;
        self.parsePrecedence(PREC_UNARY);

        let operator = match operatorType {
            TOKEN_PLUS  =>  Operator::Plus,
            TOKEN_MINUS =>  Operator::Minus,
            _ => panic!("Unary operator {:?} not found!", operatorType)
        };

        self.astPush(Ast::unaryOp {
            label: token.name,
            operator
        });
    }

    fn binary(&mut self) {
        let token = self.parser.previous() ;
        let operatorType = token.tokenType ;
        let rule = self.getRule(operatorType) ;
        let prec = rule.prec + 1 ;
        self.parsePrecedence(prec) ;

        let operator = match operatorType {
            TOKEN_PLUS => Operator::Plus,
            TOKEN_MINUS => Operator::Minus,
            TOKEN_STAR => Operator::Mul,
            TOKEN_SLASH => Operator::Div,
            TOKEN_EQUAL_EQUAL => Operator::Eq,
            TOKEN_GREATER => Operator::Gt,
            TOKEN_LESS => Operator::Lt,
            TOKEN_GREATER_EQUAL => Operator::GtEq,
            TOKEN_LESS_EQUAL => Operator::LtEq,
            _ => panic!("Operator {:?} not found!", operatorType)
        } ;

        self.astPush(Ast::binop {
            label: token.name,
            operator
        });
    }

    fn getRule(&mut self, tokenType: TokenType) -> Rule {
        match tokenType {
            TOKEN_LEFT_PAREN    => Rule::new(GROUPING, NONE, PREC_NONE),
            //TOKEN_LEFT_BRACE    => Rule::new(BLOCKING, NONE, PREC_NONE),
            TOKEN_MINUS
            | TOKEN_PLUS        => Rule::new(UNARY, BINARY , PREC_TERM),
            TOKEN_SLASH
            | TOKEN_STAR        => Rule::new(NONE, BINARY, PREC_FACTOR),
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
            | TOKEN_IDENTIFIER  => Rule::new(VARIABLE, NONE, PREC_NONE),
            TOKEN_AND           => Rule::new(NONE, AND, PREC_AND),
            TOKEN_OR           => Rule::new(NONE, OR, PREC_AND),
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
            VARIABLE => self.variable(),
            //BLOCKING => self.block(),
            AND => self.and_(),
            OR => self.or_(),
            _ => {}
         }
    }

    fn parsePrecedence(&mut self, precedence: u8) {
        self.advance() ;
        let token_type = self.parser.previous().tokenType ;
        let prefixRule = self.getRule(token_type).prefix ;
        if prefixRule == NONE {
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
        self.astPush(Ast::print);
    }

    fn and_(&mut self) {
        self.astPush(Ast::jumpIfFalse {popType:NOPOP} ) ;
        self.astPush(Ast::pop);

        self.parsePrecedence(PREC_AND);
        self.astPush(Ast::backpatch {
            jumpType: JumpType::jumpIfFalse
        });

    }

    fn or_(&mut self) {
        self.astPush(Ast::jumpIfFalse {popType:NOPOP} ) ;
        self.astPush(Ast::jump ) ;

        self.astPush(Ast::backpatch {jumpType: JumpType::jumpIfFalse});
        self.astPush(Ast::pop);

        self.parsePrecedence(PREC_OR);
        self.astPush(Ast::backpatch {jumpType: JumpType::Jump});
    }

    fn ifStatement(&mut self) {

        self.expression();
        //self.astPush(Ast::conditional) ;

        self.astPush(Ast::jumpIfFalse {popType: POP} ) ;
        self.declaration();
        self.astPush(Ast::jump) ;

        self.astPush(Ast::backpatch {
            jumpType: JumpType::jumpIfFalse
        });

        if self.t_match(TOKEN_ELSE) {
            self.declaration();
        }
        self.astPush(Ast::backpatch {
            jumpType: JumpType::Jump
        });
    }

    fn beginScope(&mut self) {
        //self.astPush(Ast::block) ;
    }

    fn endScope(&mut self) {
        //self.astPush(Ast::endBlock) ;
    }

    fn statement(&mut self) {
        if self.t_match(TOKEN_PRINT) {
            self.printStatement();
        } else if self.t_match(TOKEN_LEFT_BRACE) {
            self.beginScope();
            self.block();
            self.endScope();
        } else if self.t_match(TOKEN_IF) {
            self.ifStatement() ;
        } else {
            self.expression();
        }
    }

    // *** Variable management **

    fn namedVariable(&mut self) {

        // This is the variable name we just encountered
        let varname = self.parser.previous().name ;
        if self.t_match(TOKEN_EQUAL) {
            self.expression() ;
            self.astPush(Ast::setVar {
                varname: varname.clone(),
                datatype: DataType::None
            });
        } else {
            // If not, then it's being used as an expression
            self.astPush(Ast::namedVar {
                varname
            });
        }
    }

    fn variable(&mut self) {
        self.namedVariable() ;
    }

    fn varDeclaration(&mut self) {

        // This is the location of the variable in the constants table we just allocated
        self.advance();
        let varname = self.parser.previous().name ;
        let mut isAssigned = false ;

        if self.t_match(TOKEN_EQUAL) {
            self.expression() ;
            isAssigned = true ;
        } else {
            self.astPush(Ast::literal {
                label: "nil".to_string(),
                value: Value::nil,
                dataType: DataType::None
            });
        }

        self.astPush(Ast::varDecl {
            varname: varname.clone(),
            assigned: isAssigned
        });

    }

    fn parseVariable(&mut self, errorMessage: &'static str)  {
        self.consume(TOKEN_IDENTIFIER, errorMessage) ;
        self.namedVariable() ;

    }

    fn declaration(&mut self) {
        if self.t_match(TOKEN_VAR) {
            self.varDeclaration() ;
        } else {
            self.statement();
        }
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

        let tree = self.buildTree() ;
        self.walkTree(tree, 1) ;

        disassembleChunk(self.chunk, "Code") ;

        !self.parser.hadError
    }

}