use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{Chunk, writeChunk, backPatch, OpCode, writeu16Chunk, addConstant, currentLocation};
use crate::value::{Value, Property, Class, Visibilty, Function};
use crate::scanner::*;
use TokenType::* ;
use std::io::{stderr, Write} ;
use OpCode::*;
use std::str::{FromStr};
use crate::ast::* ;
use crate::symbol::{SymbolTable, Symbol};
use crate::errors::InterpretResult ;

use ExpFunctions::* ;
use crate::debug::{disassembleInstruction, disassembleChunk};
use std::env::var;
use std::collections::HashMap;

use crate::ast::JumpPop::{NOPOP, POP};
use crate::ast::Node::*;
use std::rc::Rc;
use crate::value::Value::array;

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

const PRINT_NODES: bool = false ;

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
    ARRAY_BUILD,
    HASH_BUILD,
    AND,
    OR,
    CALL
}

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

#[derive(Debug, Clone)]
pub struct Location {
    pub locations: HashMap<String, Vec<usize>>
}

impl Location {

    pub fn new() -> Self {
        Self {
            locations: HashMap::new()
        }
    }

    pub fn addLocation(&mut self, tag: &str, location: usize) -> usize {

        // Initilize a new vector for this tag if it's the
        // first time we use it
        if ! self.locations.contains_key(tag) {
            self.locations.insert(tag.to_string(), Vec::new()) ;
        }

        self.locations.get_mut(tag).unwrap().push(location) ;
        location

    }

    pub fn peekLocation(&mut self, tag: &str) -> usize {

        if ! self.locations.contains_key(tag) {
            panic!("Location tag {} does not exist", tag) ;
        }

        self.locations.get_mut(tag)
            .unwrap()
            .last()
            .unwrap()
            .clone()

    }

    pub fn popLocation(&mut self, tag: &str) -> usize {

        if ! self.locations.contains_key(tag) {
            panic!("Location tag {} does not exist", tag) ;
        }

        self.locations.get_mut(tag)
            .unwrap()
            .pop()
            .unwrap()

    }

    pub fn hasKey(&self, tag: &str) -> bool {
        self.locations.contains_key(tag)
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
    nodes: Vec<Node>,

    localCount: usize,
    scopeDepth: usize,

    jumpifFalse: Vec<usize>,
    jump: Vec<usize>,
    breakjump: Vec<usize>,
    loopDepth: usize,

    symbTable: SymbolTable,

    locations: Vec<Location>,
    locationPtr: usize,

    functionStore: &'a mut Vec<Function>,
    callNodes: Vec<Node>,
}

impl<'a> Compiler<'a> {

    pub fn new(source: String, chunk: &'a mut Chunk, funcStore: &'a mut Vec<Function>) -> Self {

        Compiler {
            chunk,
            scanner: Scanner::new(source),
            parser: Parser::new(),
            nodes: Vec::new() ,
            localCount: 0,
            scopeDepth: 0,
            jumpifFalse: Vec::new(),
            jump: Vec::new(),
            breakjump: Vec::new(),
            loopDepth: 0,
            symbTable: SymbolTable::new(),

            locations: Vec::new(),
            locationPtr: 0,
            functionStore: funcStore,

            callNodes: Vec::new()

        }
    }

    pub fn updateSource(&mut self, source: String) {
        self.scanner.LoadSource(&source) ;
    }

    // Symbol table operations
    pub fn addVariable(&mut self, varname: String, datatype: DataType) -> usize {
        self.symbTable.addSymbol(varname, datatype)
    }

    pub fn getVariable(&mut self, varname: String) -> Symbol {
        let symb = self.symbTable.getSymbol(varname.clone()) ;
        if let Ok(..) = symb {
            symb.unwrap()
        } else {
            panic!("Cannot find variable {}", varname) ;
        }
    }

    pub fn getFunction(&self, funcName: String) -> Result<usize, String> {
        let mut loc = 0 ;
        for f in self.functionStore.clone() {

            if f.name == funcName {
                return Ok(loc)
            }
            loc+=1 ;
        }
        let msg = format!("Function '{}' not found", funcName) ;
        Err(msg)
    }

    pub fn addFunction(&mut self, f: Function) {
        // Check that this function doesn't already exist
        let res = self.getFunction(f.name.clone()) ;
        match res {
            Ok(x) => {
                if self.functionStore[x].isStub {
                    self.functionStore[x] = f ;
                } else {
                    let msg = format!("Function with name: '{}' already exists", f.name) ;
                    self.errorAtCurrent(&msg) ;
                }
            },
            Err(s)  => {
                self.functionStore.push(f)
            }
        }
    }

    pub fn addStubFunction(&mut self, funcName: String) -> usize {
        let f = Function {
            name: funcName,
            isStub: true,
            arity: 0,
            chunk: Chunk::new(),
            returnType: DataType::None
        } ;
        self.functionStore.push(f) ;
        self.functionStore.len()-1
    }

    fn nodePush(&mut self, node: Node) {
        self.nodes.push(node) ;
    }

    fn storeCallNode(&mut self, node: Node) {
        self.callNodes.push(node) ;
    }

    pub fn error(&mut self, message: &str) {
        let token = self.parser.previous() ;
        let line = token.line ;
        self.errorAt(token, message, line);
    }

    pub fn errorAt(&mut self, token: Token, message: &str, line: usize) {
        if self.parser.panicMode {
            return ;
        }
        self.parser.panicMode = true ;
        let _ = stderr().write_fmt(format_args!("[line {}] Error", line)) ;

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

    pub fn errorAtAst(&mut self, message: &str, line: usize) {
        let token = self.parser.current() ;
        self.parser.panicMode = true ;
        let _ = stderr().write_fmt(format_args!("[line {}] Error", line)) ;
        let _ = stderr().write_fmt(format_args!(" {}\n", message)) ;
        self.parser.hadError = true ;
    }

    pub fn errorAtCurrent(&mut self, message: &str) {
        let token = self.parser.current() ;
        let line = token.line ;
        self.errorAt(token, message, line);
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

    pub fn arrayBuilder(&mut self) {
        // We already consumed the left bracket
        let mut arity: usize = 0 ; // Starting off with 0 elements
        let mut values: Vec<Node> = Vec::new() ;

        let mut elementType = DataType::None ;

        let line = self.parser.previous().line ;

        while ! self.t_match(TOKEN_RIGHT_BRACKET) {

            arity += 1 ;
            // The value we're adding to the array
            self.expression() ;

            self.t_match(TOKEN_COMMA) ; // Consume the comma if there is one

            // Pop that value expression from above and put it into the array
            let node = self.nodes.pop().unwrap();

            // The first value sets the requirement for the value of the remaining elements
            let curType = node.clone().getDataType() ;
            if arity == 1 {
                elementType = curType ;
            }

            // Make sure the value of this node is the same as the others
            if elementType != curType {
                self.errorAtCurrent(
                    &format!("The array is of type: {:?} but the element [{}] is: {:?}"
                                            , elementType, arity-1, curType)
                    )   ;
            }

            values.push(node) ;
        }

        let valueType = match elementType {
            DataType::Integer => DataType::IArray,
            DataType::Float => DataType::FArray,
            DataType::String => DataType::SArray,
            DataType::Bool => DataType::BArray,
            _ => DataType::None
        } ;

        self.nodePush(Array {
            line,
            arity,
            valueType,
            elementType,
            values
        }) ;
    }

    pub fn hashBuilder(&mut self) {
        // We already consumed the '@['

        let mut keys: Vec<Node> = Vec::new() ;
        let mut values: Vec<Node> = Vec::new() ;

        let mut arity= 0 ;
        let line = self.parser.previous().line ;

        while ! self.t_match(TOKEN_RIGHT_BRACKET) {

            arity+=1 ;

            self.expression() ; // The key
            let key = self.nodes.pop().unwrap();
            keys.insert(0,key) ;

            self.consume(TOKEN_COLON,"Expect ':' between the key and value") ;

            self.expression() ; // The value
            let value = self.nodes.pop().unwrap();
            values.insert(0, value) ;

            self.t_match(TOKEN_COMMA) ; // Get the comma if there is one

        }

        self.nodePush(Dict {
            line,
            arity,
            keys,
            values
        }) ;
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

        let retVal = self.nodes.pop().unwrap() ;
        let line = self.parser.previous().line ;
        self.nodePush(Node::Return {
            line,
            returnVal: Box::new(retVal)
        });

    }

    fn class(&mut self) {
        let classToken = self.parser.previous();
        let className = classToken.name ;
        let line = classToken.line ;
        let mut properties: Vec<Property> = Vec::new();

        self.consume(TOKEN_LEFT_BRACE, "Expect '{' after the class name") ;
        while ! self.t_match(TOKEN_RIGHT_BRACE) {

            // Check if we have one of the visibility keywords
            let mut visibility = Visibilty::public ;
            if self.t_match(TOKEN_PRIVATE) {
                visibility = Visibilty::private ;
            }

            // Now the property name
            if self.t_match(TOKEN_IDENTIFIER) {

                let propertyName = self.parser.previous().name ;

                // Now check the type starting with the native types
                self.advance() ;
                let typeToken = self.parser.previous() ;

                let propertyType = match typeToken.tokenType {
                    TOKEN_STRING => {DataType::String},
                    TOKEN_BOOL => {DataType::Bool},
                    TOKEN_INTEGER => {DataType::Integer},
                    TOKEN_FLOAT => {DataType::Float},
                    TOKEN_IDENTIFIER => {DataType::Object},
                    _ => {
                        self.errorAtCurrent("Unknown property type") ;
                        panic!("Unknoxn property type") ;
                    }
                };

                // Check if this is an array
                if self.t_match(TOKEN_LEFT_BRACE) {
                    // It's an array
                    self.consume(TOKEN_RIGHT_BRACE, "Expect a ']' after the array size") ;
                }

                let property = Property {
                    name: propertyName,
                    visibility ,
                    propertyType
                };

                properties.push(property) ;

            }
        }

        self.nodePush(class {
            line,
            propertyCount: properties.len() ,
            properties: vec![]
        }) ;
    }

    fn literal(&mut self) {
        let token = self.parser.previous();
        let label= token.label.to_string();

        match token.tokenType {
           TOKEN_FALSE => self.nodePush(
                Node::Value {
                    line: token.line,
                    label,
                    value: Value::logical(false),
                    dataType: DataType::Bool
                }),
           TOKEN_TRUE => self.nodePush(
                Node::Value {
                    line: token.line,
                    label,
                    value: Value::logical(true),
                    dataType: DataType::Bool
                }),
            TOKEN_NIL => self.nodePush(
                Node::Value {
                    line: token.line,
                    label,
                    value: Value::nil,
                    dataType: DataType::Nil
                }),
            _ => self.errorAtCurrent("Unknown literal")
        }
    }

    fn text(&mut self) {
        let token = self.parser.previous() ;
        let label = token.name ;

        self.nodePush(
            Node::Value {
                line: token.line,
                label: label.clone(),
                value: Value::string(label),
                dataType: DataType::String
        });
    }

    fn integer(&mut self) {
        let token = self.parser.previous() ;
        let label= token.name ;
        let integer = i64::from_str(label.as_str()).unwrap() ;
        self.nodePush(
            Node::Value {
                line: token.line,
                label,
                value: Value::integer(integer) ,
                dataType: DataType::Integer
            });
    }

    fn float(&mut self) {
        let token = self.parser.previous() ;
        let label= token.name ;
        let float = f64::from_str(label.as_str()).unwrap() ;
        self.nodePush(
            Node::Value {
                line: token.line,
                label,
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

        let node = Node::UnaryExpr {
            line: token.line,
            op: operator,
            child: Box::new(self.nodes.pop().unwrap()),
        };
        self.nodePush(node);
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
            TOKEN_BANG_EQUAL => Operator::NEq,
            _ => panic!("Operator {:?} not found!", operatorType)
        } ;

        let node = Node::BinaryExpr {
            line: token.line,
            op: operator,
            rhs: Box::new(self.nodes.pop().unwrap()),
            lhs: Box::new(self.nodes.pop().unwrap())
        };
        self.nodePush(node);

    }

    fn getRule(&mut self, tokenType: TokenType) -> Rule {
        match tokenType {
            TOKEN_LEFT_PAREN    => Rule::new(GROUPING, CALL, PREC_CALL),
            TOKEN_LEFT_BRACKET  => Rule::new(ARRAY_BUILD, NONE, PREC_NONE),
            TOKEN_AT_BRACKET    => Rule::new(HASH_BUILD, NONE, PREC_NONE),
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
            TOKEN_BANG_EQUAL
            | TOKEN_EQUAL_EQUAL   => Rule::new(NONE, BINARY, PREC_EQUALITY),
            TOKEN_GREATER
            | TOKEN_GREATER_EQUAL
            | TOKEN_LESS
            | TOKEN_LESS_EQUAL  => Rule::new(NONE, BINARY, PREC_COMPARISON),
            TOKEN_IDENTIFIER  => Rule::new(VARIABLE, NONE, PREC_NONE),
            TOKEN_AND           => Rule::new(NONE, AND, PREC_AND),
            TOKEN_OR           => Rule::new(NONE, OR, PREC_OR),
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
            AND => self.and_(),
            OR => self.or_(),
            ARRAY_BUILD => self.arrayBuilder(),
            HASH_BUILD => self.hashBuilder(),
            CALL => self.call(),
            _ => {
                self.error("Binary expression {func} uknown") ;
            }
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
        let line = self.parser.previous().line ;
        self.expression();
        let printExpr = self.nodes.pop().unwrap() ;
        self.nodePush(Node::Print {
            line,
            printExpr: Box::new(printExpr)
        })
    }

    fn and_(&mut self) {
        let line = self.parser.previous().line ;
        self.parsePrecedence(PREC_AND);
        let node =  self.nodes.pop().unwrap() ;
        self.nodes.push(And {
            line,
            expr: Box::new(node)
        }) ;
    }

    fn or_(&mut self) {
        let line = self.parser.previous().line ;
        self.parsePrecedence(PREC_OR);
        let node =  self.nodes.pop().unwrap() ;
        self.nodes.push(Or {
            line,
            expr: Box::new(node)
        }) ;
    }

    fn ifStatement(&mut self) {
        let mut conditional: Vec<Node> = Vec::new() ;
        self.nodePush(Node::If);

        // This is the logical condition
        let nodePosition = self.nodes.len()-1 ;
        self.expression() ;

        loop {
            let curNode = self.nodes.last().unwrap().clone();
            let curPos = self.nodes.len() -1 ;
            if nodePosition == curPos {
                break;
            }
            conditional.insert(0,self.nodes.pop().unwrap()) ;
        }

        // Load the statements here
        let mut nodes: Vec<Node> = Vec::new() ;
        let nodePosition = self.nodes.len()-1 ;
        self.statement() ;

        loop {
            let curNode = self.nodes.last().unwrap().clone();
            // Since we're popping the values off the node stack, we know
            // that we need to stop at the ELSE statement
            let curPos = self.nodes.len() -1 ;
            if nodePosition == curPos {
                break;
            }
            nodes.insert(0,self.nodes.pop().unwrap()) ;
        }

        let mut hasElse = false ;
        let mut elsenodes: Vec<Node> = Vec::new() ;
        if self.t_match(TOKEN_ELSE) {

            hasElse = true ;

            let nodePosition = self.nodes.len()-1 ;
            self.statement();

            loop {
                let curNode = self.nodes.last().unwrap().clone();
                // Since we're popping the values off the node stack, we know
                // that we need to stop at the ELSE statement
                let curPos = self.nodes.len() -1 ;
                if nodePosition == curPos {
                    break;
                }
                elsenodes.insert(0,self.nodes.pop().unwrap()) ;
            }

        }

        let ifNode = Node::Endif {
            hasElse,
            condition: conditional,
            statements: nodes,
            elsenode: elsenodes
        } ;

        self.nodePush(ifNode) ;
    }

    fn whileStatement(&mut self) {

        self.pushScope() ;
        let mut conditional: Vec<Node> = Vec::new() ;
        let line = self.parser.previous().line ;
        // Signals the beginning of the while
        self.nodePush(Node::While);

        let nodePosition = self.nodes.len()-1 ;

        // This is the logical expression
        self.expression() ;

        // Conditional Statements since the expression above
        // could be made up of compound statements
        loop {

            // Get the next node generated from the above
            let curNode = self.nodes.last().unwrap().clone();

            // If after popping the statements above, we find the
            let curPos = self.nodes.len() -1 ;
            if nodePosition == curPos {
                break;
            }
            conditional.insert(0,self.nodes.pop().unwrap()) ;
        }

        let nodePosition = self.nodes.len()-1 ;
        self.statement();
        let mut nodes: Vec<Node> = Vec::new() ;

        loop {
            let curNode = self.nodes.last().unwrap().clone();
            let curPos = self.nodes.len() -1 ;
            if nodePosition == curPos {
                break;
            }
            nodes.insert(0,self.nodes.pop().unwrap()) ;
        }

        let whileNode = Node::EndWhile {
            line,
            condition: conditional,
            statements: nodes
        } ;

        self.popScope() ;
        self.nodePush(whileNode) ;

    }

    fn breakStatement(&mut self) {
        self.nodePush(Node::Break);
    }

    fn continueStatement(&mut self) {
        self.nodePush(Node::Continue);
    }

    fn beginScope(&mut self) {
        self.nodePush(Node::Block);
    }

    fn endScope(&mut self) {
        self.nodePush(Node::EndBlock);
    }

    fn statement(&mut self) {
        if self.t_match(TOKEN_PRINT) {
            self.printStatement();
        } else if self.t_match(TOKEN_LEFT_BRACE) {
            self.beginScope();
            self.block();
            self.endScope();
        } else if self.t_match(TOKEN_IF) {
            self.ifStatement();
        } else if self.t_match(TOKEN_WHILE) {
            self.whileStatement();
        } else if self.t_match(TOKEN_BREAK) {
            self.breakStatement();
        } else if self.t_match(TOKEN_CONTINUE) {
            self.continueStatement();
        } else {
            self.expression();
        }
    }

    fn declaration(&mut self) {
        if self.t_match(TOKEN_VAR) {
            self.varDeclaration();
        } else if self.t_match(TOKEN_FUN) {
            self.funcDeclaration() ;
        } else {
            self.statement();
        }
    }

    // *** Variable management **

    fn assignValueToVar(&mut self, varname: String) {
        let line = self.parser.previous().line ;
        self.expression() ;

        let childVal = self.nodes.pop().unwrap() ;

        self.nodePush(Node::setVar {
            line,
            name: varname,
            datatype: childVal.clone().getDataType(),
            child: Box::new(childVal)
        });
    }

    fn assignValueToArray(&mut self, varname: String,n: Node) {
        let line = self.parser.previous().line ;
        // The value we're about to assign
        self.expression() ;
        let childVal = self.nodes.pop().unwrap() ;

        self.nodePush(Node::setArray {
            line,
            name: varname,
            index: Box::new(n),
            child: Box::new(childVal)
        });
    }

    fn assignValueToHash(&mut self, varname: String,n: Node) {
        let line = self.parser.previous().line ;
        // The value we're about to assign
        self.expression() ;
        let childVal = self.nodes.pop().unwrap() ;

        self.nodePush(Node::setHash {
            line,
            name: varname,
            key: Box::new(n),
            child: Box::new(childVal)
        });
    }

    fn namedHashKey(&mut self, varname: String) {

        let line = self.parser.previous().line ;

        // If we have this, then it means that we're referring to
        // a hash key

        self.expression() ; // Key expression
        self.consume(TOKEN_RIGHT_BRACKET, "Expect ']' after hash element expression");

        // Tells us which element in the hash we're looking for
        let keyExpr = self.nodes.pop().unwrap() ;

        if self.t_match(TOKEN_EQUAL) {
            // This is the value we're assigning
            self.assignValueToHash(varname, keyExpr) ;

        } else {
            // If not, then it's being used as an expression

            let node = Node::namedHash {
                line,
                name: varname,
                key: Box::new(keyExpr),
            };
            self.nodePush(node);
        }

    }

    fn namedArrayElement(&mut self, varname: String) {
        let line = self.parser.previous().line ;
        // If we have this, then it means that we're referring to
        // an array element. This next expression gives us the element number
        self.expression() ;
        self.consume(TOKEN_RIGHT_BRACKET, "Expect ']' after array element expression");

        // Tells us which element in the array we're looking for. We grab the node that
        // contains the above expression
        let indexExpr = self.nodes.pop().unwrap() ;

        // If this isn't a number, we should abort
        if indexExpr.clone().getDataType() != DataType::Integer {
            self.errorAtCurrent("The array element must be an integer");
        }

        if self.t_match(TOKEN_EQUAL) {
            // This is the value we're assigning
            self.assignValueToArray(varname, indexExpr) ;

        } else {
            // If not, then it's being used as an expression
            let node = Node::namedArray {
                line,
                name: varname,
                index: Box::new(indexExpr)
            };
            self.nodePush(node);
        }

    }

    fn namedSingleVariable(&mut self, varname: String) {
        let line = self.parser.previous().line ;
        // This means we're assigning a value
        if self.t_match(TOKEN_EQUAL) {
            self.assignValueToVar(varname) ;
        } else {
            // If not, then it's being used as an expression
            let node = Node::namedVar {
                line,
                name: varname
            };
            self.nodePush(node);
        }
    }

    fn namedFunction(&mut self, funcName: String) {
        // We're about to make a function call and we need
        // to check for the existence of this function which
        // may have been declared AFTER this call is being made

        // So first we check to make sure this function exists in
        // the function store. If so, great. If not, we have to create
        // a stub, a placeholder, for this function so that subsequent
        // calls can find it. We'll fill in the details of the function
        // in a subsequent AST pass

        let f = self.getFunction(funcName.clone()) ;
        let mut isStub = false ;
        if f.is_err() {
            // We didn't find the function, so add the stub here
            self.addStubFunction(funcName.clone()) ;
            isStub = true ;
        }

        let line = self.parser.previous().line ;

        let n = Node::functionVar {
            line: 0,
            name: funcName,
            isStub
        };
        self.nodePush(n) ;
    }

    fn namedVariable(&mut self) {

        // This is the variable name we just encountered
        let varname = self.parser.previous().name ;

        // Check if it's a function
        if self.check(TOKEN_LEFT_PAREN) {
            self.namedFunction(varname) ;
            return ;
        }

        // Check if it's an array
        if self.t_match(TOKEN_LEFT_BRACKET) {
            self.namedArrayElement(varname) ;
            return
        }

        // Check if it's a hash
        if self.t_match(TOKEN_AT_BRACKET) {
            self.namedHashKey(varname) ;
            return
        }

        // If we got here, then it means it's a variable
        self.namedSingleVariable(varname) ;
    }

    fn variable(&mut self) {
        self.namedVariable() ;
    }

    fn varDeclaration(&mut self) {
        let line = self.parser.previous().line ;
        self.advance();

        let token = self.parser.previous() ;
        let varname = token.name ;
        let mut isAssigned = false ;

        if self.t_match(TOKEN_EQUAL) {
            self.expression() ;
            isAssigned = true ;
        } else {
            // Assign NIL to the unassigned variable
            self.nodePush(
                Node::Value {
                    line: token.line,
                    label: varname.clone(),
                    value: Value::nil,
                    dataType: DataType::Nil
                })
        }
        // This is the expression that gets assigned to the variable
        let declExpr = self.nodes.pop().unwrap() ;

        let node = Node::VarDecl {
            line,
            name: varname.clone(),
            assigned: isAssigned,
            varExpr: Box::new(declExpr)
        };

        println!("Adding variable {}", varname) ;

        self.nodePush(node);

    }

    fn parseVariable(&mut self, errorMessage: &'static str)  {
        self.consume(TOKEN_IDENTIFIER, errorMessage) ;
        self.namedVariable() ;
    }

    pub fn call(&mut self) {
        /*
        let fNode = self.nodes.last().unwrap().clone() ;
        let mut funcName = "".to_string() ;
        match fNode {
            Node::namedVar{
                line, name
            } => {
                    funcName = name ;
            },
            _ => {
                println!("{:?}", fNode) ;
            }
        }
        */
        let line = self.parser.previous().line ;
        let mut params: Vec<Node> = Vec::new() ;

        let mut arity = 0 ;
        while !self.t_match(TOKEN_RIGHT_PAREN) {
            arity+=1 ;
            self.expression() ;
            params.push(self.nodes.pop().unwrap()) ;
            self.t_match(TOKEN_COMMA) ;
        }
        let fnode = Node::call {
            line,
            arity,
            func: String::new(),
            parameters: params,
            returnType: DataType::None
        };
        self.nodePush(fnode.clone()) ;
        self.storeCallNode(fnode) ;
    }

    fn match_declared_type(&mut self) -> DataType {
        if self.t_match(TOKEN_DATA_TYPE(TokenData::STRING)) {
            return DataType::String
        } else if self.t_match(TOKEN_DATA_TYPE(TokenData::INTEGER)) {
            return DataType::Integer
        } else if self.t_match(TOKEN_DATA_TYPE(TokenData::FLOAT)) {
            return DataType::Float
        } else if self.t_match(TOKEN_DATA_TYPE(TokenData::BOOL)) {
            return DataType::Bool
        }  else if self.t_match(TOKEN_IDENTIFIER) {
            //
        }
        DataType::None
    }

    fn makeParameter(&mut self) -> Node {

        let line = self.parser.previous().line ;

        // Name of the parameter variable
        self.consume(TOKEN_IDENTIFIER, "Expect parameter name after '('") ;
        let name = self.parser.previous().name ;

        // Type of parameter
        self.consume(TOKEN_COLON, "Expect ':' after parameter name") ;

        let dataType = self.match_declared_type();

        Node::parameter {
            line ,
            name ,
            dataType
        }

    }

    fn funcDeclaration(&mut self) {

        let line = self.parser.previous().line ;

        // We consumed 'func' already. Now we need to get the name of the
        // function itself
        self.consume(TOKEN_IDENTIFIER, "Expect a function name after 'func'") ;
        let funcName = self.parser.previous().name ;

        let mut arity: u16 = 0 ;

        let msg = format!("Expect a '(' after the function {}", funcName) ;
        self.consume(TOKEN_LEFT_PAREN, &msg) ;

        let mut parameters: Vec<Node> = Vec::new() ;
        while ! self.t_match(TOKEN_RIGHT_PAREN) {
            // Count the number of parameters
            arity+=1 ;
            let p = self.makeParameter() ;
            parameters.push(p) ;
            self.t_match(TOKEN_COMMA) ;
        }

        // See if there is a return type
        let returnType = self.match_declared_type() ;

        // Now grab the statements inside the function
        let mut statements = Vec::new() ;

        // This needs to begin with a brace
        self.consume(TOKEN_LEFT_BRACE, "Expect a '{' after the function header") ;
        while ! self.t_match(TOKEN_RIGHT_BRACE) {
            self.statement() ;
            let statement = self.nodes.pop().unwrap() ;
            statements.push( statement) ;
        }

        let funcNode = Node::function {
            line,
            name: funcName,
            arity,
            parameters,
            statements,
            returnType
        };

        //self.nodePush(funcNode) ;
        self.walkTree(funcNode);
    }

    pub fn view_tree(&self) {
        for t in &self.nodes {
            println!("{:?}", t) ;
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

        let tree = self.nodes.clone() ;
        let startNode = Node::Root {
            children: tree
        };

        self.nodes = vec![startNode];
        self.walkTree(self.nodes[0].clone()) ;

        //self.check_calls() ;

        //if self.parser.hadError {
            disassembleChunk(self.chunk, "code") ;
        //}

        let result = self.parser.hadError ;
        self.parser.hadError = false ;
        !result
    }

    /** AST Operations **/

    pub fn generateBreakStatement(&mut self) {
        if self.loopDepth == 0 {
            self.errorAtAst("BREAK statement must be inside a loop", 0);
        }

        // Jump to the end of the parent loop
        writeChunk(self.chunk, OP_JUMP as u8, 0);
        self.addLocation("innerbreak");
        writeu16Chunk(self.chunk, 9999_u16, 0);
    }

    pub fn generateContinueStatement(&mut self) {
        if self.loopDepth == 0 {
            self.errorAtAst("BREAK statement must be inside a loop", 0);
        }

        writeChunk(self.chunk, OP_LOOP as u8, 0);
        let loc = currentLocation(self.chunk) ;
        let inner = self.popLocation("innercontinue") ;

        let continueTo = loc - inner+2;
        writeu16Chunk(self.chunk, continueTo as u16, 0);
    }

    // Location management for control of flow statements

    pub fn pushScope(&mut self) {
        self.locations.push(Location::new()) ;
        self.locationPtr+=1
    }

    pub fn popScope(&mut self) {
        self.locations.pop() ;
        self.locationPtr-=1 ;
    }

    pub fn backpatchInner(&mut self, tag: &str) {
        let ptr = self.locationPtr-1 ;
        if self.locations[ptr].locations.contains_key(tag) {
            for b in self.locations[ptr].locations[tag].clone() {
                backPatch(self.chunk, b);
            }
        }
    }

    pub fn addLocation(&mut self, tag: &str) -> usize {
        let location = currentLocation(self.chunk) ;
        self.addSpecificLocation(tag, location)
    }

    pub fn addSpecificLocation(&mut self, tag: &str, location: usize) -> usize {

        let ptr = self.locationPtr-1 ;

        self.locations[ptr].addLocation(tag, location);
        location
    }

    pub fn peekLocation(&mut self, tag: &str) -> usize {

        let ptr = self.locationPtr-1 ;
        self.locations[ptr].peekLocation(tag)

    }

    pub fn popLocation(&mut self, tag: &str) -> usize {

        let ptr = self.locationPtr-1 ;
        self.locations[ptr].popLocation(tag)

    }

    // Walk tree
    pub fn walkTree(&mut self, node: Node) -> DataType {

        macro_rules! writeOp {
            ($byte:expr, $line:expr) => {
                writeChunk(self.chunk, $byte as u8, $line)
            };
        }

        macro_rules! writeOperand {
            ($value:expr) => {
                writeu16Chunk(self.chunk, $value, 0);
            };
        }

        match node {
            Node::Root { children: nodes } => {
                for n in nodes {
                    self.walkTree(n);
                }
                DataType::None
            }
            Node::Value { line, label, value, dataType } => {
                match dataType {
                    DataType::Nil => {
                        writeOp!(OP_NIL, line);
                    },
                    DataType::String => {
                        let constant_index = self.makeConstant(value);

                        writeOp!(OP_SCONSTANT, line);
                        writeOperand!(constant_index);
                    },
                    _ => {
                        let constant_index = self.makeConstant(value);
                        writeOp!(OP_CONSTANT, line);
                        writeOperand!(constant_index);
                    }
                }
                dataType
            },

            Node::Dict {
                line,
                arity,
                keys,
                values
            } => {

                let mut elements= 0 ;
                for k in keys {
                    let v = values[elements].clone();

                    let keyType = self.walkTree(k) ;
                    let valType = self.walkTree(v) ;

                    elements+=1 ;
                }

                writeOp!(OP_PUSH, line);
                writeOperand!(elements as u16);

                writeOp!(OP_NEWDICT, line) ;

                DataType::Dict
            },

            Node::namedHash {
                line,
                name,
                key
            } => {
                let symbol = self.getVariable(name.clone()) ;
                // The index of the array
                let keyType = self.walkTree(*key, ) ;

                writeOp!(OP_GETHELEMENT, line);
                writeOperand!(symbol.location as u16);

                symbol.datatype
            },

            Node::setHash {
                line,
                name,
                key,
                child
            } => {
                // The stack should contain:
                // -- key
                // -- Value
                // Then OP_SETHELEMENT first pops index off the stack to get to the element
                // followed by popping the value we're assigning to the variable

                let symbol = self.getVariable(name.clone()) ;

                // The value we're assigning
                let valueDataType = self.walkTree(*child);

                // The hash key
                let keyType = self.walkTree(*key) ;

                // Todo: Check that the variable type matches the value type

                writeOp!(OP_SETHELEMENT, line);
                writeOperand!(symbol.location as u16);

                valueDataType
            },

            Node::Array {
                line,
                arity,
                valueType,
                elementType,
                values
            } => {

                let mut elements:u16 = 0 ;

                for n in values {
                    elements+=1 ;
                    let curType = self.walkTree(n) ;
                    if  curType != elementType {
                        let msg = format!("Array is of type {:?} but element [{}] is of type {:?}", valueType, elements-1, curType);
                        self.errorAtAst(&msg, 0) ;
                    }
                }

                // Push the arity
                writeOp!(OP_PUSH, line);
                writeOperand!(elements);

                // Push the datatype of the array elements
                writeOp!(OP_PUSH, line) ;
                writeOperand!(elementType.to_operand());


                writeOp!(OP_NEWARRAY, line) ;

                valueType

            },

            Node::BinaryExpr {
                line,
                op,
                lhs,
                rhs
            } => {
                let mut l_type = self.walkTree(*lhs);
                let r_type = self.walkTree(*rhs );

                if l_type != r_type {
                    match (l_type, r_type) {
                        //(DataType::Float, DataType::Integer) => {},
                        (DataType::Integer, DataType::Float) => {
                            l_type = DataType::Float
                        },
                        _ => {
                            let msg = format!("Incompatible datatypes: {:?} and {:?}"
                                              , l_type, r_type);
                            self.errorAtAst(msg.as_str(), line);
                        }
                    }
                }

                let operatorString = format!("{}{}", l_type.emit(), op.emit()) ;

                match  operatorString.as_str() {
                    "IADD" => {writeOp!(OP_IADD, line); DataType::Integer},
                    "ISUB" => {writeOp!(OP_ISUBTRACT, line);DataType::Integer},
                    "IMUL" => {writeOp!(OP_IMULTIPLY, line);DataType::Integer},
                    "IDIV" => {writeOp!(OP_IDIVIDE, line);DataType::Integer},

                    "FADD" => {writeOp!(OP_FADD, line);DataType::Float},
                    "FSUB" => {writeOp!(OP_FSUBTRACT, line);DataType::Float},
                    "FMUL" => {writeOp!(OP_FMULTIPLY, line);DataType::Float},
                    "FDIV" => {writeOp!(OP_FDIVIDE, line);DataType::Float},

                    "IEQ"  => {writeOp!(OP_IEQ, line);DataType::Bool},
                    "FEQ"  => {writeOp!(OP_FEQ, line);DataType::Bool},
                    "SEQ"  => {writeOp!(OP_SEQ, line);DataType::Bool},

                    "INEQ"  => {writeOp!(OP_INEQ, line);DataType::Bool},
                    "FNEQ"  => {writeOp!(OP_FNEQ, line);DataType::Bool},
                    "SNEQ"  => {writeOp!(OP_SNEQ, line);DataType::Bool},

                    "IGT"  => {writeOp!(OP_IGT, line);DataType::Bool},
                    "FGT"  => {writeOp!(OP_FGT, line);DataType::Bool},
                    "SGT"  => {writeOp!(OP_SGT, line);DataType::Bool},

                    "ILT"  => {writeOp!(OP_ILT, line);DataType::Bool},
                    "FLT"  => {writeOp!(OP_FLT, line);DataType::Bool},
                    "SLT"  => {writeOp!(OP_SLT, line);DataType::Bool},

                    "IGTEQ"  => {writeOp!(OP_IGTEQ, line);DataType::Bool},
                    "FGTEQ"  => {writeOp!(OP_FGTEQ, line);DataType::Bool},
                    "SGTEQ"  => {writeOp!(OP_SGTEQ, line);DataType::Bool},

                    "ILTEQ"  => {writeOp!(OP_ILTEQ, line);DataType::Bool},
                    "FLTEQ"  => {writeOp!(OP_FLTEQ, line);DataType::Bool},
                    "SLTEQ"  => {writeOp!(OP_SLTEQ, line);DataType::Bool},

                    _ => {
                        let msg = format!("Binary operator '{}' not found!", operatorString) ;
                        self.errorAtCurrent(&msg);
                        DataType::None
                    }
                }
            },

            Node::UnaryExpr { line, op, child } => {
                let dataType = self.walkTree(*child, );

                match op {
                    Operator::Minus => {
                        match dataType {
                            DataType::Integer => {
                                writeOp!(OP_INEGATE, line) ;
                            },
                            DataType::Float => {
                                writeOp!(OP_FNEGATE, line) ;
                            },
                            _ => {
                                self.errorAtAst("Can only negate integer or float expressions", line);
                            }
                        }
                    },
                    Operator::Plus => {

                    },
                    _ => {
                        self.errorAtAst("Wrong unaryt expression", line);
                    }
                }
                dataType
            },

            Node::VarDecl {
                line,
                name,
                assigned,
                varExpr
            } => {

                let datatype = self.walkTree(*varExpr, );
                let loc = self.addVariable(name.clone(), datatype) ;

                writeOp!(OP_SETVAR, line);
                writeOperand!(loc as u16);

                datatype
            },

            Node::class {
                line,
                propertyCount,
                properties
            } => {

                DataType::Object
            },

            Node::setArray {
                line,
                name,
                index,
                child
            } => {

                // The stack should contain:
                // -- index
                // -- Value
                // Then OP_xSETAELEMENT first pops index off the stack to get to the element
                // followed by popping the value we're assigning to the variable

                let symbol = self.getVariable(name.clone()) ;
                let arrayValueType = symbol.datatype ;

                // The value we're assigning
                let valueDataType = self.walkTree(*child, );

                // The index of the array
                let indexType = self.walkTree(*index, ) ;

                match arrayValueType {
                    DataType::IArray => if valueDataType != DataType::Integer {
                       self.errorAtCurrent(&format!("Cannot assign a value type {:?} to an integer array", valueDataType))
                    },
                    DataType::FArray => if valueDataType != DataType::Float {
                        self.errorAtCurrent(&format!("Cannot assign a value type {:?} to a float array", valueDataType))
                    },
                    DataType::SArray => if valueDataType != DataType::String {
                        self.errorAtCurrent(&format!("Cannot assign a value type {:?} to an String array", valueDataType))
                    },
                    DataType::BArray => if valueDataType != DataType::Bool {
                        self.errorAtCurrent(&format!("Cannot assign a value type {:?} to a Bool array", valueDataType))
                    },
                    _ => panic!("Unknown array value type {:?}", valueDataType)
                };

                let opcode = match arrayValueType {
                    DataType::IArray => OP_ISETAELEMENT,
                    DataType::FArray => OP_FSETAELEMENT,
                    DataType::SArray => OP_SSETAELEMENT,
                    DataType::BArray => OP_BSETAELEMENT,
                    _ => panic!("Unknown array value type {:?}", valueDataType)
                };

                writeOp!(opcode, line);
                writeOperand!(symbol.location as u16);

                valueDataType

            } ,

            Node::namedArray {
                line,
                name,
                index
            } => {
                let symbol = self.getVariable(name.clone()) ;
                let arrayValueType = symbol.datatype ;
                // The index of the array
                let indexType = self.walkTree(*index, ) ;

                let opcode = match arrayValueType {
                    DataType::IArray => OP_IGETAELEMENT,
                    DataType::FArray => OP_FGETAELEMENT,
                    DataType::SArray => OP_SGETAELEMENT,
                    DataType::BArray => OP_BGETAELEMENT,
                    _ => panic!("Unknown array value type")
                };

                let elementType = match arrayValueType {
                    DataType::IArray => DataType::Integer,
                    DataType::FArray => DataType::Float,
                    DataType::SArray => DataType::String,
                    DataType::BArray => DataType::Bool,
                    _ => panic!("Unknown array value type")
                };


                writeOp!(opcode, line);
                writeOperand!(symbol.location as u16);

                elementType
            } ,

            Node::setVar {
                line,
                name,
                datatype,
                child
            } => {
                let symbol = self.getVariable(name.clone()) ;
                let valueDataType = self.walkTree(*child);

                let mut varType = symbol.datatype ;
                if varType == DataType::None {
                    varType = valueDataType;
                }

                // Make sure we don't set a value incompatible with the storage
                // type of the variable
                if varType != valueDataType {
                    let msg = format!("Variable is of type {:?} and value is of type {:?}",
                                      varType, valueDataType);
                    self.errorAtCurrent(&msg) ;
                }

                writeOp!(OP_SETVAR, line);
                writeOperand!(symbol.location as u16);

                varType
            },

            Node::namedVar {
                line,
                name
            } => {
                let symbol = self.getVariable(name.clone()) ;

                writeOp!(OP_LOADVAR, line);
                writeOperand!(symbol.location as u16);

                symbol.datatype
            },

            Node::functionVar {
                line,
                name,
                isStub
            } => {
                let func = self.getFunction(name) ;
                match func {
                    Ok(x) => {
                        writeOp!(OP_LOADFUNC, line);
                        writeOperand!(x as u16);

                        let f = self.functionStore[x].clone() ;
                        f.returnType
                    },
                    Err(s) => {
                        self.errorAtAst(&s, line) ;
                        DataType::None
                    }
                }
            },

            Node::Block => {
                self.symbTable.pushLevel();
                DataType::None
            },

            Node::EndBlock => {
                for _ in 0..self.symbTable.varCount() {
                    writeOp!(OP_POP,0) ;
                }
                self.symbTable.popLevel();
                DataType::None
            },

            Node::And {
                line,
                expr
            } => {
                writeOp!(OP_JUMP_IF_FALSE_NOPOP,line) ;
                let loc = currentLocation(self.chunk);
                writeOperand!(9999_u16) ;
                writeOp!(OP_POP,0) ;

                if self.walkTree(*expr,  ) != DataType::Bool {
                    self.errorAtAst("Condition on the right of AND must evaluate to True or False", 0);
                }

                backPatch(self.chunk, loc ) ;

                DataType::Bool
            },

            Node::Or {
                line,
                expr
            } => {
                writeOp!(OP_JUMP_IF_FALSE_NOPOP,line) ;
                let loc = currentLocation(self.chunk);
                writeOperand!(9999_u16) ;

                writeOp!(OP_JUMP,0) ;
                let jmp = currentLocation(self.chunk);
                writeOperand!(9999_u16) ;

                backPatch(self.chunk, loc) ;
                writeOp!(OP_POP,0) ;

                if self.walkTree(*expr,  ) != DataType::Bool {
                    self.errorAtAst("Condition on the right of OR must evaluate to True or False", 0);
                }

                backPatch(self.chunk, jmp ) ;
                DataType::Bool
            },

            Node::Print {
                line,
                printExpr
            } => {
                let datatype = self.walkTree(*printExpr);
                match datatype {
                    DataType::String => writeOp!(OP_SPRINT,line),
                    _ => writeOp!(OP_PRINT,line)
                }
                datatype
            },

            Node::Return {
                line,
                returnVal
            } => {
                let datatype = self.walkTree(*returnVal);
                writeOp!(OP_RETURN,line);
                datatype
            },

            Node::Break => {
                DataType::None
            },

            Node::Continue => {
                DataType::None
            },
            Node::If => {
                //self.chunk.pushScope() ;
                DataType::None
            },

            Node::Endif {
                hasElse,
                condition,
                statements,
                elsenode
            } => {

                // Collect all the logical condition expressions
                for e in condition {
                    let dataType = self.walkTree(e,  ) ;
                    if dataType != DataType::Bool {
                        self.errorAtAst("IF condition must evaluate to True or False", 0) ;
                    }
                }

                // We jump from here if the IF resolves to FALSE
                writeOp!(OP_JUMP_IF_FALSE, 0) ;
                // If it's false we need to jump to the end of this block
                let jumpFromLocation =  currentLocation(self.chunk);
                writeOperand!(9999_u16) ;

                // The commands that execute if the statement is true
                for n in statements {

                    if n == Node::Break {
                        self.generateBreakStatement() ;
                    }

                    if n == Node::Continue {
                        self.generateContinueStatement() ;
                    }

                    self.walkTree(n);

                }

                let mut afterElseJump = 0 ;

                if hasElse {

                    // If the condition was true, we jump from here
                    writeOp!(OP_JUMP, 0) ;
                    afterElseJump = currentLocation(self.chunk);
                    writeOperand!(9999_u16);

                    // If there was an else, and the condition was false,
                    // we wind up here
                    backPatch(self.chunk, jumpFromLocation);

                    for n in elsenode {

                        if n == Node::Break {
                            self.generateBreakStatement() ;
                        }

                        if n == Node::Continue {
                           self.generateContinueStatement() ;
                        }

                        self.walkTree(n, );
                    }

                } else {
                    // If this has no ELSE, this is where the false condition
                    // lands you
                    backPatch(self.chunk, jumpFromLocation);
                }

                if hasElse {
                    backPatch(self.chunk, afterElseJump);
                }
                //self.chunk.popScope() ;
                DataType::None
            },

            Node::EndWhile {
                line,
                condition,
                statements
            } => {

                self.loopDepth+=1 ;
                self.pushScope() ;

                let beginLocation = currentLocation(self.chunk);
                self.addLocation("innercontinue") ;

                // Collect all the logical condition expressions
                for e in condition {
                    let dataType = self.walkTree(e,  ) ;
                    if dataType != DataType::Bool {
                        self.errorAtAst("WHILE condition must evaluate to True or False", 0) ;
                    }
                }

                // We should have a logical value on the stack here
                writeOp!(OP_JUMP_IF_FALSE_NOPOP, 0) ;
                let jumpFromLocation = currentLocation(self.chunk);
                writeOperand!(9999_u16) ;

                writeOp!(OP_POP, 0) ;

                // All the statements inside the WHILE block
                for n in statements {

                    if n == Node::Break {
                        writeOp!(OP_JUMP, 0) ;
                        self.addLocation("break");
                        writeOperand!(9999_u16 as u16) ;
                    }

                    if n == Node::Continue {
                        let loc = currentLocation(self.chunk) ;
                        writeOp!(OP_LOOP, 0) ;
                        let continueTo = loc - beginLocation+3;
                        writeOperand!(continueTo as u16) ;
                    }

                    self.walkTree(n);

                }

                writeOp!(OP_LOOP, 0) ;
                let loc = currentLocation(self.chunk) ;
                let backJumpBy = loc - beginLocation+2;
                writeOperand!(backJumpBy as u16) ;

                backPatch(self.chunk, jumpFromLocation) ;
                self.backpatchInner("innerbreak") ;
                self.backpatchInner("break") ;

                writeOp!(OP_POP, 0) ;

                self.loopDepth-=1 ;
                self.popScope() ;
                DataType::None
            },

            Node::While => {
                DataType::None
            },

            Node::Logical {
                line,
                expr
            } => {
                DataType::Bool
            },

            Node::call {
                line,
                arity,
                func,
                parameters,
                returnType
            } => {
                /*
                let res = self.getFunction(func.clone());

                let mut loc = 0 ;
                match res {
                    Ok(x) => {
                        loc = x ;
                    }  ,
                    Err(s) => {
                        // The function hasn't been built yet, so we create a
                        // placeholder in the functionStore so that we can
                        // compile this call. In a subsequent pass, we'll
                        // check to make sure that the function was actually created
                        loc = self.addStubFunction(func) ;
                    }
                }

                let f = self.functionStore[loc].clone() ;
                */
                let mut arity:u16 = 0 ;
                for n in parameters {
                    arity+=1 ;
                    self.walkTree(n) ;
                }

                // If there were parameters, they should be
                // on the stack now
                writeOp!(OP_CALL, line) ;
                writeOperand!(arity) ;
                DataType::None

            },

            Node::parameter {
                line,
                name,
                dataType
            } => {

                let loc = self.addVariable(name.clone(), dataType);
                dataType
            },

            Node::function {
                line,
                name,
                arity,
                parameters,
                statements,
                returnType
            } => {

                // Insert an unfinished function now so that
                // the following statements see a reference to
                // an existing function when it comes time to resolve the
                // name for recursion
                let mut func = Function::new(
                    name,
                    arity,
                    Chunk::new(),
                    returnType
                ) ;

                self.addFunction(func.clone()) ;
                let location = self.functionStore.len()-1 ;

                // Stash the current chunk
                let prev = self.chunk.clone() ;

                // Push a new chunk in the compiler
                *self.chunk = Chunk::new() ;

                self.symbTable.pushLevel() ;
                // Run the parameter nodes
                for n in parameters {
                    self.walkTree(n) ;
                }

                // These statements go in the new chunk
                for n in statements {
                    self.walkTree(n) ;
                }
                // Return from the function
                self.symbTable.popLevel() ;
                writeOp!(OP_RETURN, line) ;

                // Now that the instructions are there, we replace it
                // with the completed function
                func.chunk = self.chunk.clone() ;

                // Put the old chunk back
                *self.chunk = prev ;
                self.functionStore[location] = func ;

                returnType
            },


        }
    }

    // One more pass over the tree to make sure that all calls'
    // parameter types match the function signatures
    pub fn check_calls(&mut self) {
        // This collection contains only the 'Node::call' type of
        // node from the tree
        for n in self.callNodes.clone() {
            match n {
                Node::call {
                    line,
                    arity,
                    func,
                    parameters,
                    returnType
                } => {
                    // Try to get the function location
                    let res = self.getFunction(func.clone()) ;
                    match res {
                        Ok(x) => {
                            // The function is a stub so it was never defined. We're
                            // calling a non-existent function
                            if self.functionStore[x].isStub {
                                self.errorAtAst("Function doesn't exist", line);
                            }
                            // The signatures don't match
                            let funcArity = self.functionStore[x].arity;
                            if funcArity != arity {
                                let msg = format!("Function '{}' expects {} parameters, but you supplied {}",func, funcArity, arity) ;
                                self.errorAtAst(&msg, line);
                            }

                        },
                        // We really shouldn't even be able to reach this section
                        // at all because all calls in the code generate a stub
                        // if the function isn't found at the time we compile the call
                        Err(s) => {
                            // The function doesn't even exist
                            self.errorAtAst(&s, line) ;
                        }
                    }
                },
                _ => {}
            }
        }
    }


}