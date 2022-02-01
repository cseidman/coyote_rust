use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{Chunk, writeChunk, backPatch, OpCode, writeu16Chunk, addConstant, currentLocation, Location};
use crate::value::{Value};
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
    AND,
    OR
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
    pub nodes: Vec<Node>,

    pub localCount: usize,
    pub scopeDepth: usize,

    pub jumpifFalse: Vec<usize>,
    pub jump: Vec<usize>,
    pub breakjump: Vec<usize>,
    pub loopDepth: usize,

}

impl<'a> Compiler<'a> {

    pub fn new(source: String, chunk: &'a mut Chunk) -> Self {

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

        }
    }

    pub fn updateSource(&mut self, source: String) {
        self.scanner.LoadSource(&source) ;
    }

    // Symbol table operations
    pub fn addVariable(&mut self, varname: String, datatype: DataType) -> usize {
        self.chunk.symbTable.addSymbol(varname, datatype)
    }

    pub fn getVariable(&mut self, varname: String) -> Symbol {
        let symb = self.chunk.symbTable.getSymbol(varname.clone()) ;
        if let Ok(..) = symb {
            symb.unwrap()
        } else {
            panic!("Cannot find variable {}", varname) ;
        }
    }

    fn nodePush(&mut self, node: Node) {
        self.nodes.push(node) ;
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
        let mut arity: usize = 0 ;
        let mut values: Vec<Node> = Vec::new() ;

        while ! self.t_match(TOKEN_RIGHT_BRACKET) {
            arity += 1 ;
            self.expression() ;
            self.t_match(TOKEN_COMMA) ;
            let node = self.nodes.pop().unwrap();
            values.push(node) ;
        }
        self.nodePush(Array {
            arity,
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
        self.nodePush(Node::Return {
            returnVal: Box::new(retVal)
        });

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
                value: Value::string(Rc::new(label)),
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
            TOKEN_LEFT_PAREN    => Rule::new(GROUPING, NONE, PREC_NONE),
            TOKEN_LEFT_BRACKET  => Rule::new(ARRAY_BUILD, NONE, PREC_NONE),
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
        self.expression();
        let printExpr = self.nodes.pop().unwrap() ;
        self.nodePush(Node::Print {
            printExpr: Box::new(printExpr)
        })
    }

    fn and_(&mut self) {
        self.parsePrecedence(PREC_AND);
        let node =  self.nodes.pop().unwrap() ;
        self.nodes.push(And {
            expr: Box::new(node)
        }) ;
    }

    fn or_(&mut self) {
        self.parsePrecedence(PREC_OR);
        let node =  self.nodes.pop().unwrap() ;
        self.nodes.push(Or {
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

        self.chunk.pushScope() ;
        let mut conditional: Vec<Node> = Vec::new() ;

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
            condition: conditional,
            statements: nodes
        } ;

        self.chunk.popScope() ;
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

    // *** Variable management **

    fn assignValueToVar(&mut self, varname: String) {
        self.expression() ;

        let childVal = self.nodes.pop().unwrap() ;

        self.nodePush(Node::setVar {
            name: varname,
            datatype: DataType::None,
            child: Box::new(childVal)
        });
    }

    fn assignValueToArray(&mut self, varname: String,n: Node) {


        // The value we're about to assign
        self.expression() ;
        let childVal = self.nodes.pop().unwrap() ;

        self.nodePush(Node::setArray {
            name: varname,
            index: Box::new(n),
            child: Box::new(childVal)
        });
    }

    fn namedArrayElement(&mut self, varname: String) {

        // If we have this, then it means that we're referring to
        // an array element

        self.expression() ;
        self.consume(TOKEN_RIGHT_BRACKET, "Expect ']' after array element expression");

        // Tells us which element in the array we're looking for
        let indexExpr = self.nodes.pop().unwrap() ;

        if self.t_match(TOKEN_EQUAL) {
            // This is the value we're assigning
            self.assignValueToArray(varname, indexExpr) ;

        } else {
            // If not, then it's being used as an expression

            let node = Node::namedArray {
                name: varname,
                index: Box::new(indexExpr)
            };
            self.nodePush(node);
        }

    }

    fn namedSingleVariable(&mut self, varname: String) {
        // This means we're assigning a value
        if self.t_match(TOKEN_EQUAL) {
            self.assignValueToVar(varname) ;
        } else {
            // If not, then it's being used as an expression
            let node = Node::namedVar {
                name: varname
            };
            self.nodePush(node);
        }
    }

    fn namedVariable(&mut self) {

        // This is the variable name we just encountered
        let varname = self.parser.previous().name ;

        if self.t_match(TOKEN_LEFT_BRACKET) {
            self.namedArrayElement(varname) ;
            return
        }

        self.namedSingleVariable(varname) ;
    }

    fn variable(&mut self) {
        self.namedVariable() ;
    }

    fn varDeclaration(&mut self) {

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

        let declExpr = self.nodes.pop().unwrap() ;
        let node = Node::VarDecl {
            name: varname,
            assigned: isAssigned,
            varExpr: Box::new(declExpr)
        };

        self.nodePush(node);

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
        self.walkTree(self.nodes[0].clone(), 1) ;

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
        self.chunk.addLocation("innerbreak");
        writeu16Chunk(self.chunk, 9999_u16, 0);
    }

    pub fn generateContinueStatement(&mut self) {
        if self.loopDepth == 0 {
            self.errorAtAst("BREAK statement must be inside a loop", 0);
        }

        writeChunk(self.chunk, OP_LOOP as u8, 0);
        let loc = currentLocation(self.chunk) ;
        let inner = self.chunk.popLocation("innercontinue") ;

        let continueTo = loc - inner+2;
        writeu16Chunk(self.chunk, continueTo as u16, 0);
    }

    pub fn walkTree(&mut self, node: Node, level: usize) -> DataType {

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
                    self.walkTree(n, level);
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
            Node::Array {
                arity,
                values
            } => {

                let mut dataType = DataType::Nil ;
                let mut elements:u16 = 0 ;

                for n in values {
                    elements+=1 ;
                    let tmpType = self.walkTree(n, level +2) ;
                    if dataType == DataType::Nil || dataType == tmpType {
                        dataType = tmpType ;
                    } else {
                        self.errorAtAst("Array must have the same data types", 0) ;
                    }
                }

                writeOp!(OP_PUSH, 0);
                writeOperand!(elements);

                writeOp!(OP_NEWARRAY, 0) ;
                DataType::Array
            },

            Node::BinaryExpr {
                line,
                op,
                lhs,
                rhs
            } => {
                let mut l_type = self.walkTree(*lhs, level + 2);
                let r_type = self.walkTree(*rhs, level + 2);

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

                match format!("{}{}", l_type.emit(), op.emit()).as_str() {
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
                        self.errorAtCurrent("Binary operator not found!");
                        DataType::None
                    }
                }
            },

            Node::UnaryExpr { line, op, child } => {
                let dataType = self.walkTree(*child, level + 2);

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
                name,
                assigned,
                varExpr
            } => {

                let datatype = self.walkTree(*varExpr, level + 2);
                let loc = self.addVariable(name.clone(), datatype) ;
                writeOp!(OP_SETVAR, 0);
                self.chunk.addComment(format!("Declare and store variable {}", name)) ;
                writeOperand!(loc as u16);

                datatype
            },

            Node::setArray {
                name,
                index,
                child
            } => {

                // The stack should contain:
                // -- index
                // -- Value
                // Then OP_SETAELEMENT first pops index off the stack to get to the element
                // followed by popping the value we're assigning to the variable

                let symbol = self.getVariable(name.clone()) ;

                // The value we're assigning
                let valueDataType = self.walkTree(*child, level + 2);

                // The index of the array
                let indexType = self.walkTree(*index, level + 2) ;


                // Todo: Check that the variable type matches the value type

                writeOp!(OP_SETAELEMENT, 0);
                self.chunk.addComment(format!("Store array {}", name)) ;
                writeOperand!(symbol.location as u16);

                valueDataType

            } ,

            Node::namedArray {
                name,
                index
            } => {
                let symbol = self.getVariable(name.clone()) ;
                // The index of the array
                let indexType = self.walkTree(*index, level + 2) ;

                writeOp!(OP_GETAELEMENT, 0);
                self.chunk.addComment(format!("Load array variable {}", name)) ;
                writeOperand!(symbol.location as u16);

                symbol.datatype
            } ,

            Node::setVar {
                name,
                datatype,
                child
            } => {
                let symbol = self.getVariable(name.clone()) ;
                let valueDataType = self.walkTree(*child, level + 2);

                let mut varType = datatype ;
                if varType == DataType::None {
                    varType = valueDataType;
                }
                // Todo: Check that the variable type matches the value type

                writeOp!(OP_SETVAR, 0);
                self.chunk.addComment(format!("Store variable {}", name)) ;
                writeOperand!(symbol.location as u16);

                varType
            },

            Node::namedVar {
                name
            } => {
                let symbol = self.getVariable(name.clone()) ;

                writeOp!(OP_LOADVAR, 0);
                self.chunk.addComment(format!("Load variable {}", name)) ;
                writeOperand!(symbol.location as u16);

                symbol.datatype
            },

            Node::Block => {
                self.chunk.symbTable.pushLevel();
                DataType::None
            },

            Node::EndBlock => {
                self.chunk.symbTable.popLevel();
                DataType::None
            },

            Node::And {
                expr
            } => {
                writeOp!(OP_JUMP_IF_FALSE_NOPOP,0) ;
                let loc = currentLocation(self.chunk);
                writeOperand!(9999_u16) ;
                writeOp!(OP_POP,0) ;

                if self.walkTree(*expr, level + 2 ) != DataType::Bool {
                    self.errorAtAst("Condition on the right of AND must evaluate to True or False", 0);
                }

                backPatch(self.chunk, loc ) ;

                DataType::Bool
            },

            Node::Or {
                expr
            } => {
                writeOp!(OP_JUMP_IF_FALSE_NOPOP,0) ;
                let loc = currentLocation(self.chunk);
                writeOperand!(9999_u16) ;

                writeOp!(OP_JUMP,0) ;
                let jmp = currentLocation(self.chunk);
                writeOperand!(9999_u16) ;

                backPatch(self.chunk, loc) ;
                writeOp!(OP_POP,0) ;

                if self.walkTree(*expr, level + 2 ) != DataType::Bool {
                    self.errorAtAst("Condition on the right of OR must evaluate to True or False", 0);
                }

                backPatch(self.chunk, jmp ) ;
                DataType::Bool
            },

            Node::Print {
                printExpr
            } => {
                let datatype = self.walkTree(*printExpr, level + 2);
                match datatype {
                    DataType::String => writeOp!(OP_SPRINT,0),
                    _ => writeOp!(OP_PRINT,0)
                }
                datatype
            },

            Node::Return {
                returnVal
            } => {
                let datatype = self.walkTree(*returnVal, level + 2);
                writeOp!(OP_RETURN,0);
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
                    let dataType = self.walkTree(e, level + 2 ) ;
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

                    self.walkTree(n, level +2);

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

                        self.walkTree(n, level + 2);
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
                condition,
                statements
            } => {

                self.loopDepth+=1 ;
                self.chunk.pushScope() ;

                let beginLocation = currentLocation(self.chunk);
                self.chunk.addLocation("innercontinue") ;

                // Collect all the logical condition expressions
                for e in condition {
                    let dataType = self.walkTree(e, level + 2 ) ;
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
                        self.chunk.addLocation("break");
                        writeOperand!(9999_u16 as u16) ;
                    }

                    if n == Node::Continue {
                        let loc = currentLocation(self.chunk) ;
                        writeOp!(OP_LOOP, 0) ;
                        let continueTo = loc - beginLocation+3;
                        writeOperand!(continueTo as u16) ;
                    }

                    self.walkTree(n, level +2);

                }

                writeOp!(OP_LOOP, 0) ;
                let loc = currentLocation(self.chunk) ;
                let backJumpBy = loc - beginLocation+2;
                writeOperand!(backJumpBy as u16) ;

                backPatch(self.chunk, jumpFromLocation) ;
                self.chunk.backpatchInner("innerbreak") ;
                self.chunk.backpatchInner("break") ;

                writeOp!(OP_POP, 0) ;

                self.loopDepth-=1 ;
                self.chunk.popScope() ;
                DataType::None
            },

            Node::While => {
                DataType::None
            },


            Node::Logical {
                expr
            } => {
                DataType::Bool
            }

        }
    }


}