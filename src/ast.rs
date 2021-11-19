use crate::scanner::{TokenType} ;
use TokenType::* ;

#[derive(PartialEq, Copy, Clone)]
pub enum OpType {
    Literal,
    Binop,
    Unary,
    Statement
}

// This struct is used to determine to what extent
// types are compatible for binary operations
#[derive(PartialEq, Copy, Clone, Debug)]
pub enum DataType {
    Integer,
    Float,
    String,
    Bool,
    None
}
impl DataType {
    fn emit(&self) -> String {
        match self {
            DataType::Integer => "I",
            DataType::Float => "F",
            DataType::String => "S",
            DataType::Bool => "B",
            _ => "_"
        }.to_string()
    }
}

#[derive(Clone)]
pub struct Ast {
    pub tokenType: TokenType,
    pub opType: OpType,
    pub label: String,
    pub value: u64,
    pub dataType: DataType
}

#[derive(Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div
}
impl Operator {
    fn emit(&self) -> String {
        match self {
            Operator::Plus=>"ADD",
            Operator::Minus=>"SUB",
            Operator::Div=>"DIV",
            Operator::Mul=>"MUL"
        }.to_string()
    }
}


#[derive(Clone)]
pub enum Node {
    Value(Ast),
    UnaryExpr {
        op: Operator,
        child: Box<Node>,
    },
    BinaryExpr {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}


pub fn walkTree(node: Box<Node>, level: usize) -> DataType {

    match *node {
        Node::Value(x) => {
            println!("OP_{}PUSH {}",x.dataType.emit(), x.value) ;
            return x.dataType ;
        },
        Node::BinaryExpr{op,lhs,rhs} => {
            let l_type = walkTree(lhs,level+2) ;
            let r_type = walkTree(rhs,level+2) ;
            println!("{}{}",l_type.emit(),op.emit(),) ;
            return l_type ;
        },
        Node::UnaryExpr{op,child} => {
            return walkTree(child,level+2) ;

        }
    }
}

pub fn buildTree(ast: &Vec<Ast>) -> Box<Node> {

    println!("** Tree **") ;

    let mut nodes = Vec::<Node>::new() ;
    let len = ast.len() ;
    for i in ast {

        if i.opType == OpType::Binop {

            let op = match i.tokenType {
                TOKEN_PLUS => Operator::Plus,
                TOKEN_MINUS => Operator::Minus,
                TOKEN_STAR => Operator::Mul,
                TOKEN_SLASH => Operator::Div,
                _ => Operator::Plus
            };

            let node = Node::BinaryExpr {
                op ,
                rhs: Box::new(nodes.pop().unwrap()),
                lhs: Box::new(nodes.pop().unwrap())
            };
            nodes.push(node) ;
        } else {
            nodes.push(Node::Value(i.clone()));
        }
    }
    Box::<Node>::new(nodes[0].clone())
}