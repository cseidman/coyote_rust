use crate::scanner::{TokenType} ;
use TokenType::* ;

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
    Statement(Ast),
    Root {
        children: Box<Vec<Node>>
    }
}


pub fn walkTree(node: Node, level: usize) -> DataType {

    match node {
        Node::Root {children: nodes} => {
            for n in *nodes {
                walkTree(n, level) ;
            }
            DataType::None
        }
        Node::Value(x) => {
            println!("OP_{}PUSH {}",x.dataType.emit(), x.value) ;
            x.dataType
        },
        Node::BinaryExpr{op,lhs,rhs} => {
            let l_type = walkTree(*lhs,level+2) ;
            let r_type = walkTree(*rhs,level+2) ;
            println!("{}{}",l_type.emit(),op.emit()) ;
            l_type
        },
        Node::UnaryExpr{op,child} => {
            let dataType = walkTree(*child,level+2);
            match op {
                Operator::Minus => println!("{}NEG",dataType.emit()) ,
                _ => {}
            }
            dataType
        },
        Node::Statement(x) => {
            match x.tokenType {
                TOKEN_VAR => {

                },
                TOKEN_PRINT => {
                    println!("OP_PRINT") ;
                }
                TOKEN_START => {
                    println!("START") ;
                }
                _ => {}
            }
            DataType::None
        }
        _ => {DataType::None}
    }
}

pub fn buildTree(ast: &[Ast]) -> Node {

    println!("** Tree **") ;

    let mut nodes = Vec::<Node>::new() ;

    let len = ast.len() ;
    for i in ast {
        match i.opType {
            OpType::Literal => {
                nodes.push(Node::Value(i.clone()))
            },
            OpType::Binop => {
                let op = match i.tokenType {
                    TOKEN_PLUS => Operator::Plus,
                    TOKEN_MINUS => Operator::Minus,
                    TOKEN_STAR => Operator::Mul,
                    TOKEN_SLASH => Operator::Div,
                    _ => Operator::Plus
                };

                let node = Node::BinaryExpr {
                    op,
                    rhs: Box::new(nodes.pop().unwrap()),
                    lhs: Box::new(nodes.pop().unwrap())
                };
                nodes.push(node);
            },
            OpType::Unary => {
                let op = match i.tokenType {
                    TOKEN_PLUS => Operator::Plus,
                    TOKEN_MINUS => Operator::Minus,
                    _ => Operator::Plus
                };
                let node = Node::UnaryExpr {
                    op,
                    child: Box::new(nodes.pop().unwrap()),
                };
                nodes.push(node);
            },
            OpType::Statement => {
                let node = Node::Statement (
                    Ast {
                        tokenType: i.tokenType,
                        opType: i.opType,
                        label: i.label.to_string(),
                        value: 0,
                        dataType: i.dataType
                    }
                );
                nodes.push(node) ;
            }
            _ => {}
        }

    }
    // Set the root node
    let rootNode = Node::Root { children: Box::new(nodes)};
    rootNode


}