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
    Nil,
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
pub enum Ast {
    literal {
        tokenType: TokenType,
        label: String,
        value: u64,
        dataType: DataType
    },
    binop {
        tokenType: TokenType,
        label: String,
        operator: Operator
    },
    unaryOp {
        tokenType: TokenType,
        label: String,
        operator: Operator
    },
    varDecl {
        varname: String ,
        location: usize,
        scope: Scope
    },
    namedVar {
        varname: String ,
        location: usize,
        scope: Scope
    },
    statement {
        tokenType: TokenType
    }
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
#[derive(Clone, PartialEq)]
pub enum Scope {
    Global,
    Local
}

#[derive(Clone)]
pub enum Node {
    Value {
        label: String,
        value: u64,
        dataType: DataType
    },
    UnaryExpr {
        op: Operator,
        child: Box<Node>,
    },
    BinaryExpr {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Statement {
        tokenType: TokenType
    },
    VarDecl {
        name: String ,
        location: usize,
        scope: Scope
    },
    namedVar {
        name: String ,
        location: usize,
        scope: Scope
    },
    Root {
        children: Vec<Node>
    }
}


pub fn walkTree(node: Node, level: usize) -> DataType {

    match node {
        Node::Root {children: nodes} => {
            for n in nodes {
                walkTree(n, level) ;
            }
            DataType::None
        }
        Node::Value{ label, value, dataType} => {
            if dataType == DataType::Nil {
                println!("OP_NIL");
            } else {
                println!("OP_{}PUSH {}", dataType.emit(), value);
            }
            dataType
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
        Node::Statement{
            tokenType
        } => {
            match tokenType {
                TOKEN_PRINT => {
                    println!("OP_PRINT") ;
                }
                TOKEN_START => {
                    println!("START") ;
                }
                _ => {}
            }
            DataType::None
        },
        Node::VarDecl {
            name,
            location,
            scope
        } => {
            if scope == Scope::Global {
                println!("OP_DEFINE_GLOBAL {}", location);
            } else {
                println!("OP_DEFINE_LOCAL {}", location);
            }
            DataType::None
        },
        Node::namedVar {
            name,
            location,
            scope
        } => {
            if scope == Scope::Global {
                println!("OP_GET_GLOBAL {}", location);
            } else {
                println!("OP_GET_LOCAL {}", location);
            }
            DataType::None
        },
    }
}

pub fn buildTree(ast: &[Ast]) -> Node {

    println!("** Tree **") ;

    let mut nodes = Vec::<Node>::new() ;


    let len = ast.len() ;
    for i in 0 .. len {
        let e = ast[i].clone();
        match e {
            Ast::literal {tokenType, label, value, dataType } => {
                let node = Node::Value {
                    label,
                    value,
                    dataType
                };
                nodes.push(node) ;
            },
            Ast::binop { tokenType, label, operator } => {

                let node = Node::BinaryExpr {
                    op: operator,
                    rhs: Box::new(nodes.pop().unwrap()),
                    lhs: Box::new(nodes.pop().unwrap())
                };
                nodes.push(node);
            },
            Ast::unaryOp {tokenType, label, operator} => {

                let node = Node::UnaryExpr {
                    op: operator,
                    child: Box::new(nodes.pop().unwrap()),
                };
                nodes.push(node);
            },
            Ast::statement {tokenType} => {
                let node = Node::Statement {
                    tokenType
                };
                nodes.push(node) ;
            },
            Ast::varDecl {
                varname,
                location,
                scope
            } => {
                let node = Node::VarDecl {
                    name: varname,
                    location,
                    scope
                }  ;
                nodes.push(node) ;
            },
            Ast::namedVar {
                varname,
                location,
                scope
            } => {
                let node = Node::namedVar {
                    name: varname,
                    location,
                    scope
                }  ;
                nodes.push(node) ;
            }
        }

    }
    // Set the root node
    Node::Root { children: nodes}


}