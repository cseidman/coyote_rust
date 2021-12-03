use crate::scanner::{TokenType} ;
use TokenType::* ;
use crate::chunk::{Chunk, writeChunk, OpCode, writeu16Chunk, addConstant};
use crate::chunk::OpCode::*;
use crate::compiler::* ;
use crate::value::{Value};

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
        value: Value,
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
        scope: Scope,
        datatype: DataType
    },
    namedVar {
        varname: String ,
        location: usize,
        scope: Scope,
        datatype: DataType
    },
    ret {
        datatype: DataType
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
        value: Value,
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
        scope: Scope,
        datatype: DataType
    },
    namedVar {
        name: String ,
        location: usize,
        scope: Scope,
        datatype: DataType
    },
    Return {
        datatype: DataType
    },
    Root {
        children: Vec<Node>
    }
}




impl<'a> Compiler<'a> {

    pub fn walkTree(&mut self, node: Node, level: usize) -> DataType {
        macro_rules! writeOp {
            ($byte:expr) => {
                writeChunk(self.chunk, $byte as u8, 0)
            };
        }

        macro_rules! writeByte {
            ($byte:expr) => {
                writeChunk(self.chunk, $byte, 0)
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
            Node::Value { label, value, dataType } => {
                match dataType {
                    DataType::Nil => {
                        println!("OP_NIL");
                        writeOp!(OP_NIL);
                    },
                    DataType::String => {
                        let constant_index = value.get_integer() as u16;
                        println!("OP_SCONSTANT {}", constant_index);
                        writeOp!(OP_SCONSTANT);
                        writeOperand!(constant_index);
                    },
                    _ => {
                        let constant_index = self.makeConstant(value);
                        println!("OP_CONSTANT {}", constant_index);
                        writeOp!(OP_CONSTANT);
                        writeOperand!(constant_index);
                    }
                }
                dataType
            },
            Node::BinaryExpr { op, lhs, rhs } => {
                let l_type = self.walkTree(*lhs, level + 2);
                let r_type = self.walkTree(*rhs, level + 2);
                println!("{}{}", l_type.emit(), op.emit());
                match format!("{}{}", l_type.emit(), op.emit()).as_str() {
                    "IADD" => writeOp!(OP_IADD),
                    "ISUB" => writeOp!(OP_ISUBTRACT),
                    "IMUL" => writeOp!(OP_IMULTIPLY),
                    "IDIV" => writeOp!(OP_IDIVIDE),
                    "FADD" => writeOp!(OP_FADD),
                    "FSUB" => writeOp!(OP_FSUBTRACT),
                    "FMUL" => writeOp!(OP_FMULTIPLY),
                    "FDIV" => writeOp!(OP_FDIVIDE),
                    _ => {
                        // TODO make an error
                    }
                }
                l_type
            },
            Node::UnaryExpr { op, child } => {
                let dataType = self.walkTree(*child, level + 2);
                match op {
                    Operator::Minus => println!("{}NEG", dataType.emit()),
                    _ => {}
                }
                dataType
            },
            Node::Statement {
                tokenType
            } => {
                match tokenType {
                    TOKEN_PRINT => {
                        println!("OP_PRINT");
                    }
                    TOKEN_START => {
                        println!("START");
                    }
                    _ => {}
                }
                DataType::None
            },
            Node::VarDecl {
                name,
                location,
                scope,
                datatype
            } => {
                if scope == Scope::Global {
                    println!("OP_DEFINE_GLOBAL {}", location);
                    match datatype {
                        DataType::Integer => writeOp!(OP_DEFINE_IGLOBAL),
                        DataType::Float => writeOp!(OP_DEFINE_FGLOBAL),
                        DataType::Bool => writeOp!(OP_DEFINE_BGLOBAL),
                        DataType::String => writeOp!(OP_DEFINE_SGLOBAL),
                        _ => {}
                    }
                } else {
                    println!("OP_DEFINE_LOCAL {}", location);
                    match datatype {
                        DataType::Integer => writeOp!(OP_DEFINE_ILOCAL),
                        DataType::Float => writeOp!(OP_DEFINE_FLOCAL),
                        DataType::Bool => writeOp!(OP_DEFINE_BLOCAL),
                        DataType::String => writeOp!(OP_DEFINE_SLOCAL),
                        _ => {}
                    }
                }
                DataType::None
            },
            Node::Return { datatype } => {
                writeOp!(OP_RETURN);
                datatype
            },
            Node::namedVar {
                name,
                location,
                scope,
                datatype,
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

    pub fn buildTree(&mut self ) -> Node {
        println!("** Tree **");

        let mut nodes = Vec::<Node>::new();

        let len = self.ast.len();
        for i in 0..len {
            let e = self.ast[i].clone();
            match e {
                Ast::literal { tokenType, label, value, dataType } => {
                    let node = Node::Value {
                        label,
                        value,
                        dataType
                    };
                    nodes.push(node);
                },
                Ast::binop { tokenType, label, operator } => {
                    let node = Node::BinaryExpr {
                        op: operator,
                        rhs: Box::new(nodes.pop().unwrap()),
                        lhs: Box::new(nodes.pop().unwrap())
                    };
                    nodes.push(node);
                },
                Ast::unaryOp { tokenType, label, operator } => {
                    let node = Node::UnaryExpr {
                        op: operator,
                        child: Box::new(nodes.pop().unwrap()),
                    };
                    nodes.push(node);
                },
                Ast::statement { tokenType } => {
                    let node = Node::Statement {
                        tokenType
                    };
                    nodes.push(node);
                },
                Ast::varDecl {
                    varname,
                    location,
                    scope,
                    datatype
                } => {
                    let node = Node::VarDecl {
                        name: varname,
                        location,
                        scope,
                        datatype
                    };
                    nodes.push(node);
                },
                Ast::ret { datatype } => {
                    nodes.push(Node::Return {
                        datatype
                    });
                },
                Ast::namedVar {
                    varname,
                    location,
                    scope,
                    datatype
                } => {
                    let node = Node::namedVar {
                        name: varname,
                        location,
                        scope,
                        datatype
                    };
                    nodes.push(node);
                }
            }
        }
        // Set the root node
        Node::Root { children: nodes }
    }
}