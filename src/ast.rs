use crate::scanner::{TokenType} ;
use TokenType::* ;
use crate::chunk::{Chunk, writeChunk, backPatch, OpCode, writeu16Chunk, addConstant, currentLocation};
use crate::chunk::OpCode::*;
use crate::compiler::* ;
use crate::value::{Value};
use std::env::var;
use crate::ast::Ast::backpatch;

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
        label: String,
        value: Value,
        dataType: DataType
    },
    binop {
        label: String,
        operator: Operator
    },
    unaryOp {
        label: String,
        operator: Operator
    },
    setVar {
        varname: String,
        datatype: DataType
    },
    varDecl {
        varname: String ,
        assigned: bool
    },
    namedVar {
        varname: String
    },
    ret ,
    statement {
        tokenType: TokenType
    },
    print ,
    block,
    endBlock,
    jumpIfFalse {
        popType: JumpPop
    } ,
    jump ,
    backpatch {
        jumpType: JumpType
    },
    pop
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
    LtEq
}
impl Operator {
    fn emit(&self) -> String {
        match self {
            Operator::Plus  => "ADD",
            Operator::Minus => "SUB",
            Operator::Div   => "DIV",
            Operator::Mul   => "MUL",
            Operator::Eq    => "EQ",
            Operator::Gt    => "GT",
            Operator::Lt    => "LT",
            Operator::GtEq  => "GTEQ",
            Operator::LtEq  => "LTEQ"
        }.to_string()
    }
}

#[derive(Clone, Debug)]
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
    pop
}

impl<'a> Compiler<'a> {

   
    pub fn walkTree(&mut self, node: Node, level: usize) -> DataType {
        macro_rules! writeOp {
            ($byte:expr) => {
                writeChunk(self.chunk, $byte as u8, 0)
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
                        writeOp!(OP_NIL);
                    },
                    DataType::String => {
                        let constant_index = value.get_integer() as u16;

                        writeOp!(OP_SCONSTANT);
                        writeOperand!(constant_index);
                    },
                    _ => {
                        let constant_index = self.makeConstant(value);
                        writeOp!(OP_CONSTANT);
                        writeOperand!(constant_index);
                    }
                }
                dataType
            },

            Node::BinaryExpr { op, lhs, rhs } => {
                let l_type = self.walkTree(*lhs, level + 2);
                let r_type = self.walkTree(*rhs, level + 2);

                match format!("{}{}", l_type.emit(), op.emit()).as_str() {
                    "IADD" => {writeOp!(OP_IADD); DataType::Integer},
                    "ISUB" => {writeOp!(OP_ISUBTRACT);DataType::Integer},
                    "IMUL" => {writeOp!(OP_IMULTIPLY);DataType::Integer},
                    "IDIV" => {writeOp!(OP_IDIVIDE);DataType::Integer},

                    "FADD" => {writeOp!(OP_FADD);DataType::Float},
                    "FSUB" => {writeOp!(OP_FSUBTRACT);DataType::Float},
                    "FMUL" => {writeOp!(OP_FMULTIPLY);DataType::Float},
                    "FDIV" => {writeOp!(OP_FDIVIDE);DataType::Float},

                    "IEQ"  => {writeOp!(OP_IEQ);DataType::Bool},
                    "FEQ"  => {writeOp!(OP_FEQ);DataType::Bool},
                    "SEQ"  => {writeOp!(OP_SEQ);DataType::Bool},

                    "IGT"  => {writeOp!(OP_IGT);DataType::Bool},
                    "FGT"  => {writeOp!(OP_FGT);DataType::Bool},
                    "SGT"  => {writeOp!(OP_SGT);DataType::Bool},

                    "ILT"  => {writeOp!(OP_ILT);DataType::Bool},
                    "FLT"  => {writeOp!(OP_FLT);DataType::Bool},
                    "SLT"  => {writeOp!(OP_SLT);DataType::Bool},

                    "IGTEQ"  => {writeOp!(OP_IGTEQ);DataType::Bool},
                    "FGTEQ"  => {writeOp!(OP_FGTEQ);DataType::Bool},
                    "SGTEQ"  => {writeOp!(OP_SGTEQ);DataType::Bool},

                    "ILTEQ"  => {writeOp!(OP_ILTEQ);DataType::Bool},
                    "FLTEQ"  => {writeOp!(OP_FLTEQ);DataType::Bool},
                    "SLTEQ"  => {writeOp!(OP_SLTEQ);DataType::Bool},

                    _ => {
                        self.errorAtCurrent("Binary operator not found!");
                        DataType::None
                    }
                }
            },

            Node::UnaryExpr { op, child } => {
                let dataType = self.walkTree(*child, level + 2);
                match op {
                    Operator::Minus => {
                        match dataType {
                            DataType::Integer => {
                                writeOp!(OP_INEGATE) ;
                            },
                            DataType::Float => {
                                writeOp!(OP_FNEGATE) ;
                            },
                            _ => {}
                        }
                    },
                    Operator::Plus => {},
                    _ => {}
                }
                dataType
            },

            Node::Statement {
                tokenType
            } => {
                match tokenType {
                    TOKEN_START => {
                        println!("START");
                    }
                    _ => {}
                }
                DataType::None
            },

            Node::VarDecl {
                name,
                assigned,
                varExpr
            } => {
                let datatype = self.walkTree(*varExpr, level + 2);
                let loc = self.addVariable(name, datatype) ;
                writeOp!(OP_SETVAR);
                writeOperand!(loc as u16);

                datatype
            },

            Node::setVar {
                name,
                datatype,
                child
            } => {
                let symbol = self.getVariable(name) ;
                let valueDataType = self.walkTree(*child, level + 2);

                let mut varType = datatype ;
                if varType == DataType::None {
                    varType = valueDataType;
                }
                // Todo: Check that the variable type matches the value type

                writeOp!(OP_SETVAR);
                writeOperand!(symbol.location as u16);

                varType
            },

            Node::namedVar {
                name
            } => {

                let symbol = self.getVariable(name) ;

                 writeOp!(OP_LOADVAR);
                 writeOperand!(symbol.location as u16);

                symbol.datatype
            },

            Node::Block => {
                self.symbTable.pushLevel();
                DataType::None
            },

            Node::EndBlock => {
                self.symbTable.popLevel();
                DataType::None
            },

            Node::jumpIfFalse {
                popType
            } => {
                match popType {
                    JumpPop::POP => writeOp!(OP_JUMP_IF_FALSE),
                    JumpPop::NOPOP => writeOp!(OP_JUMP_IF_FALSE_NOPOP),
                }
                writeOperand!(9999_u16) ;
                let loc = currentLocation(&self.chunk) -2;
                self.jumpifFalse.push(loc);

                DataType::None
            },

            Node::backpatch {
                jumpType
            } => {

                let location = match jumpType {
                    JumpType::jumpIfFalse => self.jumpifFalse.pop().unwrap() ,
                    JumpType::Jump => self.jump.pop().unwrap()
                } ;

                backPatch(&mut self.chunk, location);

                DataType::None
            },

            Node::jump  => {

                writeOp!(OP_JUMP);
                writeOperand!(9999_u16) ;
                let loc = currentLocation(&self.chunk) -2;
                self.jump.push(loc) ;
                DataType::None
            },

            Node::pop => {
                writeOp!(OP_POP);
                DataType::None
            },

            Node::Print {
                printExpr
            } => {
                let datatype = self.walkTree(*printExpr, level + 2);
                match datatype {
                    DataType::String => writeOp!(OP_SPRINT),
                    _ => writeOp!(OP_PRINT)
                }
                datatype
            },

            Node::Return {
                returnVal
            } => {
                let datatype = self.walkTree(*returnVal, level + 2);
                writeOp!(OP_RETURN);
                datatype
            }

        }
    }

    pub fn buildTree(&mut self ) -> Node {
        println!("** Tree **");

        let mut nodes = Vec::<Node>::new();

        let len = self.ast.len();

        for i in 0..len {
            let e = self.ast[i].clone();
            match e {
                Ast::literal {
                    label,
                    value,
                    dataType
                } => {
                    let node = Node::Value {
                        label,
                        value,
                        dataType
                    };
                    nodes.push(node);
                },
                Ast::binop {
                    label,
                    operator
                } => {
                    let node = Node::BinaryExpr {
                        op: operator,
                        rhs: Box::new(nodes.pop().unwrap()),
                        lhs: Box::new(nodes.pop().unwrap())
                    };
                    nodes.push(node);
                },
                Ast::unaryOp {label,
                    operator
                } => {
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

                Ast::setVar {
                    varname,
                    datatype
                } => {

                    let childVal = nodes.pop().unwrap() ;

                    nodes.push(Node::setVar {
                        name: varname,
                        datatype,
                        child: Box::new(childVal)
                    });
                },

                Ast::varDecl {
                    varname,
                    assigned,
                } => {
                    let declExpr = nodes.pop().unwrap() ;
                    let node = Node::VarDecl {
                        name: varname,
                        assigned,
                        varExpr: Box::new(declExpr)
                    };
                    nodes.push(node);
                },
                Ast::ret => {
                    let retVal = nodes.pop().unwrap() ;
                    nodes.push(Node::Return {
                        returnVal: Box::new(retVal)
                    });
                },
                Ast::namedVar {
                    varname
                } => {
                    let node = Node::namedVar {
                        name: varname
                    };
                    nodes.push(node);
                },
                Ast::block => {
                    nodes.push(Node::Block);
                },
                Ast::endBlock => {
                    nodes.push(Node::EndBlock);
                },

                Ast::jumpIfFalse {
                    popType
                } => {

                    nodes.push(Node::jumpIfFalse {
                        popType
                    }) ;
                },

                Ast::jump => {
                    nodes.push(Node::jump);
                },

                Ast::backpatch {
                    jumpType
                } => {
                    nodes.push(Node::backpatch {
                        jumpType
                    });
                },

                Ast::pop => {
                    nodes.push(Node::pop)
                },

                Ast::print => {
                    let printExpr = nodes.pop().unwrap() ;
                    nodes.push(Node::Print {
                        printExpr: Box::new(printExpr)
                    })
                }
            }
        }
        // Set the root node
        Node::Root { children: nodes }
    }
}