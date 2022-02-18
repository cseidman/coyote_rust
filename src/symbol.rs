use crate::value::Value ;
use crate::ast::DataType;
use std::collections::HashMap;

// Symbol Types
#[derive(Clone)]
pub enum ObjectType {
    variable,
    datatype
}

// Symbol entry for the class type
pub struct ClassSymbol {
    name: String, // Name of the class
    level: usize,
}

// Symbol tables for variables
#[derive(Debug, Clone)]
pub struct Symbol {
    name: String,
    level: usize,
    pub location: usize,
    pub datatype: DataType,
}

#[derive(Debug, Clone)]
pub struct SymbolLevel {
    symbols: HashMap<String, Symbol>,
    level: usize,
    nextSlot: usize
}

impl SymbolLevel {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            level: 0,
            nextSlot: 0
        }
    }
}
#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbolLevel: Vec<SymbolLevel>,
    level: usize,
    nextSlot: usize
}

impl SymbolTable {

    pub fn new() -> Self {

        let mut t = SymbolTable {
           symbolLevel: Vec::new(),
           level: 0,
           nextSlot: 0
        };

        // Creates an empty slot for the top level variables
        t.symbolLevel.push(SymbolLevel::new());
        t
    }

    pub fn varCount(&self) -> usize {
        self.symbolLevel[self.level-1].symbols.len()
    }

    pub fn debug(&self) {
        for l in self.symbolLevel.iter() {
            for s in l.symbols.keys() {
                println!("Level {} Symbol {}", l.level, s);
            }
        }
    }

    pub fn pushLevel(&mut self) {
        let mut symbLevel = SymbolLevel::new() ;
        symbLevel.nextSlot = self.symbolLevel[self.level].nextSlot ;
        self.symbolLevel.push(symbLevel) ;
        self.level+=1 ;

    }

    pub fn popLevel(&mut self) {
        self.symbolLevel.pop() ;
        self.level-=1 ;
    }

    pub fn addSymbol(
        &mut self,
        name: String,
        datatype: DataType
    ) -> usize {
        let currentSlot = self.symbolLevel[self.level].nextSlot ;
        self.symbolLevel[self.level].nextSlot+=1 ;

        let symb = Symbol {
            name: name.clone(),
            level: self.level,
            location: currentSlot,
            datatype
        };
        self.symbolLevel[self.level].symbols.insert(name.clone(), symb) ;
        currentSlot

    }

    pub fn getSymbol(&self, name: String) -> Result<Symbol, &str> {
        let mut lvl = self.level ;
        loop {

            if self.symbolLevel[lvl].symbols.contains_key(&name) {
                return Ok(self.symbolLevel[lvl]
                    .symbols
                    .get(&name)
                    .unwrap()
                    .clone());
            }

            // We're done
            if lvl == 0 {
                break ;
            }

            lvl-=1 ;

        }
        Err("Symbol not found")
    }
}

