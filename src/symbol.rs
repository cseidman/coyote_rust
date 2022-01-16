use crate::value::Value ;
use crate::ast::DataType;

#[derive(Clone)]
pub struct Symbol {
    name: String,
    level: usize,
    pub location: usize,
    pub datatype: DataType
}
#[derive(Clone)]
pub struct SymbolLevel {
    symbols: Vec<Symbol>,
    level: usize,
    nextSlot: usize
}

impl SymbolLevel {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
            level: 0,
            nextSlot: 0
        }
    }
}
#[derive(Clone)]
pub struct SymbolTable {
    symbolLevel: Vec<SymbolLevel>,
    level: usize,
    nextSlot: usize
}

impl SymbolTable {

    pub fn new() -> Self {

        let mut t= SymbolTable {
           symbolLevel: Vec::new(),
           level: 0,
           nextSlot: 0
        };

        // Creates an empty slot for the top level variables
        t.symbolLevel.push(SymbolLevel::new());
        t
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
            name,
            level: self.level,
            location: currentSlot,
            datatype
        };
        self.symbolLevel[self.level].symbols.push(symb) ;
        currentSlot

    }

    pub fn getSymbol(&self, name: String) -> Result<Symbol, &str> {
        let mut lvl = self.level ;
        loop {

            for symb in self.symbolLevel[lvl].symbols.iter() {
                if name == symb.name {
                    return Ok(symb.clone());
                }
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

#[cfg(test)]
mod test {

    use super::* ;

    #[test]
    pub fn run_symbols() {

        // Load symbol table
        let mut t = SymbolTable::new() ;
        assert_eq!(t.level,0, "Make sure level is 0");

        let loc = t.addSymbol(
            "varx".to_string(),
            DataType::Integer
        );

        let symb = t.getSymbol("varx".to_string()) ;
        assert_eq!(symb.unwrap().name, "varx".to_string()) ;

        t.pushLevel() ;
        assert_eq!(t.level,1 );

        let loc2 = t.addSymbol(
            "varx".to_string(),
            DataType::Integer
        );

        let symb2 = t.getSymbol("varx".to_string()) ;
        assert_eq!(symb2.unwrap().name, "varx".to_string()) ;

        let symb3 = t.getSymbol("vary".to_string()) ;
        assert!(symb3.is_err(), "Unable to find 'vary'") ;

        t.popLevel() ;
        assert_eq!(t.level, 0);

    }

}