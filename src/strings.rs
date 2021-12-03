pub struct MemPool {
    strings: Vec<Option<String>>,
    freeSlots: Vec<usize>,
    slotPtr: usize,
    freeSlotCount: u16
}

impl StringPool {
    pub fn new() -> Self {
        StringPool {
            strings: Vec::new(),
            freeSlots: Vec::new(),
            slotPtr: 0,
            freeSlotCount: 0
        }
    }

    pub fn store(&mut self, s: String) -> usize {
        // Get the next free slot
        let slot = self.getFreeSlot() ;
        self.strings[slot] = Some(s) ;
        self.freeSlotCount-=1 ;
        slot
    }

    // Makes a new entry in the strings vector
    // and adds it to the list of free slots
    fn makeNewSlot(&mut self) {
        // If none, then add a new string entry
        self.strings.push(None) ;
        // Increment the slot pointer to make it the top of
        // the free pointer stack
        let newPointer = self.strings.len()-1 ;
        self.freeSlots.push(newPointer);
        self.freeSlotCount+=1 ;
    }

    fn getFreeSlot(&mut self) -> usize {
        // Check to see if any free slots are available
        if self.freeSlotCount == 0 {
            self.makeNewSlot() ;
        }

        // Get to the value at the top of the stack
        let freeSlot = self.freeSlots[self.slotPtr];
        // The next slot is now the free slot
        self.slotPtr+=1 ;
        freeSlot
    }

    pub fn release(&mut self, slot: usize) {
        // Blank out the value in this slot
        self.strings[slot] = None ;
        //
        self.slotPtr-=1 ;
        self.freeSlots[self.slotPtr] = slot ;
        self.freeSlotCount+=1 ;
    }

    pub fn getIndex(&self, s: String) -> usize {
        let res = self.strings.binary_search(&Some(s)) ;
        if let Ok(..) = res {
            res.unwrap()
        } else {
            panic!("Variable not found");
        }

    }

    pub fn getValue(&self, slot: usize) -> &String {
        self.strings[slot].as_ref().unwrap()
    }

}

#[cfg(test)]
pub mod test {

    use super::* ;

    #[test]
    pub fn string_test() {
        let mut s = StringPool::new() ;

        let s01 = s.store("Hey".to_string()) ;
        assert_eq!(s01,0) ;
        let val = s.getValue(s01) ;
        assert_eq!(*val, "Hey".to_string()) ;
        println!("Got string '{}'", val) ;
        s.release(s01) ;

        let s02 = s.store("Whoa".to_string()) ;
        assert_eq!(s02,0) ;
        let val = s.getValue(s02) ;
        assert_eq!(*val, "Whoa".to_string()) ;
        println!("Got string '{}'", val) ;

        let s03 = s.store("Alriiight".to_string()) ;
        assert_eq!(s03,1) ;
        let val = s.getValue(s03) ;
        assert_eq!(*val, "Alriiight".to_string()) ;
        println!("Got string '{}'", val) ;
        s.release(s02) ;
        s.getValue(s03) ;

        let s04 = s.store("Whoa2".to_string()) ;
        assert_eq!(s04,0) ;
        let val = s.getValue(s04) ;
        assert_eq!(*val, "Whoa2".to_string()) ;
        println!("Got string '{}'", val) ;

    }
}