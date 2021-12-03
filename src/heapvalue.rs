use std::collections::HashMap;
use crate::value::{Value};

#[derive(Clone, Debug)]
pub enum HeapValue {
    string(String),
    hash(HashMap<Value,Value>),
    nil
}

impl HeapValue {
    pub fn getString(self) -> String {
        if let HeapValue::string(s) = self  {
            s
        } else {
            "".to_string()
        }
    }
}

pub type HeapValueArray = Vec<HeapValue>;

pub struct MemPool {
    heapValues: Vec<HeapValue>,
    freeSlots: Vec<usize>,
    slotPtr: usize,
    freeSlotCount: u16
}

impl MemPool {
    pub fn new() -> Self {
        MemPool {
            heapValues: Vec::new(),
            freeSlots: Vec::new(),
            slotPtr: 0,
            freeSlotCount: 0
        }
    }

    pub fn store(&mut self, s: String) -> usize {
        // Get the next free slot
        let slot = self.getFreeSlot() ;
        self.heapValues[slot] = HeapValue::string(s) ;
        self.freeSlotCount-=1 ;
        slot
    }

    // Makes a new entry in the strings vector
    // and adds it to the list of free slots
    fn makeNewSlot(&mut self) {
        // If none, then add a new string entry
        self.heapValues.push(HeapValue::nil) ;
        // Increment the slot pointer to make it the top of
        // the free pointer stack
        let newPointer = self.heapValues.len()-1 ;
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
        self.heapValues[slot] = HeapValue::nil ;
        //
        self.slotPtr-=1 ;
        self.freeSlots[self.slotPtr] = slot ;
        self.freeSlotCount+=1 ;
    }

}

#[cfg(test)]
pub mod test {

    use super::* ;

    #[test]
    pub fn mempool_test() {

    }
}