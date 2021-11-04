pub type Value = f64 ;


pub fn printValue(value: Value) {
    print!("{}",value) ;
}


pub struct ValueArray{
    pub values: Vec<Value>
}

impl ValueArray {
    pub fn new() -> Self {
        ValueArray{
            values: Vec::new()
        }
    }
}

pub fn writeValueArray(array: &mut ValueArray, value: Value) {
    array.values.push(value) ;
}
