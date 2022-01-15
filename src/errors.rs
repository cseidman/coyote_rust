#[derive(PartialOrd, PartialEq)]
pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
}

pub fn ReportError(message: String) {
    println!("Error: {}", message) ;
}