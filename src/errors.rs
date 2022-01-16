#[derive(PartialOrd, PartialEq)]
pub enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
}

pub fn ReportError(message: String, line: usize) {
    println!("Line {}: Error: {}", line, message) ;
}