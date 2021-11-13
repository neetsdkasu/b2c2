// b2c2-common crate
// author: Leonardone @ NEETSDKASU

#[derive(Debug)]
pub struct SyntaxError {
    pub line_number: usize,
    pub position: usize,
    pub message: String,
}

impl SyntaxError {
    pub fn new(line_number: usize, position: usize, message: String) -> Self {
        Self {
            line_number,
            position,
            message,
        }
    }
}
