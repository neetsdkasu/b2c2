#[allow(dead_code)]
mod casl2;

#[allow(dead_code)]
mod jis_x_201;

#[allow(dead_code)]
mod tokenizer;

#[allow(dead_code)]
mod parser;

#[derive(Debug)]
pub struct SyntaxError {
    line_number: usize,
    position: usize,
    message: String,
}

impl SyntaxError {
    fn new(line_number: usize, position: usize, message: String) -> Self {
        Self {
            line_number,
            position,
            message,
        }
    }
}

fn main() {
    println!("Hello, world!");
}
