use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::Path;

#[allow(dead_code)]
mod casl2;
#[allow(dead_code)]
mod compiler;
#[allow(dead_code)]
mod jis_x_201;
#[allow(dead_code)]
mod parser;
#[allow(dead_code)]
mod stat;
#[allow(dead_code)]
mod tokenizer;

const APP_NAME: &str = "b2c2";

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

fn show_usage() -> io::Result<i32> {
    let current_exe = env::current_exe()?
        .file_name()
        .and_then(|f| f.to_str().map(|n| n.to_string()))
        .unwrap_or_else(|| APP_NAME.to_string());

    eprintln!("
BASIC言語風の独自のプログラミング言語(?)のソースコードファイルからCASL2のソースコードファイルを生成します
    
USAGE:
    {current_exe} -src <FILE>    
",
    current_exe = current_exe
);

    Ok(2)
}

fn main() {
    std::process::exit(match run_app() {
        Ok(code) => code,
        Err(error) => {
            eprintln!("{}", error);
            1
        }
    });
}

fn run_app() -> io::Result<i32> {
    let args: Vec<_> = env::args().skip(1).collect();

    if args.is_empty() {
        return show_usage();
    }

    let mut iter = args.into_iter();

    let mut src_file: Option<String> = None;

    while let Some(arg) = iter.next() {
        if arg.as_str() == "-src" {
            if src_file.is_some() {
                eprintln!("ERROR: DUPLICATE OPTION -src");
                return show_usage();
            }
            if let Some(file) = iter.next() {
                if !Path::new(&file).exists() {
                    eprintln!("ERROR: NOT FOUND {}", file);
                    return Ok(3);
                }
                src_file = Some(file);
            } else {
                eprintln!("ERROR: NO FILE");
                return show_usage();
            }
        } else {
            eprintln!("ERROR: UNKOWN OPTION {}", arg);
            return show_usage();
        }
    }

    let src_file = match src_file {
        Some(file) => file,
        None => {
            eprintln!("ERROR: MISSING SOURCE FILE");
            return show_usage();
        }
    };

    let basic_src = {
        let file = fs::File::open(src_file)?;
        let reader = io::BufReader::new(file);
        match parser::parse(reader)? {
            Ok(src) => src,
            Err(error) => {
                eprintln!("{:?}", error);
                return Ok(4);
            }
        }
    };

    let casl2_src = match compiler::compile(None, &basic_src) {
        Ok(src) => src,
        Err(error) => {
            eprintln!("{:?}", error);
            return Ok(5);
        }
    };

    let mut dst_file = fs::File::create("MAIN.cas")?;

    for stmt in casl2_src {
        writeln!(&mut dst_file, "{}", stmt)?;
    }

    dst_file.flush()?;

    eprintln!("GENERATED: MAIN.cas");

    Ok(0)
}
