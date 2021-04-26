use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::Path;

mod casl2;
mod compiler;
#[allow(dead_code)]
mod jis_x_201;
mod parser;
mod stat;
mod tokenizer;

const APP_NAME: &str = "b2c2";

const FLAG_SOURCE_FILE: &str = "-src";
const FLAG_REMOVE_COMMENT: &str = "-remove-comment";
const FLAG_REMOVE_NOP: &str = "-remove-nop";
const FLAG_REMOVE_UNREFERENCED_LABEL: &str = "-remove-unreferenced-label";
const FLAG_SPLIT_SUBROUTINES: &str = "-split-subroutines";
const FLAG_TRY_MAKE_SNIPPETS: &str = "-try-make-snippets";
const FLAG_PROGRAM_NAME: &str = "-program-name";
const FLAG_STATISTICS: &str = "-statistics";

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
    {current_exe} {flag_src} <FILE> [<OPTIONS>]

OPTIONS:
    {flag_remove_nop}
                        生成されるCASL2ソースコードからNOP行を除去しNOPにつけられたラベルを整理する
    {flag_remove_unreferenced_label}
                        生成されるCASL2ソースコードから未使用ラベルを除去する
    {flag_remove_comment}
                        生成されるCASL2ソースコードからコメント行を除去する
    {flag_try_make_snippets}
                        生成されるCASL2ソースコードの重複コードをサブルーチン化することで行数を減らすことを試みる

    {flag_split_subroutines}
                        生成されるCASL2プログラムで使用される組み込みサブルーチンを分離し外部プログラムとして出力する

    {flag_program_name} <NAME>
                        生成されるCASL2プログラムの名前を指定する

    {flag_statistics}
                        生成されるCASL2ソースコードの統計情報ぽいものを出力する
",
        flag_src = FLAG_SOURCE_FILE,
        flag_remove_nop = FLAG_REMOVE_NOP,
        flag_remove_unreferenced_label = FLAG_REMOVE_UNREFERENCED_LABEL,
        flag_remove_comment = FLAG_REMOVE_COMMENT,
        flag_try_make_snippets = FLAG_TRY_MAKE_SNIPPETS,
        flag_split_subroutines = FLAG_SPLIT_SUBROUTINES,
        flag_program_name = FLAG_PROGRAM_NAME,
        flag_statistics = FLAG_STATISTICS,
        current_exe = current_exe,
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
    let mut iter = env::args().skip(1);

    let mut src_file: Option<String> = None;
    let mut flag = compiler::Flag::default();
    let mut flag_statistics = false;

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            FLAG_SOURCE_FILE => {
                if src_file.is_some() {
                    eprintln!("ERROR: DUPLICATE OPTION {}", arg);
                    return show_usage();
                }
                if let Some(file) = iter.next() {
                    if !Path::new(&file).exists() {
                        eprintln!("ERROR: NOT FOUND {}", file);
                        return Ok(3);
                    }
                    src_file = Some(file);
                } else {
                    eprintln!("ERROR: NO SOURCE FILE");
                    return show_usage();
                }
            }
            FLAG_REMOVE_COMMENT => {
                if flag.remove_comment {
                    eprintln!("ERROR: DUPLICATE OPTION {}", arg);
                    return show_usage();
                }
                flag.remove_comment = true;
            }
            FLAG_REMOVE_NOP => {
                if flag.remove_nop {
                    eprintln!("ERROR: DUPLICATE OPTION {}", arg);
                    return show_usage();
                }
                flag.remove_nop = true;
            }
            FLAG_REMOVE_UNREFERENCED_LABEL => {
                if flag.remove_unreferenced_label {
                    eprintln!("ERROR: DUPLICATE OPTION {}", arg);
                    return show_usage();
                }
                flag.remove_unreferenced_label = true;
            }
            FLAG_SPLIT_SUBROUTINES => {
                if flag.split_subroutines {
                    eprintln!("ERROR: DUPLICATE OPTION {}", arg);
                    return show_usage();
                }
                flag.split_subroutines = true;
            }
            FLAG_TRY_MAKE_SNIPPETS => {
                if flag.try_make_snippets {
                    eprintln!("ERROR: DUPLICATE OPTION {}", arg);
                    return show_usage();
                }
                flag.try_make_snippets = true;
            }
            FLAG_PROGRAM_NAME => {
                if flag.program_name.is_some() {
                    eprintln!("ERROR: DUPLICATE OPTION {}", arg);
                    return show_usage();
                }
                if let Some(name) = iter.next() {
                    flag.program_name = Some(name);
                } else {
                    eprintln!("ERROR: NO PROGRAM NAME");
                    return show_usage();
                }
            }
            FLAG_STATISTICS => {
                if flag_statistics {
                    eprintln!("ERROR: DUPLICATE OPTION {}", arg);
                    return show_usage();
                }
                flag_statistics = true;
            }
            _ => {
                eprintln!("ERROR: UNKOWN OPTION {}", arg);
                return show_usage();
            }
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

    let casl2_src_list = match compiler::compile_with_flag(&basic_src, flag) {
        Err(error) => {
            eprintln!("{:?}", error);
            return Ok(5);
        }
        Ok(mut list) => {
            for option in basic_src.iter().filter_map(|stmt| {
                if let parser::Statement::CompileOption { option } = stmt {
                    Some(option)
                } else {
                    None
                }
            }) {
                match option {
                    parser::CompileOption::Eof { common: true } => {
                        let src = compiler::subroutine::get_util_eof_store_code();
                        list.push(("EOF".to_string(), src));
                    }
                    parser::CompileOption::Allocator {
                        enabled: true,
                        common: true,
                        size,
                    } => {
                        let src = compiler::subroutine::get_util_allocator_code_for_common(*size);
                        list.push(("ALLOC".to_string(), src));
                    }
                    _ => {}
                }
            }
            list
        }
    };

    for (name, casl2_src) in casl2_src_list {
        let file_name = format!("{}.cas", name);

        let mut dst_file = fs::File::create(&file_name)?;

        for stmt in casl2_src.iter() {
            writeln!(&mut dst_file, "{}", stmt)?;
        }

        dst_file.flush()?;

        eprintln!("GENERATED: {}", file_name);

        if flag_statistics {
            let file_name = format!("{}.stat.txt", name);
            let statistics = stat::analyze(&casl2_src);
            fs::write(&file_name, statistics)?;
            eprintln!("GENERATED: {}", file_name);
        }
    }

    Ok(0)
}
