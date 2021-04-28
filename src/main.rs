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

fn main() {
    std::process::exit(match run_app() {
        Ok(code) => code,
        Err(error) => {
            eprintln!("{}", error);
            1
        }
    });
}

// コマンドライン引数を解釈して指定の処理を実行する
fn run_app() -> io::Result<i32> {
    let mut iter = env::args().skip(1);

    let mut src_file: Option<String> = None;
    let mut flag = compiler::Flag::default();
    let mut flag_statistics = false;

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            FLAG_SOURCE_FILE => {
                if src_file.is_some() {
                    eprintln!("ERROR: {}が重複してます", arg);
                    return show_usage();
                }
                if let Some(file) = iter.next() {
                    if !Path::new(&file).exists() {
                        eprintln!("ERROR: ファイルが見つかりません ( {} )", file);
                        return Ok(3);
                    }
                    src_file = Some(file);
                } else {
                    eprintln!("ERROR:　ソースファイルが指定されてません");
                    return show_usage();
                }
            }
            FLAG_REMOVE_COMMENT => {
                if flag.remove_comment {
                    eprintln!("ERROR: {}が重複してます", arg);
                    return show_usage();
                }
                flag.remove_comment = true;
            }
            FLAG_REMOVE_NOP => {
                if flag.remove_nop {
                    eprintln!("ERROR: {}が重複してます", arg);
                    return show_usage();
                }
                flag.remove_nop = true;
            }
            FLAG_REMOVE_UNREFERENCED_LABEL => {
                if flag.remove_unreferenced_label {
                    eprintln!("ERROR: {}が重複してます", arg);
                    return show_usage();
                }
                flag.remove_unreferenced_label = true;
            }
            FLAG_SPLIT_SUBROUTINES => {
                if flag.split_subroutines {
                    eprintln!("ERROR: {}が重複してます", arg);
                    return show_usage();
                }
                flag.split_subroutines = true;
            }
            FLAG_TRY_MAKE_SNIPPETS => {
                if flag.try_make_snippets {
                    eprintln!("ERROR: {}が重複してます", arg);
                    return show_usage();
                }
                flag.try_make_snippets = true;
            }
            FLAG_PROGRAM_NAME => {
                if flag.program_name.is_some() {
                    eprintln!("ERROR: {}が重複してます", arg);
                    return show_usage();
                }
                if let Some(name) = iter.next() {
                    flag.program_name = Some(name);
                } else {
                    eprintln!("ERROR: プログラム名が指定されてません");
                    return show_usage();
                }
            }
            FLAG_STATISTICS => {
                if flag_statistics {
                    eprintln!("ERROR: {}が重複してます", arg);
                    return show_usage();
                }
                flag_statistics = true;
            }
            _ => {
                eprintln!("ERROR: 不正なコマンドライン引数です ( {} )", arg);
                return show_usage();
            }
        }
    }

    let src_file = match src_file {
        Some(file) => file,
        None => {
            eprintln!("ERROR: ソースファイルが指定されてません");
            return show_usage();
        }
    };

    if src_file.to_ascii_lowercase().ends_with(".cas") {
        return process_casl2(src_file, flag, flag_statistics);
    }

    compile_basic(src_file, flag, flag_statistics)
}

// BASICソースファイルをCASL2にコンパイルする
fn compile_basic(src_file: String, flag: compiler::Flag, flag_statistics: bool) -> io::Result<i32> {
    let text = fs::read_to_string(src_file)?;

    let basic_src = match parser::parse(text.as_bytes())? {
        Ok(src) => src,
        Err(error) => {
            eprintln!("{:?}", error);
            let line_number = if error.line_number == 0 {
                0
            } else {
                error.line_number - 1
            };
            if let Some(line) = text.lines().nth(line_number) {
                eprintln!("{}", line);
                eprintln!("{ch:>pos$}", ch = "^", pos = error.position);
            }
            return Ok(4);
        }
    };

    let casl2_src_list = match compiler::compile_with_flag(&basic_src, flag) {
        Err(error) => {
            eprintln!("CompileError {{ {} }}", error);
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

        eprintln!("生成されたファイル: {}", file_name);

        if flag_statistics {
            let file_name = format!("{}.stat.txt", name);
            let statistics = stat::analyze(&casl2_src);
            fs::write(&file_name, statistics)?;
            eprintln!("生成されたファイル: {}", file_name);
        }
    }

    Ok(0)
}

// 入力ソースファイルがCASL2だった場合
fn process_casl2(src_file: String, flag: compiler::Flag, flag_statistics: bool) -> io::Result<i32> {
    let text = fs::read_to_string(&src_file)?;

    let casl2_src = match casl2::parse(text.as_str()) {
        Ok(src) => src,
        Err(error) => {
            eprintln!("{:?}", error);
            return Ok(6);
        }
    };

    if !casl2::utils::is_program(&casl2_src) {
        eprintln!("ERROR: 不明な理由によりこのソースコーを取り扱うことができませんでした");
        return Ok(7);
    }

    let mut casl2_src = if let Some(new_name) = flag.program_name.as_ref() {
        if !casl2::Label::from(new_name).is_valid() {
            eprintln!("ERROR: 禁止されている名前です  ( {} )", new_name);
            return Ok(8);
        }
        let old_name = casl2::utils::get_program_name(&casl2_src).expect("BUG");
        match casl2::utils::change_label(&casl2_src, old_name, new_name.as_str()) {
            Some(src) => src,
            None => {
                eprintln!(
                    "ERROR: ソースコード内で既に使用されている名前です ( {} )",
                    new_name
                );
                return Ok(9);
            }
        }
    } else {
        casl2_src
    };

    if flag.remove_comment {
        casl2_src = compiler::optimize::remove_comment(&casl2_src);
    }

    if flag.remove_unreferenced_label {
        casl2_src = compiler::optimize::remove_unreferenced_label(&casl2_src);
    }

    if flag.try_make_snippets {
        casl2_src = compiler::optimize::collect_duplicates(casl2_src);
    }

    if flag.remove_nop {
        casl2_src = compiler::optimize::remove_nop(&casl2_src);
    }

    let casl2_src_list = if flag.split_subroutines {
        compiler::utils::split_subroutines(casl2_src)
    } else {
        let program_name = casl2::utils::get_program_name(&casl2_src)
            .expect("BUG")
            .to_string();
        vec![(program_name, casl2_src)]
    };

    let backup = src_file.clone() + ".backup";
    let _copy_size = fs::copy(src_file, &backup)?;
    eprintln!("生成されたファイル: {}", backup);

    for (name, casl2_src) in casl2_src_list {
        let file_name = format!("{}.cas", name);

        let mut dst_file = fs::File::create(&file_name)?;

        for stmt in casl2_src.iter() {
            writeln!(&mut dst_file, "{}", stmt)?;
        }

        dst_file.flush()?;

        eprintln!("生成されたファイル: {}", file_name);

        if flag_statistics {
            let file_name = format!("{}.stat.txt", name);
            let statistics = stat::analyze(&casl2_src);
            fs::write(&file_name, statistics)?;
            eprintln!("生成されたファイル: {}", file_name);
        }
    }

    Ok(0)
}

// 使い方表示
fn show_usage() -> io::Result<i32> {
    let current_exe = env::current_exe()?
        .file_name()
        .and_then(|f| f.to_str().map(|n| n.to_string()))
        .unwrap_or_else(|| APP_NAME.to_string());

    eprintln!("
BASIC言語風の独自のプログラミング言語(?)のソースコードファイルからCASL2のソースコードファイルを生成します

USAGE:
    {current_exe} {flag_src} <FILE> [OPTIONS]

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
