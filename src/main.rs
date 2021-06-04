use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

mod casl2;
mod compiler;
mod debugger;
mod jis_x_201;
mod parser;
mod stat;
mod tokenizer;

const APP_NAME: &str = "b2c2";

const FLAG_SOURCE_FILE: &str = "-src";
const FLAG_DESTINATION_DIRECTORY: &str = "-dst";
const FLAG_REMOVE_COMMENT: &str = "-remove-comment";
const FLAG_REMOVE_NOP: &str = "-remove-nop";
const FLAG_REMOVE_UNREFERENCED_LABEL: &str = "-remove-unreferenced-label";
const FLAG_SPLIT_SUBROUTINES: &str = "-split-subroutines";
const FLAG_TRY_MAKE_SNIPPETS: &str = "-try-make-snippets";
const FLAG_PROGRAM_NAME: &str = "-program-name";
const FLAG_STATISTICS: &str = "-statistics";
const FLAG_RUN_DEBUGGER: &str = "-run";

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

#[derive(Clone, Default)]
pub struct Flags {
    src_file: Option<String>,
    pub compiler: compiler::Flag,
    pub statistics: bool,
    pub dst_dir: Option<String>,
    run_debugger: Option<DebugTarget>,
}

#[derive(Clone, Copy)]
enum DebugTarget {
    Casl2,
    Basic,
    NonStep,
}

// コマンドライン引数を解釈して指定の処理を実行する
fn run_app() -> io::Result<i32> {
    let mut flags = Flags::default();
    match flags.parse(env::args().skip(1)) {
        Ok(0) => {}
        result => return result,
    }

    let src_file = match flags.src_file.take() {
        Some(file) => file,
        None => {
            eprintln!("ERROR: ソースファイルが指定されてません");
            return show_usage();
        }
    };

    match flags.run_debugger {
        Some(DebugTarget::Casl2) => return debugger::run_casl2(src_file, flags),
        Some(DebugTarget::Basic) => return debugger::run_basic(src_file, flags),
        Some(DebugTarget::NonStep) => return debugger::run_nonstep(src_file, flags),
        _ => {}
    }

    if src_file.to_ascii_lowercase().ends_with(".cas") {
        return process_casl2(src_file, flags);
    }

    compile_basic(src_file, flags)
}

// BASICソースファイルをCASL2にコンパイルする
fn compile_basic(src_file: String, flags: Flags) -> io::Result<i32> {
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

    let casl2_src_list = match compiler::compile_with_flag(&basic_src, flags.compiler.clone()) {
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

    let path = flags.create_dst_dir()?;

    save_casl2_src_list(path, casl2_src_list, flags.statistics)?;

    Ok(0)
}

// 入力ソースファイルがCASL2だった場合
fn process_casl2(src_file: String, flags: Flags) -> io::Result<i32> {
    let text = fs::read_to_string(&src_file)?;

    let casl2_src = match casl2::parse(text.as_str()) {
        Ok(src) => src,
        Err(error) => {
            eprintln!("{:?}", error);
            return Ok(6);
        }
    };

    if let Some(msg) = casl2::utils::find_syntax_error(&casl2_src) {
        eprintln!("SyntaxError{{ {} }}", msg);
        return Ok(7);
    }

    let casl2_src = if let Some(new_name) = flags.compiler.program_name.as_ref() {
        if !casl2::Label::from(new_name).is_valid() {
            eprintln!("ERROR: 禁止されている名前です  ( {} )", new_name);
            return Ok(8);
        }
        let old_name = casl2::utils::get_program_name(&casl2_src).unwrap();
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

    let casl2_src_list = flags.compiler.apply(casl2_src);

    let mut path = flags.create_dst_dir()?;

    let backup = src_file.clone() + ".backup";
    let backup = Path::new(&backup);
    if backup.parent() == Some(&path) {
        if let Some(file) = backup.file_name() {
            path.push(file);
            let _copy_size = fs::copy(src_file, &path)?;
            if path.exists() && path.is_file() {
                eprintln!("生成されたファイル　(上書): {}", path.display());
            } else {
                eprintln!("生成されたファイル: {}", path.display());
            }
            path.pop();
        }
    }

    save_casl2_src_list(path, casl2_src_list, flags.statistics)?;

    Ok(0)
}

impl Flags {
    // 出力先ディレクトリの生成
    pub fn create_dst_dir(&self) -> io::Result<PathBuf> {
        if let Some(dir) = self.dst_dir.as_ref() {
            let path = Path::new(dir);
            if !path.exists() || !path.is_dir() {
                fs::create_dir_all(dir)?;
            }
            Ok(path.to_path_buf())
        } else {
            Ok(PathBuf::new())
        }
    }
}

// 生成したCASL2ソースコードの保存
fn save_casl2_src_list(
    mut path: PathBuf,
    casl2_src_list: Vec<(String, Vec<casl2::Statement>)>,
    statistics: bool,
) -> io::Result<()> {
    for (name, casl2_src) in casl2_src_list {
        let file_name = format!("{}.cas", name);
        path.push(file_name);
        {
            let mut dst_file = fs::File::create(&path)?;

            for stmt in casl2_src.iter() {
                writeln!(&mut dst_file, "{}", stmt)?;
            }

            dst_file.flush()?;
        }
        if path.exists() && path.is_file() {
            eprintln!("生成されたファイル(上書): {}", path.display());
        } else {
            eprintln!("生成されたファイル: {}", path.display());
        }
        path.pop();

        if statistics {
            let file_name = format!("{}.stat.txt", name);
            path.push(file_name);
            let statistics = stat::analyze(&casl2_src);
            fs::write(&path, statistics)?;
            if path.exists() && path.is_file() {
                eprintln!("生成されたファイル(上書): {}", path.display());
            } else {
                eprintln!("生成されたファイル: {}", path.display());
            }
            path.pop();
        }
    }

    Ok(())
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
    {flag_destination_directory} <DIR>
                        生成されるファイルの出力先ディレクトリを指定する

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

    {flag_run_debugger} <TARGET>
                        生成されるCASL2プログラムに対してテスト実行もする
                        TARGETにはCASL2/BASIC/NONSTEPのいずれかを指定する
                          CASL2   .. CASL2のコード単位でステップ実行する
                          BASIC   .. BASICのコード単位でステップ実行する (生成されるCASL2プログラムにステップ実行用のコードが挿入されます)
                          NONSTEP .. プログラムの実行だけをします (ステップ数の制限がありません、無限ループのバグがある場合は止まりません)
",
        flag_src = FLAG_SOURCE_FILE,
        flag_destination_directory = FLAG_DESTINATION_DIRECTORY,
        flag_remove_nop = FLAG_REMOVE_NOP,
        flag_remove_unreferenced_label = FLAG_REMOVE_UNREFERENCED_LABEL,
        flag_remove_comment = FLAG_REMOVE_COMMENT,
        flag_try_make_snippets = FLAG_TRY_MAKE_SNIPPETS,
        flag_split_subroutines = FLAG_SPLIT_SUBROUTINES,
        flag_program_name = FLAG_PROGRAM_NAME,
        flag_statistics = FLAG_STATISTICS,
        flag_run_debugger = FLAG_RUN_DEBUGGER,
        current_exe = current_exe,
    );

    Ok(2)
}

// コマンドライン引数を取得
impl Flags {
    fn parse<T>(&mut self, mut iter: T) -> io::Result<i32>
    where
        T: Iterator<Item = String>,
    {
        while let Some(arg) = iter.next() {
            match arg.as_str() {
                FLAG_SOURCE_FILE => {
                    if self.src_file.is_some() {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    if let Some(file) = iter.next() {
                        let path = Path::new(&file);
                        if !path.exists() || !path.is_file() {
                            eprintln!("ERROR: ファイルが見つかりません ( {} )", file);
                            return Ok(3);
                        }
                        if path.metadata()?.len() > 1_000_000 {
                            eprintln!("ERROR: ファイルサイズが大きすぎます ( {} )", file);
                            return Ok(3);
                        }
                        self.src_file = Some(file);
                    } else {
                        eprintln!("ERROR:　ソースファイルが指定されてません");
                        return show_usage();
                    }
                }
                FLAG_DESTINATION_DIRECTORY => {
                    if self.dst_dir.is_some() {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    if let Some(dir) = iter.next() {
                        self.dst_dir = Some(dir);
                    } else {
                        eprintln!("ERROR:　出力先が指定されてません");
                        return show_usage();
                    }
                }
                FLAG_REMOVE_COMMENT => {
                    if self.compiler.remove_comment {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    self.compiler.remove_comment = true;
                }
                FLAG_REMOVE_NOP => {
                    if self.compiler.remove_nop {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    self.compiler.remove_nop = true;
                }
                FLAG_REMOVE_UNREFERENCED_LABEL => {
                    if self.compiler.remove_unreferenced_label {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    self.compiler.remove_unreferenced_label = true;
                }
                FLAG_SPLIT_SUBROUTINES => {
                    if self.compiler.split_subroutines {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    self.compiler.split_subroutines = true;
                }
                FLAG_TRY_MAKE_SNIPPETS => {
                    if self.compiler.try_make_snippets {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    self.compiler.try_make_snippets = true;
                }
                FLAG_PROGRAM_NAME => {
                    if self.compiler.program_name.is_some() {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    if let Some(name) = iter.next() {
                        self.compiler.program_name = Some(name);
                    } else {
                        eprintln!("ERROR: プログラム名が指定されてません");
                        return show_usage();
                    }
                }
                FLAG_STATISTICS => {
                    if self.statistics {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    self.statistics = true;
                }
                FLAG_RUN_DEBUGGER => {
                    if self.run_debugger.is_some() {
                        eprintln!("ERROR: {}が重複してます", arg);
                        return show_usage();
                    }
                    if let Some(target) = iter.next() {
                        if "CASL2".eq_ignore_ascii_case(&target) {
                            self.run_debugger = Some(DebugTarget::Casl2);
                        } else if "BASIC".eq_ignore_ascii_case(&target) {
                            self.compiler.for_debug_basic = true;
                            self.run_debugger = Some(DebugTarget::Basic);
                        } else if "NonStep".eq_ignore_ascii_case(&target) {
                            self.run_debugger = Some(DebugTarget::NonStep);
                        } else {
                            eprintln!("ERROR: ターゲットが不正です");
                            return show_usage();
                        }
                    } else {
                        eprintln!("ERROR: ターゲットが指定されてません");
                        return show_usage();
                    }
                }
                _ => {
                    eprintln!("ERROR: 不正なコマンドライン引数です ( {} )", arg);
                    return show_usage();
                }
            }
        }

        Ok(0)
    }
}
