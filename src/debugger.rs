use crate::casl2;
use crate::compiler;
use crate::parser;
use crate::stat;
use crate::Flags;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

pub fn run(src_file: String, mut flags: Flags) -> io::Result<i32> {
    let mut emu = Emulator::new();

    if src_file.to_ascii_lowercase().ends_with(".cas") {
        match emu.compile_casl2(&src_file, &flags, true) {
            Ok(0) => {}
            result => return result,
        }
    } else {
        match emu.compile_basic(&src_file, &flags, true) {
            Ok(0) => {}
            result => return result,
        }
    }

    flags.compiler.program_name = None;

    todo!();
}

struct Emulator {
    // COMET2のメモリ
    mem: [u16; 65536],

    // 挿入するプログラムの埋め込み開始位置
    compile_pos: usize,

    // 変数名とラベルの対応 (ソースファイル名, 変数ラベルマップ)
    variables: HashMap<String, compiler::Labels>,

    // ファイルごとのラベルとメモリ位置の対応 (ファイル名, (ラベル, メモリ位置))
    local_labels: HashMap<String, HashMap<String, usize>>,
}

impl Emulator {
    fn new() -> Self {
        Self {
            mem: [0; 65536],
            variables: HashMap::new(),
            compile_pos: 0x0400,
            local_labels: HashMap::new(),
        }
    }
}

impl Emulator {
    fn compile_basic(&mut self, src_file: &str, flags: &Flags, save: bool) -> io::Result<i32> {
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

        let casl2_src_list = match compiler::compile_for_debugger(&basic_src, &flags.compiler) {
            Err(error) => {
                eprintln!("CompileError {{ {} }}", error);
                return Ok(5);
            }
            Ok((labels, list)) => {
                if let Some((name, _)) = list.last() {
                    self.variables.insert(name.clone(), labels.clone());
                    self.variables.insert(src_file.into(), labels);
                } else {
                    unreachable!("BUG");
                };
                list
            }
        };

        let mut path = flags.create_dst_dir()?;

        // EOF,ALLOC
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
                    if save {
                        save_casl2_src(&mut path, "EOF", &src, flags.statistics)?;
                    }
                }
                parser::CompileOption::Allocator {
                    enabled: true,
                    common: true,
                    size,
                } => {
                    let src = compiler::subroutine::get_util_allocator_code_for_common(*size);
                    if save {
                        save_casl2_src(&mut path, "ALLOC", &src, flags.statistics)?;
                    }
                }
                _ => {}
            }
        }

        for (name, casl2_src) in casl2_src_list {
            if save {
                save_casl2_src(&mut path, &name, &casl2_src, flags.statistics)?;
            }

            self.compile(src_file, name, casl2_src);
        }

        Ok(0)
    }
}

fn save_casl2_src(
    path: &mut PathBuf,
    name: &str,
    casl2_src: &[casl2::Statement],
    statistics: bool,
) -> io::Result<()> {
    let file_name = format!("{}.cas", name);
    path.push(file_name);
    {
        let mut dst_file = fs::File::create(&path)?;

        for stmt in casl2_src.iter() {
            writeln!(&mut dst_file, "{}", stmt)?;
        }

        dst_file.flush()?;
        eprintln!("生成されたファイル: {}", path.display());
    }
    path.pop();

    if statistics {
        let file_name = format!("{}.stat.txt", name);
        path.push(file_name);
        let statistics = stat::analyze(casl2_src);
        fs::write(&path, statistics)?;
        eprintln!("生成されたファイル: {}", path.display());
        path.pop();
    }

    Ok(())
}

impl Emulator {
    fn compile_casl2(&mut self, src_file: &str, flags: &Flags, save: bool) -> io::Result<i32> {
        let text = fs::read_to_string(src_file)?;

        let _ = (text, flags, save);

        todo!()
    }
}

impl Emulator {
    fn compile(&mut self, src_file: &str, name: String, src: Vec<casl2::Statement>) {
        let _ = (src_file, name, src);

        todo!()
    }
}
