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

// プログラムリスト (ファイル名, ラベル, ソースコード(コードメモリ位置, コード))
type ProgramList = Vec<(String, String, Vec<(usize, casl2::Statement)>)>;

struct Emulator {
    // COMET2のメモリ
    mem: [u16; 65536],

    // 挿入するプログラムの埋め込み開始位置
    compile_pos: usize,

    // 変数名とラベルの対応 (ソースファイル名, 変数ラベルマップ)
    variables: HashMap<String, compiler::Labels>,

    // ファイルごとのラベルとメモリ位置の対応 (ファイル名, (ラベル, メモリ位置))
    local_labels: HashMap<String, HashMap<String, usize>>,

    // プログラムのエントリラベルとメモリ位置の対応 (ラベル, メモリ位置)
    program_labels: HashMap<String, usize>,

    // プログラムリスト (ファイル名, ラベル, ソースコード(コードメモリ位置, コード))
    program_list: ProgramList,
}

impl Emulator {
    fn new() -> Self {
        Self {
            mem: [0; 65536],
            variables: HashMap::new(),
            compile_pos: 0x0400,
            local_labels: HashMap::new(),
            program_labels: HashMap::new(),
            program_list: ProgramList::new(),
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
                    self.compile("EOF.cas", "EOF".into(), src);
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
                    self.compile("ALLOC.cas", "ALLOC".into(), src);
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
        let _ = (src_file, name);

        let mut code = Vec::with_capacity(src.len());
        let mut labels = HashMap::new();

        for stmt in src {
            let pos = self.compile_pos;
            if let casl2::Statement::Code { label, command, .. } = &stmt {
                self.compile_pos += command.len();
                if let Some(label) = label {
                    if !matches!(command, casl2::Command::Start { .. }) {
                        labels.insert(label.as_str().to_string(), pos);
                    }
                }
                self.compile_command(pos, command);
            }
            code.push((pos, stmt));
        }

        todo!()
    }

    fn compile_command(&mut self, pos: usize, command: &casl2::Command) {
        match command {
            casl2::Command::Start { .. }
            | casl2::Command::End
            | casl2::Command::Ds { .. }
            | casl2::Command::Dc { .. } => {}
            casl2::Command::In { .. } => {
                let src = casl2::parse(
                    r#" PUSH 0,GR1
                        PUSH 0,GR2
                        LAD GR1,0
                        LAD GR2,0
                        SVC 0
                        POP GR2
                        POP GR1
                    "#,
                )
                .unwrap();
                let mut pos = pos;
                for stmt in src.iter() {
                    if let casl2::Statement::Code { command, .. } = stmt {
                        self.mem[pos] = command.first_byte();
                        pos += command.len();
                    }
                }
            }
            casl2::Command::Out { .. } => {
                let src = casl2::parse(
                    r#" PUSH 0,GR1
                        PUSH 0,GR2
                        LAD GR1,0
                        LAD GR2,0
                        SVC 1
                        POP GR2
                        POP GR1
                    "#,
                )
                .unwrap();
                let mut pos = pos;
                for stmt in src.iter() {
                    if let casl2::Statement::Code { command, .. } = stmt {
                        self.mem[pos] = command.first_byte();
                        pos += command.len();
                    }
                }
            }
            casl2::Command::Rpush => {
                use casl2::IndexRegister::*;
                let mut pos = pos;
                for &x in &[Gr1, Gr2, Gr3, Gr4, Gr5, Gr6, Gr7] {
                    let cmd = casl2::Command::P {
                        code: casl2::P::Push,
                        adr: casl2::Adr::Dec(0),
                        x: Some(x),
                    };
                    self.mem[pos] = cmd.first_byte();
                    self.mem[pos + 1] = 0;
                    pos += 2;
                }
            }
            casl2::Command::Rpop => {
                use casl2::Register::*;
                let mut pos = pos;
                for &r in &[Gr7, Gr6, Gr5, Gr4, Gr3, Gr2, Gr1] {
                    let cmd = casl2::Command::Pop { r };
                    self.mem[pos] = cmd.first_byte();
                    pos += 1;
                }
            }
            casl2::Command::R { .. }
            | casl2::Command::Pop { .. }
            | casl2::Command::Ret
            | casl2::Command::Nop => {
                self.mem[pos] = command.first_byte();
            }
            casl2::Command::A { .. } | casl2::Command::P { .. } => {
                self.mem[pos] = command.first_byte();
            }
        }
    }
}

impl casl2::Command {
    // このやり方はちょっと厳しい・・・
    fn op_code(&self) -> u16 {
        match self {
            casl2::Command::Start { .. }
            | casl2::Command::End
            | casl2::Command::Ds { .. }
            | casl2::Command::Dc { .. }
            | casl2::Command::In { .. }
            | casl2::Command::Out { .. }
            | casl2::Command::Rpush
            | casl2::Command::Rpop => unreachable!("BUG"),

            casl2::Command::R { code, .. } => {
                use casl2::R::*;
                match code {
                    Ld => 0x14,
                    Adda => 0x24,
                    Addl => 0x26,
                    Suba => 0x25,
                    Subl => 0x27,
                    And => 0x34,
                    Or => 0x35,
                    Xor => 0x36,
                    Cpa => 0x44,
                    Cpl => 0x45,
                }
            }
            casl2::Command::A { code, .. } => {
                use casl2::A::*;
                match code {
                    Ld => 0x10,
                    St => 0x11,
                    Lad => 0x12,
                    Adda => 0x20,
                    Addl => 0x22,
                    Suba => 0x21,
                    Subl => 0x23,
                    And => 0x30,
                    Or => 0x31,
                    Xor => 0x32,
                    Cpa => 0x40,
                    Cpl => 0x41,
                    Sla => 0x50,
                    Sra => 0x51,
                    Sll => 0x52,
                    Srl => 0x53,
                }
            }
            casl2::Command::P { code, .. } => {
                use casl2::P::*;
                match code {
                    Jpl => 0x65,
                    Jmi => 0x61,
                    Jnz => 0x62,
                    Jze => 0x63,
                    Jov => 0x66,
                    Jump => 0x64,
                    Push => 0x70,
                    Call => 0x80,
                    Svc => 0xF0,
                }
            }
            casl2::Command::Pop { .. } => 0x71,
            casl2::Command::Ret => 0x81,
            casl2::Command::Nop => 0x00,
        }
    }

    fn first_byte(&self) -> u16 {
        match self {
            casl2::Command::Start { .. }
            | casl2::Command::End
            | casl2::Command::Ds { .. }
            | casl2::Command::Dc { .. }
            | casl2::Command::In { .. }
            | casl2::Command::Out { .. }
            | casl2::Command::Rpush
            | casl2::Command::Rpop => unreachable!("BUG"),

            casl2::Command::R { r1, r2, .. } => {
                (self.op_code() << 8) | ((*r1 as u16) << 4) | (*r2 as u16)
            }
            casl2::Command::A { r, x, .. } => {
                let x = x.map(|x| x as u16).unwrap_or(0);
                (self.op_code() << 8) | ((*r as u16) << 4) | x
            }
            casl2::Command::P { x, .. } => {
                let x = x.map(|x| x as u16).unwrap_or(0);
                (self.op_code() << 8) | x
            }
            casl2::Command::Pop { r } => (self.op_code() << 8) | ((*r as u16) << 4),
            casl2::Command::Ret | casl2::Command::Nop => self.op_code() << 8,
        }
    }

    fn len(&self) -> usize {
        match self {
            casl2::Command::Start { .. } | casl2::Command::End => 0,
            casl2::Command::Ds { size } => *size as usize,
            casl2::Command::Dc { constants } => constants
                .iter()
                .map(|c| {
                    use casl2::Constant::*;
                    match c {
                        Dec(_) | Hex(_) | Label(_) => 1,
                        Str(s) => s.chars().count(),
                    }
                })
                .sum(),
            casl2::Command::In { .. } | casl2::Command::Out { .. } => {
                // 2  2 PUSH 0,GR1
                // 2  4 PUSH 0,GR2
                // 2  6 LAD GR1,POS
                // 2  8 LAD GR2,LEN
                // 2 10 SVC 0 (or 1)
                // 1 11 POP GR2
                // 1 12 POP GR1
                12
            }
            casl2::Command::Rpush => 14,
            casl2::Command::Rpop => 7,
            casl2::Command::R { .. } => 1,
            casl2::Command::A { .. } => 2,
            casl2::Command::P { .. } => 2,
            casl2::Command::Pop { .. } => 1,
            casl2::Command::Ret => 1,
            casl2::Command::Nop => 1,
        }
    }
}
