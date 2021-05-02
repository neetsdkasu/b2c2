use crate::casl2;
use crate::compiler;
use crate::jis_x_201;
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

    if emu.start_point.is_none() {
        eprintln!("入力ファイルが正しくありません");
        return Ok(101);
    }

    flags.compiler.program_name = None;

    if !emu.unknown_labels.is_empty() {
        // 未解決ラベルを解決する処理を入れる
        todo!();
    }

    Ok(0)
}

// プログラムリスト (ファイル名, ラベル, ソースコード(コードメモリ位置, コード))
type Program = (String, String, Vec<(usize, casl2::Statement)>);

// コールスタック用に確保する最大サイズ　(1呼び出しにつき、RPUSHで7語、MEMで1語、合計8語として、再帰呼び出しの深さ200とすると2000語分程度あればよいか？)
const CALLSTACK_MAX_SIZE: usize = 0x0800; // 2048
                                          // プログラムコードを埋め込みを開始する位置 (0はダメ)
const BEGIN_OF_PROGRAM: usize = 0x0400; // 1024
                                        // COMET2の容量(語数)
const MEMORY_SIZE: usize = 0x10000;
// スタックポインタの始点
// 仕様どおりにするなら最初のプログラム起動時はOSからCALLが呼ばれるはずで
// SP   <- (SP) - 1  (SP = 0xFFFD)
// (SP) <- (PR)      (0xFFFDの位置に初期PRの値が放り込まれる)
// PR   <- address
// RET命令で取り出した値が初期PRの値と同じならプログラム終了と同等になるが
// RET命令でSPが0xFFFDでは無い時に初期PRの値と同じならスタックが壊れてる(笑)
// またSPが0xFFFDのRET命令で初期PRと異なる値を得たならプログラムが暴走する(笑)
const CALLSTACK_START_POSITION: usize = 0xFFFF;
// プログラムレジスタ(PR)の初期値
// OSがCALLでプログラムを呼び出した位置に相当する
// RET命令でこの値が取り出されたら、プログラム終了だがSP値が適切ではない場合はOSが暴走する(笑)
// 今回OSは作らないため、OSのコード実体はないため、RETでこの値が来たらエミュレータ終了である
// BEGIN_OF_PROGRAMより手前のメモリ帯にOSのコードがあると想定してる
const INITIAL_PROGRAM_REGISTER: usize = 0x0234; // 564

struct Emulator {
    // COMET2のメモリ
    mem: [u16; MEMORY_SIZE],

    // レジスタ GR0～GR7
    general_registers: [u16; 8],

    // スタックポインタ SP
    // (CASL2からのアクセス手段はないため利便性からu16ではなくusizeで管理)
    // (OSはSPを直接操作できるような事が仕様に書かれてるが具体的手段自体は実装依存ぽい)
    // CALLやPUSHでは先にSPが更新される、つまりSPはスタックの値の存在する先頭アドレス、か
    stack_pointer: usize,

    // プログラムレジスタ PR
    // (CALL命令でスタックに積む以外でu16である必要性はないため利便性からusizeで管理)
    // "次に"実行すべき命令のアドレスらしい
    // つまり命令をCPUに伝える前にPRは更新する必要があるぽい？(CALL/RETの仕様を考えると)
    //  CALLが  (SP) <- (PR), PR <- address
    //  RETが   PR <- ( (SP) )
    program_register: usize,

    // フラグレジスタ FR (CASL2からの直接的なアクセス手段はないため、boolで管理)
    //  OF overflow_flag 加減算ではオーバーフロー、シフト演算では溢れビット
    //  SF sign_flag 算術か論理に関わらず符号ビットが立ってるか否からしい？
    //  ZF zero_flag 値が0ならフラグ立つ
    overflow_flag: bool,
    sign_flag: bool,
    zero_flag: bool,

    // プログラムの開始点のエントリラベル
    // (START命令に引数があるとBEGIN_OF_PROGRAMにはならないこともあるため)
    start_point: Option<String>,

    // 挿入するプログラムの埋め込み開始位置
    compile_pos: usize,

    // 変数名とラベルの対応 (ソースファイル名, (プログラムエントリラベル, 変数ラベルマップ))
    variables: HashMap<String, (String, compiler::LabelSet)>,

    // プログラムごとのラベルとメモリ位置の対応 (プログラムエントリラベル, (ラベル, メモリ位置))
    local_labels: HashMap<String, HashMap<String, usize>>,

    // プログラムのエントリラベルとメモリ位置の対応 (ラベル, メモリ位置)
    program_labels: HashMap<String, usize>,

    // プログラムリスト (ファイル名, ラベル, ソースコード(コードメモリ位置, コード))
    program_list: Vec<Program>,

    // 未解決ラベル(おそらく外部定義)
    unknown_labels: HashMap<String, Vec<usize>>,
}

impl Emulator {
    fn new() -> Self {
        Self {
            mem: [0; MEMORY_SIZE],
            general_registers: [0; 8],
            stack_pointer: CALLSTACK_START_POSITION,
            program_register: INITIAL_PROGRAM_REGISTER,
            overflow_flag: false,
            sign_flag: false,
            zero_flag: false,

            start_point: None,
            variables: HashMap::new(),
            compile_pos: BEGIN_OF_PROGRAM,
            local_labels: HashMap::new(),
            program_labels: HashMap::new(),
            program_list: Vec::new(),
            unknown_labels: HashMap::new(),
        }
    }

    fn enough_remain(&self, len: usize) -> bool {
        len < self.mem.len() - self.compile_pos - CALLSTACK_MAX_SIZE
    }

    fn has_asccess_permission(&self, pos: usize) -> bool {
        (BEGIN_OF_PROGRAM..self.compile_pos).contains(&pos)
    }
}

#[derive(Clone, Debug)]
enum RuntimeError {
    NormalTermination {
        position: usize,
    },
    AbnormalTermination {
        position: usize,
        op_code: String,
    },
    ExecutePermissionDenied {
        position: usize,
    },
    AccessPermissionDenied {
        position: usize,
        op_code: String,
        address: usize,
    },
    InvalidOpCode {
        position: usize,
        op_code: String,
    },
    StackOverflow {
        position: usize,
        op_code: String,
        stack_pointer: usize,
    },
}

impl Emulator {
    fn step_through_code(&mut self) -> Result<(), RuntimeError> {
        let pr = self.program_register;
        if !self.has_asccess_permission(pr) {
            return Err(RuntimeError::ExecutePermissionDenied { position: pr });
        }
        self.program_register += 1;
        let op_code = self.mem[pr];
        let r1 = ((op_code >> 4) & 0xF) as usize;
        let r2 = (op_code & 0xF) as usize;
        let op_code_size = get_op_code_size(op_code);

        if op_code_size == 0 {
            return Err(RuntimeError::InvalidOpCode {
                position: pr,
                op_code: get_op_code_form(op_code, 0),
            });
        }

        if op_code_size == 1 {
            if !(0..8).contains(&r1) || !(0..8).contains(&r2) {
                return Err(RuntimeError::InvalidOpCode {
                    position: pr,
                    op_code: get_op_code_form(op_code, 0),
                });
            }
            match (op_code >> 8) & 0xFF {
                0x14 => {
                    // LD r1,r2  (OF=0,SF,ZF)
                    let r2_value = self.general_registers[r2];
                    self.general_registers[r1] = r2_value;
                    self.overflow_flag = false;
                    self.sign_flag = (r2_value as i16) < 0;
                    self.zero_flag = r2_value == 0;
                }
                0x24 => {
                    // ADDA r1,r2 (OF,SF,ZF)
                    let r1_value = self.general_registers[r1] as i16;
                    let r2_value = self.general_registers[r2] as i16;
                    let (value, overflow) = r1_value.overflowing_add(r2_value);
                    self.general_registers[r1] = value as u16;
                    self.overflow_flag = overflow;
                    self.sign_flag = value < 0;
                    self.zero_flag = value == 0;
                }
                0x26 => {
                    // ADDL r1,r2 (OF,SF,ZF)
                    let r1_value = self.general_registers[r1];
                    let r2_value = self.general_registers[r2];
                    let (value, overflow) = r1_value.overflowing_add(r2_value);
                    self.general_registers[r1] = value;
                    self.overflow_flag = overflow;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x25 => {
                    // SUBA r1,r2 (OF,SF,ZF)
                    let r1_value = self.general_registers[r1] as i16;
                    let r2_value = self.general_registers[r2] as i16;
                    let (value, overflow) = r1_value.overflowing_sub(r2_value);
                    self.general_registers[r1] = value as u16;
                    self.overflow_flag = overflow;
                    self.sign_flag = value < 0;
                    self.zero_flag = value == 0;
                }
                0x27 => {
                    // SUBL r1,r2 (OF,SF,ZF)
                    let r1_value = self.general_registers[r1];
                    let r2_value = self.general_registers[r2];
                    let (value, overflow) = r1_value.overflowing_sub(r2_value);
                    self.general_registers[r1] = value;
                    self.overflow_flag = overflow;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x34 => {
                    // AND r1,r2 (OF=0,SF,ZF)
                    let r2_value = self.general_registers[r2];
                    let value = self.general_registers[r1] & r2_value;
                    self.general_registers[r1] = value;
                    self.overflow_flag = false;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x35 => {
                    // OR r1,r2 (OF=0,SF,ZF)
                    let r2_value = self.general_registers[r2];
                    let value = self.general_registers[r1] | r2_value;
                    self.general_registers[r1] = value;
                    self.overflow_flag = false;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x36 => {
                    // XOR r1,r2 (OF=0,SF,ZF)
                    let r2_value = self.general_registers[r2];
                    let value = self.general_registers[r1] ^ r2_value;
                    self.general_registers[r1] = value;
                    self.overflow_flag = false;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x44 => {
                    // CPA r1,r2 (OF=0,SF,ZF)
                    let r1_value = self.general_registers[r1] as i16;
                    let r2_value = self.general_registers[r2] as i16;
                    self.overflow_flag = false;
                    self.sign_flag = r1_value < r2_value;
                    self.zero_flag = r1_value == r2_value;
                }
                0x45 => {
                    // CPL r1,r2 (OF=0,SF,ZF)
                    let r1_value = self.general_registers[r1];
                    let r2_value = self.general_registers[r2];
                    self.overflow_flag = false;
                    self.sign_flag = r1_value < r2_value;
                    self.zero_flag = r1_value == r2_value;
                }
                0x71 if r2 == 0 => {
                    // POP r
                    if self.stack_pointer >= CALLSTACK_START_POSITION {
                        return Err(RuntimeError::StackOverflow {
                            position: pr,
                            op_code: get_op_code_form(op_code, 0),
                            stack_pointer: self.stack_pointer,
                        });
                    }
                    let value = self.mem[self.stack_pointer];
                    self.stack_pointer += 1;
                    self.general_registers[r1] = value;
                }
                0x81 if r1 == 0 && r2 == 0 => {
                    // RET
                    if self.stack_pointer >= CALLSTACK_START_POSITION {
                        return Err(RuntimeError::StackOverflow {
                            position: pr,
                            op_code: get_op_code_form(op_code, 0),
                            stack_pointer: self.stack_pointer,
                        });
                    }
                    let adr = self.mem[self.stack_pointer] as usize;
                    self.stack_pointer += 1;
                    if adr == INITIAL_PROGRAM_REGISTER {
                        if self.stack_pointer == CALLSTACK_START_POSITION {
                            return Err(RuntimeError::NormalTermination { position: pr });
                        } else {
                            return Err(RuntimeError::AbnormalTermination {
                                position: pr,
                                op_code: get_op_code_form(op_code, 0),
                            });
                        }
                    }
                    if !self.has_asccess_permission(adr) {
                        return Err(RuntimeError::AccessPermissionDenied {
                            position: pr,
                            op_code: get_op_code_form(op_code, 0),
                            address: adr,
                        });
                    }
                    self.program_register = adr;
                }
                0x00 if r1 == 0 && r2 == 0 => {
                    // NOP
                }
                _ => {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, 0),
                    })
                }
            }
            return Ok(());
        }

        assert_eq!(op_code_size, 2);

        let adr = self.mem[pr + 1];
        self.program_register += 1;

        if !(0..8).contains(&r1) || !(0..8).contains(&r2) {
            return Err(RuntimeError::InvalidOpCode {
                position: pr,
                op_code: get_op_code_form(op_code, adr),
            });
        }

        match (op_code >> 8) & 0xFF {
            0x12 => {
                // LAD r,adr,x
                let value = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                self.general_registers[r1] = value;
                return Ok(());
            }
            0x70 => {
                // PUSH adr,x
                if r1 != 0 {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, adr),
                    });
                }
                let value = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let sp = self.stack_pointer - 1;
                if sp < self.mem.len() - CALLSTACK_MAX_SIZE {
                    return Err(RuntimeError::StackOverflow {
                        position: pr,
                        op_code: get_op_code_form(op_code, adr),
                        stack_pointer: self.stack_pointer,
                    });
                }
                self.stack_pointer = sp;
                self.mem[sp] = value;
                return Ok(());
            }
            0xF0 => {
                // SVC adr,x
                if r1 != 0 {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, adr),
                    });
                }
                let value = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                if value == 1 {
                    // IN
                    todo!("IN");
                } else if value == 2 {
                    // OUT
                    todo!("OUT");
                }
                return Ok(());
            }
            _ => {}
        }

        let access = adr as usize
            + if r2 == 0 {
                0
            } else {
                self.general_registers[r2] as usize
            };

        if !self.has_asccess_permission(access) {
            return Err(RuntimeError::AccessPermissionDenied {
                position: pr,
                op_code: get_op_code_form(op_code, adr),
                address: access,
            });
        }

        match (op_code >> 8) & 0xFF {
            0x10 => {
                // LD r,adr,x (OF=0,SF,ZF)
            }
            0x11 => {
                // ST r,adr,x
            }
            0x20 => {
                // ADDA r,adr,x
            }
            0x22 => {
                // ADDL r,adr,x
            }
            0x21 => {
                // SUBA r,adr,x
            }
            0x23 => {
                // SUBL r,adr,x
            }
            0x30 => {
                // AND r,adr,x
            }
            0x31 => {
                // OR r,adr,x
            }
            0x32 => {
                // XOR r,adr,x
            }
            0x40 => {
                // CPA r,adr,x
            }
            0x41 => {
                // CPL r,adr,x
            }
            0x50 => {
                // SLA r,adr,x
            }
            0x51 => {
                // SRA r,adr,x
            }
            0x52 => {
                // SLL r,adr,x
            }
            0x53 => {
                // SRL r,adr,x
            }
            _ => {}
        }

        if r1 != 0 {
            return Err(RuntimeError::InvalidOpCode {
                position: pr,
                op_code: get_op_code_form(op_code, adr),
            });
        }

        match (op_code >> 8) & 0xFF {
            0x65 => {
                // JPL adr,x
            }
            0x61 => {
                // JMI adr,x
            }
            0x62 => {
                // JNZ adr,x
            }
            0x63 => {
                // JZE adr,x
            }
            0x66 => {
                // JOV adr,x
            }
            0x64 => {
                // JUMP adr,x
            }
            0x80 => {
                // CALL adr,x
            }
            _ => {
                // UNKNOWN CODE
                return Err(RuntimeError::InvalidOpCode {
                    position: pr,
                    op_code: get_op_code_form(op_code, adr),
                });
            }
        }
        Ok(())
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
                    if self.start_point.is_none() {
                        self.start_point = Some(name.clone());
                    }
                    self.variables
                        .insert(src_file.into(), (name.clone(), labels));
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
                    if !self.program_labels.contains_key(&"EOF".to_string()) {
                        if let Err(msg) = self.compile("EOF.cas", "EOF".into(), src) {
                            eprintln!("CompileError{{ {} }}", msg);
                            return Ok(100);
                        }
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
                    if !self.program_labels.contains_key(&"ALLOC".to_string()) {
                        if let Err(msg) = self.compile("ALLOC.cas", "ALLOC".into(), src) {
                            eprintln!("CompileError{{ {} }}", msg);
                            return Ok(100);
                        }
                    }
                }
                _ => {}
            }
        }

        for (name, casl2_src) in casl2_src_list {
            if save {
                save_casl2_src(&mut path, &name, &casl2_src, flags.statistics)?;
            }

            if self.program_labels.contains_key(&name) {
                continue;
            }

            if let Err(msg) = self.compile(src_file, name, casl2_src) {
                eprintln!("CompileError{{ {} }}", msg);
                return Ok(100);
            }
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
    fn compile(
        &mut self,
        src_file: &str,
        name: String,
        src: Vec<casl2::Statement>,
    ) -> Result<(), String> {
        let mut code = Vec::with_capacity(src.len());
        let mut labels = HashMap::new();
        let mut literals = HashMap::new();

        for stmt in src {
            let pos = self.compile_pos;
            if let casl2::Statement::Code { label, command, .. } = &stmt {
                let len = command.len();
                if !self.enough_remain(len) {
                    return Err("メモリ不足でプログラムをロードできませんでした".into());
                }
                self.compile_pos += len;
                if let Some(label) = label {
                    if !matches!(command, casl2::Command::Start { .. }) {
                        labels.insert(label.as_string().clone(), pos);
                    }
                }
                match command {
                    casl2::Command::A { adr, .. } | casl2::Command::P { adr, .. } => {
                        literals.insert(adr.clone(), 0_usize);
                    }
                    _ => {}
                }
                self.compile_command(pos, command);
            }
            code.push((pos, stmt));
        }

        for (adr, pos) in literals.iter_mut() {
            match adr {
                casl2::Adr::LiteralDec(d) => {
                    if !self.enough_remain(1) {
                        return Err("メモリ不足でプログラムをロードできませんでした".into());
                    }
                    *pos = self.compile_pos;
                    self.mem[*pos] = *d as u16;
                    self.compile_pos += 1;
                }
                casl2::Adr::LiteralHex(h) => {
                    if !self.enough_remain(1) {
                        return Err("メモリ不足でプログラムをロードできませんでした".into());
                    }
                    *pos = self.compile_pos;
                    self.mem[*pos] = *h;
                    self.compile_pos += 1;
                }
                casl2::Adr::LiteralStr(s) => {
                    let len = s.chars().count();
                    if !self.enough_remain(len) {
                        return Err("メモリ不足でプログラムをロードできませんでした".into());
                    }
                    *pos = self.compile_pos;
                    for (i, ch) in s.chars().enumerate() {
                        let ch = jis_x_201::convert_from_char(ch);
                        self.mem[*pos + i] = ch as u16;
                    }
                    self.compile_pos += len;
                }
                _ => {}
            }
        }

        for (mempos, stmt) in code.iter() {
            let command = if let casl2::Statement::Code { command, .. } = stmt {
                command
            } else {
                continue;
            };
            match command {
                casl2::Command::Start { entry_point } => {
                    if let Some(label) = entry_point {
                        let label = label.as_str().to_string();
                        if let Some(pos) = labels.get(&label) {
                            self.program_labels.insert(name.clone(), *pos);
                        } else {
                            return Err(format!("指定されたラベルが見つかりません ( {} )", label));
                        }
                    } else {
                        self.program_labels.insert(name.clone(), *mempos);
                    }
                }
                casl2::Command::Dc { constants } => {
                    let mut pos = *mempos;
                    for cnst in constants.iter() {
                        match cnst {
                            casl2::Constant::Dec(d) => {
                                self.mem[pos] = *d as u16;
                                pos += 1;
                            }
                            casl2::Constant::Hex(h) => {
                                self.mem[pos] = *h;
                                pos += 1;
                            }
                            casl2::Constant::Str(s) => {
                                for ch in s.chars() {
                                    self.mem[pos] = jis_x_201::convert_from_char(ch) as u16;
                                    pos += 1;
                                }
                            }
                            casl2::Constant::Label(label) => {
                                let label = label.as_string();
                                if let Some(label_pos) = labels.get(label) {
                                    self.mem[pos] = *label_pos as u16;
                                } else if let Some(label_pos) =
                                    self.program_labels.get(label).copied()
                                {
                                    self.mem[pos] = label_pos as u16;
                                } else {
                                    self.unknown_labels
                                        .entry(label.clone())
                                        .or_insert_with(Vec::new)
                                        .push(pos);
                                }
                                pos += 1;
                            }
                        }
                    }
                }
                casl2::Command::In { pos, len } | casl2::Command::Out { pos, len } => {
                    let pos = pos.as_string();
                    let len = len.as_string();
                    if let Some(label_pos) = labels.get(pos) {
                        self.mem[*mempos + 5] = *label_pos as u16;
                    } else if let Some(label_pos) = self.program_labels.get(pos).copied() {
                        self.mem[*mempos + 5] = label_pos as u16;
                    } else {
                        self.unknown_labels
                            .entry(pos.clone())
                            .or_insert_with(Vec::new)
                            .push(*mempos + 5);
                    }
                    if let Some(label_pos) = labels.get(len) {
                        self.mem[*mempos + 7] = *label_pos as u16;
                    } else if let Some(label_pos) = self.program_labels.get(len).copied() {
                        self.mem[*mempos + 7] = label_pos as u16;
                    } else {
                        self.unknown_labels
                            .entry(len.clone())
                            .or_insert_with(Vec::new)
                            .push(*mempos + 7);
                    }
                }
                casl2::Command::A { adr, .. } | casl2::Command::P { adr, .. } => match adr {
                    casl2::Adr::Dec(d) => self.mem[*mempos + 1] = *d as u16,
                    casl2::Adr::Hex(h) => self.mem[*mempos + 1] = *h,
                    casl2::Adr::Label(label) => {
                        let label = label.as_string();
                        if let Some(label_pos) = labels.get(label) {
                            self.mem[*mempos + 1] = *label_pos as u16;
                        } else if let Some(label_pos) = self.program_labels.get(label).copied() {
                            self.mem[*mempos + 1] = label_pos as u16;
                        } else {
                            self.unknown_labels
                                .entry(label.clone())
                                .or_insert_with(Vec::new)
                                .push(*mempos + 1);
                        }
                    }
                    casl2::Adr::LiteralDec(_)
                    | casl2::Adr::LiteralHex(_)
                    | casl2::Adr::LiteralStr(_) => {
                        let label_pos = literals.get(adr).expect("BUG");
                        self.mem[*mempos + 1] = *label_pos as u16;
                    }
                },
                _ => {}
            }
        }

        if let Some(pos_list) = self.unknown_labels.remove(&name) {
            let label_pos = *self.program_labels.get(&name).expect("BUG");
            for pos in pos_list {
                self.mem[pos] = label_pos as u16;
            }
        }

        self.local_labels.insert(name.clone(), labels);
        self.program_list.push((src_file.into(), name, code));

        Ok(())
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
            casl2::Command::Out { .. } => {
                let src = casl2::parse(
                    r#" PUSH 0,GR1
                        PUSH 0,GR2
                        LAD GR1,0
                        LAD GR2,0
                        SVC 2
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

fn get_op_code_form(op_code: u16, adr: u16) -> String {
    let r1 = (op_code >> 4) & 0xF;
    let r2 = op_code & 0xF;
    match (op_code >> 8) & 0xFF {
        0x14 => format!("#{:04X}: LD GR{},GR{}", op_code, r1, r2),
        0x24 => format!("#{:04X}: ADDA GR{},GR{}", op_code, r1, r2),
        0x26 => format!("#{:04X}: ADDL GR{},GR{}", op_code, r1, r2),
        0x25 => format!("#{:04X}: SUBA GR{},GR{}", op_code, r1, r2),
        0x27 => format!("#{:04X}: SUBL GR{},GR{}", op_code, r1, r2),
        0x34 => format!("#{:04X}: AND GR{},GR{}", op_code, r1, r2),
        0x35 => format!("#{:04X}: OR GR{},GR{}", op_code, r1, r2),
        0x36 => format!("#{:04X}: XOR GR{},GR{}", op_code, r1, r2),
        0x44 => format!("#{:04X}: CPA GR{},GR{}", op_code, r1, r2),
        0x45 => format!("#{:04X}: CPL GR{},GR{}", op_code, r1, r2),
        0x12 => format!("#{:04X}: LAD GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x70 => format!("#{:04X}: PUSH #{:04X},GR{}", op_code, adr, r2),
        0xF0 => format!("#{:04X}: SVC #{:04X},GR{}", op_code, adr, r2),
        0x71 => format!("#{:04X}: POP GR{}", op_code, r1),
        0x81 => format!("#{:04X}: RET", op_code),
        0x00 => format!("#{:04X}: NOP", op_code),
        0x10 => format!("#{:04X}: LD GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x11 => format!("#{:04X}: ST GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x20 => format!("#{:04X}: ADDA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x22 => format!("#{:04X}: ADDL GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x21 => format!("#{:04X}: SUBA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x23 => format!("#{:04X}: SUBL GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x30 => format!("#{:04X}: AND GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x31 => format!("#{:04X}: OR GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x32 => format!("#{:04X}: XOR GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x40 => format!("#{:04X}: CPA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x41 => format!("#{:04X}: CPL GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x50 => format!("#{:04X}: SLA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x51 => format!("#{:04X}: SRA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x52 => format!("#{:04X}: SLL GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x53 => format!("#{:04X}: SRL GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x65 => format!("#{:04X}: JPL #{:04X},GR{}", op_code, adr, r2),
        0x61 => format!("#{:04X}: JMI #{:04X},GR{}", op_code, adr, r2),
        0x62 => format!("#{:04X}: JNZ #{:04X},GR{}", op_code, adr, r2),
        0x63 => format!("#{:04X}: JZE #{:04X},GR{}", op_code, adr, r2),
        0x66 => format!("#{:04X}: JOV #{:04X},GR{}", op_code, adr, r2),
        0x64 => format!("#{:04X}: JUMP #{:04X},GR{}", op_code, adr, r2),
        0x80 => format!("#{:04X}: CALL #{:04X},GR{}", op_code, adr, r2),
        _ => format!("#{:04X}: UNKNOWN", op_code),
    }
}

fn get_op_code_size(op_code: u16) -> usize {
    match (op_code >> 8) & 0xFF {
        0x14 => 1, //format!("#{:04X}: LD GR{},GR{}", op_code, r1, r2),
        0x24 => 1, //format!("#{:04X}: ADDA GR{},GR{}",op_code,  r1, r2),
        0x26 => 1, //format!("#{:04X}: ADDL GR{},GR{}", op_code, r1, r2),
        0x25 => 1, //format!("#{:04X}: SUBA GR{},GR{}", op_code, r1, r2),
        0x27 => 1, //format!("#{:04X}: SUBL GR{},GR{}", op_code, r1, r2),
        0x34 => 1, //format!("#{:04X}: AND GR{},GR{}", op_code, r1, r2),
        0x35 => 1, //format!("#{:04X}: OR GR{},GR{}", op_code, r1, r2),
        0x36 => 1, //format!("#{:04X}: XOR GR{},GR{}", op_code, r1, r2),
        0x44 => 1, //format!("#{:04X}: CPA GR{},GR{}", op_code, r1, r2),
        0x45 => 1, //format!("#{:04X}: CPL GR{},GR{}", op_code, r1, r2),

        0x12 => 2, //format!("#{:04X}: LAD GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x70 => 2, //format!("#{:04X}: PUSH #{:04X},GR{}",op_code,  adr, r2),
        0xF0 => 2, //format!("#{:04X}: SVC #{:04X},GR{}", op_code, adr, r2),

        0x71 => 1, //format!("#{:04X}: POP GR{}", op_code, r1),
        0x81 => 1, //format!("#{:04X}: RET",op_code),
        0x00 => 1, //format!("#{:04X}: NOP",op_code),

        0x10 => 2, //format!("#{:04X}: LD GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x11 => 2, //format!("#{:04X}: ST GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x20 => 2, //format!("#{:04X}: ADDA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x22 => 2, //format!("#{:04X}: ADDL GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x21 => 2, //format!("#{:04X}: SUBA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x23 => 2, //format!("#{:04X}: SUBL GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x30 => 2, //format!("#{:04X}: AND GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x31 => 2, //format!("#{:04X}: OR GR{},#{:04X},GR{}",op_code,  r1, adr, r2),
        0x32 => 2, //format!("#{:04X}: XOR GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x40 => 2, //format!("#{:04X}: CPA GR{},#{:04X},GR{}",op_code,  r1, adr, r2),
        0x41 => 2, //format!("#{:04X}: CPL GR{},#{:04X},GR{}",op_code,  r1, adr, r2),
        0x50 => 2, //format!("#{:04X}: SLA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x51 => 2, //format!("#{:04X}: SRA GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x52 => 2, //format!("#{:04X}: SLL GR{},#{:04X},GR{}",op_code,  r1, adr, r2),
        0x53 => 2, //format!("#{:04X}: SRL GR{},#{:04X},GR{}",op_code,  r1, adr, r2),
        0x65 => 2, //format!("#{:04X}: JPL #{:04X},GR{}", op_code, adr, r2),
        0x61 => 2, //format!("#{:04X}: JMI #{:04X},GR{}", op_code, adr, r2),
        0x62 => 2, //format!("#{:04X}: JNZ #{:04X},GR{}", op_code, adr, r2),
        0x63 => 2, //format!("#{:04X}: JZE #{:04X},GR{}", op_code, adr, r2),
        0x66 => 2, //format!("#{:04X}: JOV #{:04X},GR{}", op_code, adr, r2),
        0x64 => 2, //format!("#{:04X}: JUMP #{:04X},GR{}",op_code,  adr, r2),
        0x80 => 2, //format!("#{:04X}: CALL #{:04X},GR{}", op_code, adr, r2),

        _ => 0, //format!("#{:04X}: UNKNOWN",op_code),
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
                // 2 10 SVC 1 (or 2)
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
