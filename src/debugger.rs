use crate::casl2;
use crate::compiler;
use crate::jis_x_201;
use crate::parser;
use crate::stat;
use crate::Flags;
use std::borrow::Cow;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::convert::TryFrom;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};

pub fn run_nonstep(src_file: String, flags: Flags) -> io::Result<i32> {
    let _ = (src_file, flags);
    todo!()
}

pub fn run_basic(src_file: String, flags: Flags) -> io::Result<i32> {
    let _ = (src_file, flags);
    todo!()
}

pub fn run_casl2(src_file: String, mut flags: Flags) -> io::Result<i32> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let mut emu = Emulator::new();

    match if src_file.to_ascii_lowercase().ends_with(".cas") {
        emu.compile_casl2(&src_file, &flags, true)
    } else {
        emu.compile_basic(&src_file, &flags, true)
    } {
        Ok(0) => {}
        result => return result,
    }

    if emu.start_point.is_none() {
        eprintln!("入力ファイルが正しくありません");
        return Ok(101);
    }

    flags.compiler.program_name = None;

    if !emu.unknown_labels.is_empty() {
        // 未解決ラベルを解決する処理を入れる
        let src_dir = Path::new(&src_file).parent().unwrap();
        match resolve_files(&mut emu, &mut stdin, &mut stdout, &src_dir, &flags) {
            Ok(0) => {}
            Ok(99) => return Ok(0),
            result => return result,
        }
    }

    match interactive_before_start(&mut emu, &mut stdin, &mut stdout) {
        Ok(0) => {}
        Ok(99) => return Ok(0),
        result => return result,
    }

    emu.init_to_start();

    for _ in 0..10000000 {
        match emu.step_through_code(&mut stdin, &mut stdout) {
            Ok(_) => {}
            Err(RuntimeError::IoError(error)) => return Err(error),
            Err(error) => {
                writeln!(stdout, "{:?}", error)?;
                break;
            }
        }
    }
    writeln!(stdout, "TOTAL STEPS: {}", emu.step_count)?;

    Ok(0)
}

fn interactive_before_start<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
) -> io::Result<i32> {
    let mut line = String::new();

    let command_list = vec![
        "help",
        "show-reg",
        "show-mem",
        "show-labels",
        "show-var",
        "set-reg",
        "set-mem",
        "set-mem-fill",
        "cancel",
    ]
    .join(" ");

    loop {
        writeln!(stdout)?;
        writeln!(stdout, "実行開始前の設定")?;
        writeln!(stdout, "使用可能なデバッガコマンド: {}", command_list)?;
        writeln!(stdout)?;
        write!(stdout, "> ")?;
        stdout.flush()?;
        line.clear();
        if stdin.read_line(&mut line)? == 0 {
            eprintln!("入力がキャンセルされました");
            io::stderr().flush()?;
            writeln!(stdout, "テスト実行を中止します")?;
            return Ok(99);
        }
        let line = line.trim();
        let mut cmd_and_param = line.splitn(2, ' ').map(|s| s.trim());
        let cmd = cmd_and_param.next().unwrap();
        match cmd {
            "help" => {
                show_command_help_before_start(cmd_and_param.next(), stdout)?;
            }
            "run" => return Ok(0),
            "cancel" => {
                writeln!(stdout, "テスト実行を中止します")?;
                return Ok(99);
            }
            "show-reg" => show_reg(emu, stdout)?,
            "show-labels" => show_label(emu, stdout, cmd_and_param.next())?,
            "set-reg" => {
                let msg = set_reg(emu, cmd_and_param.next());
                writeln!(stdout, "{}", msg)?;
            }
            _ => {
                writeln!(stdout, "コマンドが正しくありません")?;
            }
        }
    }
}

fn show_label<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let mut count = 0;
    if let Some(label) = param {
        let label = label.to_string();
        match emu.program_labels.get(&label) {
            Some(adr) => {
                writeln!(stdout, " #{:04X} {:<8}", adr, label)?;
            }
            None => {
                writeln!(stdout, "ラベル{}が見つかりません", label)?;
                return Ok(());
            }
        }
        let label_map = match emu.local_labels.get(&label) {
            Some(map) => map,
            None => {
                writeln!(stdout, "ラベル{}にローカルラベルは設定されてません", label)?;
                return Ok(());
            }
        };
        for (adr, key) in label_map
            .iter()
            .map(|(key, adr)| (adr, key))
            .collect::<BTreeSet<_>>()
        {
            write!(stdout, " #{:04X} {:<8}    ", adr, key,)?;
            count += 1;
            if (count & 3) == 0 {
                writeln!(stdout)?;
            }
        }
    } else {
        for (adr, key) in emu
            .program_labels
            .iter()
            .map(|(key, adr)| (adr, key))
            .collect::<BTreeSet<_>>()
        {
            write!(stdout, " #{:04X} {:<8}    ", adr, key)?;
            count += 1;
            if (count & 3) == 0 {
                writeln!(stdout)?;
            }
        }
        for (adr, key) in emu
            .labels_for_debug
            .iter()
            .map(|(key, (adr, _))| (adr, key))
            .collect::<BTreeSet<_>>()
        {
            write!(stdout, " #{:04X} {:<8}   ", adr, key)?;
            count += 1;
            if (count & 3) == 0 {
                writeln!(stdout)?;
            }
        }
    }
    if (count & 3) != 0 {
        writeln!(stdout)?;
    }
    Ok(())
}

fn show_reg<W: Write>(emu: &Emulator, stdout: &mut W) -> io::Result<()> {
    writeln!(stdout)?;
    writeln!(
        stdout,
        "PR:  #{:04}         SP:  #{:04X}         FR:  (OF: {}, SF: {}, ZF: {})",
        emu.program_register,
        emu.stack_pointer,
        if emu.overflow_flag { '1' } else { '0' },
        if emu.sign_flag { '1' } else { '0' },
        if emu.zero_flag { '1' } else { '0' }
    )?;
    writeln!(
        stdout,
        "GR0: #{0:04X}({0:6}) GR1: #{1:04X}({1:6}) GR2: #{2:04X}({2:6}) GR3: #{3:04X}({3:6})",
        emu.general_registers[0] as i16,
        emu.general_registers[1] as i16,
        emu.general_registers[2] as i16,
        emu.general_registers[3] as i16
    )?;
    writeln!(
        stdout,
        "GR4: #{0:04X}({0:6}) GR5: #{1:04X}({1:6}) GR6: #{2:04X}({2:6}) GR7: #{3:04X}({3:6})",
        emu.general_registers[4] as i16,
        emu.general_registers[5] as i16,
        emu.general_registers[6] as i16,
        emu.general_registers[7] as i16
    )?;
    Ok(())
}

enum ExtendedLabel {
    GlobalLabel(String),
    LocalLabel(String, String),
    GlobalLabelOffset(String, u16),
    LocalLabelOffset(String, String, u16),
}

impl<'a> casl2::Tokenizer<'a> {
    fn extended_label(&mut self) -> Result<Option<ExtendedLabel>, &'static str> {
        let global_label = match self.word() {
            Some(s) => s,
            None => return Ok(None),
        };
        if !self.colon() {
            return Ok(Some(ExtendedLabel::GlobalLabel(global_label)));
        }
        if let Some(offset) = self.hex() {
            return Ok(Some(ExtendedLabel::GlobalLabelOffset(global_label, offset)));
        }
        let local_label = match self.word() {
            Some(s) => s,
            None => return Err("引数が不正です"),
        };
        if !self.colon() {
            return Ok(Some(ExtendedLabel::LocalLabel(global_label, local_label)));
        }
        if let Some(offset) = self.hex() {
            Ok(Some(ExtendedLabel::LocalLabelOffset(
                global_label,
                local_label,
                offset,
            )))
        } else {
            Err("引数が不正です")
        }
    }
}

fn get_single_value(
    emu: &mut Emulator,
    tokenizer: &mut casl2::Tokenizer<'_>,
) -> Result<u16, Cow<'static, str>> {
    match tokenizer.extended_label()? {
        Some(ExtendedLabel::GlobalLabel(label)) => match emu.program_labels.get(&label) {
            Some(adr) => Ok(*adr as u16),
            None => Err(Cow::Owned(format!("ラベル{}が見つかりません", label))),
        },
        Some(ExtendedLabel::GlobalLabelOffset(label, offset)) => {
            // TODO +offsetのオーバーフローチェック
            match emu.program_labels.get(&label) {
                Some(adr) => Ok(*adr as u16 + offset),
                None => Err(Cow::Owned(format!("ラベル{}が見つかりません", label))),
            }
        }
        Some(ExtendedLabel::LocalLabel(glabel, llabel)) => match emu.local_labels.get(&glabel) {
            Some(label_map) => match label_map.get(&llabel) {
                Some(adr) => Ok(*adr as u16),
                None => Err(Cow::Owned(format!("ラベル{}が見つかりません", llabel))),
            },
            None => Err(Cow::Owned(format!("ラベル{}が見つかりません", glabel))),
        },
        Some(ExtendedLabel::LocalLabelOffset(glabel, llabel, offset)) => {
            // TODO +offsetのオーバーフローチェック
            match emu.local_labels.get(&glabel) {
                Some(label_map) => match label_map.get(&llabel) {
                    Some(adr) => Ok(*adr as u16 + offset),
                    None => Err(Cow::Owned(format!("ラベル{}が見つかりません", llabel))),
                },
                None => Err(Cow::Owned(format!("ラベル{}が見つかりません", glabel))),
            }
        }
        None => {
            let value = match tokenizer.value() {
                Some(value) => value,
                None => return Err(Cow::Borrowed("引数が不正です")),
            };
            match value {
                casl2::Token::Word(_) => unreachable!("BUG"),
                casl2::Token::Dec(d) => Ok(d as u16),
                casl2::Token::Hex(h) => Ok(h),
                casl2::Token::Str(_) => todo!(),
                casl2::Token::LitDec(_) => todo!(),
                casl2::Token::LitHex(_) => todo!(),
                casl2::Token::LitStr(_) => todo!(),
            }
        }
    }
}

fn set_reg<'a, 'b>(emu: &mut Emulator, param: Option<&'a str>) -> Cow<'b, str> {
    let params = match param {
        Some(param) => param.split_whitespace().collect::<Vec<_>>(),
        None => return Cow::Borrowed("引数がありません"),
    };

    let (reg, value) = if let [reg, value] = params.as_slice() {
        (reg, value)
    } else {
        return Cow::Borrowed("引数の数が正しくありません");
    };

    match casl2::Register::try_from(*reg) {
        Ok(reg) => {
            let mut tokenizer = casl2::Tokenizer::new(value);
            match get_single_value(emu, &mut tokenizer) {
                Err(msg) => msg,
                Ok(value) => {
                    emu.general_registers[reg as usize] = value;
                    Cow::Owned(format!("{}に#{:04X}を設定しました", reg, value))
                }
            }
        }
        Err(_) => todo!(),
    }
}

fn show_command_help_before_start<W: Write>(cmd: Option<&str>, stdout: &mut W) -> io::Result<()> {
    writeln!(stdout)?;
    writeln!(stdout, "コマンドヘルプ")?;

    let cmd = match cmd {
        Some(cmd) => cmd,
        None => {
            writeln!(
                stdout,
                r#"
    set-reg <REGISTER> <VALUE>
                レジスタに値を設定する
    set-mem <ADDRESS> <VALUE1>[,<VALUE2>..]
                メモリの指定アドレスにデータを書き込む
    set-mem-fill <ADDRESS> <LENGTH> <VALUE>
                メモリの指定アドレスから指定の長さ分を指定の値で埋める
    show-reg
                各レジスタの現在の値を表示する
    show-mem <ADDRESS> <LENGTH>
                メモリの指定アドレスから指定の長さ分の領域の各値を列挙する
    show-labels [<PROGRAM_ENTRY>]
                ラベルの一覧とアドレスを表示する。PROGRAM_ENTRYを指定した場合はローカルラベルを表示する
    show-var <BASIC_SRC_FILE>
                指定したBASICプログラムの変数名と対応するラベルとアドレスと値を表示する
    cancel
                テスト実行を中止する
    help <COMMAND_NAME>
                指定デバッガコマンドの詳細ヘルプを表示する
"#
            )?;
            return Ok(());
        }
    };

    if "set-reg".eq_ignore_ascii_case(cmd) {
        writeln!(
            stdout,
            r#"
    set-reg <REGISTER> <VALUE>
                レジスタに値を設定する
                    REGISTER .. GR0～GR7
                    VALUE    .. 10進定数,16進定数,文字定数(1文字),アドレス定数,リテラル
                    REGISTER .. OF,SF,ZF
                    VALUE    .. 0,1"#
        )?;
    }

    if "set-mem".eq_ignore_ascii_case(cmd) {
        writeln!(
            stdout,
            r#"
    set-mem <ADDRESS> <VALUE1>[,<VALUE2>..]
                メモリの指定アドレスにデータを書き込む
                値を複数列挙した場合はアドレスから連続した領域に書き込まれる
                値が文字定数の場合は文字数分の連続した領域に書き込まれる
                    ADDRESS  .. 16進定数,アドレス定数
                    VALUE*   .. 10進定数,16進定数,文字定数,アドレス定数,リテラル"#
        )?;
    }

    if "set-mem-fill".eq_ignore_ascii_case(cmd) {
        writeln!(
            stdout,
            r#"
    set-mem-fill <ADDRESS> <LENGTH> <VALUE>
                メモリの指定アドレスから指定の長さ分を指定の値で埋める
                    ADDRESS  .. 16進定数,アドレス定数
                    LENGTH   .. 正の10進定数,16進定数
                    VALUE    .. 10進定数,16進定数,文字定数(1文字),アドレス定数,リテラル"#
        )?;
    }

    writeln!(
        stdout,
        r#"
    10進定数
                -32768　～ 32767
    16進定数
                #0000 ～ #FFFF
    文字定数
                'X' 'Abc123' 'Let''s go' などの文字・文字列を引用符で囲ったもの
    アドレス定数
        ラベル           
                        グローバルラベル (各プログラムのエントリラベル、デバッガコマンドで定義したラベル)
                            例: MAIN
        ラベル:ラベル
                        ローカルラベル (各プログラムの内部のラベル)。 プログラムのエントリラベル:内部ラベル　で表記
                            例: MAIN:I1
        ラベル:16進定数       
                        グローバルラベルの位置から16進定数分の相対位置のアドレスを返す
                            例: MYMEM:#0012
        ラベル:ラベル:16進定数   
                        ローカルラベルの位置から16進定数分の相対位置のアドレスを返す
                            例: MAIN:MEM:#0015
    リテラル
                10進定数、16進定数、文字定数の頭に=を付けて指定
                定数格納の領域を確保しそのアドレスを返す
                    例: =123 =#ABCD ='XYZ'
              
"#
    )?;

    Ok(())
}

fn get_program_name(file: &str) -> io::Result<Option<String>> {
    let text = fs::read_to_string(file)?;
    if file.to_ascii_lowercase().ends_with(".cas") {
        match casl2::parse(&text) {
            Err(_) => Ok(None),
            Ok(code) => Ok(casl2::utils::get_program_name(&code).map(|s| s.to_string())),
        }
    } else {
        match parser::parse(text.as_bytes())? {
            Err(_) => Ok(None),
            Ok(code) => Ok(code.iter().find_map(|stmt| {
                if let parser::Statement::ProgramName { name } = stmt {
                    Some(name.clone())
                } else {
                    None
                }
            })),
        }
    }
}

fn resolve_files<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    src_dir: &Path,
    flags: &Flags,
) -> io::Result<i32> {
    const LIST_UP_SIZE: usize = 8;
    let mut label_cache = HashMap::new();
    let dst_dir = flags.create_dst_dir()?;
    let casl2_flags = {
        let mut flags = flags.clone();
        flags.compiler = Default::default();
        flags
    };
    while let Some((label, files)) = emu.suggest_load_file(&dst_dir, src_dir)? {
        writeln!(stdout)?;
        writeln!(stdout, "未解決のラベル: {}", label)?;
        if files.is_empty() {
            writeln!(
                stdout,
                "ラベル{}を解決できるファイルのパスを入力してください",
                label
            )?;
            writeln!(
                stdout,
                "-1を入力するとテスト実行を中止します。0を入力するとRETのみのダミーコードで解決されます。"
            )?;
        } else {
            for (i, file_name) in files.iter().enumerate().take(LIST_UP_SIZE) {
                writeln!(stdout, " 候補 {}: {}", i + 1, file_name)?;
            }
            writeln!(stdout, "ラベル{}を解決できるファイルが候補一覧にある場合は番号を、ない場合は解決できるファイルのパスを入力してください", label)?;
            writeln!(
                stdout,
                "-1を入力するとテスト実行を中止します。0を入力するとRETのみのダミーコードで解決されます。"
            )?;
            writeln!(
                stdout,
                "空行の場合は全ての候補を順に調べラベル{}の解決を試みます",
                label
            )?;
        }
        write!(stdout, "> ")?;
        stdout.flush()?;
        let mut line = String::new();
        if stdin.read_line(&mut line)? == 0 {
            eprintln!("入力がキャンセルされました");
            io::stderr().flush()?;
            writeln!(stdout, "テスト実行を中止します")?;
            return Ok(99);
        }
        let line = line.trim();
        let file = if line.is_empty() {
            if files.is_empty() {
                continue;
            }
            writeln!(stdout, "全ての候補を調べます...")?;
            let mut found = None;
            for file in files.iter() {
                if let Some(name) = label_cache.get(file) {
                    if name == &label {
                        found = Some(file.clone());
                        break;
                    }
                }
                match get_program_name(&file)? {
                    Some(name) if name == label => {
                        found = Some(file.clone());
                        break;
                    }
                    Some(name) => {
                        label_cache.insert(file.clone(), name);
                    }
                    _ => {}
                }
            }
            if let Some(file) = found {
                file
            } else {
                writeln!(stdout, "解決できるファイルを見つけられませんでした")?;
                writeln!(stdout, "テスト実行を中止します")?;
                return Ok(99);
            }
        } else {
            let count = files.len().min(LIST_UP_SIZE) as i32;
            let file = match line.parse::<i32>() {
                Ok(0) => {
                    writeln!(
                        stdout,
                        "ラベル{}にはRETのみのダミーコードを割り当てます",
                        label
                    )?;
                    if let Err(msg) = emu.resolve_dummy_code(label) {
                        eprintln!("CompileError{{ {} }}", msg);
                        return Ok(20);
                    }
                    continue;
                }
                Ok(-1) => {
                    writeln!(stdout, "テスト実行を中止します")?;
                    return Ok(99);
                }
                Ok(num) if num >= 1 && num <= count => files[num as usize - 1].clone(),
                _ => {
                    let path = Path::new(line);
                    if path.is_file() && path.exists() {
                        line.to_string()
                    } else {
                        writeln!(stdout, "{}が見つかりません", line)?;
                        continue;
                    }
                }
            };
            match get_program_name(&file)? {
                Some(name) if name == label => file,
                _ => {
                    writeln!(stdout, "{}はラベル{}を解決できませんでした", line, label)?;
                    continue;
                }
            }
        };
        match if file.to_ascii_lowercase().ends_with(".cas") {
            emu.compile_casl2(&file, &casl2_flags, false)
        } else {
            emu.compile_basic(&file, &flags, false)
        } {
            Ok(0) => {}
            result => return result,
        }
        writeln!(stdout, "{}を読み込みました", file)?;
    }
    Ok(0)
}

// プログラムリスト (ファイル名, ラベル, ソースコード(コードメモリ位置, コード))
type Program = (String, String, Vec<(usize, casl2::Statement)>);

// コールスタック用に確保する最大サイズ　(1呼び出しにつき、RPUSHで7語、MEMで1語、合計8語として、再帰呼び出しの深さ200とすると2000語分程度あればよいか？)
const CALLSTACK_MAX_SIZE: usize = 0x0800; /* 2048 */

// プログラムコードを埋め込みを開始する位置 (0はダメ)
const BEGIN_OF_PROGRAM: usize = 0x2000; /* 8192 */

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

    // 最後に実行したコードの位置(PRの値)
    last_run_position: usize,

    // CASL2ステップ数(step_through_code呼び出し回数)
    step_count: usize,

    // BASICステップ (DebugBasicStepのid)
    basic_step: Option<usize>,
    // BASICステップ数 (DebugBasicStepの呼び出し回数)
    basic_step_count: usize,

    // プログラムの開始点のエントリラベル
    // (START命令に引数があるとBEGIN_OF_PROGRAMにはならないこともあるため)
    start_point: Option<String>,

    // デバッガコマンドから設定したラベル
    labels_for_debug: HashMap<String, (usize, casl2::Statement)>,

    // 挿入するプログラムの埋め込み開始位置
    compile_pos: usize,

    // BASICコードの情報 (ソースファイル名, (プログラムエントリラベル, デバッグ用情報))
    basic_info: HashMap<String, (String, compiler::DebugInfo)>,

    // プログラムごとのラベルとメモリ位置の対応 (プログラムエントリラベル, (ラベル, メモリ位置))
    local_labels: HashMap<String, HashMap<String, usize>>,

    // プログラムのエントリラベルとメモリ位置の対応 (プログラムエントリラベル, メモリ位置)
    program_labels: HashMap<String, usize>,

    // プログラムリスト (ファイル名, ラベル, ソースコード(コードメモリ位置, コード))
    program_list: Vec<Program>,

    // 未解決ラベル(おそらく外部定義)
    unknown_labels: HashMap<String, Vec<usize>>,

    // 読み込み済みファイル一覧
    loaded_files: HashSet<String>,
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

            last_run_position: INITIAL_PROGRAM_REGISTER - 2,
            step_count: 0,
            basic_step: None,
            basic_step_count: 0,
            labels_for_debug: HashMap::new(),

            start_point: None,
            basic_info: HashMap::new(),
            compile_pos: BEGIN_OF_PROGRAM,
            local_labels: HashMap::new(),
            program_labels: HashMap::new(),
            program_list: Vec::new(),
            unknown_labels: HashMap::new(),
            loaded_files: HashSet::new(),
        }
    }

    fn last_run(&self) -> String {
        let pos = self.last_run_position;
        let op_code = self.mem[pos];
        let adr = self.mem[pos + 1];
        format!("[#{:04X}] {}", pos, get_op_code_form(op_code, adr))
    }

    fn init_to_start(&mut self) {
        let position = self
            .start_point
            .as_ref()
            .and_then(|label| self.program_labels.get(label))
            .copied()
            .expect("BUG");

        let call = casl2::Command::P {
            code: casl2::P::Call,
            adr: casl2::Adr::Hex(position as u16),
            x: None,
        };

        let stack_pointer = CALLSTACK_START_POSITION - 1;

        self.mem[INITIAL_PROGRAM_REGISTER - 2] = call.first_word();
        self.mem[INITIAL_PROGRAM_REGISTER - 1] = position as u16;
        self.last_run_position = INITIAL_PROGRAM_REGISTER - 2;
        self.mem[stack_pointer] = INITIAL_PROGRAM_REGISTER as u16;
        self.stack_pointer = stack_pointer;
        self.program_register = position;
        self.step_count = 0;
        self.basic_step = None;
        self.basic_step_count = 0;

        // GRとFRは不定
    }

    fn enough_remain(&self, len: usize) -> bool {
        len < self.mem.len() - self.compile_pos - CALLSTACK_MAX_SIZE
    }

    fn has_asccess_permission(&self, pos: usize) -> bool {
        (BEGIN_OF_PROGRAM..self.compile_pos).contains(&pos)
    }
}

fn edit_distance(str1: &str, str2: &str) -> usize {
    let str1 = str1.as_bytes();
    let str2 = str2.as_bytes();
    let mut dp = vec![vec![0; str1.len() + 1]; 2];
    for (i, v) in dp[0].iter_mut().enumerate() {
        *v = i;
    }
    for (i, ch2) in str2.iter().enumerate() {
        let prev = i & 1;
        let next = prev ^ 1;
        dp[next][0] = i + 1;
        for (k, ch1) in str1.iter().enumerate() {
            let cost = if ch1 == ch2 { 0 } else { 1 };
            dp[next][k + 1] = (dp[next][k] + 1)
                .min(dp[prev][k] + cost)
                .min(dp[prev][k + 1] + 1);
        }
    }
    dp[str2.len() & 1][str1.len()]
}

impl Emulator {
    fn resolve_dummy_code(&mut self, label: String) -> Result<(), String> {
        if !self.enough_remain(1) {
            return Err("メモリ不足でダミープログラムをロードできませんでした".into());
        }
        let label_pos = self.compile_pos;
        self.compile_pos += 1;
        self.mem[label_pos] = casl2::Command::Ret.first_word();
        if let Some(pos_list) = self.unknown_labels.remove(&label) {
            for pos in pos_list {
                self.mem[pos] = label_pos as u16;
            }
        }
        self.program_labels.insert(label, label_pos);
        Ok(())
    }

    fn suggest_load_file(
        &self,
        dst_dir: &Path,
        src_dir: &Path,
    ) -> io::Result<Option<(String, Vec<String>)>> {
        let target_label = match self.unknown_labels.keys().next() {
            Some(label) => label.clone(),
            None => return Ok(None),
        };
        let lc_target_label = target_label.to_ascii_lowercase();
        let target_name = format!("{}.cas", target_label);
        let lc_target_name = target_name.to_ascii_lowercase();
        let bas_target_name = format!("{}.bas", target_label);
        let lc_bas_target_name = bas_target_name.to_ascii_lowercase();
        let targets = [&target_label, &target_name, &bas_target_name];
        let lc_targets = [&lc_target_label, &lc_target_name, &lc_bas_target_name];
        let mut list = Vec::new();
        for (i, dir) in [dst_dir, src_dir].iter().enumerate() {
            if i == 0 && dst_dir == src_dir {
                continue;
            }
            for entry in dir.read_dir()? {
                let entry = entry?;
                if !entry.path().is_file() {
                    continue;
                }
                let path = entry.path().to_string_lossy().into_owned();
                if self.loaded_files.contains(&path) {
                    continue;
                }
                let file_name = entry.file_name().to_string_lossy().into_owned();
                let lc_file_name = file_name.to_ascii_lowercase();
                let dist1 = targets
                    .iter()
                    .map(|target| edit_distance(&target, &file_name))
                    .min()
                    .unwrap();
                let dist2 = lc_targets
                    .iter()
                    .map(|target| edit_distance(&target, &lc_file_name))
                    .min()
                    .unwrap();
                let dist = dist1.min(dist2);
                list.push((dist, i, path));
            }
        }
        list.sort();
        let list: Vec<_> = list.into_iter().map(|(.., p)| p).collect();
        Ok(Some((target_label, list)))
    }
}

#[derive(Debug)]
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
    StackEmpty {
        position: usize,
        op_code: String,
    },
    IoError(io::Error),
}

impl From<io::Error> for RuntimeError {
    fn from(error: io::Error) -> Self {
        Self::IoError(error)
    }
}

impl Emulator {
    fn step_through_code<R, W>(&mut self, stdin: &mut R, stdout: &mut W) -> Result<(), RuntimeError>
    where
        R: io::BufRead,
        W: io::Write,
    {
        let pr = self.program_register;
        if !self.has_asccess_permission(pr) {
            return Err(RuntimeError::ExecutePermissionDenied { position: pr });
        }
        self.step_count += 1;
        self.basic_step = None;
        self.last_run_position = pr;
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
                        return Err(RuntimeError::StackEmpty {
                            position: pr,
                            op_code: get_op_code_form(op_code, 0),
                        });
                    }
                    let value = self.mem[self.stack_pointer];
                    self.stack_pointer += 1;
                    self.general_registers[r1] = value;
                }
                0x81 if r1 == 0 && r2 == 0 => {
                    // RET
                    if self.stack_pointer >= CALLSTACK_START_POSITION {
                        return Err(RuntimeError::StackEmpty {
                            position: pr,
                            op_code: get_op_code_form(op_code, 0),
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
            0x50 => {
                // SLA r,adr,x
                let shift = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let r1_value = self.general_registers[r1];
                let r1_value = if (0..16).contains(&shift) {
                    self.overflow_flag = (((r1_value as u32) << shift) & 0x10000) != 0;
                    (r1_value & 0x8000) | ((r1_value << shift) & 0x7FFF)
                } else {
                    self.overflow_flag = false;
                    0
                };
                self.general_registers[r1] = r1_value;
                self.sign_flag = (r1_value as i16) < 0;
                self.zero_flag = r1_value == 0;
                return Ok(());
            }
            0x51 => {
                // SRA r,adr,x
                let shift = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let r1_value = self.general_registers[r1] as i16;
                let r1_value = if (0..16).contains(&shift) {
                    self.overflow_flag = ((((r1_value as i32) << 1) >> shift) & 1) != 0;
                    r1_value >> shift
                } else {
                    self.overflow_flag = r1_value < 0;
                    (r1_value >> 15) >> 1
                };
                self.general_registers[r1] = r1_value as u16;
                self.sign_flag = r1_value < 0;
                self.zero_flag = r1_value == 0;
                return Ok(());
            }
            0x52 => {
                // SLL r,adr,x
                let shift = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let r1_value = self.general_registers[r1] as u32;
                let r1_value = if (0..=16).contains(&shift) {
                    self.overflow_flag = ((r1_value << shift) & 0x10000) != 0;
                    (r1_value << shift) as u16
                } else {
                    self.overflow_flag = false;
                    0
                };
                self.general_registers[r1] = r1_value;
                self.sign_flag = (r1_value as i16) < 0;
                self.zero_flag = r1_value == 0;
                return Ok(());
            }
            0x53 => {
                // SRL r,adr,x
                let shift = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let r1_value = self.general_registers[r1] as u32;
                let r1_value = if (0..=16).contains(&shift) {
                    self.overflow_flag = (((r1_value << 1) >> shift) & 1) != 0;
                    (r1_value >> shift) as u16
                } else {
                    self.overflow_flag = false;
                    0
                };
                self.general_registers[r1] = r1_value;
                self.sign_flag = (r1_value as i16) < 0;
                self.zero_flag = r1_value == 0;
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
                    let pos = self.general_registers[1] as usize;
                    let len = self.general_registers[2] as usize;
                    if !self.has_asccess_permission(len) {
                        return Err(RuntimeError::AccessPermissionDenied {
                            position: pr,
                            op_code: get_op_code_form(op_code, adr),
                            address: len,
                        });
                    }
                    write!(stdout, "? ")?;
                    stdout.flush()?;
                    let mut line = String::new();
                    match stdin.read_line(&mut line)? {
                        0 => self.mem[len] = (-1_i16) as u16,
                        _ => {
                            let line = line.lines().next().unwrap();
                            let line = jis_x_201::convert_kana_wide_full_to_half(&line);
                            self.mem[len] = line.chars().count().min(256) as u16;
                            for (i, ch) in line.chars().enumerate().take(256) {
                                let ch = jis_x_201::convert_from_char(ch) as u16;
                                if !self.has_asccess_permission(pos + i) {
                                    return Err(RuntimeError::AccessPermissionDenied {
                                        position: pr,
                                        op_code: get_op_code_form(op_code, adr),
                                        address: pos + i,
                                    });
                                }
                                self.mem[pos + i] = ch;
                            }
                        }
                    }
                } else if value == 2 {
                    // OUT
                    let pos = self.general_registers[1] as usize;
                    let len = self.general_registers[2] as usize;
                    if !self.has_asccess_permission(len) {
                        return Err(RuntimeError::AccessPermissionDenied {
                            position: pr,
                            op_code: get_op_code_form(op_code, adr),
                            address: len,
                        });
                    }
                    let len = self.mem[len] as usize;
                    let mut line = String::new();
                    for i in 0..len {
                        if !self.has_asccess_permission(pos + i) {
                            return Err(RuntimeError::AccessPermissionDenied {
                                position: pr,
                                op_code: get_op_code_form(op_code, adr),
                                address: pos + i,
                            });
                        }
                        let ch = (self.mem[pos + i] & 0xFF) as u8;
                        line.push(jis_x_201::convert_to_char(ch, true));
                    }
                    writeln!(stdout, "{}", line)?;
                    stdout.flush()?;
                }
                return Ok(());
            }
            0xE0 => {
                // DebugBasicStep id
                if r1 != 0 || r2 != 0 {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, adr),
                    });
                }
                self.basic_step = Some(adr as usize);
                self.basic_step_count += 1;
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
                let value = self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                return Ok(());
            }
            0x11 => {
                // ST r,adr,x
                let r1_value = self.general_registers[r1];
                self.mem[access] = r1_value;
                return Ok(());
            }
            0x20 => {
                // ADDA r,adr,x (OF,SF,ZF)
                let (value, overflow) =
                    (self.general_registers[r1] as i16).overflowing_add(self.mem[access] as i16);
                self.general_registers[r1] = value as u16;
                self.overflow_flag = overflow;
                self.sign_flag = value < 0;
                self.zero_flag = value == 0;
                return Ok(());
            }
            0x22 => {
                // ADDL r,adr,x
                let (value, overflow) =
                    self.general_registers[r1].overflowing_add(self.mem[access]);
                self.general_registers[r1] = value;
                self.overflow_flag = overflow;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                return Ok(());
            }
            0x21 => {
                // SUBA r,adr,x
                let (value, overflow) =
                    (self.general_registers[r1] as i16).overflowing_sub(self.mem[access] as i16);
                self.general_registers[r1] = value as u16;
                self.overflow_flag = overflow;
                self.sign_flag = value < 0;
                self.zero_flag = value == 0;
                return Ok(());
            }
            0x23 => {
                // SUBL r,adr,x
                let (value, overflow) =
                    self.general_registers[r1].overflowing_sub(self.mem[access]);
                self.general_registers[r1] = value;
                self.overflow_flag = overflow;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                return Ok(());
            }
            0x30 => {
                // AND r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] & self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                return Ok(());
            }
            0x31 => {
                // OR r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] | self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                return Ok(());
            }
            0x32 => {
                // XOR r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] ^ self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                return Ok(());
            }
            0x40 => {
                // CPA r,adr,x (OF=0,SF,ZF)
                let r1_value = self.general_registers[r1] as i16;
                let value = self.mem[access] as i16;
                self.overflow_flag = false;
                self.sign_flag = r1_value < value;
                self.zero_flag = r1_value == value;
                return Ok(());
            }
            0x41 => {
                // CPL r,adr,x (OF=0,SF,ZF)
                let r1_value = self.general_registers[r1];
                let value = self.mem[access];
                self.overflow_flag = false;
                self.sign_flag = r1_value < value;
                self.zero_flag = r1_value == value;
                return Ok(());
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
                if !self.sign_flag && !self.zero_flag {
                    self.program_register = access;
                }
            }
            0x61 => {
                // JMI adr,x
                if self.sign_flag {
                    self.program_register = access;
                }
            }
            0x62 => {
                // JNZ adr,x
                if !self.zero_flag {
                    self.program_register = access;
                }
            }
            0x63 => {
                // JZE adr,x
                if self.zero_flag {
                    self.program_register = access;
                }
            }
            0x66 => {
                // JOV adr,x
                if self.overflow_flag {
                    self.program_register = access;
                }
            }
            0x64 => {
                // JUMP adr,x
                self.program_register = access;
            }
            0x80 => {
                // CALL adr,x
                let sp = self.stack_pointer - 1;
                if sp < self.mem.len() - CALLSTACK_MAX_SIZE {
                    return Err(RuntimeError::StackOverflow {
                        position: pr,
                        op_code: get_op_code_form(op_code, adr),
                        stack_pointer: self.stack_pointer,
                    });
                }
                self.stack_pointer = sp;
                let pr = self.program_register;
                self.mem[sp] = pr as u16;
                self.program_register = access;
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
            Ok((debug_info, list)) => {
                if let Some((name, _)) = list.last() {
                    if self.start_point.is_none() {
                        self.start_point = Some(name.clone());
                    }
                    self.basic_info
                        .insert(src_file.into(), (name.clone(), debug_info));
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
                        self.save_casl2_src(&mut path, "EOF", &src, flags.statistics)?;
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
                        self.save_casl2_src(&mut path, "ALLOC", &src, flags.statistics)?;
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
                self.save_casl2_src(&mut path, &name, &casl2_src, flags.statistics)?;
            }

            if self.program_labels.contains_key(&name) {
                continue;
            }

            if let Err(msg) = self.compile(src_file, name, casl2_src) {
                eprintln!("CompileError{{ {} }}", msg);
                return Ok(100);
            }
        }

        self.loaded_files
            .insert(Path::new(src_file).to_string_lossy().into_owned());

        Ok(0)
    }
}

impl Emulator {
    fn save_casl2_src(
        &mut self,
        path: &mut PathBuf,
        name: &str,
        casl2_src: &[casl2::Statement],
        statistics: bool,
    ) -> io::Result<()> {
        let file_name = format!("{}.cas", name);
        path.push(file_name);
        {
            self.loaded_files
                .insert(path.to_string_lossy().into_owned());

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
            self.loaded_files
                .insert(path.to_string_lossy().into_owned());
            let statistics = stat::analyze(casl2_src);
            fs::write(&path, statistics)?;
            eprintln!("生成されたファイル: {}", path.display());
            path.pop();
        }

        Ok(())
    }
}

impl Emulator {
    fn compile_casl2(&mut self, src_file: &str, flags: &Flags, save: bool) -> io::Result<i32> {
        let text = fs::read_to_string(src_file)?;

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

        let casl2_src = if let Some(new_name) = flags.compiler.program_name.as_ref() {
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

        let casl2_src_list = flags.compiler.apply(casl2_src);

        if let Some((name, _)) = casl2_src_list.last() {
            if self.start_point.is_none() {
                self.start_point = Some(name.clone());
            }
        }

        let mut path = flags.create_dst_dir()?;

        if save {
            let backup = src_file.to_string() + ".backup";
            let backup = Path::new(&backup);
            if backup.parent() == Some(&path) {
                if let Some(file) = backup.file_name() {
                    path.push(file);
                    let _copy_size = fs::copy(src_file, &path)?;
                    eprintln!("生成されたファイル: {}", path.display());
                    path.pop();
                }
            }
        }

        for (name, casl2_src) in casl2_src_list {
            if save {
                self.save_casl2_src(&mut path, &name, &casl2_src, flags.statistics)?;
            }

            if self.program_labels.contains_key(&name) {
                continue;
            }

            if let Err(msg) = self.compile(src_file, name, casl2_src) {
                eprintln!("CompileError{{ {} }}", msg);
                return Ok(100);
            }
        }

        self.loaded_files
            .insert(Path::new(src_file).to_string_lossy().into_owned());

        Ok(0)
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
                    let s = jis_x_201::convert_kana_wide_full_to_half(&s);
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
                                let s = jis_x_201::convert_kana_wide_full_to_half(&s);
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
                        let len = command.len();
                        self.mem[pos] = command.first_word();
                        match command {
                            casl2::Command::P { adr, .. } | casl2::Command::A { adr, .. } => {
                                match adr {
                                    casl2::Adr::Dec(d) => self.mem[pos + 1] = *d as u16,
                                    casl2::Adr::Hex(h) => self.mem[pos + 1] = *h,
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                        pos += len;
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
                        let len = command.len();
                        self.mem[pos] = command.first_word();
                        match command {
                            casl2::Command::P { adr, .. } | casl2::Command::A { adr, .. } => {
                                match adr {
                                    casl2::Adr::Dec(d) => self.mem[pos + 1] = *d as u16,
                                    casl2::Adr::Hex(h) => self.mem[pos + 1] = *h,
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                        pos += len;
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
                    self.mem[pos] = cmd.first_word();
                    self.mem[pos + 1] = 0;
                    pos += 2;
                }
            }
            casl2::Command::Rpop => {
                use casl2::Register::*;
                let mut pos = pos;
                for &r in &[Gr7, Gr6, Gr5, Gr4, Gr3, Gr2, Gr1] {
                    let cmd = casl2::Command::Pop { r };
                    self.mem[pos] = cmd.first_word();
                    pos += 1;
                }
            }
            casl2::Command::R { .. }
            | casl2::Command::A { .. }
            | casl2::Command::P { .. }
            | casl2::Command::Pop { .. }
            | casl2::Command::Ret
            | casl2::Command::Nop => {
                self.mem[pos] = command.first_word();
            }
            casl2::Command::DebugBasicStep { id } => {
                self.mem[pos] = command.first_word();
                self.mem[pos + 1] = *id as u16;
            }
        }
    }
}

fn get_op_code_form(op_code: u16, adr: u16) -> String {
    let r1 = (op_code >> 4) & 0xF;
    let r2 = op_code & 0xF;
    match (op_code >> 8) & 0xFF {
        0x14 => format!("#{:04X}: LD    GR{},GR{}", op_code, r1, r2),
        0x24 => format!("#{:04X}: ADDA  GR{},GR{}", op_code, r1, r2),
        0x26 => format!("#{:04X}: ADDL  GR{},GR{}", op_code, r1, r2),
        0x25 => format!("#{:04X}: SUBA  GR{},GR{}", op_code, r1, r2),
        0x27 => format!("#{:04X}: SUBL  GR{},GR{}", op_code, r1, r2),
        0x34 => format!("#{:04X}: AND   GR{},GR{}", op_code, r1, r2),
        0x35 => format!("#{:04X}: OR    GR{},GR{}", op_code, r1, r2),
        0x36 => format!("#{:04X}: XOR   GR{},GR{}", op_code, r1, r2),
        0x44 => format!("#{:04X}: CPA   GR{},GR{}", op_code, r1, r2),
        0x45 => format!("#{:04X}: CPL   GR{},GR{}", op_code, r1, r2),
        0x81 => format!("#{:04X}: RET", op_code),
        0x00 => format!("#{:04X}: NOP", op_code),
        0x71 => format!("#{:04X}: POP   GR{}", op_code, r1),
        0xE0 => format!("#{:04X}: DEBUGBASICSTEP {}", op_code, adr),
        code if r2 == 0 => match code {
            0x12 => format!("#{:04X}: LAD   GR{},#{:04X}", op_code, r1, adr),
            0x70 => format!("#{:04X}: PUSH  #{:04X}", op_code, adr),
            0xF0 => format!("#{:04X}: SVC   #{:04X}", op_code, adr),
            0x10 => format!("#{:04X}: LD    GR{},#{:04X}", op_code, r1, adr),
            0x11 => format!("#{:04X}: ST    GR{},#{:04X}", op_code, r1, adr),
            0x20 => format!("#{:04X}: ADDA  GR{},#{:04X}", op_code, r1, adr),
            0x22 => format!("#{:04X}: ADDL  GR{},#{:04X}", op_code, r1, adr),
            0x21 => format!("#{:04X}: SUBA  GR{},#{:04X}", op_code, r1, adr),
            0x23 => format!("#{:04X}: SUBL  GR{},#{:04X}", op_code, r1, adr),
            0x30 => format!("#{:04X}: AND   GR{},#{:04X}", op_code, r1, adr),
            0x31 => format!("#{:04X}: OR    GR{},#{:04X}", op_code, r1, adr),
            0x32 => format!("#{:04X}: XOR   GR{},#{:04X}", op_code, r1, adr),
            0x40 => format!("#{:04X}: CPA   GR{},#{:04X}", op_code, r1, adr),
            0x41 => format!("#{:04X}: CPL   GR{},#{:04X}", op_code, r1, adr),
            0x50 => format!("#{:04X}: SLA   GR{},#{:04X}", op_code, r1, adr),
            0x51 => format!("#{:04X}: SRA   GR{},#{:04X}", op_code, r1, adr),
            0x52 => format!("#{:04X}: SLL   GR{},#{:04X}", op_code, r1, adr),
            0x53 => format!("#{:04X}: SRL   GR{},#{:04X}", op_code, r1, adr),
            0x65 => format!("#{:04X}: JPL   #{:04X}", op_code, adr),
            0x61 => format!("#{:04X}: JMI   #{:04X}", op_code, adr),
            0x62 => format!("#{:04X}: JNZ   #{:04X}", op_code, adr),
            0x63 => format!("#{:04X}: JZE   #{:04X}", op_code, adr),
            0x66 => format!("#{:04X}: JOV   #{:04X}", op_code, adr),
            0x64 => format!("#{:04X}: JUMP  #{:04X}", op_code, adr),
            0x80 => format!("#{:04X}: CALL  #{:04X}", op_code, adr),
            _ => format!("#{:04X}: UNKNOWN", op_code),
        },
        0x12 => format!("#{:04X}: LAD   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x70 => format!("#{:04X}: PUSH  #{:04X},GR{}", op_code, adr, r2),
        0xF0 => format!("#{:04X}: SVC   #{:04X},GR{}", op_code, adr, r2),
        0x10 => format!("#{:04X}: LD    GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x11 => format!("#{:04X}: ST    GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x20 => format!("#{:04X}: ADDA  GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x22 => format!("#{:04X}: ADDL  GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x21 => format!("#{:04X}: SUBA  GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x23 => format!("#{:04X}: SUBL  GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x30 => format!("#{:04X}: AND   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x31 => format!("#{:04X}: OR    GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x32 => format!("#{:04X}: XOR   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x40 => format!("#{:04X}: CPA   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x41 => format!("#{:04X}: CPL   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x50 => format!("#{:04X}: SLA   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x51 => format!("#{:04X}: SRA   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x52 => format!("#{:04X}: SLL   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x53 => format!("#{:04X}: SRL   GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x65 => format!("#{:04X}: JPL   #{:04X},GR{}", op_code, adr, r2),
        0x61 => format!("#{:04X}: JMI   #{:04X},GR{}", op_code, adr, r2),
        0x62 => format!("#{:04X}: JNZ   #{:04X},GR{}", op_code, adr, r2),
        0x63 => format!("#{:04X}: JZE   #{:04X},GR{}", op_code, adr, r2),
        0x66 => format!("#{:04X}: JOV   #{:04X},GR{}", op_code, adr, r2),
        0x64 => format!("#{:04X}: JUMP  #{:04X},GR{}", op_code, adr, r2),
        0x80 => format!("#{:04X}: CALL  #{:04X},GR{}", op_code, adr, r2),
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

        0x71 => 1, //format!("#{:04X}: POP GR{}", op_code, r1),
        0x81 => 1, //format!("#{:04X}: RET",op_code),
        0x00 => 1, //format!("#{:04X}: NOP",op_code),

        0xE0 => 2, //format!("#{:04X}: DEBUGBASICSTEP {}", op_code, adr),

        0x12 => 2, //format!("#{:04X}: LAD GR{},#{:04X},GR{}", op_code, r1, adr, r2),
        0x70 => 2, //format!("#{:04X}: PUSH #{:04X},GR{}",op_code,  adr, r2),
        0xF0 => 2, //format!("#{:04X}: SVC #{:04X},GR{}", op_code, adr, r2),
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
            casl2::Command::DebugBasicStep { .. } => 0xE0,
        }
    }

    fn first_word(&self) -> u16 {
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
            casl2::Command::Ret | casl2::Command::Nop | casl2::Command::DebugBasicStep { .. } => {
                self.op_code() << 8
            }
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
            casl2::Command::DebugBasicStep { .. } => 2,
        }
    }
}
