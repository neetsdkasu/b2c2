use self::emu::*;
use self::misc::*;
use self::utils::*;
use crate::casl2;
use crate::compiler;
use crate::jis_x_201;
use crate::parser;
use crate::stat;
use crate::tokenizer;
use crate::Flags;
use ext::*;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};

mod emu;
mod ext;
mod misc;
mod utils;

const RET_OP_CODE: u16 = 0x8100;
const REQUEST_CONTINUE: i32 = 0x300_0000;
const REQUEST_BREAK: i32 = 0x400_0000;
const REQUEST_QUIT: i32 = 99;
const DEFAULT_COMET2_LIMIT_ON_BASIC_MODE: u64 = 1_000_000_000_000;

#[derive(Clone, Copy)]
enum RunMode {
    Step(u64),
    GoToBreakPoint(u64),
    SkipSubroutine(u64),
    BasicStep {
        basic_step_count: u64,
        comet2_limit: u64,
    },
    GoToBasicBreakPoint {
        basic_limit: u64,
        comet2_limit: u64,
    },
    SkipBasicSubroutine {
        basic_limit: u64,
        comet2_limit: u64,
    },
}

#[derive(Clone, Copy)]
enum DebugMode {
    Basic,
    Casl2,
}

struct State {
    err: Option<RuntimeError>,
    run_mode: RunMode,
    start_point: Option<usize>,
    default_cmd: Vec<Option<String>>,
    debug_mode: DebugMode,
}

enum Code {
    Casl2(casl2::Command),
    In(String),
    Out(String),
}

enum ViewType {
    Bool,
    Int,
    Str,
}

enum ExtendedLabel {
    GlobalLabel(String),
    LocalLabel(String, String),
    GeneralRegister(casl2::Register),
    ProgramRegister,
    StackPointer,
    DecConst(u16),
    HexConst(u16),
    Load(Box<ExtendedLabel>),
    Sum(Box<ExtendedLabel>, Box<ExtendedLabel>),
    Diff(Box<ExtendedLabel>, Box<ExtendedLabel>),
}

enum Value {
    Int(u16),
    Str(String),
}

pub fn run_nonstep(src_file: String, flags: Flags) -> io::Result<i32> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let mut emu = Emulator::new();

    emu.debug_mode = false;

    match if src_file.to_ascii_lowercase().ends_with(".cas") {
        emu.compile_casl2(&src_file, &flags, false)
    } else {
        emu.compile_basic(&src_file, &flags, false)
    } {
        Ok(0) => {}
        result => return result,
    }

    if emu.start_point.is_none() {
        eprintln!("入力ファイルが正しくありません");
        return Ok(101);
    }

    if !emu.unknown_labels.is_empty() {
        // 未解決ラベルを解決する処理を入れる
        let src_dir = Path::new(&src_file).parent().unwrap();
        match auto_resolve_files(&mut emu, &mut stdin, &mut stdout, &src_dir, &flags) {
            Ok(0) => {}
            Ok(REQUEST_QUIT) => return Ok(0),
            result => return result,
        }
    }

    emu.init_to_start(None);

    loop {
        match emu.step_through_code(&mut stdin, &mut stdout) {
            Ok(_) => {}
            Err(RuntimeError::IoError(error)) => return Err(error),
            Err(error) => {
                eprintln!("{}", error);
                if matches!(error, RuntimeError::NormalTermination { .. }) {
                    return Ok(0);
                } else {
                    return Ok(REQUEST_QUIT);
                }
            }
        }
    }
}

pub fn run_basic(src_file: String, flags: Flags) -> io::Result<i32> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    loop {
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

        if !emu.unknown_labels.is_empty() {
            // 未解決ラベルを解決する処理を入れる
            let src_dir = Path::new(&src_file).parent().unwrap();
            match resolve_files(&mut emu, &mut stdin, &mut stdout, &src_dir, &flags) {
                Ok(0) => {}
                Ok(REQUEST_QUIT) => return Ok(0),
                result => return result,
            }
        }

        emu.init_to_start(None);

        let mut state = State {
            err: None,
            run_mode: RunMode::BasicStep {
                basic_step_count: 1,
                comet2_limit: DEFAULT_COMET2_LIMIT_ON_BASIC_MODE,
            },
            start_point: None,
            default_cmd: vec![None, None],
            debug_mode: DebugMode::Basic,
        };

        loop {
            writeln!(stdout)?;
            match state.debug_mode {
                DebugMode::Basic => {
                    show_state_basic(&emu, &mut stdout, &state)?;
                    if let Some((file, label, _)) = emu.get_current_program() {
                        if emu.basic_info.contains_key(file) {
                            show_var_for_basic(&emu, &mut stdout, Some(label.as_str()))?;
                        }
                    }

                    match interactive_basic(&mut emu, &mut stdin, &mut stdout, &mut state) {
                        Ok(0) => {}
                        Ok(REQUEST_CONTINUE) => {
                            if matches!(state.debug_mode, DebugMode::Casl2) {
                                writeln!(stdout, "CASL2デバッグモードに切り替えます")?;
                            }
                            continue;
                        }
                        Ok(REQUEST_BREAK) => break,
                        Ok(REQUEST_QUIT) => return Ok(0),
                        result => return result,
                    }
                }
                DebugMode::Casl2 => {
                    show_state(&emu, &mut stdout, &state)?;
                    show_reg(&emu, &mut stdout)?;

                    match interactive_casl2(&mut emu, &mut stdin, &mut stdout, &mut state) {
                        Ok(0) => {}
                        Ok(REQUEST_CONTINUE) => {
                            if matches!(state.debug_mode, DebugMode::Basic) {
                                writeln!(stdout, "BASICデバッグモードに切り替えます")?;
                            }
                            continue;
                        }
                        Ok(REQUEST_BREAK) => break,
                        Ok(REQUEST_QUIT) => return Ok(0),
                        result => return result,
                    }
                }
            }

            writeln!(stdout)?;

            if state.err.is_some() {
                writeln!(stdout, "プログラムは終了状態です")?;
            } else {
                let result = match state.run_mode {
                    RunMode::SkipSubroutine(limit) => {
                        do_skip_subroutine(&mut emu, &mut stdin, &mut stdout, &mut state, limit)
                    }
                    RunMode::GoToBreakPoint(limit) => {
                        do_go_to_breakpoint(&mut emu, &mut stdin, &mut stdout, &mut state, limit)
                    }
                    RunMode::Step(count) => {
                        do_step(&mut emu, &mut stdin, &mut stdout, &mut state, count)
                    }
                    RunMode::BasicStep {
                        basic_step_count,
                        comet2_limit,
                    } => do_step_basic(
                        &mut emu,
                        &mut stdin,
                        &mut stdout,
                        &mut state,
                        basic_step_count,
                        comet2_limit,
                    ),
                    RunMode::GoToBasicBreakPoint {
                        basic_limit,
                        comet2_limit,
                    } => do_go_to_breakpoint_for_basic(
                        &mut emu,
                        &mut stdin,
                        &mut stdout,
                        &mut state,
                        basic_limit,
                        comet2_limit,
                    ),
                    RunMode::SkipBasicSubroutine {
                        basic_limit,
                        comet2_limit,
                    } => do_skip_subroutine_for_basic(
                        &mut emu,
                        &mut stdin,
                        &mut stdout,
                        &mut state,
                        basic_limit,
                        comet2_limit,
                    ),
                };
                match result {
                    Ok(0) => {}
                    result => return result,
                }
            }
        }
    }
}

pub fn run_casl2(src_file: String, flags: Flags) -> io::Result<i32> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    loop {
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

        if !emu.unknown_labels.is_empty() {
            // 未解決ラベルを解決する処理を入れる
            let src_dir = Path::new(&src_file).parent().unwrap();
            match resolve_files(&mut emu, &mut stdin, &mut stdout, &src_dir, &flags) {
                Ok(0) => {}
                Ok(REQUEST_QUIT) => return Ok(0),
                result => return result,
            }
        }

        emu.init_to_start(None);

        let mut state = State {
            err: None,
            run_mode: RunMode::Step(1),
            start_point: None,
            default_cmd: vec![None, None],
            debug_mode: DebugMode::Casl2,
        };

        loop {
            writeln!(stdout)?;
            show_state(&emu, &mut stdout, &state)?;
            show_reg(&emu, &mut stdout)?;

            match interactive_casl2(&mut emu, &mut stdin, &mut stdout, &mut state) {
                Ok(0) => {}
                Ok(REQUEST_CONTINUE) => {
                    if matches!(state.debug_mode, DebugMode::Basic) {
                        writeln!(stdout, "BASICデバッグモードは使用できません")?;
                        state.debug_mode = DebugMode::Casl2;
                    }
                    continue;
                }
                Ok(REQUEST_BREAK) => break,
                Ok(REQUEST_QUIT) => return Ok(0),
                result => return result,
            }

            writeln!(stdout)?;

            if state.err.is_some() {
                writeln!(stdout, "プログラムは終了状態です")?;
            } else {
                let result = match state.run_mode {
                    RunMode::SkipSubroutine(limit) => {
                        do_skip_subroutine(&mut emu, &mut stdin, &mut stdout, &mut state, limit)
                    }
                    RunMode::GoToBreakPoint(limit) => {
                        do_go_to_breakpoint(&mut emu, &mut stdin, &mut stdout, &mut state, limit)
                    }
                    RunMode::Step(count) => {
                        do_step(&mut emu, &mut stdin, &mut stdout, &mut state, count)
                    }
                    RunMode::BasicStep { .. }
                    | RunMode::GoToBasicBreakPoint { .. }
                    | RunMode::SkipBasicSubroutine { .. } => {
                        unreachable!("BUG")
                    }
                };
                match result {
                    Ok(0) => {}
                    result => return result,
                }
            }
        }
    }
}

//
// ブレークポイントまで実行実行 (BASIC)
//
fn do_go_to_breakpoint_for_basic<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    state: &mut State,
    basic_limit: u64,
    comet2_limit: u64,
) -> io::Result<i32> {
    let mut step_limit = basic_limit;
    let mut limit = comet2_limit;
    let mut reach = false;
    while step_limit > 0 && limit > 0 {
        limit -= 1;
        match emu.step_through_code(stdin, stdout) {
            Ok(_) => {}
            Err(RuntimeError::IoError(error)) => return Err(error),
            Err(error) => {
                let last_pos = emu.last_run_position;
                let last_op_code = emu.mem[last_pos];
                if emu.break_points[last_pos] {
                    reach = true;
                } else if get_op_code_size(last_op_code) == 2 {
                    reach = emu.break_points[last_pos + 1];
                }
                state.err = Some(error);
                break;
            }
        }
        if emu.basic_step.is_some() {
            step_limit -= 1;
            limit = comet2_limit;
            if emu.break_points[emu.last_run_position] {
                reach = true;
                break;
            }
        }
    }
    writeln!(stdout)?;
    if reach {
        writeln!(stdout, "ブレークポイントに到達しました")?;
    } else if state.err.is_some() {
        if matches!(state.err, Some(RuntimeError::NormalTermination { .. })) {
            writeln!(stdout, "ブレークポイント到達前にプログラムが終了しました")?;
        } else {
            writeln!(stdout, "実行はエラーで停止しました")?;
        }
    } else if step_limit == 0 {
        writeln!(
            stdout,
            "指定ステップ数が終わる前にステップ数の制限({})に達して停止しました",
            basic_limit
        )?;
    } else if limit == 0 {
        writeln!(
            stdout,
            "指定ステップ数が終わる前にCOMET2ステップ数制限({})に達して停止しました",
            comet2_limit
        )?;
    }
    Ok(0)
}

//
// ステップ実行 (BASIC)
//
fn do_step_basic<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    state: &mut State,
    mut basic_step_count: u64,
    comet2_limit: u64,
) -> io::Result<i32> {
    let mut limit = comet2_limit;
    while basic_step_count > 0 && limit > 0 {
        limit -= 1;
        match emu.step_through_code(stdin, stdout) {
            Ok(_) => {}
            Err(RuntimeError::IoError(error)) => return Err(error),
            Err(error) => {
                state.err = Some(error);
                basic_step_count -= 1;
                break;
            }
        }
        if emu.basic_step.is_some() {
            basic_step_count -= 1;
            limit = comet2_limit;
        }
    }
    if basic_step_count != 0 {
        if state.err.is_some() {
            if matches!(state.err, Some(RuntimeError::NormalTermination { .. })) {
                writeln!(stdout, "指定ステップ数が終わる前にプログラムが終了しました")?;
            } else {
                writeln!(stdout, "指定ステップ数が終わる前にエラーで停止しました")?;
            }
        } else if limit == 0 {
            writeln!(
                stdout,
                "指定ステップ数が終わる前にCOMET2ステップ数制限({})に達して停止しました",
                comet2_limit
            )?;
        }
    }
    Ok(0)
}

//
// スキップ (BASIC)
//
fn do_skip_subroutine_for_basic<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    state: &mut State,
    basic_limit: u64,
    comet2_limit: u64,
) -> io::Result<i32> {
    let nest = emu.program_stack.len();
    let mut step_limit = basic_limit;
    let mut limit = comet2_limit;
    let mut reach = false;
    while step_limit > 0 && limit > 0 {
        limit -= 1;
        match emu.step_through_code(stdin, stdout) {
            Ok(_) => {}
            Err(RuntimeError::IoError(error)) => return Err(error),
            Err(error) => {
                reach = emu.program_stack.len() < nest;
                state.err = Some(error);
                break;
            }
        }
        if emu.basic_step.is_some() {
            step_limit -= 1;
            limit = comet2_limit;
            if emu.program_stack.len() < nest {
                reach = true;
                break;
            }
        }
    }
    writeln!(stdout)?;
    if reach {
        writeln!(stdout, "スキップ実行はサブルーチンを脱出し停止しました")?;
    } else if state.err.is_some() {
        writeln!(stdout, "スキップ実行はエラーで停止しました")?;
    } else if step_limit == 0 {
        writeln!(
            stdout,
            "スキップ実行はステップ制限数({})に到達で停止しました",
            basic_limit
        )?;
    } else if limit == 0 {
        writeln!(
            stdout,
            "スキップ実行はCOMET2ステップ制限数({})に到達で停止しました",
            comet2_limit
        )?;
    }
    Ok(0)
}

//
// ステップ実行 (CASL2)
//
fn do_step<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    state: &mut State,
    mut limit: u64,
) -> io::Result<i32> {
    while limit > 0 {
        limit -= 1;
        match emu.step_through_code(stdin, stdout) {
            Ok(_) => {}
            Err(RuntimeError::IoError(error)) => return Err(error),
            Err(error) => {
                state.err = Some(error);
                break;
            }
        }
    }
    if state.err.is_some() && limit != 0 {
        if matches!(state.err, Some(RuntimeError::NormalTermination { .. })) {
            writeln!(stdout, "指定ステップ数が終わる前にプログラムが終了しました")?;
        } else {
            writeln!(stdout, "指定ステップ数が終わる前にエラーで停止しました")?;
        }
    }
    Ok(0)
}

//
// ブレークポイントまで実行 (CASL2)
//
fn do_go_to_breakpoint<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    state: &mut State,
    step_limit: u64,
) -> io::Result<i32> {
    let mut limit = step_limit;
    let mut reach = false;
    while limit > 0 {
        limit -= 1;
        match emu.step_through_code(stdin, stdout) {
            Ok(_) => {
                let last_pos = emu.last_run_position;
                let last_op_code = emu.mem[last_pos];
                if emu.break_points[last_pos]
                    || (get_op_code_size(last_op_code) == 2 && emu.break_points[last_pos + 1])
                {
                    reach = true;
                    break;
                }
            }
            Err(RuntimeError::IoError(error)) => return Err(error),
            Err(error) => {
                let last_pos = emu.last_run_position;
                let last_op_code = emu.mem[last_pos];
                if emu.break_points[last_pos] {
                    reach = true;
                } else if get_op_code_size(last_op_code) == 2 {
                    reach = emu.break_points[last_pos + 1];
                }
                state.err = Some(error);
                break;
            }
        }
    }
    writeln!(stdout)?;
    if reach {
        writeln!(stdout, "ブレークポイントに到達しました")?;
    } else if state.err.is_some() {
        if matches!(state.err, Some(RuntimeError::NormalTermination { .. })) {
            writeln!(stdout, "ブレークポイント到達前にプログラムが終了しました")?;
        } else {
            writeln!(stdout, "実行はエラーで停止しました")?;
        }
    } else if limit == 0 {
        writeln!(
            stdout,
            "実行はステップ制限数({})に到達で停止しました",
            step_limit
        )?;
    }
    Ok(0)
}

//
// スキップ (CASL2)
//
fn do_skip_subroutine<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    state: &mut State,
    step_limit: u64,
) -> io::Result<i32> {
    let nest = emu.program_stack.len();
    let mut limit = step_limit;
    let mut reach = false;
    while limit > 0 {
        limit -= 1;
        match emu.step_through_code(stdin, stdout) {
            Ok(_) => {
                let last_pos = emu.last_run_position;
                let last_op_code = emu.mem[last_pos];
                if last_op_code == RET_OP_CODE && nest == emu.program_stack.len() + 1 {
                    reach = true;
                    break;
                }
            }
            Err(RuntimeError::IoError(error)) => return Err(error),
            Err(error) => {
                let last_pos = emu.last_run_position;
                let last_op_code = emu.mem[last_pos];
                reach = last_op_code == RET_OP_CODE && nest == emu.program_stack.len() + 1;
                state.err = Some(error);
                break;
            }
        }
    }
    writeln!(stdout)?;
    if reach {
        writeln!(stdout, "スキップ実行はRETに到達し停止しました")?;
    } else if state.err.is_some() {
        writeln!(stdout, "スキップ実行はエラーで停止しました")?;
    } else if limit == 0 {
        writeln!(
            stdout,
            "スキップ実行はステップ制限数({})に到達で停止しました",
            step_limit
        )?;
    }
    Ok(0)
}

//
// 対話処理 (BASIC)
//
fn interactive_basic<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    state: &mut State,
) -> io::Result<i32> {
    let mut line = String::new();

    let command_list = {
        use std::fmt::Write;
        let mut lines = String::new();
        let mut tmp = String::new();
        for cmd in &[
            "default-cmd",
            "fill-arr",
            "help",
            "list-files",
            "mode",
            "quit",
            "remove-breakpoint",
            "reset",
            "restart",
            "run",
            "set-breakpoint",
            "set-by-file",
            "set-elem",
            "set-len",
            "set-var",
            "show-execute-stat",
            "show-src",
            "show-state",
            "show-var",
            "skip",
            "step",
        ] {
            if tmp.len() + cmd.len() + 1 >= 80 {
                writeln!(&mut lines, "{}", tmp).unwrap();
                tmp.clear();
            }
            tmp.push_str(cmd);
            tmp.push(' ');
        }
        if !tmp.is_empty() {
            writeln!(&mut lines, "{}", tmp).unwrap();
        }
        lines
    };

    loop {
        writeln!(stdout)?;
        writeln!(stdout, "使用可能なデバッガコマンド:")?;
        writeln!(stdout, "{}", command_list)?;
        writeln!(stdout)?;
        write!(stdout, "[BASIC] > ")?;
        stdout.flush()?;
        line.clear();
        if stdin.read_line(&mut line)? == 0 {
            eprintln!("入力がキャンセルされました");
            io::stderr().flush()?;
            writeln!(stdout, "テスト実行を中止します")?;
            return Ok(REQUEST_QUIT);
        }
        let mut line = line.trim().to_string();
        if line.is_empty() {
            if let Some(defcmd) = state.default_cmd[DebugMode::Basic as usize].as_ref() {
                line = defcmd.clone();
            }
        }
        let mut cmd_and_param = line.splitn(2, ' ').map(|s| s.trim());
        let cmd = cmd_and_param.next().unwrap();
        match cmd {
            "default-cmd" => {
                if let Some(defcmd) = cmd_and_param.next() {
                    if "none".eq_ignore_ascii_case(defcmd) {
                        state.default_cmd[DebugMode::Basic as usize] = None;
                    } else {
                        state.default_cmd[DebugMode::Basic as usize] = Some(defcmd.to_string());
                    }
                }
                if let Some(defcmd) = state.default_cmd[DebugMode::Basic as usize].as_ref() {
                    writeln!(stdout, "デフォルトのデバッガコマンド: {}", defcmd)?;
                } else {
                    writeln!(stdout, "デフォルトのデバッガコマンド: none")?;
                }
            }
            "fill-arr" => fill_arr(emu, stdout, cmd_and_param.next())?,
            "help" => show_command_help_for_basic(cmd_and_param.next(), stdout)?,
            "list-files" => list_files(emu, stdout)?,
            "mode" => {
                if let Some(param) = cmd_and_param.next() {
                    if "casl2".eq_ignore_ascii_case(param) {
                        state.debug_mode = DebugMode::Casl2;
                        return Ok(REQUEST_CONTINUE);
                    } else if "basic".eq_ignore_ascii_case(param) {
                        writeln!(stdout, "現在BASICデバッグモードです")?;
                    } else {
                        writeln!(stdout, "引数が不正です")?;
                    }
                } else {
                    writeln!(stdout, "現在BASICデバッグモードです")?;
                }
            }
            "quit" => {
                writeln!(stdout, "テスト実行を中止します")?;
                return Ok(REQUEST_QUIT);
            }
            "remove-breakpoint" => {
                set_breakpoint_for_basic(emu, stdout, cmd_and_param.next(), false)?
            }
            "reset" => {
                writeln!(stdout, "エミュレータをリセットします")?;
                writeln!(stdout)?;
                return Ok(REQUEST_BREAK);
            }
            "restart" => {
                emu.init_to_start(None);
                state.err = None;
                writeln!(stdout, "プログラムをリスタートします")?;
                writeln!(stdout)?;
                return Ok(REQUEST_CONTINUE);
            }
            "run" => {
                if let Some(param) = cmd_and_param.next() {
                    let mut iter = param.splitn(2, ' ').map(|s| s.trim());
                    match iter.next().unwrap().parse::<u64>() {
                        Ok(basic_limit) if basic_limit > 0 => {
                            if let Some(rest) = iter.next() {
                                match rest.parse::<u64>() {
                                    Ok(comet2_limit) if comet2_limit > 0 => {
                                        state.run_mode = RunMode::GoToBasicBreakPoint {
                                            basic_limit,
                                            comet2_limit,
                                        };
                                        return Ok(0);
                                    }
                                    _ => writeln!(stdout, "引数が不正です")?,
                                }
                            } else {
                                state.run_mode = RunMode::GoToBasicBreakPoint {
                                    basic_limit,
                                    comet2_limit: DEFAULT_COMET2_LIMIT_ON_BASIC_MODE,
                                };
                                return Ok(0);
                            }
                        }
                        _ => writeln!(stdout, "引数が不正です")?,
                    };
                } else {
                    state.run_mode = RunMode::GoToBasicBreakPoint {
                        basic_limit: 10000,
                        comet2_limit: DEFAULT_COMET2_LIMIT_ON_BASIC_MODE,
                    };
                    return Ok(0);
                }
            }
            "set-breakpoint" => set_breakpoint_for_basic(emu, stdout, cmd_and_param.next(), true)?,
            "set-by-file" => todo!(),
            "set-elem" => set_elem(emu, stdout, cmd_and_param.next())?,
            "set-len" => set_len(emu, stdout, cmd_and_param.next())?,
            "set-var" => set_var(emu, stdout, cmd_and_param.next())?,
            "show-execute-stat" => show_execute_stat(emu, stdout, cmd_and_param.next())?,
            "show-src" => show_src_basic(emu, stdout, cmd_and_param.next())?,
            "show-state" => show_state_basic(emu, stdout, state)?,
            "show-var" => show_var_for_basic(emu, stdout, cmd_and_param.next())?,
            "skip" => {
                if let Some(param) = cmd_and_param.next() {
                    let mut iter = param.splitn(2, ' ').map(|s| s.trim());
                    match iter.next().unwrap().parse::<u64>() {
                        Ok(basic_limit) if basic_limit > 0 => {
                            if let Some(rest) = iter.next() {
                                match rest.parse::<u64>() {
                                    Ok(comet2_limit) if comet2_limit > 0 => {
                                        state.run_mode = RunMode::SkipBasicSubroutine {
                                            basic_limit,
                                            comet2_limit,
                                        };
                                        return Ok(0);
                                    }
                                    _ => writeln!(stdout, "引数が不正です")?,
                                }
                            } else {
                                state.run_mode = RunMode::SkipBasicSubroutine {
                                    basic_limit,
                                    comet2_limit: DEFAULT_COMET2_LIMIT_ON_BASIC_MODE,
                                };
                                return Ok(0);
                            }
                        }
                        _ => writeln!(stdout, "引数が不正です")?,
                    };
                } else {
                    state.run_mode = RunMode::SkipBasicSubroutine {
                        basic_limit: 10000,
                        comet2_limit: DEFAULT_COMET2_LIMIT_ON_BASIC_MODE,
                    };
                    return Ok(0);
                }
            }
            "step" | "s" => {
                if let Some(param) = cmd_and_param.next() {
                    let mut iter = param.splitn(2, ' ').map(|s| s.trim());
                    match iter.next().unwrap().parse::<u64>() {
                        Ok(basic_step_count) if basic_step_count > 0 => {
                            if let Some(rest) = iter.next() {
                                match rest.parse::<u64>() {
                                    Ok(comet2_limit) if comet2_limit > 0 => {
                                        state.run_mode = RunMode::BasicStep {
                                            basic_step_count,
                                            comet2_limit,
                                        };
                                        return Ok(0);
                                    }
                                    _ => writeln!(stdout, "引数が不正です")?,
                                }
                            } else {
                                state.run_mode = RunMode::BasicStep {
                                    basic_step_count,
                                    comet2_limit: DEFAULT_COMET2_LIMIT_ON_BASIC_MODE,
                                };
                                return Ok(0);
                            }
                        }
                        _ => writeln!(stdout, "引数が不正です")?,
                    };
                } else {
                    state.run_mode = RunMode::BasicStep {
                        basic_step_count: 1,
                        comet2_limit: DEFAULT_COMET2_LIMIT_ON_BASIC_MODE,
                    };
                    return Ok(0);
                }
            }
            _ => {
                writeln!(stdout, "コマンドが正しくありません")?;
            }
        }
    }
}

//
// 対話処理 (CASL2)
//
fn interactive_casl2<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    state: &mut State,
) -> io::Result<i32> {
    let mut line = String::new();

    let command_list = {
        use std::fmt::Write;
        let mut lines = String::new();
        let mut tmp = String::new();
        for cmd in vec![
            "add-dc",
            "add-ds",
            "copy-mem",
            "default-cmd",
            "dump-code",
            "dump-mem",
            "fill-mem",
            "find-code",
            "find-src",
            "find-value",
            "help",
            "list-files",
            "mode",
            "quit",
            "remove-breakpoint",
            "reset",
            "restart",
            "run",
            "set-breakpoint",
            "set-by-file",
            "set-label",
            "set-mem",
            "set-reg",
            "set-start",
            "show-labels",
            "show-mem",
            "show-mem-stat",
            "show-reg",
            "show-src",
            "show-state",
            "show-var",
            "skip",
            "step",
            "write-code",
        ] {
            if tmp.len() + cmd.len() + 1 >= 80 {
                writeln!(&mut lines, "{}", tmp).unwrap();
                tmp.clear();
            }
            tmp.push_str(cmd);
            tmp.push(' ');
        }
        if !tmp.is_empty() {
            writeln!(&mut lines, "{}", tmp).unwrap();
        }
        lines
    };

    loop {
        writeln!(stdout)?;
        writeln!(stdout, "使用可能なデバッガコマンド:")?;
        writeln!(stdout, "{}", command_list)?;
        writeln!(stdout)?;
        write!(stdout, "[CASL2] > ")?;
        stdout.flush()?;
        line.clear();
        if stdin.read_line(&mut line)? == 0 {
            eprintln!("入力がキャンセルされました");
            io::stderr().flush()?;
            writeln!(stdout, "テスト実行を中止します")?;
            return Ok(REQUEST_QUIT);
        }
        let mut line = line.trim().to_string();
        if line.is_empty() {
            if let Some(defcmd) = state.default_cmd[DebugMode::Casl2 as usize].as_ref() {
                line = defcmd.clone();
            }
        }
        let mut cmd_and_param = line.splitn(2, ' ').map(|s| s.trim());
        let cmd = cmd_and_param.next().unwrap();
        match cmd {
            "add-dc" => add_dc(emu, stdout, cmd_and_param.next())?,
            "add-ds" => add_ds(emu, stdout, cmd_and_param.next())?,
            "copy-mem" => copy_mem(emu, stdout, cmd_and_param.next())?,
            "default-cmd" => {
                if let Some(defcmd) = cmd_and_param.next() {
                    if "none".eq_ignore_ascii_case(defcmd) {
                        state.default_cmd[DebugMode::Casl2 as usize] = None;
                    } else {
                        state.default_cmd[DebugMode::Casl2 as usize] = Some(defcmd.to_string());
                    }
                }
                if let Some(defcmd) = state.default_cmd[DebugMode::Casl2 as usize].as_ref() {
                    writeln!(stdout, "デフォルトのデバッガコマンド: {}", defcmd)?;
                } else {
                    writeln!(stdout, "デフォルトのデバッガコマンド: none")?;
                }
            }
            "dump-code" => dump_code(emu, stdout, cmd_and_param.next())?,
            "dump-mem" => dump_mem(emu, stdout, cmd_and_param.next())?,
            "fill-mem" => fill_mem(emu, stdout, cmd_and_param.next())?,
            "find-code" => find_code(emu, stdout, cmd_and_param.next())?,
            "find-src" => find_src(emu, stdout, cmd_and_param.next())?,
            "find-value" => find_value(emu, stdout, cmd_and_param.next())?,
            "help" => show_command_help(cmd_and_param.next(), stdout)?,
            "list-files" => list_files(emu, stdout)?,
            "mode" => {
                if let Some(param) = cmd_and_param.next() {
                    if "basic".eq_ignore_ascii_case(param) {
                        state.debug_mode = DebugMode::Basic;
                        return Ok(REQUEST_CONTINUE);
                    } else if "casl2".eq_ignore_ascii_case(param) {
                        writeln!(stdout, "現在CASL2デバッグモードです")?;
                    } else {
                        writeln!(stdout, "引数が不正です")?;
                    }
                } else {
                    writeln!(stdout, "現在CASL2デバッグモードです")?;
                }
            }
            "quit" => {
                writeln!(stdout, "テスト実行を中止します")?;
                return Ok(REQUEST_QUIT);
            }
            "remove-breakpoint" => set_breakpoint(emu, stdout, cmd_and_param.next(), false)?,
            "reset" => {
                writeln!(stdout, "エミュレータをリセットします")?;
                writeln!(stdout)?;
                return Ok(REQUEST_BREAK);
            }
            "restart" => {
                emu.init_to_start(state.start_point);
                state.err = None;
                writeln!(stdout, "プログラムをリスタートします")?;
                writeln!(stdout)?;
                return Ok(REQUEST_CONTINUE);
            }
            "run" => {
                if let Some(s) = cmd_and_param.next() {
                    match s.parse::<u64>() {
                        Ok(v) if v > 0 => state.run_mode = RunMode::GoToBreakPoint(v),
                        _ => {
                            writeln!(stdout, "引数が不正です")?;
                            continue;
                        }
                    }
                } else {
                    state.run_mode = RunMode::GoToBreakPoint(10000);
                }
                return Ok(0);
            }
            "set-breakpoint" => set_breakpoint(emu, stdout, cmd_and_param.next(), true)?,
            "set-by-file" => set_by_file(emu, stdout, state, cmd_and_param.next())?,
            "set-label" => set_label(emu, stdout, cmd_and_param.next())?,
            "set-mem" => set_mem(emu, stdout, cmd_and_param.next())?,
            "set-reg" => {
                let msg = set_reg(emu, cmd_and_param.next());
                writeln!(stdout, "{}", msg)?;
            }
            "set-start" => {
                if let Some(s) = cmd_and_param.next() {
                    let s = s.to_ascii_uppercase();
                    let mut tokenizer = casl2::Tokenizer::new(s.as_str());
                    match tokenizer.extended_label() {
                        Ok(Some(lb)) => {
                            if !tokenizer.rest().is_empty() {
                                writeln!(stdout, "引数が不正です")?;
                            } else {
                                match lb.get_address(emu) {
                                    Ok(adr) => {
                                        state.start_point = Some(adr);
                                        writeln!(stdout, "スタートポイントを{}に設定しました", lb)?;
                                    }
                                    Err(msg) => writeln!(stdout, "{}", msg)?,
                                }
                            }
                        }
                        Ok(_) => writeln!(stdout, "引数が不正です")?,
                        Err(msg) => writeln!(stdout, "{}", msg)?,
                    }
                } else {
                    state.start_point = None;
                    let name = emu.start_point.as_ref().expect("BUG");
                    writeln!(stdout, "スタートポイントを{}に戻しました", name)?;
                }
            }
            "show-labels" => show_labels(emu, stdout, cmd_and_param.next())?,
            "show-mem" => show_mem(emu, stdout, cmd_and_param.next())?,
            "show-mem-stat" => show_mem_stat(emu, stdout, cmd_and_param.next())?,
            "show-reg" => show_reg(emu, stdout)?,
            "show-src" => show_src(emu, stdout, cmd_and_param.next())?,
            "show-state" => show_state(emu, stdout, state)?,
            "show-var" => show_var(emu, stdout, cmd_and_param.next())?,
            "skip" => {
                if let Some(s) = cmd_and_param.next() {
                    match s.parse::<u64>() {
                        Ok(v) if v > 0 => state.run_mode = RunMode::SkipSubroutine(v),
                        _ => {
                            writeln!(stdout, "引数が不正です")?;
                            continue;
                        }
                    }
                } else {
                    state.run_mode = RunMode::SkipSubroutine(10000);
                }
                return Ok(0);
            }
            "step" | "s" => {
                if let Some(s) = cmd_and_param.next() {
                    match s.parse::<u64>() {
                        Ok(v) if v > 0 => state.run_mode = RunMode::Step(v),
                        _ => {
                            writeln!(stdout, "引数が不正です")?;
                            continue;
                        }
                    }
                } else {
                    state.run_mode = RunMode::Step(1);
                }
                return Ok(0);
            }
            "write-code" => write_code(emu, stdout, cmd_and_param.next())?,
            _ => {
                writeln!(stdout, "コマンドが正しくありません")?;
            }
        }
    }
}

//
// show-var [<BASIC_PROGRAM_ENTRY> [<VAR_NAME1>[,<VAR_NAME2>..]]]
//
fn show_var_for_basic<W: Write>(
    emu: &Emulator,
    stdout: &mut W,
    param: Option<&str>,
) -> io::Result<()> {
    let (pg, var_list) = match param {
        None => (emu.get_current_program(), None),
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let label = iter.next().unwrap();
            let pg = if label == "." {
                emu.get_current_program()
            } else {
                emu.program_list
                    .iter()
                    .find(|(_, x, _)| x.eq_ignore_ascii_case(label))
            };
            match iter.next() {
                None => (pg, None),
                Some(param) => {
                    let list = param.split(',').map(|s| s.trim()).collect::<Vec<_>>();
                    (pg, Some(list))
                }
            }
        }
    };

    let (file, pg_label, stmt) = match pg {
        Some(pg) => pg,
        None => {
            writeln!(stdout, "BASICコードの情報が見つかりませんでした")?;
            return Ok(());
        }
    };

    let label_set = match emu.basic_info.get(file) {
        Some((_, info)) => &info.label_set,
        None => {
            writeln!(stdout, "BASICコードの情報が見つかりませんでした")?;
            return Ok(());
        }
    };

    let is_current = !stmt.is_empty() && {
        let &(fp, _) = stmt.first().unwrap();
        let &(lp, _) = stmt.last().unwrap();
        (fp..=lp).contains(&emu.program_register)
    };

    let is_running = !stmt.is_empty() && {
        let &(fp, _) = stmt.first().unwrap();
        let &(lp, _) = stmt.last().unwrap();
        emu.program_stack
            .iter()
            .any(|(_, ret)| (fp..=lp).contains(ret))
    };

    writeln!(stdout, "{}の変数", pg_label)?;

    if !(is_running || is_current) || (is_current && matches!(emu.basic_step, Some(0) | None)) {
        return print_var_def(stdout, label_set, var_list);
    }

    use parser::VarType as V;

    if let Some(list) = var_list {
        for name in list {
            if let Some((label, arg)) = label_set.argument_labels.get(name) {
                match arg.var_type {
                    V::Boolean | V::Integer => write!(stdout, " ByVal {:<18} = ", name)?,
                    V::RefBoolean | V::RefInteger => write!(stdout, " ByRef {:<18} = ", name)?,
                    _ => unreachable!("BUG"),
                }
                match emu.resolve_label(pg_label, label) {
                    Some((pos, None, V::Boolean)) => match emu.mem[pos] {
                        0x0000 => writeln!(stdout, "False")?,
                        0xFFFF => writeln!(stdout, "True")?,
                        _ => writeln!(stdout, "????")?,
                    },
                    Some((pos, None, V::Integer)) => writeln!(stdout, "{}", emu.mem[pos] as i16)?,
                    _ => writeln!(stdout, "(データ破損)")?,
                }
            } else if let Some((label, arg)) = label_set.str_argument_labels.get(name) {
                match arg.var_type {
                    V::String => write!(stdout, " ByVal {:<18} = ", name)?,
                    V::RefString => write!(stdout, " ByRef {:<18} = ", name)?,
                    _ => unreachable!("BUG"),
                }
                match emu.resolve_label(pg_label, label) {
                    Some((len, Some(pos), V::String)) => {
                        let len = emu.mem[len] as usize;
                        let s = format!(
                            r#""{}""#,
                            emu.mem[pos..pos + len.min(256)]
                                .iter()
                                .map(|x| jis_x_201::convert_to_char(*x as u8, true))
                                .collect::<String>()
                                .replace('"', r#""""#)
                        );
                        if len <= 256 {
                            writeln!(stdout, "({:>3}) {}", len, s)?;
                        } else {
                            let s = s.chars().take(30).collect::<String>();
                            writeln!(stdout, "({:>3}) {}  (データ破損)", len, s)?;
                        }
                        if (1..=256).contains(&len) {
                            let t = emu.mem[pos..pos + len.min(256)]
                                .iter()
                                .map(|v| format!("{}", *v as i16))
                                .collect::<Vec<_>>()
                                .join(", ");
                            writeln!(stdout, " {:5} {:18} = {}", "", "", t)?;
                        }
                    }
                    _ => writeln!(stdout, "(データ破損)")?,
                }
            } else if let Some((label, arg)) = label_set.arr_argument_labels.get(name) {
                match arg.var_type {
                    V::ArrayOfBoolean(size) | V::ArrayOfInteger(size) => {
                        write!(stdout, " ByVal {:<18} = ", format!("{}({})", name, size))?
                    }
                    V::RefArrayOfBoolean(size) | V::RefArrayOfInteger(size) => {
                        write!(stdout, " ByRef {:<18} = ", format!("{}({})", name, size))?
                    }
                    _ => unreachable!("BUG"),
                }
                match emu.resolve_label(pg_label, label) {
                    Some((pos, None, V::ArrayOfBoolean(size))) => {
                        let s = emu.mem[pos..pos + size]
                            .iter()
                            .map(|v| match *v {
                                0x0000 => "False",
                                0xFFFF => "True ",
                                _ => "???? ",
                            })
                            .collect::<Vec<_>>()
                            .join(", ");
                        writeln!(stdout, "{}", s)?;
                    }
                    Some((pos, None, V::ArrayOfInteger(size))) => {
                        let s = emu.mem[pos..pos + size]
                            .iter()
                            .map(|v| format!("{}", *v as i16))
                            .collect::<Vec<_>>()
                            .join(", ");
                        writeln!(stdout, "{}", s)?;
                    }
                    _ => writeln!(stdout, "(データ破損)")?,
                }
            } else if let Some(label) = label_set.bool_var_labels.get(name) {
                write!(stdout, " Dim   {:<18} = ", name)?;
                match emu.resolve_label(pg_label, label) {
                    Some((pos, None, V::Boolean)) => match emu.mem[pos] {
                        0x0000 => writeln!(stdout, "False")?,
                        0xFFFF => writeln!(stdout, "True")?,
                        _ => writeln!(stdout, "????")?,
                    },
                    _ => writeln!(stdout, "(データ破損)")?,
                }
            } else if let Some(label) = label_set.int_var_labels.get(name) {
                write!(stdout, " Dim   {:<18} = ", name)?;
                match emu.resolve_label(pg_label, label) {
                    Some((pos, None, V::Integer)) => writeln!(stdout, "{}", emu.mem[pos] as i16)?,
                    _ => writeln!(stdout, "(データ破損)")?,
                }
            } else if let Some(label) = label_set.str_var_labels.get(name) {
                write!(stdout, " Dim   {:<18} = ", name)?;
                match emu.resolve_label(pg_label, label) {
                    Some((len, Some(pos), V::String)) => {
                        let len = emu.mem[len] as usize;
                        let s = format!(
                            r#""{}""#,
                            emu.mem[pos..pos + len.min(256)]
                                .iter()
                                .map(|x| jis_x_201::convert_to_char(*x as u8, true))
                                .collect::<String>()
                                .replace('"', r#""""#)
                        );
                        if len <= 256 {
                            writeln!(stdout, "({:>3}) {}", len, s)?;
                        } else {
                            let s = s.chars().take(30).collect::<String>();
                            writeln!(stdout, "({:>3}) {}  (データ破損)", len, s)?;
                        }
                        if (1..=256).contains(&len) {
                            let t = emu.mem[pos..pos + len.min(256)]
                                .iter()
                                .map(|v| format!("{}", *v as i16))
                                .collect::<Vec<_>>()
                                .join(", ");
                            writeln!(stdout, " {:5} {:18} = {}", "", "", t)?;
                        }
                    }
                    _ => writeln!(stdout, "(データ破損)")?,
                }
            } else if let Some(label) = label_set.bool_arr_labels.get(name) {
                match emu.resolve_label(pg_label, label) {
                    Some((pos, None, V::ArrayOfBoolean(size))) => {
                        let s = emu.mem[pos..pos + size]
                            .iter()
                            .map(|v| match *v {
                                0x0000 => "False",
                                0xFFFF => "True",
                                _ => "????",
                            })
                            .collect::<Vec<_>>()
                            .join(", ");
                        writeln!(
                            stdout,
                            " Dim   {:<18} = {}",
                            format!("{}({})", name, size),
                            s
                        )?;
                    }
                    _ => writeln!(stdout, " Dim   {:<18} = (データ破損)", "")?,
                }
            } else if let Some(label) = label_set.int_arr_labels.get(name) {
                match emu.resolve_label(pg_label, label) {
                    Some((pos, None, V::ArrayOfInteger(size))) => {
                        let s = emu.mem[pos..pos + size]
                            .iter()
                            .map(|v| format!("{}", *v as i16))
                            .collect::<Vec<_>>()
                            .join(", ");
                        writeln!(
                            stdout,
                            " Dim   {:<18} = {}",
                            format!("{}({})", name, size),
                            s
                        )?;
                    }
                    _ => writeln!(stdout, " Dim   {:<18} = (データ破損)", "")?,
                }
            } else {
                writeln!(stdout, "変数{}は存在しません", name)?;
            }
        }
        return Ok(());
    }

    let mut omit_name: Option<&String> = None;

    for (name, (label, arg)) in label_set.argument_labels.iter().collect::<BTreeMap<_, _>>() {
        match arg.var_type {
            V::Boolean | V::Integer => write!(stdout, " ByVal {:<18} = ", name)?,
            V::RefBoolean | V::RefInteger => write!(stdout, " ByRef {:<18} = ", name)?,
            _ => unreachable!("BUG"),
        }
        match emu.resolve_label(pg_label, label) {
            Some((pos, None, V::Boolean)) => match emu.mem[pos] {
                0x0000 => writeln!(stdout, "False")?,
                0xFFFF => writeln!(stdout, "True")?,
                _ => writeln!(stdout, "????")?,
            },
            Some((pos, None, V::Integer)) => writeln!(stdout, "{}", emu.mem[pos] as i16)?,
            _ => writeln!(stdout, "(データ破損)")?,
        }
    }

    for (name, (label, arg)) in label_set
        .str_argument_labels
        .iter()
        .collect::<BTreeMap<_, _>>()
    {
        match arg.var_type {
            V::String => write!(stdout, " ByVal {:<18} = ", name)?,
            V::RefString => write!(stdout, " ByRef {:<18} = ", name)?,
            _ => unreachable!("BUG"),
        }
        match emu.resolve_label(pg_label, label) {
            Some((len, Some(pos), V::String)) => {
                let len = emu.mem[len] as usize;
                let s = format!(
                    r#""{}""#,
                    emu.mem[pos..pos + len.min(256)]
                        .iter()
                        .map(|x| jis_x_201::convert_to_char(*x as u8, true))
                        .collect::<String>()
                        .replace('"', r#""""#)
                );
                if s.chars().count() < 40 {
                    writeln!(stdout, "({:>3}) {}", len, s)?;
                } else if len <= 256 {
                    let s = s.chars().take(30).collect::<String>();
                    writeln!(stdout, "({:>3}) {}  (省略)", len, s)?;
                    omit_name = Some(name);
                } else {
                    let s = s.chars().take(30).collect::<String>();
                    writeln!(stdout, "({:>3}) {}  (データ破損)", len, s)?;
                }
                if (1..=256).contains(&len) {
                    let t = emu.mem[pos..pos + len.min(256)]
                        .iter()
                        .map(|v| format!("{}", *v as i16))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(stdout, " {:5} {:18} = ", "", "")?;
                    if t.chars().count() < 46 {
                        writeln!(stdout, "{}", t)?;
                    } else {
                        let t = t.chars().take(36).collect::<String>();
                        writeln!(stdout, "{}  (省略)", t)?;
                        omit_name = Some(name);
                    }
                }
            }
            _ => writeln!(stdout, "(データ破損)")?,
        }
    }

    for (name, (label, arg)) in label_set
        .arr_argument_labels
        .iter()
        .collect::<BTreeMap<_, _>>()
    {
        match arg.var_type {
            V::ArrayOfBoolean(size) | V::ArrayOfInteger(size) => {
                write!(stdout, " ByVal {:<18} = ", format!("{}({})", name, size))?
            }
            V::RefArrayOfBoolean(size) | V::RefArrayOfInteger(size) => {
                write!(stdout, " ByRef {:<18} = ", format!("{}({})", name, size))?
            }
            _ => unreachable!("BUG"),
        }
        match emu.resolve_label(pg_label, label) {
            Some((pos, None, V::ArrayOfBoolean(size))) => {
                let s = emu.mem[pos..pos + size]
                    .iter()
                    .map(|v| match *v {
                        0x0000 => "False",
                        0xFFFF => "True ",
                        _ => "???? ",
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                if s.chars().count() < 46 {
                    writeln!(stdout, "{}", s)?;
                } else {
                    let s = s.chars().take(36).collect::<String>();
                    writeln!(stdout, "{}  (省略)", s)?;
                    omit_name = Some(name);
                }
            }
            Some((pos, None, V::ArrayOfInteger(size))) => {
                let s = emu.mem[pos..pos + size]
                    .iter()
                    .map(|v| format!("{}", *v as i16))
                    .collect::<Vec<_>>()
                    .join(", ");
                if s.chars().count() < 46 {
                    writeln!(stdout, "{}", s)?;
                } else {
                    let s = s.chars().take(36).collect::<String>();
                    writeln!(stdout, "{}  (省略)", s)?;
                    omit_name = Some(name);
                }
            }
            _ => writeln!(stdout, "(データ破損)")?,
        }
    }

    for (name, label) in label_set.bool_var_labels.iter().collect::<BTreeMap<_, _>>() {
        write!(stdout, " Dim   {:<18} = ", name)?;
        match emu.resolve_label(pg_label, label) {
            Some((pos, None, V::Boolean)) => match emu.mem[pos] {
                0x0000 => writeln!(stdout, "False")?,
                0xFFFF => writeln!(stdout, "True")?,
                _ => writeln!(stdout, "????")?,
            },
            _ => writeln!(stdout, "(データ破損)")?,
        }
    }

    for (name, label) in label_set.int_var_labels.iter().collect::<BTreeMap<_, _>>() {
        write!(stdout, " Dim   {:<18} = ", name)?;
        match emu.resolve_label(pg_label, label) {
            Some((pos, None, V::Integer)) => writeln!(stdout, "{}", emu.mem[pos] as i16)?,
            _ => writeln!(stdout, "(データ破損)")?,
        }
    }

    for (name, label) in label_set.str_var_labels.iter().collect::<BTreeMap<_, _>>() {
        write!(stdout, " Dim   {:<18} = ", name)?;
        match emu.resolve_label(pg_label, label) {
            Some((len, Some(pos), V::String)) => {
                let len = emu.mem[len] as usize;
                let s = format!(
                    r#""{}""#,
                    emu.mem[pos..pos + len.min(256)]
                        .iter()
                        .map(|x| jis_x_201::convert_to_char(*x as u8, true))
                        .collect::<String>()
                        .replace('"', r#""""#)
                );
                if s.chars().count() < 40 {
                    writeln!(stdout, "({:>3}) {}", len, s)?;
                } else if len <= 256 {
                    let s = s.chars().take(30).collect::<String>();
                    writeln!(stdout, "({:>3}) {}  (省略)", len, s)?;
                    omit_name = Some(name);
                } else {
                    let s = s.chars().take(30).collect::<String>();
                    writeln!(stdout, "({:>3}) {}  (データ破損)", len, s)?;
                }
                if (1..=256).contains(&len) {
                    let t = emu.mem[pos..pos + len.min(256)]
                        .iter()
                        .map(|v| format!("{}", *v as i16))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(stdout, " {:5} {:18} = ", "", "")?;
                    if t.chars().count() < 46 {
                        writeln!(stdout, "{}", t)?;
                    } else {
                        let t = t.chars().take(36).collect::<String>();
                        writeln!(stdout, "{}  (省略)", t)?;
                        omit_name = Some(name);
                    }
                }
            }
            _ => writeln!(stdout, "(データ破損)")?,
        }
    }

    for (name, label) in label_set.bool_arr_labels.iter().collect::<BTreeMap<_, _>>() {
        match emu.resolve_label(pg_label, label) {
            Some((pos, None, V::ArrayOfBoolean(size))) => {
                write!(stdout, " Dim   {:<18} = ", format!("{}({})", name, size))?;
                let s = emu.mem[pos..pos + size]
                    .iter()
                    .map(|v| match *v {
                        0x0000 => "False",
                        0xFFFF => "True",
                        _ => "????",
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                if s.chars().count() < 46 {
                    writeln!(stdout, "{}", s)?;
                } else {
                    let s = s.chars().take(36).collect::<String>();
                    writeln!(stdout, "{}  (省略)", s)?;
                    omit_name = Some(name);
                }
            }
            _ => writeln!(stdout, " Dim   {:<18} = (データ破損)", "")?,
        }
    }

    for (name, label) in label_set.int_arr_labels.iter().collect::<BTreeMap<_, _>>() {
        match emu.resolve_label(pg_label, label) {
            Some((pos, None, V::ArrayOfInteger(size))) => {
                write!(stdout, " Dim   {:<18} = ", format!("{}({})", name, size))?;
                let s = emu.mem[pos..pos + size]
                    .iter()
                    .map(|v| format!("{}", *v as i16))
                    .collect::<Vec<_>>()
                    .join(", ");
                if s.chars().count() < 46 {
                    writeln!(stdout, "{}", s)?;
                } else {
                    let s = s.chars().take(36).collect::<String>();
                    writeln!(stdout, "{}  (省略)", s)?;
                    omit_name = Some(name);
                }
            }
            _ => writeln!(stdout, " Dim   {:<18} = (データ破損)", "")?,
        }
    }

    if let Some(name) = omit_name {
        writeln!(stdout)?;
        writeln!(
            stdout,
            "※省略された値を確認するには変数名を指定した実行が必要です"
        )?;
        writeln!(stdout, "  ( 例: show-var {} {} )", pg_label, name)?;
    }

    Ok(())
}

//
// set-len <STR_VAR_NAME> <LENGTH>
//
fn set_len<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    if matches!(emu.basic_step, Some(0) | None) {
        writeln!(stdout, "現在地点での変数の設定は行えません")?;
        return Ok(());
    }
    let (var_name, new_len) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let var_name = iter.next().unwrap();
            let rest = match iter.next() {
                Some(rest) => rest,
                None => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
            };
            match rest.parse::<u8>() {
                Ok(len) => (var_name, len as u16),
                Err(_) => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
            }
        }
    };

    let target = {
        let (file, pg_label, label_set) = match emu.get_current_program() {
            None => {
                writeln!(stdout, "現在地点での変数の設定は行えません")?;
                return Ok(());
            }
            Some((file, _, _)) => match emu.basic_info.get(file) {
                Some((label, info)) => (file, label, &info.label_set),
                None => {
                    writeln!(stdout, "現在地点での変数の設定は行えません")?;
                    return Ok(());
                }
            },
        };

        let target = {
            if let Some((label, _)) = label_set.str_argument_labels.get(var_name) {
                emu.resolve_label(pg_label, label)
            } else if let Some(label) = label_set.str_var_labels.get(var_name) {
                emu.resolve_label(pg_label, label)
            } else {
                None
            }
        };

        match target {
            Some(target) => target,
            None => {
                writeln!(stdout, "{}に文字列変数{}が見つかりません", file, var_name)?;
                return Ok(());
            }
        }
    };

    if let (len, Some(_), parser::VarType::String) = target {
        emu.mem[len] = new_len;
    } else {
        unreachable!("たぶん");
    }

    writeln!(stdout, "{}の長さを{}を設定しました", var_name, new_len)?;

    Ok(())
}

//
// fill-arr <VAR_NAME> <VALUE>
//
fn fill_arr<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    if matches!(emu.basic_step, Some(0) | None) {
        writeln!(stdout, "現在地点での変数の設定は行えません")?;
        return Ok(());
    }
    let (var_name, value) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let var_name = iter.next().unwrap();
            match iter.next() {
                Some(value) => (var_name, value),
                None => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
            }
        }
    };

    let target = {
        let (file, pg_label, label_set) = match emu.get_current_program() {
            None => {
                writeln!(stdout, "現在地点での変数の設定は行えません")?;
                return Ok(());
            }
            Some((file, _, _)) => match emu.basic_info.get(file) {
                Some((label, info)) => (file, label, &info.label_set),
                None => {
                    writeln!(stdout, "現在地点での変数の設定は行えません")?;
                    return Ok(());
                }
            },
        };

        let target = {
            if label_set.argument_labels.contains_key(var_name) {
                writeln!(stdout, "{}は要素アクセスできません", var_name)?;
                return Ok(());
            } else if let Some((label, _)) = label_set.arr_argument_labels.get(var_name) {
                emu.resolve_label(pg_label, label)
            } else if let Some((label, _)) = label_set.str_argument_labels.get(var_name) {
                emu.resolve_label(pg_label, label)
            } else if label_set.int_var_labels.contains_key(var_name)
                || label_set.bool_var_labels.contains_key(var_name)
            {
                writeln!(stdout, "{}は要素アクセスできません", var_name)?;
                return Ok(());
            } else if let Some(label) = label_set
                .int_arr_labels
                .get(var_name)
                .or_else(|| label_set.bool_arr_labels.get(var_name))
            {
                emu.resolve_label(pg_label, label)
            } else if let Some(label) = label_set.str_var_labels.get(var_name) {
                emu.resolve_label(pg_label, label)
            } else {
                None
            }
        };

        match target {
            Some(target) => target,
            None => {
                writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
                return Ok(());
            }
        }
    };

    let value = {
        let res = match tokenizer::Tokenizer::new(value.as_bytes()).next() {
            Some(res) => res?,
            None => {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        };
        match res {
            Ok((_, tokens)) => tokens.into_iter().map(|(_, tk)| tk).collect::<Vec<_>>(),
            Err(_) => {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    use parser::VarType as V;

    match target {
        (pos, None, V::ArrayOfBoolean(size)) => {
            if let Some((vs, s)) = take_basic_bool_values(&value) {
                if let [v] = vs.as_slice() {
                    emu.mem[pos..pos + size].fill(*v);
                    writeln!(stdout, "{}の内容を{}で埋めました", var_name, s)?;
                    return Ok(());
                }
            }
        }
        (pos, None, V::ArrayOfInteger(size)) => {
            if let Some((vs, s)) = take_basic_int_values(&value) {
                if let [v] = vs.as_slice() {
                    emu.mem[pos..pos + size].fill(*v);
                    writeln!(stdout, "{}の内容を{}で埋めました", var_name, s)?;
                    return Ok(());
                }
            }
        }
        (len, Some(pos), V::String) => {
            let size = emu.mem[len] as usize;
            if size > 256 {
                writeln!(stdout, "{}は壊れています", var_name)?;
                return Ok(());
            }
            if let Some((vs, s)) = take_basic_int_values(&value) {
                if let [v] = vs.as_slice() {
                    emu.mem[pos..pos + size].fill(*v);
                    writeln!(stdout, "{}の内容を{}で埋めました", var_name, s)?;
                    return Ok(());
                }
            }
        }
        _ => unreachable!("BUG"),
    }

    writeln!(stdout, "引数が不正です")?;
    Ok(())
}

//
// set-elem <VAR_NAME> <INDEX> <VALUE>
//
fn set_elem<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    if matches!(emu.basic_step, Some(0) | None) {
        writeln!(stdout, "現在地点での変数の設定は行えません")?;
        return Ok(());
    }
    let (var_name, index, value) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let var_name = iter.next().unwrap();
            let mut iter = match iter.next() {
                Some(rest) => rest.splitn(2, ' ').map(|s| s.trim()),
                None => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
            };
            let index = iter.next().unwrap();
            match iter.next() {
                Some(value) => (var_name, index, value),
                None => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
            }
        }
    };

    let target = {
        let (file, pg_label, label_set) = match emu.get_current_program() {
            None => {
                writeln!(stdout, "現在地点での変数の設定は行えません")?;
                return Ok(());
            }
            Some((file, _, _)) => match emu.basic_info.get(file) {
                Some((label, info)) => (file, label, &info.label_set),
                None => {
                    writeln!(stdout, "現在地点での変数の設定は行えません")?;
                    return Ok(());
                }
            },
        };

        let target = {
            if label_set.argument_labels.contains_key(var_name) {
                writeln!(stdout, "{}は要素アクセスできません", var_name)?;
                return Ok(());
            } else if let Some((label, _)) = label_set.arr_argument_labels.get(var_name) {
                emu.resolve_label(pg_label, label)
            } else if let Some((label, _)) = label_set.str_argument_labels.get(var_name) {
                emu.resolve_label(pg_label, label)
            } else if label_set.int_var_labels.contains_key(var_name)
                || label_set.bool_var_labels.contains_key(var_name)
            {
                writeln!(stdout, "{}は要素アクセスできません", var_name)?;
                return Ok(());
            } else if let Some(label) = label_set
                .int_arr_labels
                .get(var_name)
                .or_else(|| label_set.bool_arr_labels.get(var_name))
            {
                emu.resolve_label(pg_label, label)
            } else if let Some(label) = label_set.str_var_labels.get(var_name) {
                emu.resolve_label(pg_label, label)
            } else {
                None
            }
        };

        match target {
            Some(target) => target,
            None => {
                writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
                return Ok(());
            }
        }
    };

    let index = match index.parse::<u16>() {
        Ok(index) => index as usize,
        Err(_) => {
            writeln!(stdout, "Indexが不正です")?;
            return Ok(());
        }
    };

    let value = {
        let res = match tokenizer::Tokenizer::new(value.as_bytes()).next() {
            Some(res) => res?,
            None => {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        };
        match res {
            Ok((_, tokens)) => tokens.into_iter().map(|(_, tk)| tk).collect::<Vec<_>>(),
            Err(_) => {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    use parser::VarType as V;

    match target {
        (pos, None, V::ArrayOfBoolean(size)) => {
            if index >= size {
                writeln!(stdout, "Indexが不正です")?;
                return Ok(());
            }
            if let Some((vs, s)) = take_basic_bool_values(&value) {
                if let [v] = vs.as_slice() {
                    emu.mem[pos + index] = *v;
                    writeln!(stdout, "{}({})に{}を設定しました", var_name, index, s)?;
                    return Ok(());
                }
            }
        }
        (pos, None, V::ArrayOfInteger(size)) => {
            if index >= size {
                writeln!(stdout, "Indexが不正です")?;
                return Ok(());
            }
            if let Some((vs, s)) = take_basic_int_values(&value) {
                if let [v] = vs.as_slice() {
                    emu.mem[pos + index] = *v;
                    writeln!(stdout, "{}({})に{}を設定しました", var_name, index, s)?;
                    return Ok(());
                }
            }
        }
        (len, Some(pos), V::String) => {
            let size = emu.mem[len] as usize;
            if size > 256 {
                writeln!(stdout, "{}は壊れています", var_name)?;
                return Ok(());
            }
            if index >= size {
                writeln!(stdout, "Indexが不正です")?;
                return Ok(());
            }
            if let Some((vs, s)) = take_basic_int_values(&value) {
                if let [v] = vs.as_slice() {
                    emu.mem[pos + index] = *v;
                    writeln!(stdout, "{}({})に{}を設定しました", var_name, index, s)?;
                    return Ok(());
                }
            }
        }
        _ => unreachable!("BUG"),
    }

    writeln!(stdout, "引数が不正です")?;
    Ok(())
}

//
// set-var <VAR_NAME> <VALUE1>[,<VALUE2>..]
//
fn set_var<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    if emu.basic_step.is_none() {
        writeln!(stdout, "現在地点での変数の設定は行えません")?;
        return Ok(());
    }
    let (var_name, values) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let var_name = iter.next().unwrap();
            let values = match iter.next() {
                None => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
                Some(values) => {
                    let res = match tokenizer::Tokenizer::new(values.as_bytes()).next() {
                        Some(res) => res?,
                        None => {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                    };
                    match res {
                        Ok((_, tokens)) => tokens.into_iter().map(|(_, tk)| tk).collect::<Vec<_>>(),
                        Err(_) => {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                    }
                }
            };
            (var_name, values)
        }
    };

    let target = {
        let (file, pg_label, label_set) = match emu.get_current_program() {
            None => {
                writeln!(stdout, "現在地点での変数の設定は行えません")?;
                return Ok(());
            }
            Some((file, _, _)) => match emu.basic_info.get(file) {
                Some((label, info)) => (file, label, &info.label_set),
                None => {
                    writeln!(stdout, "現在地点での変数の設定は行えません")?;
                    return Ok(());
                }
            },
        };

        if let Some((label, arg)) = label_set.argument_labels.get(var_name) {
            if matches!(emu.basic_step, Some(0)) {
                Err(arg.clone())
            } else {
                match emu.resolve_label(pg_label, label) {
                    Some(res) => Ok(res),
                    None => {
                        writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
                        return Ok(());
                    }
                }
            }
        } else if let Some((label, arg)) = label_set.arr_argument_labels.get(var_name) {
            if matches!(emu.basic_step, Some(0)) {
                Err(arg.clone())
            } else {
                match emu.resolve_label(pg_label, label) {
                    Some(res) => Ok(res),
                    None => {
                        writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
                        return Ok(());
                    }
                }
            }
        } else if let Some((label, arg)) = label_set.str_argument_labels.get(var_name) {
            if matches!(emu.basic_step, Some(0)) {
                Err(arg.clone())
            } else {
                match emu.resolve_label(pg_label, label) {
                    Some(res) => Ok(res),
                    None => {
                        writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
                        return Ok(());
                    }
                }
            }
        } else if matches!(emu.basic_step, Some(0)) {
            writeln!(stdout, "現在地点での変数の設定は行えません")?;
            return Ok(());
        } else if let Some(label) = label_set
            .int_var_labels
            .get(var_name)
            .or_else(|| label_set.bool_var_labels.get(var_name))
        {
            match emu.resolve_label(pg_label, label) {
                Some(res) => Ok(res),
                None => {
                    writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
                    return Ok(());
                }
            }
        } else if let Some(label) = label_set
            .int_arr_labels
            .get(var_name)
            .or_else(|| label_set.bool_arr_labels.get(var_name))
        {
            match emu.resolve_label(pg_label, label) {
                Some(res) => Ok(res),
                None => {
                    writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
                    return Ok(());
                }
            }
        } else if let Some(label) = label_set.str_var_labels.get(var_name) {
            match emu.resolve_label(pg_label, label) {
                Some(res) => Ok(res),
                None => {
                    writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
                    return Ok(());
                }
            }
        } else {
            writeln!(stdout, "{}に変数{}が見つかりません", file, var_name)?;
            return Ok(());
        }
    };

    use parser::VarType as V;
    use tokenizer::Token as T;

    match target {
        Ok((pos, None, V::Boolean)) => {
            if let Some((vs, s)) = take_basic_bool_values(&values) {
                if let [v] = vs.as_slice() {
                    emu.mem[pos] = *v;
                    writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                    return Ok(());
                }
            }
        }
        Ok((pos, None, V::Integer)) => {
            if let Some((vs, s)) = take_basic_int_values(&values) {
                if let [v] = vs.as_slice() {
                    emu.mem[pos] = *v;
                    writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                    return Ok(());
                }
            }
        }
        Ok((pos, None, V::ArrayOfBoolean(size))) => {
            if let Some((vs, s)) = take_basic_bool_values(&values) {
                if vs.len() == size {
                    for (i, v) in vs.iter().enumerate() {
                        emu.mem[pos + i] = *v;
                    }
                    writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                    return Ok(());
                }
            }
        }
        Ok((pos, None, V::ArrayOfInteger(size))) => {
            if let Some((vs, s)) = take_basic_int_values(&values) {
                if vs.len() == size {
                    for (i, v) in vs.iter().enumerate() {
                        emu.mem[pos + i] = *v;
                    }
                    writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                    return Ok(());
                }
            }
        }
        Ok((len, Some(pos), V::String)) => {
            if let [T::String(s)] = values.as_slice() {
                let s = jis_x_201::convert_kana_wide_full_to_half(s)
                    .chars()
                    .take(256)
                    .collect::<String>();
                emu.mem[len] = s.chars().count() as u16;
                for (i, ch) in s.chars().enumerate() {
                    emu.mem[pos + i] = jis_x_201::convert_from_char(ch) as u16;
                }
                writeln!(
                    stdout,
                    r#"{}に"{}"を設定しました"#,
                    var_name,
                    s.replace('"', r#""""#)
                )?;
                return Ok(());
            }
            if let Some((vs, s)) = take_basic_int_values(&values) {
                if vs.len() <= 256 {
                    emu.mem[len] = vs.len() as u16;
                    for (i, v) in vs.iter().enumerate() {
                        emu.mem[pos + i] = *v;
                    }
                    writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                    return Ok(());
                }
            }
        }
        Ok(_) => unreachable!("BUG"),
        Err(arg) => match arg.var_type {
            V::Boolean => {
                if let Some((vs, s)) = take_basic_bool_values(&values) {
                    if let [v] = vs.as_slice() {
                        emu.general_registers[arg.register1 as usize] = *v;
                        writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                        return Ok(());
                    }
                }
            }
            V::Integer => {
                if let Some((vs, s)) = take_basic_int_values(&values) {
                    if let [v] = vs.as_slice() {
                        emu.general_registers[arg.register1 as usize] = *v;
                        writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                        return Ok(());
                    }
                }
            }
            V::String => {
                if let [T::String(s)] = values.as_slice() {
                    if s.is_empty() {
                        emu.general_registers[arg.register1 as usize] = 0;
                        emu.general_registers[arg.register2.unwrap() as usize] = 0;
                        writeln!(stdout, r#"{}に""を設定しました"#, var_name)?;
                    } else {
                        match emu.make_literal_str(s) {
                            Err(s) => writeln!(stdout, "{}", s)?,
                            Ok((pos, s)) => {
                                emu.general_registers[arg.register1 as usize] =
                                    s.chars().count().min(256) as u16;
                                emu.general_registers[arg.register2.unwrap() as usize] = pos as u16;
                                writeln!(
                                    stdout,
                                    r#"{}に"{}"を設定しました"#,
                                    var_name,
                                    s.replace('"', r#""""#)
                                )?;
                            }
                        }
                    }
                    return Ok(());
                }
                if let Some((vs, s)) = take_basic_int_values(&values) {
                    if vs.len() <= 256 {
                        if !emu.enough_remain(vs.len()) {
                            writeln!(stdout, "メモリ不足でリテラル領域を確保できませんでした")?;
                            return Ok(());
                        }
                        let pos = emu.compile_pos;
                        emu.compile_pos += vs.len();
                        emu.general_registers[arg.register1 as usize] = vs.len() as u16;
                        emu.general_registers[arg.register2.unwrap() as usize] = pos as u16;
                        for (i, v) in vs.iter().enumerate() {
                            emu.mem[pos + i] = *v;
                        }
                        writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                        return Ok(());
                    }
                }
            }
            V::ArrayOfBoolean(size) | V::RefArrayOfBoolean(size) => {
                if !emu.enough_remain(size) {
                    writeln!(stdout, "メモリ不足でリテラル領域を確保できません")?;
                    return Ok(());
                }
                if let Some((vs, s)) = take_basic_bool_values(&values) {
                    if vs.len() == size {
                        let pos = emu.compile_pos;
                        emu.compile_pos += vs.len();
                        emu.general_registers[arg.register1 as usize] = pos as u16;
                        for (i, v) in vs.iter().enumerate() {
                            emu.mem[pos + i] = *v;
                        }
                        writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                        return Ok(());
                    }
                }
            }
            V::ArrayOfInteger(size) | V::RefArrayOfInteger(size) => {
                if !emu.enough_remain(size) {
                    writeln!(stdout, "メモリ不足でリテラル領域を確保できません")?;
                    return Ok(());
                }
                if let Some((vs, s)) = take_basic_int_values(&values) {
                    if vs.len() == size {
                        let pos = emu.compile_pos;
                        emu.compile_pos += vs.len();
                        emu.general_registers[arg.register1 as usize] = pos as u16;
                        for (i, v) in vs.iter().enumerate() {
                            emu.mem[pos + i] = *v;
                        }
                        writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                        return Ok(());
                    }
                }
            }
            V::RefBoolean => {
                if !emu.enough_remain(1) {
                    writeln!(stdout, "メモリ不足でリテラル領域を確保できません")?;
                    return Ok(());
                }
                if let Some((vs, s)) = take_basic_bool_values(&values) {
                    if let [v] = vs.as_slice() {
                        let pos = emu.compile_pos;
                        emu.compile_pos += 1;
                        emu.general_registers[arg.register1 as usize] = pos as u16;
                        emu.mem[pos] = *v;
                        writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                        return Ok(());
                    }
                }
            }
            V::RefInteger => {
                if !emu.enough_remain(1) {
                    writeln!(stdout, "メモリ不足でリテラル領域を確保できません")?;
                    return Ok(());
                }
                if let Some((vs, s)) = take_basic_int_values(&values) {
                    if let [v] = vs.as_slice() {
                        let pos = emu.compile_pos;
                        emu.compile_pos += 1;
                        emu.general_registers[arg.register1 as usize] = pos as u16;
                        emu.mem[pos] = *v;
                        writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                        return Ok(());
                    }
                }
            }
            V::RefString => {
                if !emu.enough_remain(257) {
                    writeln!(stdout, "メモリ不足でリテラル領域を確保できませんでした")?;
                    return Ok(());
                }
                if let [T::String(s)] = values.as_slice() {
                    let t = jis_x_201::convert_kana_wide_full_to_half(s)
                        .chars()
                        .take(256)
                        .collect::<String>();
                    let pos = emu.compile_pos;
                    emu.compile_pos += 257;
                    emu.general_registers[arg.register1 as usize] = pos as u16;
                    emu.general_registers[arg.register2.unwrap() as usize] = pos as u16 + 1;
                    emu.mem[pos] = t.chars().count() as u16;
                    for (i, ch) in t.chars().enumerate() {
                        emu.mem[pos + i + 1] = jis_x_201::convert_from_char(ch) as u16;
                    }
                    writeln!(
                        stdout,
                        r#"{}に"{}"を設定しました"#,
                        var_name,
                        t.replace('"', r#""""#)
                    )?;
                    return Ok(());
                }
                if let Some((vs, s)) = take_basic_int_values(&values) {
                    if vs.len() <= 256 {
                        let pos = emu.compile_pos;
                        emu.compile_pos += 257;
                        emu.general_registers[arg.register1 as usize] = pos as u16;
                        emu.general_registers[arg.register2.unwrap() as usize] = pos as u16 + 1;
                        emu.mem[pos] = vs.len() as u16;
                        for (i, v) in vs.iter().enumerate() {
                            emu.mem[pos + i + 1] = *v;
                        }
                        writeln!(stdout, "{}に{}を設定しました", var_name, s)?;
                        return Ok(());
                    }
                }
            }
        },
    }

    writeln!(stdout, "引数が不正です")?;
    Ok(())
}

//
// remove-breakpoint <BASIC_PROGRAM_ENTRY> <STATEMENT_ID1>[,<STATEMENT_ID2>..]
// set-breakpoint <BASIC_PROGRAM_ENTRY> <STATEMENT_ID1>[,<STATEMENT_ID2>..]
//
fn set_breakpoint_for_basic<W: Write>(
    emu: &mut Emulator,
    stdout: &mut W,
    param: Option<&str>,
    value: bool,
) -> io::Result<()> {
    let (label, params) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let label = iter.next().unwrap();
            match iter.next() {
                None => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
                Some(param) => (label, param),
            }
        }
    };

    let (bps, points) = {
        let pg = if label == "." {
            emu.get_current_program()
        } else {
            emu.program_list
                .iter()
                .find(|(_, x, _)| x.eq_ignore_ascii_case(label))
        };

        let (file, label, stmt) = match pg {
            Some((file, label, stmt)) => (file, label, stmt),
            None => {
                writeln!(stdout, "BASICコードの情報が見つかりませんでした")?;
                return Ok(());
            }
        };

        let info = match emu.basic_info.get(file) {
            Some((_, info)) => info,
            None => {
                writeln!(stdout, "{}のBASICコードの情報が見つかりませんでした", label)?;
                return Ok(());
            }
        };

        let mut points = vec![];
        for p in params.split(',') {
            match p.trim().parse::<usize>() {
                Ok(v) if v < info.status_hint.len() => points.push(v),
                _ => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
            }
        }

        let bps = stmt
            .iter()
            .filter_map(|(pos, stmt)| match stmt {
                casl2::Statement::Code {
                    command: casl2::Command::DebugBasicStep { id },
                    ..
                } => Some((*pos, *id)),
                _ => None,
            })
            .collect::<Vec<_>>();

        (bps, points)
    };

    for p in points {
        if let Some((pos, _)) = bps.iter().find(|(_, id)| *id == p) {
            emu.break_points[*pos] = value;
        }
    }

    let label = if label == "." {
        emu.get_current_program()
            .map(|(_, label, _)| label.as_str())
            .unwrap()
    } else {
        label
    };

    if value {
        writeln!(
            stdout,
            "{}の{}にブレークポイントを設定しました",
            label.to_ascii_uppercase(),
            params
        )?;
    } else {
        writeln!(
            stdout,
            "{}の{}からブレークポイントを解除しました",
            label.to_ascii_uppercase(),
            params
        )?;
    }

    Ok(())
}

//
// show-execute-stat [<BASIC_PROGRAM_ENTRY>]
//
fn show_execute_stat<W: Write>(
    emu: &Emulator,
    stdout: &mut W,
    param: Option<&str>,
) -> io::Result<()> {
    let (file, label, stmt) = match param {
        Some(".") => match emu.get_current_program() {
            Some(pg) => pg,
            None => {
                writeln!(stdout, "BASICコードの情報が見つかりませんでした")?;
                return Ok(());
            }
        },
        Some(label) => match emu
            .program_list
            .iter()
            .find(|(_, x, _)| x.eq_ignore_ascii_case(label))
        {
            Some(pg) => pg,
            None => {
                writeln!(stdout, "{}のBASICコードの情報が見つかりませんでした", label)?;
                return Ok(());
            }
        },
        None => match emu.get_current_program() {
            Some(pg) => pg,
            None => {
                writeln!(stdout, "引数が必要です")?;
                return Ok(());
            }
        },
    };

    let info = match emu.basic_info.get(file) {
        Some((_, info)) => info,
        None => {
            writeln!(stdout, "{}のBASICコードの情報が見つかりませんでした", label)?;
            return Ok(());
        }
    };

    writeln!(stdout, "{}の実行情報", label)?;

    let bps = stmt
        .iter()
        .filter_map(|(pos, stmt)| match stmt {
            casl2::Statement::Code {
                command: casl2::Command::DebugBasicStep { id },
                ..
            } => Some((*pos, *id)),
            _ => None,
        })
        .collect::<Vec<_>>();

    writeln!(stdout, "実行回数")?;
    for (i, (n, hint, _)) in info.status_hint.iter().enumerate() {
        let (bp, cnt) = match bps.iter().find(|(_, id)| *id == i) {
            Some((pos, id)) => {
                assert_eq!(*id, i);
                let cnt = emu.execute_count[*pos];
                if emu.break_points[*pos] {
                    ("*", cnt)
                } else {
                    (" ", cnt)
                }
            }
            _ => (" ", 0),
        };
        writeln!(
            stdout,
            "{0:8} {1:5}: {2} {3:4$}{5}",
            cnt,
            i,
            bp,
            "",
            n * 2,
            hint
        )?;
    }

    Ok(())
}

//
// show-src [<BASIC_PROGRAM_ENTRY>] (BASIC)
//
fn show_src_basic<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (file, label, stmt) = match param {
        Some(".") => match emu.get_current_program() {
            Some(pg) => pg,
            None => {
                writeln!(stdout, "BASICコードの情報が見つかりませんでした")?;
                return Ok(());
            }
        },
        Some(label) => match emu
            .program_list
            .iter()
            .find(|(_, x, _)| x.eq_ignore_ascii_case(label))
        {
            Some(pg) => pg,
            None => {
                writeln!(stdout, "{}のBASICコードの情報が見つかりませんでした", label)?;
                return Ok(());
            }
        },
        None => match emu.get_current_program() {
            Some(pg) => pg,
            None => {
                writeln!(stdout, "引数が必要です")?;
                return Ok(());
            }
        },
    };

    let info = match emu.basic_info.get(file) {
        Some((_, info)) => info,
        None => {
            writeln!(stdout, "{}のBASICコードの情報が見つかりませんでした", label)?;
            return Ok(());
        }
    };

    writeln!(stdout, "{}のBASICコード", label)?;

    print_var_def(stdout, &info.label_set, None)?;

    let bps = stmt
        .iter()
        .filter_map(|(pos, stmt)| match stmt {
            casl2::Statement::Code {
                command: casl2::Command::DebugBasicStep { id },
                ..
            } => Some((*pos, *id)),
            _ => None,
        })
        .collect::<Vec<_>>();

    for (i, (n, hint, _)) in info.status_hint.iter().enumerate() {
        let bp = match bps.iter().find(|(_, id)| *id == i) {
            Some((pos, id)) if emu.break_points[*pos] => {
                assert_eq!(*id, i);
                "*"
            }
            _ => " ",
        };
        writeln!(stdout, "{0:5}: {1} {2:3$}{4}", i, bp, "", n * 2, hint)?;
    }

    Ok(())
}

//
// show-state (BASIC)
//
fn show_state_basic<W: Write>(emu: &Emulator, stdout: &mut W, state: &State) -> io::Result<()> {
    writeln!(stdout, "Step Count: {}", emu.basic_step_count)?;

    write!(stdout, "Call State:")?;
    for (i, (pos, _)) in emu.program_stack.iter().enumerate() {
        if i > 0 {
            write!(stdout, " >")?;
        }
        if let Some((k, _)) = emu.all_label_list.iter().find(|(_, v)| v == pos) {
            write!(stdout, " {}", k)?;
        } else {
            write!(stdout, " ????")?;
        }
    }
    writeln!(stdout)?;

    let bp = if emu.break_points[emu.last_run_position] {
        "*"
    } else {
        " "
    };

    if let Some(((pg_label, info), hint_id)) = emu
        .get_current_program()
        .and_then(|(file, _, _)| emu.basic_info.get(file))
        .zip(emu.basic_step)
    {
        if let Some((n, s, extra)) = info.status_hint.get(hint_id as usize) {
            writeln!(stdout, "Last:")?;
            writeln!(stdout, "{0:5}: {1} {2:3$}{4}", hint_id, bp, "", n * 2, s)?;
            if let Some(extra) = extra {
                use compiler::ExtraInfo::*;
                writeln!(stdout, "Info:")?;
                match extra {
                    For { counter, to, step } => {
                        let local_labels = emu.local_labels.get(pg_label).expect("BUG");
                        match emu.resolve_label(pg_label, counter) {
                            Some((pos, None, parser::VarType::Integer)) => {
                                write!(stdout, "  counter: {}", emu.mem[pos] as i16)?;
                                if let Some(to) = to {
                                    let to = emu.mem[*local_labels.get(to).expect("BUG")];
                                    write!(stdout, ", to: {}", to)?;
                                }
                                if let Some(step) = step {
                                    let step = emu.mem[*local_labels.get(step).expect("BUG")];
                                    write!(stdout, ", step: {}", step)?;
                                }
                                writeln!(stdout)?;
                            }
                            _ => writeln!(stdout, "  counter: (データ破損)")?,
                        }
                    }
                    Condition(reg) => {
                        let v = match emu.general_registers[*reg as usize] {
                            0x0000 => "False",
                            0xFFFF => "True",
                            _ => "????",
                        };
                        writeln!(stdout, "  condition: {}", v)?;
                    }
                    RelatedCode(code) => writeln!(stdout, "  related code: {}", code)?,
                    SelectInt(reg) => {
                        writeln!(stdout, "  value: {}", emu.general_registers[*reg as usize])?;
                    }
                    SelectStr {
                        len_value,
                        pos_address,
                    } => {
                        let len = emu.general_registers[*len_value as usize] as usize;
                        let pos = emu.general_registers[*pos_address as usize] as usize;
                        if len <= 256 {
                            let s = emu.mem[pos..pos + len]
                                .iter()
                                .map(|v| jis_x_201::convert_to_char(*v as u8, true))
                                .collect::<String>()
                                .replace('"', r#""""#);
                            writeln!(stdout, r#"  value: "{}""#, s)?;
                        } else {
                            writeln!(stdout, "  value: (データ破損)")?;
                        }
                    }
                }
            }
        } else {
            writeln!(stdout, "Last:")?;
            writeln!(stdout, "{0:5}: {1} ?????", hint_id, bp)?;
        }
    } else {
        writeln!(stdout, "Last:")?;
        writeln!(stdout, "?????: {} ?????", bp)?;
    }

    if let Some(err) = state.err.as_ref() {
        writeln!(stdout, "Program State:")?;
        writeln!(stdout, " {}", err)?;
    }

    Ok(())
}

//
// show-state (CASL2)
//
fn show_state<W: Write>(emu: &Emulator, stdout: &mut W, state: &State) -> io::Result<()> {
    if let Some(pos) = state.start_point {
        let mut label = None;
        for (lb, p) in emu.all_label_list.iter() {
            if *p == pos {
                label = Some(lb);
                break;
            }
        }
        if label.is_none() {
            for (lb, (p, _)) in emu.labels_for_debug.iter() {
                if *p == pos {
                    label = Some(lb);
                    break;
                }
            }
        }
        if label.is_none() {
            for (lb, p) in emu.alias_labels.iter() {
                if *p == pos {
                    label = Some(lb);
                    break;
                }
            }
        }
        if let Some(lb) = label {
            writeln!(stdout, "Start Point: {}", lb)?;
        } else {
            writeln!(stdout, "Start Point: #{:04X}", pos)?;
        }
    } else {
        writeln!(
            stdout,
            "Start Point: {}",
            emu.start_point.as_ref().expect("BUG")
        )?;
    }

    writeln!(stdout, "Step Count: {}", emu.step_count)?;

    write!(stdout, "Call State:")?;
    for (i, (pos, ret)) in emu.program_stack.iter().enumerate() {
        let fp = ret.checked_sub(2).expect("BUG");
        match emu.all_label_list.binary_search_by_key(pos, |(_, p)| *p) {
            Ok(mut index) => {
                while index > 0 {
                    if emu
                        .all_label_list
                        .get(index - 1)
                        .filter(|(_, p)| p == pos)
                        .is_some()
                    {
                        index -= 1;
                        continue;
                    }
                    break;
                }
                let (label, _) = emu.all_label_list.get(index).unwrap();
                if i == 0 {
                    write!(stdout, " {}", label)?;
                } else {
                    write!(stdout, " [#{:04X}]-> {}", fp, label)?;
                }
            }
            Err(_) => {
                if let Some(label) = emu
                    .labels_for_debug
                    .iter()
                    .find_map(|(k, (p, _))| if p == pos { Some(k) } else { None })
                    .or_else(|| {
                        emu.alias_labels.iter().find_map(
                            |(k, p)| {
                                if p == pos {
                                    Some(k)
                                } else {
                                    None
                                }
                            },
                        )
                    })
                {
                    if i == 0 {
                        write!(stdout, " {}", label)?;
                    } else {
                        write!(stdout, " [#{:04X}]-> {}", fp, label)?;
                    }
                } else if i == 0 {
                    write!(stdout, " #{:04X}", pos)?;
                } else {
                    write!(stdout, " [#{:04X}]-> #{:04X}", fp, pos)?;
                }
            }
        }
    }
    writeln!(stdout)?;

    if !emu.wrong_ret.is_empty() {
        writeln!(stdout, "Wrong RET: {}", emu.wrong_ret.len())?;
    }

    if state.err.is_none() {
        let last_pos = emu.last_run_position;
        let last_op_code = emu.mem[last_pos];
        let next_pos = emu.program_register;
        if last_op_code == RET_OP_CODE && next_pos >= 2 {
            writeln!(stdout, "Prev Code:")?;
            let prev_pos = next_pos - 2;
            let info = emu.get_code_info(prev_pos as u16);
            if let Some((_, src)) = info.src_code.as_ref() {
                writeln!(stdout, "{}", src)?;
            }
            writeln!(stdout, "{}", info.mem_code)?;
        }
    }

    writeln!(stdout, "Last Code:")?;
    let last_pos = emu.last_run_position;
    let info = emu.get_code_info(last_pos as u16);
    if let Some((_, src)) = info.src_code.as_ref() {
        writeln!(stdout, "{}", src)?;
    }
    writeln!(stdout, "{}", info.mem_code)?;

    if let Some(err) = state.err.as_ref() {
        writeln!(stdout, "Program State:")?;
        writeln!(stdout, " {}", err)?;
    } else {
        writeln!(stdout, "Next Code:")?;
        let next_pos = emu.program_register;
        let info = emu.get_code_info(next_pos as u16);
        if let Some((_, src)) = info.src_code.as_ref() {
            writeln!(stdout, "{}", src)?;
        }
        writeln!(stdout, "{}", info.mem_code)?;
    }
    Ok(())
}

//
// set-by-file <FILE_PATH>
//
fn set_by_file<W: Write>(
    emu: &mut Emulator,
    stdout: &mut W,
    state: &mut State,
    param: Option<&str>,
) -> io::Result<()> {
    let file = match param {
        Some(file) => file,
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
    };

    let path = Path::new(file);
    if !path.exists() || !path.is_file() {
        eprintln!("ファイルが見つかりません ({})", file);
        return Ok(());
    }
    if path.metadata()?.len() > 1_000_000 {
        eprintln!("ファイルサイズが大きすぎます ({})", file);
        return Ok(());
    }

    let text = fs::read_to_string(file)?;

    for line in text.lines() {
        let mut cmd_and_param = line.trim().splitn(2, ' ').map(|s| s.trim());
        let cmd = cmd_and_param.next().unwrap();
        match cmd {
            "add-dc" => add_dc(emu, stdout, cmd_and_param.next())?,
            "add-ds" => add_ds(emu, stdout, cmd_and_param.next())?,
            "copy-mem" => copy_mem(emu, stdout, cmd_and_param.next())?,
            "default-cmd" => {
                if let Some(defcmd) = cmd_and_param.next() {
                    if "none".eq_ignore_ascii_case(defcmd) {
                        state.default_cmd[DebugMode::Casl2 as usize] = None;
                    } else {
                        state.default_cmd[DebugMode::Casl2 as usize] = Some(defcmd.to_string());
                    }
                }
                if let Some(defcmd) = state.default_cmd[DebugMode::Casl2 as usize].as_ref() {
                    writeln!(stdout, "デフォルトのデバッガコマンド: {}", defcmd)?;
                } else {
                    writeln!(stdout, "デフォルトのデバッガコマンド: none")?;
                }
            }
            "dump-code" => dump_code(emu, stdout, cmd_and_param.next())?,
            "dump-mem" => dump_mem(emu, stdout, cmd_and_param.next())?,
            "fill-mem" => fill_mem(emu, stdout, cmd_and_param.next())?,
            "find-code" => find_code(emu, stdout, cmd_and_param.next())?,
            "find-src" => find_src(emu, stdout, cmd_and_param.next())?,
            "find-value" => find_value(emu, stdout, cmd_and_param.next())?,
            "help" => show_command_help(cmd_and_param.next(), stdout)?,
            "list-files" => list_files(emu, stdout)?,
            "remove-breakpoint" => set_breakpoint(emu, stdout, cmd_and_param.next(), false)?,
            "set-breakpoint" => set_breakpoint(emu, stdout, cmd_and_param.next(), true)?,
            "set-label" => set_label(emu, stdout, cmd_and_param.next())?,
            "set-mem" => set_mem(emu, stdout, cmd_and_param.next())?,
            "set-reg" => {
                let msg = set_reg(emu, cmd_and_param.next());
                writeln!(stdout, "{}", msg)?;
            }
            "set-start" => {
                if let Some(s) = cmd_and_param.next() {
                    let s = s.to_ascii_uppercase();
                    let mut tokenizer = casl2::Tokenizer::new(s.as_str());
                    match tokenizer.extended_label() {
                        Ok(Some(lb)) => {
                            if !tokenizer.rest().is_empty() {
                                writeln!(stdout, "引数が不正です")?;
                            } else {
                                match lb.get_address(emu) {
                                    Ok(adr) => {
                                        state.start_point = Some(adr);
                                        writeln!(stdout, "スタートポイントを{}に設定しました", lb)?;
                                    }
                                    Err(msg) => writeln!(stdout, "{}", msg)?,
                                }
                            }
                        }
                        Ok(_) => writeln!(stdout, "引数が不正です")?,
                        Err(msg) => writeln!(stdout, "{}", msg)?,
                    }
                } else {
                    state.start_point = None;
                    let name = emu.start_point.as_ref().expect("BUG");
                    writeln!(stdout, "スタートポイントを{}に戻しました", name)?;
                }
            }
            "show-labels" => show_labels(emu, stdout, cmd_and_param.next())?,
            "show-mem" => show_mem(emu, stdout, cmd_and_param.next())?,
            "show-mem-stat" => show_mem_stat(emu, stdout, cmd_and_param.next())?,
            "show-reg" => show_reg(emu, stdout)?,
            "show-src" => show_src(emu, stdout, cmd_and_param.next())?,
            "show-state" => show_state(emu, stdout, state)?,
            "show-var" => show_var(emu, stdout, cmd_and_param.next())?,
            "write-code" => write_code(emu, stdout, cmd_and_param.next())?,
            "" => {}
            _ => {
                writeln!(stdout, "コメント行としてスキップします: {}", line)?;
            }
        }
    }

    Ok(())
}

//
// write-code <ADDRESS> <COMET2_COMMAND>
//
fn write_code<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (pos, cmd) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            let adr = match emu.get_address_by_label_str(adr) {
                Ok(adr) => adr,
                Err(msg) => {
                    writeln!(stdout, "{}", msg)?;
                    return Ok(());
                }
            };
            if let Some(cmd) = iter.next() {
                match parse_casl2_command(emu, cmd, true) {
                    Ok(cmd) => (adr, cmd),
                    Err(msg) => {
                        writeln!(stdout, "{}", msg)?;
                        return Ok(());
                    }
                }
            } else {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    match cmd {
        Code::In(param) => {
            let mut tokenizer = casl2::Tokenizer::new(&param);
            match Value::take_all_values(emu, &mut tokenizer) {
                Err(msg) => {
                    writeln!(stdout, "{}", msg)?;
                    return Ok(());
                }
                Ok(params) => {
                    if let [Value::Int(buf), Value::Int(len)] = params.as_slice() {
                        let src = casl2::parse(&format!(
                            r#"
                        PUSH 0,GR1
                        PUSH 0,GR2
                        PUSH 0,GR3
                        PUSH 0,GR4
                        PUSH 0,GR5
                        PUSH 0,GR6
                        PUSH 0,GR7
                        LD  GR1,GR0
                        PUSH 0,GR1
                        LAD GR1,{buf}
                        LAD GR2,{len}
                        SVC 1
                        POP GR1
                        LD  GR0,GR1
                        POP GR7
                        POP GR6
                        POP GR5
                        POP GR4
                        POP GR3
                        POP GR2
                        POP GR1"#,
                            buf = buf,
                            len = len
                        ))
                        .unwrap();
                        let end = {
                            let mut pos = pos;
                            for stmt in src.iter() {
                                if let casl2::Statement::Code { command, .. } = stmt {
                                    let len = command.len();
                                    emu.mem[pos] = command.first_word();
                                    match command {
                                        casl2::Command::P { adr, .. }
                                        | casl2::Command::A { adr, .. } => match adr {
                                            casl2::Adr::Dec(d) => emu.mem[pos + 1] = *d as u16,
                                            casl2::Adr::Hex(h) => emu.mem[pos + 1] = *h,
                                            _ => {}
                                        },
                                        _ => {}
                                    }
                                    pos += len;
                                }
                            }
                            pos
                        };
                        writeln!(
                            stdout,
                            "#{:04X}..#{:04X}にIN        #{:04X},#{:04X}を書き込みました",
                            pos,
                            end - 1,
                            buf,
                            len
                        )?;
                    } else {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                }
            }
        }
        Code::Out(param) => {
            let mut tokenizer = casl2::Tokenizer::new(&param);
            match Value::take_all_values(emu, &mut tokenizer) {
                Err(msg) => {
                    writeln!(stdout, "{}", msg)?;
                    return Ok(());
                }
                Ok(params) => {
                    if let [Value::Int(buf), Value::Int(len)] = params.as_slice() {
                        let src = casl2::parse(&format!(
                            r#"
                        PUSH 0,GR1
                        PUSH 0,GR2
                        PUSH 0,GR3
                        PUSH 0,GR4
                        PUSH 0,GR5
                        PUSH 0,GR6
                        PUSH 0,GR7
                        LD  GR1,GR0
                        PUSH 0,GR1
                        LAD GR1,{buf}
                        LAD GR2,{len}
                        SVC 2
                        POP GR1
                        LD  GR0,GR1
                        POP GR7
                        POP GR6
                        POP GR5
                        POP GR4
                        POP GR3
                        POP GR2
                        POP GR1"#,
                            buf = buf,
                            len = len
                        ))
                        .unwrap();
                        let end = {
                            let mut pos = pos;
                            for stmt in src.iter() {
                                if let casl2::Statement::Code { command, .. } = stmt {
                                    let len = command.len();
                                    emu.mem[pos] = command.first_word();
                                    match command {
                                        casl2::Command::P { adr, .. }
                                        | casl2::Command::A { adr, .. } => match adr {
                                            casl2::Adr::Dec(d) => emu.mem[pos + 1] = *d as u16,
                                            casl2::Adr::Hex(h) => emu.mem[pos + 1] = *h,
                                            _ => {}
                                        },
                                        _ => {}
                                    }
                                    pos += len;
                                }
                            }
                            pos
                        };
                        writeln!(
                            stdout,
                            "#{:04X}..#{:04X}にOUT       #{:04X},#{:04X}を書き込みました",
                            pos,
                            end - 1,
                            buf,
                            len
                        )?;
                    } else {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                }
            }
        }
        Code::Casl2(cmd) => {
            let cmd_len = cmd.len();
            if pos
                .checked_add(cmd_len)
                .filter(|v| *v + 1 < emu.mem.len())
                .is_none()
            {
                writeln!(stdout, "メモリに十分な領域がありません")?;
                return Ok(());
            }
            match &cmd {
                casl2::Command::Start { .. } | casl2::Command::End | casl2::Command::Ds { .. } => {
                    writeln!(stdout, "指定できないコマンドです")?;
                    return Ok(());
                }
                casl2::Command::A { adr, .. } | casl2::Command::P { adr, .. } => {
                    assert_eq!(cmd.len(), 2);
                    let adr = adr.to_string();
                    let mut tokenizer = casl2::Tokenizer::new(&adr);
                    match Value::take_single_value(emu, &mut tokenizer) {
                        Err(msg) => {
                            writeln!(stdout, "{}", msg)?;
                            return Ok(());
                        }
                        Ok(Value::Int(adr)) => {
                            emu.mem[pos] = cmd.first_word();
                            emu.mem[pos + 1] = adr as u16;
                        }
                        Ok(Value::Str(s)) => {
                            if let Some(ch) = s.chars().next() {
                                let ch = jis_x_201::convert_from_char(ch);
                                emu.mem[pos] = cmd.first_word();
                                emu.mem[pos + 1] = ch as u16;
                            } else {
                                writeln!(stdout, "引数が不正です")?;
                                return Ok(());
                            }
                        }
                    }
                }
                casl2::Command::R { .. }
                | casl2::Command::Pop { .. }
                | casl2::Command::Ret
                | casl2::Command::Nop => {
                    assert_eq!(cmd.len(), 1);
                    emu.mem[pos] = cmd.first_word();
                }
                casl2::Command::Dc { constants } => {
                    let mut pos = pos;
                    for c in constants.iter() {
                        let c = c.to_string();
                        let mut tokenizer = casl2::Tokenizer::new(&c);
                        match Value::take_single_value(emu, &mut tokenizer) {
                            Err(msg) => {
                                writeln!(stdout, "{}", msg)?;
                                return Ok(());
                            }
                            Ok(Value::Int(v)) => {
                                emu.mem[pos] = v;
                                pos += 1;
                            }
                            Ok(Value::Str(s)) => {
                                for ch in s.chars() {
                                    let ch = jis_x_201::convert_from_char(ch);
                                    emu.mem[pos] = ch as u16;
                                    pos += 1;
                                }
                            }
                        }
                    }
                }
                casl2::Command::Rpush => {
                    use casl2::IndexRegister::*;
                    for (i, gr) in [Gr1, Gr2, Gr3, Gr4, Gr5, Gr6, Gr7].iter().enumerate() {
                        let cmd = casl2::Command::P {
                            code: casl2::P::Push,
                            adr: casl2::Adr::Hex(0),
                            x: Some(*gr),
                        };
                        emu.mem[pos + i] = cmd.first_word();
                    }
                }
                casl2::Command::Rpop => {
                    use casl2::Register::*;
                    for (i, gr) in [Gr7, Gr6, Gr5, Gr4, Gr3, Gr2, Gr1].iter().enumerate() {
                        let cmd = casl2::Command::Pop { r: *gr };
                        emu.mem[pos + i] = cmd.first_word();
                    }
                }
                casl2::Command::Out { .. }
                | casl2::Command::In { .. }
                | casl2::Command::DebugBasicStep { .. } => unreachable!("BUG"),
            }
            writeln!(
                stdout,
                "#{:04X}..#{:04X}に{}を書き込みました",
                pos,
                pos + cmd_len - 1,
                cmd
            )?;
        }
    }

    Ok(())
}

//
// show-src [<ADDRESS> [<LENGTH>]]
//
fn show_src<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, len) = match param {
        None => (emu.program_register, 40),
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            let adr = match emu.get_address_by_label_str(adr) {
                Ok(adr) => adr,
                Err(msg) => {
                    writeln!(stdout, "{}", msg)?;
                    return Ok(());
                }
            };
            if let Some(len) = iter.next() {
                match parse_just_u16_value(len) {
                    Some(len) if len > 0 => (adr, len as usize),
                    _ => {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                }
            } else {
                (adr, 40)
            }
        }
    };

    if adr
        .checked_add(len)
        .filter(|v| *v <= emu.mem.len())
        .is_none()
    {
        writeln!(stdout, "長さの指定が大きすぎます")?;
        return Ok(());
    }

    let mut pos = adr;
    let mut view_some = false;

    for (_file, label, statements) in emu.program_list.iter() {
        if statements.is_empty() {
            continue;
        }
        let (fp, _) = statements.first().unwrap();
        let (lp, _) = statements.last().unwrap();
        if pos < *fp || *lp < pos {
            continue;
        }
        view_some = true;
        writeln!(stdout, "Program: {}", label)?;
        let mut view = false;
        for (p, stmt) in statements.iter().skip_while(move |(p, _)| *p < pos) {
            if pos < *p {
                if *p >= adr + len {
                    if !view {
                        writeln!(stdout, "#{:04X}の位置にコードはありませんでした", pos)?;
                    }
                    return Ok(());
                }
                pos = *p;
            }
            let bp = if emu.break_points[pos] { "*" } else { " " };
            view = true;
            writeln!(stdout, "#{:04X}: {} {}", pos, bp, stmt)?;
        }
    }

    if !view_some {
        writeln!(stdout, "表示可能なコードがありませんでした")?;
    }

    Ok(())
}

//
// show-mem-stat <ADDRESS> [<LENGTH>]
//
fn show_mem_stat<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, len) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            let adr = match emu.get_address_by_label_str(adr) {
                Ok(adr) => adr,
                Err(msg) => {
                    writeln!(stdout, "{}", msg)?;
                    return Ok(());
                }
            };
            if let Some(len) = iter.next() {
                match parse_just_u16_value(len) {
                    Some(len) if len > 0 => (adr, len as usize),
                    _ => {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                }
            } else {
                (adr, 1)
            }
        }
    };

    if adr
        .checked_add(len)
        .filter(|v| *v <= emu.mem.len())
        .is_none()
    {
        writeln!(stdout, "長さの指定が大きすぎます")?;
        return Ok(());
    }

    writeln!(stdout, "書込回数 読込回数 実行回数")?;
    for pos in adr..adr + len {
        let exe = emu.execute_count[pos];
        let acc = emu.access_count[pos];
        let upd = emu.update_count[pos];
        let info = emu.get_code_info(pos as u16);
        writeln!(stdout, "{:8} {:8} {:8} {}", upd, acc, exe, info.mem_code)?;
    }

    Ok(())
}

//
// show-mem <ADDRESS> [<LENGTH>] [<TYPE>]
//
fn show_mem<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, len, view) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            let adr = match emu.get_address_by_label_str(adr) {
                Ok(adr) => adr,
                Err(msg) => {
                    writeln!(stdout, "{}", msg)?;
                    return Ok(());
                }
            };
            if let Some(rest) = iter.next() {
                let mut iter = rest.splitn(2, ' ').map(|s| s.trim());
                let token = iter.next().unwrap();
                if let Some(rest) = iter.next() {
                    // token は len
                    let len = match parse_just_u16_value(token) {
                        Some(len) if len > 0 => len,
                        _ => {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                    };
                    if let Ok(view) = ViewType::try_from(rest) {
                        (adr, len, Some(view))
                    } else {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                } else {
                    // token は len OR type
                    match parse_just_u16_value(token) {
                        Some(len) if len > 0 => (adr, len, None),
                        _ => {
                            if let Ok(view) = ViewType::try_from(token) {
                                (adr, 1, Some(view))
                            } else {
                                writeln!(stdout, "引数が不正です")?;
                                return Ok(());
                            }
                        }
                    }
                }
            } else {
                (adr, 1, None)
            }
        }
    };

    if adr
        .checked_add(len as usize)
        .filter(|v| *v <= emu.mem.len())
        .is_none()
    {
        writeln!(stdout, "長さの指定が大きすぎます")?;
        return Ok(());
    }

    writeln!(stdout, "#{:04X}から{}語分のデータ", adr, len)?;

    use std::fmt::Write;

    match view {
        Some(ViewType::Int) => {
            let mut line = String::new();
            let mut count = 0;
            for i in 0..len as usize {
                if count == 0 {
                    write!(&mut line, "#{:04X}:   ", adr + i).unwrap();
                }
                if count == 4 {
                    line.push(' ');
                }
                write!(&mut line, " {:6}", emu.mem[adr + i] as i16).unwrap();
                count += 1;
                if count == 8 {
                    count = 0;
                    writeln!(stdout, "{}", line)?;
                    line.clear();
                }
            }
            if !line.is_empty() {
                writeln!(stdout, "{}", line)?;
            }
        }
        Some(ViewType::Bool) => {
            let mut line = String::new();
            let mut count = 0;
            for i in 0..len as usize {
                if count == 0 {
                    write!(&mut line, "#{:04X}:   ", adr + i).unwrap();
                }
                if count == 4 {
                    line.push(' ');
                }
                let s = match emu.mem[adr + i] {
                    0x0000 => "False",
                    0xFFFF => "True",
                    _ => "?????",
                };
                write!(&mut line, " {:<5}", s).unwrap();
                count += 1;
                if count == 8 {
                    count = 0;
                    writeln!(stdout, "{}", line)?;
                    line.clear();
                }
            }
            if !line.is_empty() {
                writeln!(stdout, "{}", line)?;
            }
        }
        Some(ViewType::Str) => {
            let mut line = String::new();
            for i in 0..len as usize {
                let ch = emu.mem[adr + i] as u8;
                line.push(jis_x_201::convert_to_char(ch, true));
            }
            writeln!(stdout, r#""{}""#, line.replace('"', r#""""#))?;
            writeln!(stdout, "{}", line)?;
        }
        None => {
            let mut line = String::new();
            let mut count = 0;
            for i in 0..len as usize {
                if count == 0 {
                    write!(&mut line, "#{:04X}:   ", adr + i).unwrap();
                }
                if count == 4 {
                    line.push(' ');
                }
                write!(&mut line, " #{:04X}", emu.mem[adr + i]).unwrap();
                count += 1;
                if count == 8 {
                    count = 0;
                    writeln!(stdout, "{}", line)?;
                    line.clear();
                }
            }
            if !line.is_empty() {
                writeln!(stdout, "{}", line)?;
            }
        }
    }

    Ok(())
}

//
// find-src <ADDRESS> <CASL2_COMMAND>
//
fn find_src<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, cmd) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            if let Some(cmd) = iter.next() {
                (adr, cmd)
            } else {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    let adr = match emu.get_address_by_label_str(adr) {
        Ok(adr) => adr,
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
    };

    let cmd = match parse_casl2_command(emu, cmd, false) {
        Ok(Code::Casl2(cmd)) => cmd,
        Ok(_) => unreachable!("BUG"),
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
    };

    for (file, label, statements) in emu.program_list.iter() {
        if statements.is_empty() {
            continue;
        }
        let (fp, _) = statements.first().unwrap();
        let (lp, _) = statements.last().unwrap();
        if adr < *fp || *lp < adr {
            continue;
        }
        for (p, stmt) in statements.iter() {
            if *p < adr {
                continue;
            }
            if let casl2::Statement::Code { command, .. } = stmt {
                if command != &cmd {
                    continue;
                }
                let bp = if emu.break_points[*p] { "*" } else { " " };
                writeln!(stdout, "Program: {} ({})", label, file)?;
                let lbs = emu
                    .alias_labels
                    .iter()
                    .filter_map(|(k, pos)| if pos == p { Some(k) } else { None })
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(" ");
                if !lbs.is_empty() {
                    writeln!(stdout, "Labels: {}", lbs)?;
                }
                writeln!(stdout, "#{:04X}: {} {}", p, bp, stmt)?;
                return Ok(());
            }
        }
    }

    writeln!(stdout, "見つかりませんでした")?;

    Ok(())
}

//
// fill-mem <ADDRESS> <LENGTH> <VALUE>
//
fn fill_mem<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, len, value) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            let mut iter = match iter.next() {
                Some(rest) => rest.splitn(2, ' ').map(|s| s.trim()),
                None => {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
            };
            let len = iter.next().unwrap();
            if let Some(value) = iter.next() {
                (adr, len, value)
            } else {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    let adr = match emu.get_address_by_label_str(adr) {
        Ok(adr) => adr,
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
    };

    let len = match len.parse::<u16>() {
        Ok(len) => len as usize,
        Err(_) => {
            let mut tokenizer = casl2::Tokenizer::new(len);
            if let Some(h) = tokenizer.ignore_case_hex() {
                if tokenizer.rest().is_empty() {
                    h as usize
                } else {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
            } else {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    if adr
        .checked_add(len)
        .filter(|v| *v <= emu.mem.len())
        .is_none()
    {
        writeln!(stdout, "長さが大きすぎます")?;
        return Ok(());
    }

    let mut tokenizer = casl2::Tokenizer::new(value);
    let value = match Value::take_all_values(emu, &mut tokenizer) {
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
        Ok(values) => match values.as_slice() {
            [Value::Int(v)] => *v,
            [Value::Str(s)] if !s.is_empty() => {
                let ch = s.chars().next().unwrap();
                jis_x_201::convert_from_char(ch) as u16
            }
            _ => {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        },
    };

    emu.mem[adr..adr + len].fill(value);

    writeln!(
        stdout,
        "#{:04X}から{}語分を#{:04X}で埋めました",
        adr, len, value
    )?;

    Ok(())
}

//
// set-label <LABEL> <ADDRESS>
//
fn set_label<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (label, adr) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let label = iter.next().unwrap().to_ascii_uppercase();
            if !casl2::Label::from(&label).is_valid() {
                writeln!(stdout, "{}はラベルとして使用できません", label)?;
                return Ok(());
            }
            if let Some(adr) = iter.next() {
                (label, adr)
            } else {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    if emu.program_labels.contains_key(&label) {
        writeln!(stdout, "{}は既に使用されており指定できません", label)?;
        return Ok(());
    }
    if emu.labels_for_debug.contains_key(&label) {
        writeln!(stdout, "{}は既に使用されており指定できません", label)?;
        return Ok(());
    }

    let adr = match emu.get_address_by_label_str(adr) {
        Ok(adr) => adr,
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
    };

    if let Some(x) = emu.alias_labels.get(&label) {
        writeln!(stdout, "#{:04X}から#{:04X}に上書きされます", x, adr)?;
    }
    emu.alias_labels.insert(label.clone(), adr);
    writeln!(stdout, "ラベル{}に#{:04X}を設定しました", label, adr)?;

    Ok(())
}

//
// find-value <START_ADDRESS> <VALUE>
//
fn find_value<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, value) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            if let Some(value) = iter.next() {
                (adr, value)
            } else {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    let adr = match emu.get_address_by_label_str(adr) {
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
        Ok(adr) => adr,
    };

    let mut tokenizer = casl2::Tokenizer::new(value);
    let value = match Value::take_all_values_without_literal(emu, &mut tokenizer) {
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
        Ok(values) => match values.as_slice() {
            [Value::Int(v)] => *v,
            [Value::Str(s)] if !s.is_empty() => {
                let ch = s.chars().next().unwrap();
                jis_x_201::convert_from_char(ch) as u16
            }
            _ => {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        },
    };

    if let Some((i, _)) = emu
        .mem
        .iter()
        .enumerate()
        .skip(adr)
        .find(|(_, mem)| **mem == value)
    {
        let info = emu.get_code_info(i as u16);
        if let Some(lb) = info.src_entry_label.as_ref() {
            if let Some(file) = info.src_file.as_ref() {
                writeln!(stdout, "Program: {} ({})", lb, file)?;
            } else {
                writeln!(stdout, "Program: {}", lb)?;
            }
        }
        if !info.alias_labels.is_empty() {
            writeln!(stdout, "Labels: {}", info.alias_labels.join(" "))?;
        }
        if let Some((_, src)) = info.src_code.as_ref() {
            writeln!(stdout, "{}", src)?;
        }
        writeln!(stdout, "{}", info.mem_code)?;
        return Ok(());
    }

    writeln!(stdout, "見つかりませんでした")?;
    Ok(())
}

//
// find-code <START_ADDRESS> <COMET2_COMMAND>
//
fn find_code<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, cmd) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            if let Some(cmd) = iter.next() {
                (adr, cmd)
            } else {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        }
    };

    let adr = match emu.get_address_by_label_str(adr) {
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
        Ok(adr) => adr,
    };

    let cmd = match parse_casl2_command(emu, cmd, true) {
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
        Ok(Code::Casl2(cmd)) => cmd,
        Ok(_) => {
            writeln!(stdout, "指定できないコマンドです")?;
            return Ok(());
        }
    };

    match &cmd {
        casl2::Command::Start { .. }
        | casl2::Command::End
        | casl2::Command::Ds { .. }
        | casl2::Command::Dc { .. }
        | casl2::Command::In { .. }
        | casl2::Command::Out { .. }
        | casl2::Command::Rpush
        | casl2::Command::Rpop
        | casl2::Command::DebugBasicStep { .. } => {
            writeln!(stdout, "指定できないコマンドです")?;
            return Ok(());
        }
        casl2::Command::R { .. }
        | casl2::Command::A { .. }
        | casl2::Command::P { .. }
        | casl2::Command::Pop { .. }
        | casl2::Command::Ret
        | casl2::Command::Nop => {}
    }

    assert!((1..=2).contains(&cmd.len()));

    let first_word = cmd.first_word();

    let param_adr = match &cmd {
        casl2::Command::A { adr, .. } | casl2::Command::P { adr, .. } => match adr {
            casl2::Adr::Dec(d) => Some(*d as u16),
            casl2::Adr::Hex(h) => Some(*h),
            casl2::Adr::Label(lb) => match emu.get_address_by_label_str(lb.as_str()) {
                Ok(adr) => Some(adr as u16),
                Err(msg) => {
                    writeln!(stdout, "{}", msg)?;
                    return Ok(());
                }
            },
            casl2::Adr::LiteralDec(..)
            | casl2::Adr::LiteralHex(..)
            | casl2::Adr::LiteralStr(..) => {
                writeln!(stdout, "リテラルを指定することはできません")?;
                return Ok(());
            }
        },
        _ => None,
    };

    for (i, mem) in emu.mem.iter().enumerate().skip(adr) {
        if *mem != first_word {
            continue;
        }
        if let Some(adr) = param_adr {
            if let Some(mem) = emu.mem.get(i + 1) {
                if *mem == adr {
                    let info = emu.get_code_info(i as u16);
                    if let Some(lb) = info.src_entry_label.as_ref() {
                        if let Some(file) = info.src_file.as_ref() {
                            writeln!(stdout, "Program: {} ({})", lb, file)?;
                        } else {
                            writeln!(stdout, "Program: {}", lb)?;
                        }
                    }
                    if !info.alias_labels.is_empty() {
                        writeln!(stdout, "Labels: {}", info.alias_labels.join(" "))?;
                    }
                    if let Some((_, src)) = info.src_code.as_ref() {
                        writeln!(stdout, "{}", src)?;
                    }
                    writeln!(stdout, "{}", info.mem_code)?;
                    return Ok(());
                }
            }
        } else {
            let info = emu.get_code_info(i as u16);
            if let Some(lb) = info.src_entry_label.as_ref() {
                if let Some(file) = info.src_file.as_ref() {
                    writeln!(stdout, "Program: {} ({})", lb, file)?;
                } else {
                    writeln!(stdout, "Program: {}", lb)?;
                }
            }
            if !info.alias_labels.is_empty() {
                writeln!(stdout, "Labels: {}", info.alias_labels.join(" "))?;
            }
            if let Some((_, src)) = info.src_code.as_ref() {
                writeln!(stdout, "{}", src)?;
            }
            writeln!(stdout, "{}", info.mem_code)?;
            return Ok(());
        }
    }

    writeln!(stdout, "見つかりませんでした")?;
    Ok(())
}

//
// copy-mem <ADDRESS_FROM> <ADDRESS_TO> <LENGTH>
//
fn copy_mem<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let params = if let Some(param) = param {
        param.split_whitespace().collect::<Vec<_>>()
    } else {
        writeln!(stdout, "引数が必要です")?;
        return Ok(());
    };
    let (adr1s, adr2s, len) = if let [adr1, adr2, len] = params.as_slice() {
        (adr1, adr2, len)
    } else {
        writeln!(stdout, "引数が不正です")?;
        return Ok(());
    };

    let adr1 = match emu.get_address_by_label_str(adr1s) {
        Ok(adr) => adr,
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
    };

    let adr2 = match emu.get_address_by_label_str(adr2s) {
        Ok(adr) => adr,
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
    };

    let len = match len.parse::<u16>() {
        Ok(len) => len as usize,
        Err(_) => match casl2::Tokenizer::new(len).ignore_case_hex() {
            Some(len) => len as usize,
            None => {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
        },
    };

    if adr1
        .checked_add(len)
        .filter(|p| *p <= emu.mem.len())
        .is_none()
    {
        writeln!(stdout, "引数が不正です")?;
        return Ok(());
    }

    if adr2
        .checked_add(len)
        .filter(|p| *p <= emu.mem.len())
        .is_none()
    {
        writeln!(stdout, "引数が不正です")?;
        return Ok(());
    }

    emu.mem.copy_within(adr1..adr1 + len, adr2);

    writeln!(
        stdout,
        "#{:04X}から#{:04X}へ{}語コピーしました",
        adr1, adr2, len
    )?;

    Ok(())
}

//
// add-ds <LABEL> <SIZE>
//
fn add_ds<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (name, size) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let name = iter.next().unwrap().to_ascii_uppercase();
            if !casl2::Label::from(&name).is_valid() {
                writeln!(stdout, "{}はラベルとして不正です", name)?;
                return Ok(());
            }
            match iter.next() {
                None => {
                    writeln!(stdout, "引数が不足しています")?;
                    return Ok(());
                }
                Some(size) => (name, size),
            }
        }
    };

    if emu.program_labels.contains_key(&name) {
        writeln!(stdout, "{}は既に使用されており指定できません", name)?;
        return Ok(());
    }
    if emu.labels_for_debug.contains_key(&name) {
        writeln!(stdout, "{}は既に使用されており指定できません", name)?;
        return Ok(());
    }
    if emu.alias_labels.contains_key(&name) {
        writeln!(stdout, "{}は既に使用されており指定できません", name)?;
        return Ok(());
    }

    let size = match size.parse::<u16>() {
        Ok(size) => size,
        Err(_) => {
            let mut tokenizer = casl2::Tokenizer::new(size);
            if let Some(h) = tokenizer.ignore_case_hex() {
                h
            } else {
                writeln!(stdout, "サイズ指定が不正です")?;
                return Ok(());
            }
        }
    };

    if !emu.enough_remain(size as usize) {
        writeln!(stdout, "メモリに十分な領域がありません")?;
        return Ok(());
    }

    let adr = emu.compile_pos;
    emu.compile_pos += size as usize;

    let stmt = casl2::Statement::labeled(&name, casl2::Command::Ds { size });

    emu.labels_for_debug.insert(name.clone(), (adr, stmt));

    writeln!(
        stdout,
        "{}(#{:04X})に{}語分の領域を確保しました",
        name, adr, size
    )?;

    Ok(())
}

//
// add-dc <LABEL> <VALUE1>[,<VALUE2>..]
//
fn add_dc<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (name, values) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let name = iter.next().unwrap().to_ascii_uppercase();
            if !casl2::Label::from(&name).is_valid() {
                writeln!(stdout, "{}はラベルとして不正です", name)?;
                return Ok(());
            }
            match iter.next() {
                None => {
                    writeln!(stdout, "引数が不足しています")?;
                    return Ok(());
                }
                Some(values) => (name, values),
            }
        }
    };

    if emu.program_labels.contains_key(&name) {
        writeln!(stdout, "{}は既に使用されており指定できません", name)?;
        return Ok(());
    }
    if emu.labels_for_debug.contains_key(&name) {
        writeln!(stdout, "{}は既に使用されており指定できません", name)?;
        return Ok(());
    }
    if emu.alias_labels.contains_key(&name) {
        writeln!(stdout, "{}は既に使用されており指定できません", name)?;
        return Ok(());
    }

    let mut tokenizer = casl2::Tokenizer::new(values);
    let values = match Value::take_all_values(emu, &mut tokenizer) {
        Ok(values) => values,
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
    };

    let len = values
        .iter()
        .map(|v| match v {
            Value::Int(_) => 1,
            Value::Str(s) => s.chars().count(),
        })
        .sum::<usize>();

    if !emu.enough_remain(len) {
        writeln!(stdout, "メモリに十分な領域がありません")?;
        return Ok(());
    }

    let adr = emu.compile_pos;
    emu.compile_pos += len;

    let mut constants = vec![];
    let mut pos = adr;
    let mut msg = format!("{}(#{:04X})に", name, pos);
    for v in values {
        match v {
            Value::Int(v) => {
                emu.mem[pos] = v;
                pos += 1;
                msg.push_str(&format!("#{:04X},", v));
                constants.push(casl2::Constant::Hex(v));
            }
            Value::Str(s) => {
                for ch in s.chars() {
                    let v = jis_x_201::convert_from_char(ch) as u16;
                    emu.mem[pos] = v;
                    pos += 1;
                    msg.push_str(&format!("#{:04X},", v));
                    constants.push(casl2::Constant::Hex(v));
                }
            }
        }
    }

    let stmt = casl2::Statement::labeled(&name, casl2::Command::Dc { constants });

    emu.labels_for_debug.insert(name, (adr, stmt));

    writeln!(stdout, "{}を設定しました", msg)?;

    Ok(())
}

//
// set-mem <ADDRESS> <VALUE1>[,<VALUE2>..]
//
fn set_mem<W: Write>(emu: &mut Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, values) = match param {
        None => {
            writeln!(stdout, "引数が必要です")?;
            return Ok(());
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            match iter.next() {
                None => {
                    writeln!(stdout, "引数が不足しています")?;
                    return Ok(());
                }
                Some(values) => (adr, values),
            }
        }
    };

    let adr = adr.to_ascii_uppercase();
    let mut tokenizer = casl2::Tokenizer::new(adr.as_str());
    let adr = match tokenizer.extended_label() {
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
        Ok(None) => {
            writeln!(stdout, "引数が不正です")?;
            return Ok(());
        }
        Ok(Some(lb)) => {
            if !tokenizer.rest().is_empty() {
                writeln!(stdout, "引数が不正です")?;
                return Ok(());
            }
            match lb.get_address(emu) {
                Ok(adr) => adr,
                Err(msg) => {
                    writeln!(stdout, "{}", msg)?;
                    return Ok(());
                }
            }
        }
    };

    let mut tokenizer = casl2::Tokenizer::new(values);
    let values = match Value::take_all_values(emu, &mut tokenizer) {
        Ok(values) => values,
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
    };

    let len = values
        .iter()
        .map(|v| match v {
            Value::Int(_) => 1,
            Value::Str(s) => s.chars().count(),
        })
        .sum::<usize>();

    if adr + len > emu.mem.len() {
        writeln!(stdout, "十分な領域がありません")?;
        return Ok(());
    }

    let mut pos = adr;
    let mut msg = format!("#{:04X}に", pos);
    for v in values {
        match v {
            Value::Int(v) => {
                emu.mem[pos] = v;
                pos += 1;
                msg.push_str(&format!("#{:04X},", v));
            }
            Value::Str(s) => {
                for ch in s.chars() {
                    let v = jis_x_201::convert_from_char(ch) as u16;
                    emu.mem[pos] = v;
                    pos += 1;
                    msg.push_str(&format!("#{:04X},", v));
                }
            }
        }
    }

    writeln!(stdout, "{}を設定しました", msg)?;

    Ok(())
}

//
// set-breakpoint <ADDRESS1>[,<ADDRESS2>..]
//
fn set_breakpoint<W: Write>(
    emu: &mut Emulator,
    stdout: &mut W,
    param: Option<&str>,
    value: bool,
) -> io::Result<()> {
    let adr_list = match param.map(|s| s.to_ascii_uppercase()) {
        None => vec![emu.program_register],
        Some(param) => {
            let mut vs = vec![];
            let mut tokenizer = casl2::Tokenizer::new(&param);
            loop {
                if let Some(adr) = tokenizer.ignore_case_hex() {
                    vs.push(adr as usize);
                } else {
                    let label = match tokenizer.extended_label() {
                        Ok(Some(label)) => label,
                        Ok(None) => {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                        Err(msg) => {
                            writeln!(stdout, "{}", msg)?;
                            return Ok(());
                        }
                    };
                    match label.get_address(emu) {
                        Ok(adr) => vs.push(adr),
                        Err(msg) => {
                            writeln!(stdout, "{}", msg)?;
                            return Ok(());
                        }
                    }
                }
                if !tokenizer.comma() {
                    if tokenizer.rest().is_empty() {
                        break;
                    } else {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                }
            }
            vs
        }
    };

    for adr in adr_list {
        if value {
            emu.break_points[adr] = true;
            writeln!(stdout, "#{:04X}にブレークポイントを設定しました", adr)?;
        } else if emu.break_points[adr] {
            emu.break_points[adr] = false;
            writeln!(stdout, "#{:04X}のブレークポイントを解除しました", adr)?;
        } else {
            writeln!(stdout, "#{:04X}にブレークポイントは設定されていません", adr)?;
        }

        let info = emu.get_code_info(adr as u16);
        if let Some((_, src)) = info.src_code.as_ref() {
            writeln!(stdout, "{}", src)?;
        }
        writeln!(stdout, "{}", info.mem_code)?;
    }

    Ok(())
}

//
// dump-mem <ADDRESS> [<SIZE>]
//
fn dump_mem<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, size) = match param.map(|s| s.to_ascii_uppercase()) {
        None => {
            let adr = emu.program_register;
            let size = (adr + 0x80).min(0x10000) - adr;
            (adr, size)
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            let size = iter.next();
            let mut tokenizer = casl2::Tokenizer::new(adr);
            let adr = if let Some(adr) = tokenizer.ignore_case_hex() {
                if !tokenizer.rest().trim().is_empty() {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
                adr as usize
            } else {
                match tokenizer.extended_label() {
                    Ok(Some(label)) => {
                        if !tokenizer.rest().trim().is_empty() {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                        match label.get_address(emu) {
                            Ok(adr) => adr,
                            Err(msg) => {
                                writeln!(stdout, "{}", msg)?;
                                return Ok(());
                            }
                        }
                    }
                    Ok(_) => {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                    Err(msg) => {
                        writeln!(stdout, "{}", msg)?;
                        return Ok(());
                    }
                }
            };
            let size = match size {
                None => 0x80,
                Some(s) => {
                    let mut tokenizer = casl2::Tokenizer::new(s);
                    if let Some(v) = tokenizer.ignore_case_hex() {
                        if !tokenizer.rest().trim().is_empty() {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                        v as usize
                    } else if let Some(v) = tokenizer.integer() {
                        if !tokenizer.rest().trim().is_empty() {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                        if v > 0 {
                            v as usize
                        } else {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                    } else {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                }
            };
            if adr + size <= 0x10000 {
                (adr, size)
            } else {
                (adr, 0x10000 - adr)
            }
        }
    };
    let mut pos = adr & (0xFFFF ^ 0x7);
    write!(stdout, "       ")?;
    for i in 0..8 {
        if i == 4 {
            write!(stdout, " ")?;
        }
        write!(stdout, " {:4X}", (pos & 0x8) + i)?;
    }
    writeln!(stdout)?;
    write!(stdout, "       ")?;
    for i in 0..8 {
        if i == 4 {
            write!(stdout, " ")?;
        }
        write!(stdout, " {:4X}", ((pos & 0x8) + i) ^ 0x8)?;
    }
    writeln!(stdout)?;
    let mut s = String::new();
    while pos < adr + size && pos < 0x10000 {
        s.clear();
        write!(stdout, "#{:04X}: ", pos)?;
        for i in 0..8 {
            if i == 4 {
                write!(stdout, " ")?;
                s.push(' ');
            }
            if (adr..adr + size).contains(&(pos + i)) {
                let v = emu.mem[pos + i];
                write!(stdout, " {:04X}", v)?;
                s.push(jis_x_201::convert_to_char((v >> 8) as u8, true));
                s.push(jis_x_201::convert_to_char(v as u8, true));
            } else {
                write!(stdout, " ....")?;
                s.push_str("..");
            }
        }
        writeln!(stdout, "   {}", s)?;
        pos += 8;
    }
    Ok(())
}

//
// dump-code <ADDRESS> [<SIZE>]
//
fn dump_code<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (adr, size) = match param.map(|s| s.to_ascii_uppercase()) {
        None => {
            let adr = emu.program_register;
            let size = (adr + 0x20).min(0x10000) - adr;
            (adr, size)
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ').map(|s| s.trim());
            let adr = iter.next().unwrap();
            let size = iter.next();
            let mut tokenizer = casl2::Tokenizer::new(adr);
            let adr = if let Some(adr) = tokenizer.ignore_case_hex() {
                if !tokenizer.rest().trim().is_empty() {
                    writeln!(stdout, "引数が不正です")?;
                    return Ok(());
                }
                adr as usize
            } else {
                match tokenizer.extended_label() {
                    Ok(Some(label)) => {
                        if !tokenizer.rest().trim().is_empty() {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                        match label.get_address(emu) {
                            Ok(adr) => adr,
                            Err(msg) => {
                                writeln!(stdout, "{}", msg)?;
                                return Ok(());
                            }
                        }
                    }
                    Ok(_) => {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                    Err(msg) => {
                        writeln!(stdout, "{}", msg)?;
                        return Ok(());
                    }
                }
            };
            let size = match size {
                None => 0x20,
                Some(s) => {
                    let mut tokenizer = casl2::Tokenizer::new(s);
                    if let Some(v) = tokenizer.ignore_case_hex() {
                        if !tokenizer.rest().trim().is_empty() {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                        v as usize
                    } else if let Some(v) = tokenizer.integer() {
                        if !tokenizer.rest().trim().is_empty() {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                        if v > 0 {
                            v as usize
                        } else {
                            writeln!(stdout, "引数が不正です")?;
                            return Ok(());
                        }
                    } else {
                        writeln!(stdout, "引数が不正です")?;
                        return Ok(());
                    }
                }
            };
            if adr + size <= 0x10000 {
                (adr, size)
            } else {
                (adr, 0x10000 - adr)
            }
        }
    };
    let mut pos = adr;
    while pos < adr + size && pos < 0x10000 {
        let info = emu.get_code_info(pos as u16);
        writeln!(stdout, "{}", info.mem_code)?;
        pos += get_op_code_size(emu.mem[pos]).max(1);
    }
    Ok(())
}

//
// list-files
//
fn list_files<W: Write>(emu: &Emulator, stdout: &mut W) -> io::Result<()> {
    let mut last: Option<&str> = None;
    for (file, key, _) in emu.program_list.iter() {
        if let Some(f) = last.as_ref() {
            if *f == file.as_str() {
                write!(stdout, " {}", key)?;
                continue;
            }
        }
        writeln!(stdout)?;
        writeln!(stdout, " {}", file)?;
        write!(stdout, "        {}", key)?;
        last = Some(file.as_str());
    }
    writeln!(stdout)?;
    Ok(())
}

//
// show-var [<BASIC_PROGRAM_ENTRY> [<VAR_NAME1>[,<VAR_NAME2>..]]]
//
fn show_var<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (info, var_list) = match param {
        None => {
            let info = emu
                .get_current_program()
                .and_then(|(file, _, _)| emu.basic_info.get(file));
            (info, None)
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ');
            let label = iter.next().unwrap();
            let info = if label == "." {
                emu.get_current_program()
                    .and_then(|(file, _, _)| emu.basic_info.get(file))
            } else {
                emu.basic_info
                    .values()
                    .find(|(x, _)| x.eq_ignore_ascii_case(label))
            };
            let var_list = iter.next().map(|s| s.split(',').collect::<Vec<_>>());
            (info, var_list)
        }
    };

    let (key, info) = match info {
        None => {
            writeln!(stdout, "変数情報が見つかりませんでした")?;
            return Ok(());
        }
        Some((key, info)) => (key, info),
    };

    writeln!(stdout, "{}の変数", key)?;

    if let Some(var_list) = var_list {
        for name in var_list {
            if let Some((label, arg)) = info.label_set.argument_labels.get(name) {
                print_value_label(emu, stdout, key, name, label, Some(arg))?;
            } else if let Some((label, arg)) = info.label_set.str_argument_labels.get(name) {
                print_str_label(emu, stdout, key, name, label, Some(arg), false)?;
            } else if let Some((label, arg)) = info.label_set.arr_argument_labels.get(name) {
                print_arr_label(emu, stdout, key, name, label, Some(arg), false)?;
            } else if let Some(label) = info.label_set.bool_var_labels.get(name) {
                print_value_label(emu, stdout, key, name, label, None)?;
            } else if let Some(label) = info.label_set.int_var_labels.get(name) {
                print_value_label(emu, stdout, key, name, label, None)?;
            } else if let Some(label) = info.label_set.str_var_labels.get(name) {
                print_str_label(emu, stdout, key, name, label, None, false)?;
            } else if let Some(label) = info.label_set.bool_arr_labels.get(name) {
                print_arr_label(emu, stdout, key, name, label, None, false)?;
            } else if let Some(label) = info.label_set.int_arr_labels.get(name) {
                print_arr_label(emu, stdout, key, name, label, None, false)?;
            } else {
                writeln!(stdout, "{}に変数{}は存在しません", key, name)?;
            }
        }
    } else {
        let mut omit_name = None;
        for (name, (label, arg)) in info
            .label_set
            .argument_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_value_label(emu, stdout, key, name, label, Some(arg))?;
        }
        for (name, (label, arg)) in info
            .label_set
            .str_argument_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_str_label(emu, stdout, key, name, label, Some(arg), true)?;
            omit_name = Some(name);
        }
        for (name, (label, arg)) in info
            .label_set
            .arr_argument_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_arr_label(emu, stdout, key, name, label, Some(arg), true)?;
            omit_name = Some(name);
        }
        for (name, label) in info
            .label_set
            .bool_var_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_value_label(emu, stdout, key, name, label, None)?;
        }
        for (name, label) in info
            .label_set
            .int_var_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_value_label(emu, stdout, key, name, label, None)?;
        }
        for (name, label) in info
            .label_set
            .str_var_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_str_label(emu, stdout, key, name, label, None, true)?;
            omit_name = Some(name);
        }
        for (name, label) in info
            .label_set
            .bool_arr_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_arr_label(emu, stdout, key, name, label, None, true)?;
            omit_name = Some(name);
        }
        for (name, label) in info
            .label_set
            .int_arr_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_arr_label(emu, stdout, key, name, label, None, true)?;
            omit_name = Some(name);
        }
        if let Some(name) = omit_name {
            writeln!(stdout)?;
            writeln!(
                stdout,
                "※省略された値を確認するには変数名を指定した実行が必要です"
            )?;
            writeln!(stdout, "  ( 例: show-var {} {} )", key, name)?;
        }
    }
    Ok(())
}

//
// show-labels [<LABEL>]
//
fn show_labels<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let mut count = 0;
    if let Some(label) = param {
        let label = label.to_ascii_uppercase();
        match emu.program_labels.get(&label) {
            Some(adr) => {
                writeln!(stdout, " #{:04X} {:<8}", adr, label)?;
            }
            None => {
                writeln!(stdout, "プログラムエントリ{}が見つかりません", label)?;
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
        writeln!(stdout, "プログラムエントリ")?;
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
        if (count & 3) != 0 {
            writeln!(stdout)?;
        }
        count = 0;
        if !emu.labels_for_debug.is_empty() {
            writeln!(stdout, "デバッガコマンドで生成")?;
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
        for (adr, key) in emu
            .alias_labels
            .iter()
            .map(|(key, adr)| (adr, key))
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

//
// show-reg
//
fn show_reg<W: Write>(emu: &Emulator, stdout: &mut W) -> io::Result<()> {
    writeln!(stdout)?;
    writeln!(
        stdout,
        "PR:  #{:04X}         SP:  #{:04X}[{:6}] FR:  (OF: {}, SF: {}, ZF: {})",
        emu.program_register,
        emu.stack_pointer,
        CALLSTACK_START_POSITION - emu.stack_pointer,
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

//
// set-reg <REG> <VALUE>
//
fn set_reg(emu: &mut Emulator, param: Option<&str>) -> String {
    let params = match param {
        Some(param) => param.split_whitespace().collect::<Vec<_>>(),
        None => return "引数がありません".to_string(),
    };

    let (reg, value) = if let [reg, value] = params.as_slice() {
        (reg, value)
    } else {
        return "引数の数が正しくありません".to_string();
    };

    match casl2::Register::try_from(*reg) {
        Ok(reg) => {
            let mut tokenizer = casl2::Tokenizer::new(value);
            match Value::take_single_value(emu, &mut tokenizer) {
                Err(msg) => msg,
                Ok(Value::Int(value)) => {
                    if tokenizer.rest().trim().is_empty() {
                        emu.general_registers[reg as usize] = value;
                        format!("{}に#{:04X}を設定しました", reg, value)
                    } else {
                        "引数が不正です".to_string()
                    }
                }
                Ok(Value::Str(s)) => {
                    if tokenizer.rest().trim().is_empty() {
                        match s.chars().map(jis_x_201::convert_from_char).next() {
                            Some(ch) => {
                                emu.general_registers[reg as usize] = ch as u16;
                                format!("{}に{:04X}を設定しました", reg, ch as u16)
                            }
                            None => "空の文字定数は設定できません".to_string(),
                        }
                    } else {
                        "引数が不正です".to_string()
                    }
                }
            }
        }
        Err(_) => {
            if "PR".eq_ignore_ascii_case(*reg) {
                let mut tokenizer = casl2::Tokenizer::new(value);
                if let Some(adr) = tokenizer.ignore_case_hex() {
                    emu.program_register = adr as usize;
                    format!("PRに#{:04X}を設定しました", adr)
                } else {
                    match tokenizer.extended_label() {
                        Ok(Some(label)) => match label.get_address(emu) {
                            Ok(adr) => {
                                emu.program_register = adr;
                                format!("PRに#{:04X}を設定しました", adr)
                            }
                            Err(msg) => msg,
                        },
                        Ok(None) => "引数が不正です".to_string(),
                        Err(msg) => msg.to_string(),
                    }
                }
            } else if "SP".eq_ignore_ascii_case(*reg) {
                let mut tokenizer = casl2::Tokenizer::new(value);
                if let Some(adr) = tokenizer.ignore_case_hex() {
                    emu.stack_pointer = adr as usize;
                    format!("SPに#{:04X}を設定しました", adr)
                } else {
                    match tokenizer.extended_label() {
                        Ok(Some(label)) => match label.get_address(emu) {
                            Ok(adr) => {
                                emu.stack_pointer = adr;
                                format!("SPに#{:04X}を設定しました", adr)
                            }
                            Err(msg) => msg,
                        },
                        Ok(None) => "引数が不正です".to_string(),
                        Err(msg) => msg.to_string(),
                    }
                }
            } else if "OF".eq_ignore_ascii_case(*reg) {
                let flag = match *value {
                    "0" => false,
                    "1" => true,
                    _ => return "引数が不正です".to_string(),
                };
                emu.overflow_flag = flag;
                format!("OFに{}を設定しました", value)
            } else if "SF".eq_ignore_ascii_case(*reg) {
                let flag = match *value {
                    "0" => false,
                    "1" => true,
                    _ => return "引数が不正です".to_string(),
                };
                emu.sign_flag = flag;
                format!("SFに{}を設定しました", value)
            } else if "ZF".eq_ignore_ascii_case(*reg) {
                let flag = match *value {
                    "0" => false,
                    "1" => true,
                    _ => return "引数が不正です".to_string(),
                };
                emu.zero_flag = flag;
                format!("ZFに{}を設定しました", value)
            } else {
                "引数が不正です".to_string()
            }
        }
    }
}

//
// help [<TARGET>] (CASL2)
//
fn show_command_help<W: Write>(cmd: Option<&str>, stdout: &mut W) -> io::Result<()> {
    writeln!(stdout)?;
    writeln!(stdout, "コマンドヘルプ")?;

    let cmd = match cmd {
        Some(cmd) => cmd.to_ascii_lowercase(),
        None => {
            writeln!(
                stdout,
                r#"
    add-dc <LABEL> <VALUE1>[,<VALUE2>..]
                新しく領域を作り値を格納し先頭アドレスを示すラベルも作る。CASL2のDC相当
    add-ds <LABEL> <SIZE>
                新しく領域を確保し先頭アドレスを示すラベルを作る。CASL2のDS相当
    copy-mem <ADDRESS_FROM> <ADDRESS_TO> <LENGTH>
                メモリのデータをコピーする
    default-cmd [<DEBUG_COMMAND>]
                空行が入力されたときの挙動を設定する
    dump-code [<ADDRESS> [<SIZE>]]
                メモリをコード表示する
    dump-mem [<ADDRESS> [<SIZE>]]
                メモリダンプする
    fill-mem <ADDRESS> <LENGTH> <VALUE>
                メモリの指定アドレスから指定の長さ分を指定の値で埋める
    find-code <ADDRESS> <COMET2_COMMAND>
                指定のCOMET2コマンドを指定アドレス位置以降から探し最初に見つかったメモリ上のコードを表示する
    find-src <ADDRESS> <CASL2_COMMAND>
                指定のCASL2コマンドを指定アドレス位置以降から探し最初に見つかったコードを表示する
    find-value <ADDRESS> <VALUE>
                指定の値を指定アドレス位置以降から探し最初に見つかったものを表示する
    help <COMMAND_NAME>
                指定デバッガコマンドの詳細ヘルプを表示する
    help constant
                デバッガコマンドで使用する定数に関する説明を表示する
    list-files
                読み込んだソースファイルの一覧を表示する
    mode <MODE>
                指定したデバッグモードに切り替える
    quit
                テスト実行を中止する
    remove-breakpoint [<ADDRESS1>[,<ADDRESS2>..]]
                指定アドレスのブレークポイントを解除する。アドレス省略時はPRの示すアドレス
    reset
                プログラムを最初から実行しなおす。プログラムをファイルから読み込みなおし配置される
    restart
                プログラムを最初の位置から実行しなおす。メモリやGRやFRは終了時点の状態クリアされずに最後の実行状態のまま維持される
    run [<STEP_LIMIT>]
                次のブレークポイントまで実行する。ステップ制限数までにブレークポイントに到達しない場合はそこで停止する
    set-breakpoint [<ADDRESS1>[,<ADDRESS2>..]]
                指定アドレスにブレークポイントを設定する。アドレス省略時はPRの示すアドレス
    set-by-file <FILE_PATH>
                ファイルに列挙された設定系のデバッガコマンドを実行する
    set-label <LABEL> <ADDRESS>
                アドレスのエイリアスである新しいラベルを作る。
    set-mem <ADDRESS> <VALUE1>[,<VALUE2>..]
                値をメモリの指定アドレスに書き込む
    set-reg <REGISTER> <VALUE>
                値をレジスタに設定する
    set-start [<ADDRESS>]
                プログラムの開始点を変更する(restart時に影響)。省略した場合は最初の開始点に戻す
    show-labels [<PROGRAM_ENTRY>]
                ラベルの一覧とアドレスを表示する。PROGRAM_ENTRYを指定した場合はローカルラベルを表示する
    show-mem <ADDRESS> [<LENGTH>] [<TYPE>]
                メモリの指定アドレスから指定の長さ分の領域の各値を列挙する
    show-mem-stat <ADDRESS> [<LENGTH>]
                メモリの指定アドレスから指定の長さ分の領域の統計情報ぽいものを表示する
    show-reg
                各レジスタの現在の値を表示する
    show-src [<ADDRESS> [<LENGTH>]]
                指定したアドレス位置から指定長さ分の範囲にあるコードを表示する
    show-state
                直近の実行(run,skip,step)の結果を再表示する
    show-var [<BASIC_PROGRAM_ENTRY> [<VAR_NAME1>[,<VAR_NAME2>..]]]
                指定したBASICプログラムの変数名と対応するラベルとアドレスと値を表示する
    skip [<STEP_LIMIT>]
                現在のサブルーチンのRETまで実行する。ステップ制限数までにRETに到達しない場合はそこで停止する
    s    [<STEP_COUNT>]
    step [<STEP_COUNT>]
                指定ステップ数だけ実行する。STEP_COUNT省略時は1ステップだけ実行する
    write-code <ADDRESS> <COMET2_COMMAND>
                アドレス位置に指定のCOMET2コマンドを書き込む
"#
            )?;
            return Ok(());
        }
    };

    if "constant" == cmd {
        writeln!(
            stdout,
            r#"
    10進定数
                -32768　～ 32767
    16進定数
                #0000 ～ #FFFF
    文字定数
                文字・文字列を引用符で囲ったもの(引用符を2つ続けると引用符1文字の文字として扱う)
                    例: 'X'
                        'Abc123'
                        'Let''s go!'
    アドレス定数
        ラベル
                        グローバルラベル (各プログラムのエントリラベル、デバッガコマンドで定義したラベル)
                            例: MAIN
        ラベル:ラベル
                        ローカルラベル (各プログラムの内部のラベル)。 プログラムのエントリラベル:内部ラベル　で表記
                            例: MAIN:I001
                                LIB:MEM
        @レジスタ
                        レジスタ(GR*,PR,SP)の値をアドレスとする
                            例: @GR3
                                @PR
        (アドレス定数)
        (16進定数)
                        アドレス定数または16進定数の示すアドレスから読み込んだ値をアドレスとする
                            例: (MAIN:MEM)
                                (#1234)
        アドレス定数+アドレス定数
        アドレス定数+16進定数
                        2つのアドレス定数または16進定数の和をアドレスとする(オーバーフローに注意)
                            例: MAIN+@GR1
                                (MAIN:MEM)+#0101
        アドレス定数-アドレス定数
        アドレス定数-16進定数
        16進定数-アドレス定数
                        2つのアドレス定数または16進定数の差をアドレスとする(オーバーフローに注意)
                            例: LIB:S005-LIB:B001
                                MAIN:J004-#0010
                                #FFFF-@SP
    リテラル
                10進定数、16進定数、文字定数の頭に=を付けて指定
                領域を確保し値を格納の後そのアドレスを返す
                    例: =123
                        =#ABCD
                        ='XYZ'"#
        )?;
        return Ok(());
    }

    let description = match cmd.as_str() {
        "add-dc" => {
            r#"
    add-dc <LABEL> <VALUE1>[,<VALUE2>..]
                新しく領域を作り値を格納し先頭アドレスを示すラベルも作る。CASL2のDC相当
                VALUEには10進定数、16進定数、文字定数、アドレス定数、リテラルを指定できる
"#
        }
        "add-ds" => todo!(),
        "copy-mem" => todo!(),
        "default-cmd" => todo!(),
        "dump-code" => todo!(),
        "dump-mem" => todo!(),
        "fill-mem" => {
            r#"
    fill-mem <ADDRESS> <LENGTH> <VALUE>
                メモリの指定アドレスから指定の長さ分を指定の値で埋める
                    ADDRESS  .. 16進定数,アドレス定数
                    LENGTH   .. 正の10進数,16進定数
                    VALUE    .. 10進定数,16進定数,文字定数(1文字),アドレス定数,リテラル

                ※各種定数については help constat で説明"#
        }
        "find-code" => todo!(),
        "find-src" => todo!(),
        "find-value" => todo!(),
        "help" => todo!(),
        "list-files" => todo!(),
        "mode" => todo!(),
        "quit" => todo!(),
        "remove-breakpoint" => todo!(),
        "reset" => todo!(),
        "restart" => todo!(),
        "run" => todo!(),
        "set-breakpoint" => todo!(),
        "set-by-file" => todo!(),
        "set-label" => todo!(),
        "set-mem" => {
            r#"
    set-mem <ADDRESS> <VALUE1>[,<VALUE2>..]
                値をメモリの指定アドレスに書き込む
                値を複数列挙した場合はアドレスから連続した領域に書き込まれる
                値が文字定数の場合は文字数分の連続した領域に書き込まれる
                    ADDRESS  .. 16進定数,アドレス定数
                    VALUE*   .. 10進定数,16進定数,文字定数,アドレス定数,リテラル

                ※各種定数については help constat で説明"#
        }
        "set-reg" => {
            r#"
    set-reg <REGISTER> <VALUE>
                値をレジスタに設定する
                    REGISTER .. GR0～GR7
                    VALUE    .. 10進定数,16進定数,文字定数(1文字),アドレス定数,リテラル

                    REGISTER .. OF,SF,ZF
                    VALUE    .. 0,1

                    REGISTER .. PR,SP
                    VALUE    .. 16進定数,アドレス定数

                ※各種定数については help constat で説明"#
        }
        "set-start" => todo!(),
        "show-labels" => todo!(),
        "show-mem" => todo!(),
        "show-mem-stat" => todo!(),
        "show-reg" => todo!(),
        "show-src" => todo!(),
        "show-state" => todo!(),
        "show-var" => todo!(),
        "skip" => todo!(),
        "step" => todo!(),
        "write-code" => todo!(),
        _ => "コマンド名が正しくありません",
    };

    writeln!(stdout, "{}", description)?;

    Ok(())
}

//
// help [<TARGET>] (BASIC)
//
fn show_command_help_for_basic<W: Write>(cmd: Option<&str>, stdout: &mut W) -> io::Result<()> {
    writeln!(stdout)?;
    writeln!(stdout, "コマンドヘルプ")?;

    let cmd = match cmd {
        Some(cmd) => cmd.to_ascii_lowercase(),
        None => {
            writeln!(
                stdout,
                r#"
    default-cmd [<DEBUG_COMMAND>]
                空行が入力されたときの挙動を設定する
    fill-arr <VAR_NAME> <VALUE>
                現在地点のプログラムの配列変数の全ての要素を指定の値で埋める。値はBASICリテラルのみ
    help <COMMAND_NAME>
                指定デバッガコマンドの詳細ヘルプを表示する
    list-files
                読み込んだソースファイルの一覧を表示する
    mode <MODE>
                指定したモードに切り替える
    quit
                テスト実行を中止する
    remove-breakpoint <BASIC_PROGRAM_ENTRY> <STATEMENT_ID1>[,<STATEMENT_ID2>..]
                指定したBASICプログラムからブレークポイントを解除する
    reset
                プログラムを最初から実行しなおす。プログラムをファイルから読み込みなおし配置される
    restart
                プログラムを最初の位置から実行しなおす。メモリの状態はクリアされずに最後の実行状態のまま維持される
    run [<BASIC_STEP_LIMIT> [<COMET2_STEP_LIMIT>]]
                次のブレークポイントまで実行する
    set-breakpoint <BASIC_PROGRAM_ENTRY> <STATEMENT_ID1>[,<STATEMENT_ID2>..]
                指定したBASICプログラムにブレークポイントを設定する
    set-by-file <FILE_PATH>
                ファイルに列挙された設定系のデバッガコマンドを実行する
    set-elem <VAR_NAME> <INDEX> <VALUE>
                現在地点のプログラムの配列変数の要素に値を設定する。値はBASICリテラルのみ
    set-len <STR_VAR_NAME> <LENGTH>
                現在地点のプログラムの文字列変数の長さ情報のみを書き換える
    set-var <VAR_NAME> <VALUE1>[,<VALUE2>..]
                現在地点のプログラムの変数に値を設定する。値はBASICリテラルのみ
    show-execute-stat [<BASIC_PROGRAM_ENTRY>]
                指定したBASICプログラムのコード実行の統計情報ぽいものを表示する
    show-src [<BASIC_PROGRAM_ENTRY>]
                指定したBASICプログラムのコードを表示する
    show-state
                直近の実行(run,skip,step)の結果を再表示する
    show-var [<BASIC_PROGRAM_ENTRY> [<VAR_NAME1>[,<VAR_NAME2>..]]]
                指定したBASICプログラムの変数名を表示する。可能な場合は値も表示する
    skip [<BASIC_STEP_LIMIT> [<COMET2_STEP_LIMIT>]]
                現在地点のサブルーチンを抜けるまで実行する
    s    [<BASIC_STEP_COUNT> [<COMET2_STEP_LIMIT>]]
    step [<BASIC_STEP_COUNT> [<COMET2_STEP_LIMIT>]]
                指定ステップ数だけ実行する。BASIC_STEP_COUNT省略時は1ステップだけ実行する
"#
            )?;
            return Ok(());
        }
    };

    let description = match cmd.as_str() {
        "default-cmd" => todo!(),
        "fill-arr" => todo!(),
        "help" => todo!(),
        "list-files" => todo!(),
        "mode" => todo!(),
        "quit" => todo!(),
        "remove-breakpoint" => todo!(),
        "reset" => todo!(),
        "restart" => todo!(),
        "run" => todo!(),
        "set-breakpoint" => todo!(),
        "set-by-file" => todo!(),
        "set-elem" => todo!(),
        "set-len" => todo!(),
        "set-var" => todo!(),
        "show-execute-stat" => todo!(),
        "show-src" => todo!(),
        "show-state" => todo!(),
        "show-var" => todo!(),
        "skip" => todo!(),
        "step" => todo!(),
        _ => "コマンド名が正しくありません",
    };

    writeln!(stdout, "{}", description)?;

    Ok(())
}
