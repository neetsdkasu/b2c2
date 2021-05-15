use crate::casl2;
use crate::compiler;
use crate::jis_x_201;
use crate::parser;
use crate::stat;
use crate::Flags;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};

const RET_OP_CODE: u16 = 0x8100;
const REQUEST_RESTART: i32 = 0x300_0000;
const REQUEST_RESET: i32 = 0x400_0000;
const QUIT_TEST: i32 = 99;

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
            Ok(QUIT_TEST) => return Ok(0),
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
                    return Ok(QUIT_TEST);
                }
            }
        }
    }
}

pub fn run_basic(src_file: String, flags: Flags) -> io::Result<i32> {
    let _ = (src_file, flags);
    todo!()
}

#[derive(Clone, Copy)]
enum RunMode {
    Step(u64),
    GoToBreakPoint(u64),
    SkipSubroutine(u64),
}

struct State {
    err: Option<RuntimeError>,
    run_mode: RunMode,
    start_point: Option<usize>,
    default_cmd: Option<String>,
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
                Ok(QUIT_TEST) => return Ok(0),
                result => return result,
            }
        }

        emu.init_to_start(None);

        let mut state = State {
            err: None,
            run_mode: RunMode::Step(1),
            start_point: None,
            default_cmd: None,
        };

        loop {
            writeln!(stdout)?;
            show_state(&emu, &mut stdout, &state)?;
            show_reg(&emu, &mut stdout)?;

            match interactive(&mut emu, &mut stdin, &mut stdout, &mut state) {
                Ok(0) => {}
                Ok(REQUEST_RESTART) => continue,
                Ok(REQUEST_RESET) => break,
                Ok(QUIT_TEST) => return Ok(0),
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
                };
                match result {
                    Ok(0) => {}
                    result => return result,
                }
            }
        }
    }
}

impl Emulator {
    fn get_label(&self, pos: u16) -> (Vec<String>, Vec<String>) {
        let pos = pos as usize;
        let mut plabel = Vec::new();
        for (lb, p) in self.all_label_list.iter() {
            if *p == pos {
                plabel.push(lb.clone());
            }
        }
        let mut dlabel = Vec::new();
        for (lb, (p, _)) in self.labels_for_debug.iter() {
            if *p == pos {
                dlabel.push(lb.clone());
            }
        }
        for (lb, p) in self.alias_labels.iter() {
            if *p == pos {
                dlabel.push(lb.clone());
            }
        }
        (plabel, dlabel)
    }
}

fn show_state<W: Write>(emu: &Emulator, stdout: &mut W, state: &State) -> io::Result<()> {
    if let Some(pos) = state.start_point {
        let mut label = None;
        for (lb, p) in emu.all_label_list.iter() {
            if *p == pos {
                label = Some(lb);
                break;
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
                if i == 0 {
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

#[derive(Default)]
struct CodeInfo {
    pos: usize,
    mem_code: String,
    src_code: Option<(usize, String)>,
    src_file: Option<String>,
    src_entry_label: Option<String>,
    alias_labels: Vec<String>,
}

impl Emulator {
    fn get_code_info(&self, pos: u16) -> CodeInfo {
        use std::fmt::Write;
        let pos = pos as usize;
        let mut info = CodeInfo {
            pos,
            alias_labels: self
                .alias_labels
                .iter()
                .filter_map(|(k, p)| if *p == pos { Some(k.clone()) } else { None })
                .collect(),
            ..Default::default()
        };
        for (file, label, stmt) in self.program_list.iter() {
            if stmt.is_empty() {
                continue;
            }
            let (pos1, _) = stmt.first().unwrap();
            let (pos2, _) = stmt.last().unwrap();
            if pos < *pos1 || *pos2 < pos {
                continue;
            }
            let mut tmp = "";
            let mut index = match stmt.binary_search_by_key(&pos, |(p, _)| *p) {
                Ok(index) => index,
                Err(0) => {
                    tmp = "+ ";
                    0
                }
                Err(index) => {
                    tmp = "+ ";
                    index - 1
                }
            };
            let (pos, _) = &stmt[index];
            while index > 0 {
                let (p, _) = &stmt[index - 1];
                if p == pos {
                    index -= 1;
                } else {
                    break;
                }
            }
            for (_, code) in stmt.iter().skip(index).take_while(|(p, _)| p == pos) {
                match code {
                    casl2::Statement::Comment { .. } => {}
                    casl2::Statement::Code { command, .. } => match command {
                        casl2::Command::Start { .. } | casl2::Command::End => {}
                        _ => {
                            info.src_file = Some(file.clone());
                            info.src_entry_label = Some(label.clone());
                            info.src_code = Some((*pos, format!("{}{}", tmp, code)));
                            break;
                        }
                    },
                }
            }
            break;
        }
        if info.src_code.is_none() {
            for (key, (p, stmt)) in self.labels_for_debug.iter() {
                if *p == pos {
                    info.src_entry_label = Some(key.clone());
                    info.src_code = Some((*p, stmt.to_string()));
                    break;
                }
            }
        }
        let op_code = self.mem[pos];
        match get_op_code_size(op_code) {
            2 => {
                if let Some(adr) = self.mem.get(pos + 1).cloned() {
                    let bp = if self.break_points[pos] || self.break_points[pos + 1] {
                        "*"
                    } else {
                        " "
                    };
                    let s = get_op_code_form(op_code, Some(adr));
                    write!(&mut info.mem_code, " #{:04X}: {} {}", pos, bp, s).unwrap();
                    info
                } else {
                    let bp = if self.break_points[pos] { "*" } else { " " };
                    let s = get_op_code_form(op_code, None);
                    write!(&mut info.mem_code, " #{:04X}: {} {}", pos, bp, s).unwrap();
                    info
                }
            }
            _ => {
                let bp = if self.break_points[pos] { "*" } else { " " };
                let s = get_op_code_form(op_code, None);
                write!(&mut info.mem_code, " #{:04X}: {} {}", pos, bp, s).unwrap();
                info
            }
        }
    }
}

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

fn interactive<R: BufRead, W: Write>(
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
            return Ok(QUIT_TEST);
        }
        let mut line = line.trim();
        if line.is_empty() {
            if let Some(defcmd) = state.default_cmd.as_ref() {
                line = defcmd.as_str();
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
                        state.default_cmd = None;
                    } else {
                        state.default_cmd = Some(defcmd.to_string());
                    }
                }
                if let Some(defcmd) = state.default_cmd.as_ref() {
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
            "help" => show_command_help_before_start(cmd_and_param.next(), stdout)?,
            "list-files" => list_files(emu, stdout)?,
            "quit" => {
                writeln!(stdout, "テスト実行を中止します")?;
                return Ok(QUIT_TEST);
            }
            "remove-breakpoint" => set_breakpoint(emu, stdout, cmd_and_param.next(), false)?,
            "reset" => {
                writeln!(stdout, "エミュレータをリセットします")?;
                writeln!(stdout)?;
                return Ok(REQUEST_RESET);
            }
            "restart" => {
                emu.init_to_start(state.start_point);
                state.err = None;
                writeln!(stdout, "プログラムをリスタートします")?;
                writeln!(stdout)?;
                return Ok(REQUEST_RESTART);
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
            "set-by-file" => todo!(),
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
            "show-mem-stat" => todo!(),
            "show-reg" => show_reg(emu, stdout)?,
            "show-src" => todo!(),
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
            "write-code" => todo!(),
            _ => {
                writeln!(stdout, "コマンドが正しくありません")?;
            }
        }
    }
}

fn is_valid_boolean(v: u16) -> bool {
    v == 0 || v == 0xFFFF
}

impl Emulator {
    fn get_address_by_label_str(&self, label: &str) -> Result<usize, String> {
        let mut tokenizer = casl2::Tokenizer::new(label);
        match tokenizer.extended_label() {
            Err(msg) => Err(msg.to_string()),
            Ok(None) => Err("引数が不正です".to_string()),
            Ok(Some(lb)) => {
                if tokenizer.rest().is_empty() {
                    lb.get_address(self)
                } else {
                    Err("引数が不正です".to_string())
                }
            }
        }
    }
}

fn parse_comet2_command(
    emu: &Emulator,
    cmd: &str,
    resolve_adr: bool,
) -> Result<casl2::Command, String> {
    use std::fmt::Write;
    let mut iter = cmd.splitn(2, ' ').map(|s| s.trim());
    let mut cmd = format!(" {} ", iter.next().unwrap().to_ascii_uppercase());
    if let Some(param) = iter.next() {
        let mut tokenizer = casl2::Tokenizer::new(param);
        loop {
            match tokenizer.extended_label() {
                Err(msg) => return Err(msg.to_string()),
                Ok(Some(lb)) => {
                    if resolve_adr {
                        match casl2::Register::try_from(lb.to_string().as_str()) {
                            Ok(reg) => {
                                write!(&mut cmd, "{}", reg).unwrap();
                            }
                            Err(_) => {
                                let adr = lb.get_address(emu)?;
                                write!(&mut cmd, "#{:04X}", adr).unwrap();
                            }
                        }
                    } else {
                        write!(&mut cmd, "{}", lb).unwrap();
                    }
                }
                Ok(None) => match tokenizer.ignore_case_value() {
                    None => return Err("コマンドが不正です".to_string()),
                    Some(token) => {
                        write!(&mut cmd, "{}", token).unwrap();
                    }
                },
            }
            if tokenizer.comma() {
                cmd.push(',');
            } else {
                break;
            }
        }
        cmd.push_str(&tokenizer.rest());
    }

    match casl2::parse(&cmd) {
        Ok(stmt) => {
            if let [casl2::Statement::Code { command, .. }] = stmt.as_slice() {
                Ok(command.clone())
            } else {
                Err("引数が不正です".to_string())
            }
        }
        Err(error) => Err(format!("{:?}", error)),
    }
}

enum ViewType {
    Bool,
    Int,
    Str,
}

impl TryFrom<&str> for ViewType {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let s = s.to_ascii_lowercase();
        if "integer".starts_with(&s) {
            Ok(Self::Int)
        } else if "boolean".starts_with(&s) {
            Ok(Self::Bool)
        } else if "string".starts_with(&s) {
            Ok(Self::Str)
        } else {
            Err(())
        }
    }
}

fn parse_just_u16_value(s: &str) -> Option<u16> {
    match s.parse::<u16>() {
        Ok(v) => Some(v),
        Err(_) => {
            let mut tokenizer = casl2::Tokenizer::new(s);
            if let Some(v) = tokenizer.ignore_case_hex() {
                if tokenizer.rest().is_empty() {
                    Some(v)
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
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

    if len.checked_add(adr as u16).is_none() {
        writeln!(stdout, "長さ指定が大きすぎます")?;
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

    let cmd = match parse_comet2_command(emu, cmd, false) {
        Ok(cmd) => cmd,
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

    let cmd = match parse_comet2_command(emu, cmd, true) {
        Err(msg) => {
            writeln!(stdout, "{}", msg)?;
            return Ok(());
        }
        Ok(cmd) => cmd,
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
// show-var [<FILE_NAME> [<VAR_NAME1>[,<VAR_NAME2>..]]]
//
fn show_var<W: Write>(emu: &Emulator, stdout: &mut W, param: Option<&str>) -> io::Result<()> {
    let (file, info, var_list) = match param {
        None => {
            let (file, _, _) = emu.program_list.first().expect("BUG");
            (file.as_str(), emu.basic_info.get(file), None)
        }
        Some(param) => {
            let mut iter = param.splitn(2, ' ');
            let file = iter.next().unwrap();
            let var_list = iter.next().map(|s| s.split(',').collect::<Vec<_>>());
            (file, emu.basic_info.get(&file.to_string()), var_list)
        }
    };

    let (key, info) = match info {
        None => {
            writeln!(stdout, "変数情報が見つかりませんでした")?;
            return Ok(());
        }
        Some((key, info)) => (key, info),
    };

    writeln!(stdout, "{}の変数 (エントリラベルは{})", file, key)?;

    if let Some(var_list) = var_list {
        for name in var_list {
            if let Some((label, _)) = info.label_set.argument_labels.get(name) {
                print_value_label(emu, stdout, key, name, label)?;
            } else if let Some((label, _)) = info.label_set.arr_argument_labels.get(name) {
                print_arr_label(emu, stdout, key, name, label, false)?;
            } else if let Some((label, _)) = info.label_set.str_argument_labels.get(name) {
                print_str_label(emu, stdout, key, name, label, false)?;
            } else if let Some(label) = info.label_set.bool_var_labels.get(name) {
                print_value_label(emu, stdout, key, name, label)?;
            } else if let Some(label) = info.label_set.int_var_labels.get(name) {
                print_value_label(emu, stdout, key, name, label)?;
            } else if let Some(label) = info.label_set.str_var_labels.get(name) {
                print_str_label(emu, stdout, key, name, label, false)?;
            } else if let Some(label) = info.label_set.bool_arr_labels.get(name) {
                print_arr_label(emu, stdout, key, name, label, false)?;
            } else if let Some(label) = info.label_set.int_arr_labels.get(name) {
                print_arr_label(emu, stdout, key, name, label, false)?;
            } else {
                writeln!(stdout, "{}に変数{}は存在しません", file, name)?;
            }
        }
    } else {
        let mut omit_name = None;
        for (name, (label, _)) in info
            .label_set
            .argument_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_value_label(emu, stdout, key, name, label)?;
        }
        for (name, (label, _)) in info
            .label_set
            .str_argument_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_str_label(emu, stdout, key, name, label, true)?;
            omit_name = Some(name);
        }
        for (name, (label, _)) in info
            .label_set
            .arr_argument_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_arr_label(emu, stdout, key, name, label, true)?;
            omit_name = Some(name);
        }
        for (name, label) in info
            .label_set
            .bool_var_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_value_label(emu, stdout, key, name, label)?;
        }
        for (name, label) in info
            .label_set
            .int_var_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_value_label(emu, stdout, key, name, label)?;
        }
        for (name, label) in info
            .label_set
            .str_var_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_str_label(emu, stdout, key, name, label, true)?;
            omit_name = Some(name);
        }
        for (name, label) in info
            .label_set
            .bool_arr_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_arr_label(emu, stdout, key, name, label, true)?;
            omit_name = Some(name);
        }
        for (name, label) in info
            .label_set
            .int_arr_labels
            .iter()
            .collect::<BTreeMap<_, _>>()
        {
            print_arr_label(emu, stdout, key, name, label, true)?;
            omit_name = Some(name);
        }
        if let Some(name) = omit_name {
            writeln!(stdout)?;
            writeln!(
                stdout,
                "※省略された値を確認するには変数名を指定した実行が必要です　( 例: show-reg {} {} )",
                file, name
            )?;
        }
    }
    Ok(())
}

fn print_arr_label<W: Write>(
    emu: &Emulator,
    stdout: &mut W,
    key: &str,
    name: &str,
    label: &compiler::ArrayLabel,
    omit: bool,
) -> io::Result<()> {
    let map = emu.local_labels.get(key).expect("BUG");
    use compiler::ArrayLabel::*;
    match label {
        TempArrayOfBoolean(_str_labels, _size) => unreachable!("たぶん来ない"),
        TempArrayOfInteger(_str_labels, _size) => unreachable!("たぶん来ない"),
        VarArrayOfBoolean(label, size) => {
            let size = *size;
            let adr = *map.get(label).expect("BUG");
            if adr + size > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             (不正なアドレス)"#,
                    name, adr, label,
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            let mut broken = false;
            for i in 0..size {
                let v = emu.mem[adr + i];
                match v {
                    0x0000 => s.push_str("False "),
                    0xFFFF => s.push_str("True "),
                    _ => s.push_str("???? "),
                }
                t.push_str(&format!("#{:04X} ", v));
                if !is_valid_boolean(v) {
                    broken = true;
                }
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             Val: (省略){}"#,
                    name,
                    adr,
                    label,
                    if broken { " (異常値)" } else { "" }
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             Val: [{}] [{}]{}"#,
                    name,
                    adr,
                    label,
                    s.trim(),
                    t.trim(),
                    if broken { " (異常値)" } else { "" }
                )?;
            }
        }
        VarArrayOfInteger(label, size) => {
            let size = *size;
            let adr = *map.get(label).expect("BUG");
            if adr + size > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             (不正なアドレス)"#,
                    name, adr, label,
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            for i in 0..size {
                s.push_str(&format!("{} ", emu.mem[adr + i] as i16));
                t.push_str(&format!("#{:04X} ", emu.mem[adr + i]));
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             Val: (省略)"#,
                    name, adr, label
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             Val: [{}] [{}]"#,
                    name,
                    adr,
                    label,
                    s.trim(),
                    t.trim()
                )?;
            }
        }
        VarRefArrayOfBoolean(label, size) => {
            let size = *size;
            let refer = *map.get(label).expect("BUG");
            let adr = emu.mem[refer] as usize;
            if adr + size > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} (不正なアドレス)"#,
                    name, refer, label, adr
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            let mut broken = false;
            for i in 0..size {
                let v = emu.mem[adr + i];
                match v {
                    0x0000 => s.push_str("False "),
                    0xFFFF => s.push_str("True "),
                    _ => s.push_str("???? "),
                }
                t.push_str(&format!("#{:04X} ", v));
                if !is_valid_boolean(v) {
                    broken = true;
                }
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} Val: (省略){}"#,
                    name,
                    refer,
                    label,
                    adr,
                    if broken { " (異常値)" } else { "" }
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} Val: [{}] [{}]{}"#,
                    name,
                    refer,
                    label,
                    adr,
                    s.trim(),
                    t.trim(),
                    if broken { " (異常値)" } else { "" }
                )?;
            }
        }
        VarRefArrayOfInteger(label, size) => {
            let size = *size;
            let refer = *map.get(label).expect("BUG");
            let adr = emu.mem[refer] as usize;
            if adr + size > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} (不正なアドレス)"#,
                    name, refer, label, adr
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            for i in 0..size {
                s.push_str(&format!("{} ", emu.mem[adr + i] as i16));
                t.push_str(&format!("#{:04X} ", emu.mem[adr + i]));
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} Val: (省略)"#,
                    name, refer, label, adr
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} Val: [{}] [{}]"#,
                    name,
                    refer,
                    label,
                    adr,
                    s.trim(),
                    t.trim()
                )?;
            }
        }
        MemArrayOfBoolean { offset, size } => {
            let mem_pos = *map.get("MEM").expect("BUG");
            let base_adr = emu.mem[mem_pos] as usize;
            let size = *size;
            let adr = base_adr + *offset;
            if adr + size > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            (不正なアドレス)"#,
                    name, adr, offset
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            let mut broken = false;
            for i in 0..size {
                let v = emu.mem[adr + i];
                match v {
                    0x0000 => s.push_str("False "),
                    0xFFFF => s.push_str("True "),
                    _ => s.push_str("???? "),
                }
                t.push_str(&format!("#{:04X} ", v));
                if !is_valid_boolean(v) {
                    broken = true;
                }
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            Val: (省略){}"#,
                    name,
                    adr,
                    offset,
                    if broken { " (異常値)" } else { "" }
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            Val: [{}] [{}]{}"#,
                    name,
                    adr,
                    offset,
                    s.trim(),
                    t.trim(),
                    if broken { " (異常値)" } else { "" }
                )?;
            }
        }
        MemArrayOfInteger { offset, size } => {
            let mem_pos = *map.get("MEM").expect("BUG");
            let base_adr = emu.mem[mem_pos] as usize;
            let size = *size;
            let adr = base_adr + *offset;
            if adr + size > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            (不正なアドレス)"#,
                    name, adr, offset
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            for i in 0..size {
                s.push_str(&format!("{} ", emu.mem[adr + i] as i16));
                t.push_str(&format!("#{:04X} ", emu.mem[adr + i]));
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            Val: (省略)"#,
                    name, adr, offset
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}             Val: [{}] [{}]"#,
                    name,
                    adr,
                    offset,
                    s.trim(),
                    t.trim()
                )?;
            }
        }
        MemRefArrayOfBoolean { offset, size } => {
            let mem_pos = *map.get("MEM").expect("BUG");
            let base_adr = emu.mem[mem_pos] as usize;
            let size = *size;
            let refer = base_adr as usize + *offset as usize;
            if refer > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            (不正なアドレス)"#,
                    name, refer, offset
                )?;
                return Ok(());
            }
            let adr = emu.mem[refer] as usize;
            if adr + size > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} (不正なアドレス)"#,
                    name, refer, offset, adr
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            let mut broken = false;
            for i in 0..size {
                let v = emu.mem[adr + i];
                match v {
                    0x0000 => s.push_str("False "),
                    0xFFFF => s.push_str("True "),
                    _ => s.push_str("???? "),
                }
                t.push_str(&format!("#{:04X} ", v));
                if !is_valid_boolean(v) {
                    broken = true;
                }
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: (省略){}"#,
                    name,
                    refer,
                    offset,
                    adr,
                    if broken { " (異常値)" } else { "" }
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: [{}] [{}]{}"#,
                    name,
                    refer,
                    offset,
                    adr,
                    s.trim(),
                    t.trim(),
                    if broken { " (異常値)" } else { "" }
                )?;
            }
        }
        MemRefArrayOfInteger { offset, size } => {
            let mem_pos = *map.get("MEM").expect("BUG");
            let base_adr = emu.mem[mem_pos] as usize;
            let size = *size;
            let refer = base_adr as usize + *offset as usize;
            if refer > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            (不正なアドレス)"#,
                    name, refer, offset
                )?;
                return Ok(());
            }
            let adr = emu.mem[refer] as usize;
            if adr + size > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} (不正なアドレス)"#,
                    name, refer, offset, adr
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            for i in 0..size {
                s.push_str(&format!("{} ", emu.mem[adr + i] as i16));
                t.push_str(&format!("#{:04X} ", emu.mem[adr + i]));
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: (省略)"#,
                    name, refer, offset, adr
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: [{}] [{}]"#,
                    name,
                    refer,
                    offset,
                    adr,
                    s.trim(),
                    t.trim()
                )?;
            }
        }
    }
    Ok(())
}

fn print_str_label<W: Write>(
    emu: &Emulator,
    stdout: &mut W,
    key: &str,
    name: &str,
    label: &compiler::StrLabels,
    omit: bool,
) -> io::Result<()> {
    let map = emu.local_labels.get(key).expect("BUG");
    use compiler::StrLabelType::*;
    match &label.label_type {
        Const(_str) => {
            // IN/OUTの定数 LB**/LL**
            unreachable!("たぶん来ない")
        }
        Lit(_str) => {
            // リテラル ='abc'/=3
            unreachable!("たぶん来ない")
        }
        Temp => {
            // 一時変数 TB**/TL**
            unreachable!("たぶん来ない")
        }
        Var | ArgVal => {
            // 文字列変数 SB**/SL**
            // 引数 ARG*/ARG*
            let adr = *map.get(&label.len).expect("BUG");
            let len = emu.mem[adr] as usize;
            let value = len as i16;
            let broken = len > 256;
            writeln!(
                stdout,
                " {:<18} #{:04X} {:<8}             Val: {:6} [#{:04X}]{}",
                name,
                adr,
                label.len,
                value,
                len,
                if broken { " (異常値)" } else { "" }
            )?;
            let adr = *map.get(&label.pos).expect("BUG");
            if adr + len.min(256) > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             (不正なアドレス)"#,
                    "", adr, label.pos
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            for i in 0..len.min(256) {
                let ch = emu.mem[adr + i];
                t.push_str(&format!("#{:04X} ", ch));
                let ch = jis_x_201::convert_to_char(ch as u8, true);
                s.push(ch);
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             Val: (省略)"#,
                    "", adr, label.pos
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}             Val: "{}" [{}]"#,
                    "",
                    adr,
                    label.pos,
                    s.replace('"', r#""""#),
                    t.trim()
                )?;
            }
        }
        ArgRef => {
            // 引数(参照型) ARG*/ARG*
            let adr = *map.get(&label.len).expect("BUG");
            let refer = emu.mem[adr] as usize;
            let len = emu.mem[refer] as usize;
            let value = len as i16;
            let broken = len > 256;
            writeln!(
                stdout,
                " {:<18} #{:04X} {:<8}  Ref: #{:04X} Val: {:6} [#{:04X}]{}",
                name,
                adr,
                label.len,
                refer,
                value,
                len,
                if broken { " (異常値)" } else { "" }
            )?;
            let adr = *map.get(&label.pos).expect("BUG");
            let refer = emu.mem[adr] as usize;
            if refer + len.min(256) > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} (不正なアドレス)"#,
                    "", adr, label.pos, refer
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            for i in 0..len.min(256) {
                let ch = emu.mem[refer + i];
                t.push_str(&format!("#{:04X} ", ch));
                let ch = jis_x_201::convert_to_char(ch as u8, true);
                s.push(ch);
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} Val: (省略)"#,
                    "", adr, label.pos, refer
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} {:<8}  Ref: #{:04X} Val: "{}" [{}]"#,
                    "",
                    adr,
                    label.pos,
                    refer,
                    s.replace('"', r#""""#),
                    t.trim()
                )?;
            }
        }
        MemRef(offset) => {
            // メモリ(MEMからのオフセット)
            let mem_pos = *map.get("MEM").expect("BUG");
            let base_adr = emu.mem[mem_pos] as usize;
            let refer = base_adr + offset;
            if refer > emu.mem.len() {
                writeln!(
                    stdout,
                    " {:<18} #{:04X} (MEM)+#{:04X} Ref: #---- (不正なアドレス)",
                    name, refer, offset
                )?;
                return Ok(());
            }
            let adr = emu.mem[refer] as usize;
            let len = emu.mem[adr] as usize;
            let value = len as i16;
            let broken = len > 256;
            writeln!(
                stdout,
                " {:<18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: {:6} [#{:04X}]{}",
                name,
                refer,
                offset,
                adr,
                value,
                len,
                if broken { " (異常値)" } else { "" }
            )?;
            let refer = base_adr + offset + 1;
            if refer > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #---- (不正なアドレス)"#,
                    "",
                    refer,
                    offset + 1
                )?;
                return Ok(());
            }
            let adr = emu.mem[refer] as usize;
            if adr + len > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} (不正なアドレス)"#,
                    "",
                    refer,
                    offset + 1,
                    adr
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            for i in 0..len.min(256) {
                let ch = emu.mem[adr + i];
                t.push_str(&format!("#{:04X} ", ch));
                let ch = jis_x_201::convert_to_char(ch as u8, true);
                s.push(ch);
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: (省略)"#,
                    "",
                    refer,
                    offset + 1,
                    adr
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: "{}" [{}]"#,
                    "",
                    refer,
                    offset + 1,
                    adr,
                    s.replace('"', r#""""#),
                    t.trim()
                )?;
            }
        }
        MemVal(offset) => {
            // メモリ(MEMからのオフセット)
            let mem_pos = *map.get("MEM").expect("BUG");
            let base_adr = emu.mem[mem_pos] as usize;
            let adr = base_adr + offset;
            if adr > emu.mem.len() {
                writeln!(
                    stdout,
                    " {:<18} #{:04X} (MEM)+#{:04X}            (不正なアドレス)",
                    name, adr, offset
                )?;
                return Ok(());
            }
            let len = emu.mem[adr] as usize;
            let value = len as i16;
            let broken = len > 256;
            writeln!(
                stdout,
                " {:<18} #{:04X} (MEM)+#{:04X}            Val: {:6} [#{:04X}]{}",
                name,
                adr,
                offset,
                value,
                len,
                if broken { " (異常値)" } else { "" }
            )?;
            let adr = base_adr + offset + 1;
            if adr + len.min(256) > emu.mem.len() {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            (不正なアドレス)"#,
                    "",
                    adr,
                    offset + 1
                )?;
                return Ok(());
            }
            let mut s = String::new();
            let mut t = String::new();
            for i in 0..len.min(256) {
                let ch = emu.mem[adr + i];
                t.push_str(&format!("#{:04X} ", ch));
                let ch = jis_x_201::convert_to_char(ch as u8, true);
                s.push(ch);
            }
            if omit {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            Val: (省略)"#,
                    "",
                    adr,
                    offset + 1
                )?;
            } else {
                writeln!(
                    stdout,
                    r#" {:18} #{:04X} (MEM)+#{:04X}            Val: "{}" [{}]"#,
                    "",
                    adr,
                    offset + 1,
                    s.replace('"', r#""""#),
                    t.trim()
                )?;
            }
        }
    }
    Ok(())
}

fn print_value_label<W: Write>(
    emu: &Emulator,
    stdout: &mut W,
    key: &str,
    name: &str,
    label: &compiler::ValueLabel,
) -> io::Result<()> {
    let map = emu.local_labels.get(key).expect("BUG");
    use compiler::ValueLabel::*;
    match label {
        VarBoolean(s) => {
            let adr = *map.get(s).expect("BUG");
            let raw = emu.mem[adr];
            let value = if raw == 0 {
                "False"
            } else if raw == 0xFFFF {
                "True"
            } else {
                "????"
            };
            writeln!(
                stdout,
                " {:<18} #{:04X} {:<8}             Val: {:6} [#{:04X}]{}",
                name,
                adr,
                s,
                value,
                raw,
                if is_valid_boolean(raw) {
                    ""
                } else {
                    " (異常値)"
                }
            )?;
        }
        VarInteger(s) => {
            let adr = *map.get(s).expect("BUG");
            let raw = emu.mem[adr];
            let value = raw as i16;
            writeln!(
                stdout,
                " {:<18} #{:04X} {:<8}             Val: {:6} [#{:04X}]",
                name, adr, s, value, raw
            )?;
        }
        VarRefBoolean(s) => {
            let adr = *map.get(s).expect("BUG");
            let refer = emu.mem[adr];
            let raw = emu.mem[refer as usize];
            let value = if raw == 0 {
                "False"
            } else if raw == 0xFFFF {
                "True"
            } else {
                "????"
            };
            writeln!(
                stdout,
                " {:<18} #{:04X} {:<8}  Ref: #{:04X} Val: {:6} [#{:04X}]{}",
                name,
                adr,
                s,
                refer,
                value,
                raw,
                if is_valid_boolean(raw) {
                    ""
                } else {
                    " (異常値)"
                }
            )?;
        }
        VarRefInteger(s) => {
            let adr = *map.get(s).expect("BUG");
            let refer = emu.mem[adr];
            let raw = emu.mem[refer as usize];
            let value = raw as i16;
            writeln!(
                stdout,
                " {:<18} #{:04X} {:<8}  Ref: #{:04X} Val: {:6} [#{:04X}]",
                name, adr, s, refer, value, raw
            )?;
        }
        MemBoolean(offset) => {
            let pos = *map.get(&"MEM".to_string()).expect("BUG");
            let adr = emu.mem[pos] as usize + offset;
            let raw = emu.mem[adr];
            let value = if raw == 0 {
                "False"
            } else if raw == 0xFFFF {
                "True"
            } else {
                "????"
            };
            writeln!(
                stdout,
                " {:<18} #{:04X} (MEM)+#{:04X}            Val: {:6} [#{:04X}]{}",
                name,
                adr,
                offset,
                value,
                raw,
                if is_valid_boolean(raw) {
                    ""
                } else {
                    " (異常値)"
                }
            )?;
        }
        MemInteger(offset) => {
            let pos = *map.get(&"MEM".to_string()).expect("BUG");
            let adr = emu.mem[pos] as usize + offset;
            let raw = emu.mem[adr];
            let value = raw as i16;
            writeln!(
                stdout,
                " {:<18} #{:04X} (MEM)+#{:04X}            Val: {:6} [#{:04X}]",
                name, adr, offset, value, raw
            )?;
        }
        MemRefBoolean(offset) => {
            let pos = *map.get(&"MEM".to_string()).expect("BUG");
            let adr = emu.mem[pos] as usize + offset;
            let refer = emu.mem[adr];
            let raw = emu.mem[refer as usize];
            let value = if raw == 0 {
                "False"
            } else if raw == 0xFFFF {
                "True"
            } else {
                "????"
            };
            writeln!(
                stdout,
                " {:<18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: {:6} [#{:04X}]{}",
                name,
                adr,
                offset,
                refer,
                value,
                raw,
                if is_valid_boolean(raw) {
                    ""
                } else {
                    " (異常値)"
                }
            )?;
        }
        MemRefInteger(offset) => {
            let pos = *map.get(&"MEM".to_string()).expect("BUG");
            let adr = emu.mem[pos] as usize + offset;
            let refer = emu.mem[adr];
            let raw = emu.mem[refer as usize];
            let value = raw as i16;
            writeln!(
                stdout,
                " {:<18} #{:04X} (MEM)+#{:04X} Ref: #{:04X} Val: {:6} [#{:04X}]",
                name, adr, offset, refer, value, raw
            )?;
        }
    }
    Ok(())
}

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

impl<'a> casl2::Tokenizer<'a> {
    fn extended_label(&mut self) -> Result<Option<ExtendedLabel>, &'static str> {
        // MAIN
        // MAIN:MEM
        // @GR1 @PR @SP
        // #0123
        // + - ( )

        let mut value_stack = vec![];
        let mut op_stack = vec![];

        loop {
            // first char:  A-Z @ ( #
            if let Some(glabel) = self.ignore_case_word() {
                if self.colon() {
                    if let Some(llabel) = self.ignore_case_word() {
                        let glabel = glabel.to_ascii_uppercase();
                        let llabel = llabel.to_ascii_uppercase();
                        value_stack.push(ExtendedLabel::LocalLabel(glabel, llabel));
                    } else {
                        return Err("引数が不正です");
                    }
                } else {
                    let glabel = glabel.to_ascii_uppercase();
                    value_stack.push(ExtendedLabel::GlobalLabel(glabel));
                }
            } else if let Some(u) = self.uinteger() {
                value_stack.push(ExtendedLabel::DecConst(u));
            } else if let Some(hex) = self.ignore_case_hex() {
                value_stack.push(ExtendedLabel::HexConst(hex));
            } else if self.dot() {
                value_stack.push(ExtendedLabel::ProgramRegister);
            } else if self.atmark() {
                if let Some(reg) = self.ignore_case_word() {
                    if "PR".eq_ignore_ascii_case(&reg) {
                        value_stack.push(ExtendedLabel::ProgramRegister);
                    } else if "SP".eq_ignore_ascii_case(&reg) {
                        value_stack.push(ExtendedLabel::StackPointer);
                    } else if let Ok(reg) = casl2::Register::try_from(reg.as_str()) {
                        value_stack.push(ExtendedLabel::GeneralRegister(reg));
                    } else {
                        return Err("引数が不正です");
                    }
                } else {
                    return Err("引数が不正です");
                }
            } else if self.open_bracket() {
                op_stack.push(0);
                continue;
            } else {
                // 初回到達のみ別のTokenのケース、それ以外はエラー
                return if value_stack.is_empty() && op_stack.is_empty() {
                    Ok(None)
                } else {
                    Err("引数が不正です")
                };
            }
            while let Some(op) = op_stack.pop() {
                match op {
                    0 => {
                        // (
                        op_stack.push(0);
                        break;
                    }
                    1 => {
                        if value_stack.len() < 2 {
                            return Err("引数が不正です");
                        }
                        // +
                        let v2 = value_stack.pop().unwrap();
                        let v1 = value_stack.pop().unwrap();
                        value_stack.push(ExtendedLabel::Sum(Box::new(v1), Box::new(v2)));
                    }
                    2 => {
                        if value_stack.len() < 2 {
                            return Err("引数が不正です");
                        }
                        // -
                        let v2 = value_stack.pop().unwrap();
                        let v1 = value_stack.pop().unwrap();
                        value_stack.push(ExtendedLabel::Diff(Box::new(v1), Box::new(v2)));
                    }
                    _ => unreachable!("BUG"),
                }
            }
            // after char: + - ) Token
            while self.close_bracket() {
                if let Some(0) = op_stack.pop() {
                    if let Some(v) = value_stack.pop() {
                        value_stack.push(ExtendedLabel::Load(Box::new(v)));
                    } else {
                        return Err("引数が不正です");
                    }
                } else {
                    return Err("引数が不正です");
                }
            }
            if self.plus() {
                op_stack.push(1);
            } else if self.minus() {
                op_stack.push(2);
            } else {
                // ExtendedLabelが完結してればOK(以降に別Tokenが続くなど)、それ以外はErr
                return if value_stack.len() == 1 && op_stack.is_empty() {
                    Ok(value_stack.pop())
                } else {
                    Err("引数が不正です")
                };
            }
        }
    }
}

impl fmt::Display for ExtendedLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::GlobalLabel(label) => label.fmt(f),
            Self::LocalLabel(gl, ll) => format!("{}:{}", gl, ll).fmt(f),
            Self::GeneralRegister(reg) => format!("@{}", reg).fmt(f),
            Self::ProgramRegister => "@PR".fmt(f),
            Self::StackPointer => "@SP".fmt(f),
            Self::DecConst(d) => d.to_string().fmt(f),
            Self::HexConst(h) => format!("#{:04X}", h).fmt(f),
            Self::Load(adr) => format!("({})", adr.to_string()).fmt(f),
            Self::Sum(adr1, adr2) => format!("{}+{}", adr1.to_string(), adr2.to_string()).fmt(f),
            Self::Diff(adr1, adr2) => format!("{}-{}", adr1.to_string(), adr2.to_string()).fmt(f),
        }
    }
}

impl ExtendedLabel {
    fn get_address(&self, emu: &Emulator) -> Result<usize, String> {
        match self {
            Self::GlobalLabel(label) => {
                let label = label.to_ascii_uppercase();
                match emu.program_labels.get(&label) {
                    Some(adr) => Ok(*adr),
                    None => match emu.labels_for_debug.get(&label) {
                        Some((adr, _)) => Ok(*adr),
                        None => match emu.alias_labels.get(&label) {
                            Some(adr) => Ok(*adr),
                            None => Err(format!("ラベル{}が見つかりません", label)),
                        },
                    },
                }
            }
            Self::LocalLabel(glabel, llabel) => {
                match emu.local_labels.get(&glabel.to_ascii_uppercase()) {
                    Some(label_map) => match label_map.get(&llabel.to_ascii_uppercase()) {
                        Some(adr) => Ok(*adr),
                        None => Err(format!(
                            "プログラム{}にラベル{}が見つかりません",
                            glabel, llabel
                        )),
                    },
                    None => Err(format!("ラベル{}が見つかりません", glabel)),
                }
            }
            Self::GeneralRegister(reg) => Ok(emu.general_registers[*reg as usize] as usize),
            Self::ProgramRegister => Ok(emu.program_register),
            Self::StackPointer => Ok(emu.stack_pointer),
            Self::DecConst(d) => Ok(*d as usize),
            Self::HexConst(h) => Ok(*h as usize),
            Self::Load(exlabel) => {
                let adr = exlabel.get_address(emu)?;
                if let Some(v) = emu.mem.get(adr) {
                    Ok(*v as usize)
                } else {
                    Err(format!("{}はオーバーフローしました", exlabel))
                }
            }
            Self::Sum(ex_adr1, ex_adr2) => {
                let adr1 = ex_adr1.get_address(emu)?;
                let adr2 = ex_adr2.get_address(emu)?;
                if let Some(adr) = adr1.checked_add(adr2) {
                    if adr < emu.mem.len() {
                        Ok(adr)
                    } else {
                        Err(format!("{}はオーバーフローしました", self))
                    }
                } else {
                    Err(format!("{}はオーバーフローしました", self))
                }
            }
            Self::Diff(ex_adr1, ex_adr2) => {
                let adr1 = ex_adr1.get_address(emu)?;
                let adr2 = ex_adr2.get_address(emu)?;
                if let Some(adr) = adr1.checked_sub(adr2) {
                    if adr < emu.mem.len() {
                        Ok(adr)
                    } else {
                        Err(format!("{}はオーバーフローしました", self))
                    }
                } else {
                    Err(format!("{}はオーバーフローしました", self))
                }
            }
        }
    }
}

enum Value {
    Int(u16),
    Str(String),
}

impl Value {
    // リテラル生成するので注意
    fn take_all_values(
        emu: &mut Emulator,
        tokenizer: &mut casl2::Tokenizer<'_>,
    ) -> Result<Vec<Value>, String> {
        let mut values = vec![];

        loop {
            let value = Self::take_single_value(emu, tokenizer)?;

            values.push(value);

            if !tokenizer.comma() {
                if tokenizer.rest().is_empty() {
                    return Ok(values);
                } else {
                    return Err("引数が不正です".to_string());
                }
            }
        }
    }

    // リテラル生成するので注意
    fn take_single_value(
        emu: &mut Emulator,
        tokenizer: &mut casl2::Tokenizer<'_>,
    ) -> Result<Value, String> {
        match tokenizer.extended_label()? {
            Some(label) => label.get_address(emu).map(|adr| Value::Int(adr as u16)),
            None => {
                let value = match tokenizer.ignore_case_value() {
                    Some(value) => value,
                    None => return Err("引数が不正です".to_string()),
                };
                match value {
                    casl2::Token::Word(s) => match emu.get_address_by_label_str(s.as_str()) {
                        Ok(adr) => Ok(Value::Int(adr as u16)),
                        Err(msg) => Err(msg),
                    },
                    casl2::Token::Dec(d) => Ok(Value::Int(d as u16)),
                    casl2::Token::Hex(h) => Ok(Value::Int(h)),
                    casl2::Token::Str(s) => Ok(Value::Str(
                        jis_x_201::convert_kana_wide_full_to_half(&s)
                            .chars()
                            .map(jis_x_201::convert_from_char)
                            .map(|ch| jis_x_201::convert_to_char(ch, true))
                            .collect(),
                    )),
                    casl2::Token::LitDec(d) => match emu.make_literal_int(d as u16) {
                        Ok(pos) => Ok(Value::Int(pos as u16)),
                        Err(msg) => Err(msg.to_string()),
                    },
                    casl2::Token::LitHex(h) => match emu.make_literal_int(h) {
                        Ok(pos) => Ok(Value::Int(pos as u16)),
                        Err(msg) => Err(msg.to_string()),
                    },
                    casl2::Token::LitStr(s) => match emu.make_literal_str(&s) {
                        Ok(pos) => Ok(Value::Int(pos as u16)),
                        Err(msg) => Err(msg.to_string()),
                    },
                }
            }
        }
    }

    // リテラル生成するので注意
    fn take_all_values_without_literal(
        emu: &Emulator,
        tokenizer: &mut casl2::Tokenizer<'_>,
    ) -> Result<Vec<Value>, String> {
        let mut values = vec![];

        loop {
            let value = Self::take_single_value_without_literal(emu, tokenizer)?;

            values.push(value);

            if !tokenizer.comma() {
                if tokenizer.rest().is_empty() {
                    return Ok(values);
                } else {
                    return Err("引数が不正です".to_string());
                }
            }
        }
    }

    fn take_single_value_without_literal(
        emu: &Emulator,
        tokenizer: &mut casl2::Tokenizer<'_>,
    ) -> Result<Value, String> {
        match tokenizer.extended_label()? {
            Some(label) => label.get_address(emu).map(|adr| Value::Int(adr as u16)),
            None => {
                let value = match tokenizer.ignore_case_value() {
                    Some(value) => value,
                    None => return Err("引数が不正です".to_string()),
                };
                match value {
                    casl2::Token::Word(s) => match emu.get_address_by_label_str(s.as_str()) {
                        Ok(adr) => Ok(Value::Int(adr as u16)),
                        Err(msg) => Err(msg),
                    },
                    casl2::Token::Dec(d) => Ok(Value::Int(d as u16)),
                    casl2::Token::Hex(h) => Ok(Value::Int(h)),
                    casl2::Token::Str(s) => Ok(Value::Str(
                        jis_x_201::convert_kana_wide_full_to_half(&s)
                            .chars()
                            .map(jis_x_201::convert_from_char)
                            .map(|ch| jis_x_201::convert_to_char(ch, true))
                            .collect(),
                    )),
                    casl2::Token::LitDec(..)
                    | casl2::Token::LitHex(..)
                    | casl2::Token::LitStr(..) => Err("リテラルは指定できません".to_string()),
                }
            }
        }
    }
}

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

fn show_command_help_before_start<W: Write>(cmd: Option<&str>, stdout: &mut W) -> io::Result<()> {
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
    quit
                テスト実行を中止する
    remove-breakpoint [<ADDRESS1>[,<ADDRESS2>..]]
                指定アドレスのブレークポイントを解除する。アドレス省略時はPRの示すアドレス
    reset
                プログラムを最初から実行しなおす。プログラムをファイルから読み込みなおし配置される
    restart
                プログラムを最初の位置から実行しなおす。メモリやGRやFRは終了時点の状態が維持される
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
    show-src <ADDRESS> [<LENGTH>]
                指定したアドレス位置から指定長さ分の範囲にあるコードを表示する
    show-state
                直近の実行(run,skip,step)の結果を再表示する
    show-var [<BASIC_SRC_FILE> [<VAR_NAME1>[,<VAR_NAME2>..]]]
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

fn auto_resolve_files<R: BufRead, W: Write>(
    emu: &mut Emulator,
    stdin: &mut R,
    stdout: &mut W,
    src_dir: &Path,
    flags: &Flags,
) -> io::Result<i32> {
    let mut label_cache = HashMap::new();
    let dst_dir = flags.create_dst_dir()?;
    let casl2_flags = {
        let mut flags = flags.clone();
        flags.compiler = Default::default();
        flags
    };
    while let Some((label, files)) = emu.suggest_load_file(&dst_dir, src_dir)? {
        let file = {
            let mut found = None;
            for file in files.iter() {
                if let Some(name) = label_cache.get(file) {
                    if name == &label {
                        found = Some(file.clone());
                        break;
                    } else {
                        continue;
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
                writeln!(
                    stdout,
                    "ラベル{}を解決できるファイルのパスを入力してください",
                    label
                )?;
                writeln!(
                    stdout,
                    "-1を入力するとテスト実行を中止します。0を入力するとRETのみのダミーコードで解決されます。"
                )?;
                writeln!(stdout)?;
                write!(stdout, "> ")?;
                stdout.flush()?;
                let mut line = String::new();
                if stdin.read_line(&mut line)? == 0 {
                    eprintln!("入力がキャンセルされました");
                    io::stderr().flush()?;
                    writeln!(stdout, "テスト実行を中止します")?;
                    return Ok(QUIT_TEST);
                }
                let line = line.trim();
                match line.parse::<i32>() {
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
                        return Ok(QUIT_TEST);
                    }
                    _ => {
                        let path = Path::new(line);
                        if path.exists() && path.is_file() {
                            line.to_string()
                        } else {
                            writeln!(stdout, "{}が見つかりません", line)?;
                            continue;
                        }
                    }
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
        eprintln!("{}を読み込みました", file);
    }
    Ok(0)
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
    let flags = {
        let mut flags = flags.clone();
        flags.compiler.program_name = None;
        flags
    };
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
        writeln!(stdout)?;
        write!(stdout, "> ")?;
        stdout.flush()?;
        let mut line = String::new();
        if stdin.read_line(&mut line)? == 0 {
            eprintln!("入力がキャンセルされました");
            io::stderr().flush()?;
            writeln!(stdout, "テスト実行を中止します")?;
            return Ok(QUIT_TEST);
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
                    } else {
                        continue;
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
                return Ok(QUIT_TEST);
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
                    return Ok(QUIT_TEST);
                }
                Ok(num) if num >= 1 && num <= count => files[num as usize - 1].clone(),
                _ => {
                    let path = Path::new(line);
                    if path.exists() && path.is_file() {
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
const CALLSTACK_MAX_SIZE: usize = 0x2000; /* 8192語 */

// プログラムコードを埋め込みを開始する位置 (0はダメ)
const BEGIN_OF_PROGRAM: usize = 0x0100; /* 256 */

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
const INITIAL_PROGRAM_REGISTER: usize = 0x0040; // 64

struct Emulator {
    debug_mode: bool,

    // COMET2のメモリ
    mem: Vec<u16>,

    // レジスタ GR0～GR7
    general_registers: Vec<u16>,

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
    step_count: u64,

    // ブレークポイント
    break_points: Vec<bool>,

    // 各メモリ位置のコードとしての実行回数
    execute_count: Vec<u64>,
    // 各メモリ位置の参照回数 (LDやADDLやCPAやOUTなどの命令による)
    access_count: Vec<u64>,
    // 各メモリ位置の更新回数 (STやINの命令による)
    update_count: Vec<u64>,

    // サブルーチンのネストの追跡用スタック(呼び出し先アドレス、戻りアドレス)
    // RETでCOMET2のスタックメモリから取り出す値がこのスタックの値と同値だったら正常とみなす(?)
    program_stack: Vec<(usize, usize)>,
    // 異常RETの記録  (RET命令自体のメモリ位置、RET呼び出し時点のstep_count、スタックから取り出した戻りアドレス)
    wrong_ret: Vec<(usize, u64, usize)>,

    // BASICステップ (DebugBasicStepのid)
    basic_step: Option<u16>,
    // BASICステップ数 (DebugBasicStepの呼び出し回数)
    basic_step_count: u32,

    // プログラムの開始点のエントリラベル
    // (START命令に引数があるとBEGIN_OF_PROGRAMにはならないこともあるため)
    start_point: Option<String>,

    // デバッガコマンドから設定したラベル (ラベル、(メモリ位置、ラベル生成コード))
    labels_for_debug: HashMap<String, (usize, casl2::Statement)>,

    // デバッガコマンドで生成したリテラル (ラベル、(メモリ位置、値))
    literals_for_debug: HashMap<String, (usize, Value)>,

    alias_labels: HashMap<String, usize>,

    // 全ラベルのリスト(ソースコード由来のみ)
    all_label_list: Vec<(String, usize)>,

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
            debug_mode: true,
            mem: vec![0; MEMORY_SIZE],
            general_registers: vec![0; 8],
            stack_pointer: CALLSTACK_START_POSITION,
            program_register: INITIAL_PROGRAM_REGISTER,
            overflow_flag: false,
            sign_flag: false,
            zero_flag: false,

            break_points: vec![false; MEMORY_SIZE],

            last_run_position: INITIAL_PROGRAM_REGISTER - 2,
            step_count: 0,
            basic_step: None,
            basic_step_count: 0,
            labels_for_debug: HashMap::new(),
            literals_for_debug: HashMap::new(),
            alias_labels: HashMap::new(),
            program_stack: Vec::new(),
            wrong_ret: Vec::new(),
            execute_count: vec![0; MEMORY_SIZE],
            access_count: vec![0; MEMORY_SIZE],
            update_count: vec![0; MEMORY_SIZE],

            all_label_list: Vec::new(),
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

    fn init_to_start(&mut self, start_point: Option<usize>) {
        let position = start_point.unwrap_or_else(|| {
            self.start_point
                .as_ref()
                .and_then(|label| self.program_labels.get(label))
                .copied()
                .expect("BUG")
        });

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

        self.program_stack.clear();
        self.program_stack
            .push((position, INITIAL_PROGRAM_REGISTER));
        self.wrong_ret.clear();

        self.execute_count.fill(0);
        self.access_count.fill(0);
        self.update_count.fill(0);

        // GRとFRは不定
    }

    fn enough_remain(&self, len: usize) -> bool {
        len < self.mem.len() - CALLSTACK_MAX_SIZE - self.compile_pos
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
    fn make_literal_int(&mut self, v: u16) -> Result<usize, &str> {
        let key = v.to_string();
        if let Some((pos, _)) = self.literals_for_debug.get(&key) {
            return Ok(*pos);
        }
        if !self.enough_remain(1) {
            return Err("メモリ不足でリテラルを生成できません");
        }
        let pos = self.compile_pos;
        self.mem[pos] = v;
        self.compile_pos += 1;
        self.literals_for_debug.insert(key, (pos, Value::Int(v)));
        Ok(pos)
    }

    fn make_literal_str(&mut self, s: &str) -> Result<usize, &str> {
        if s.is_empty() {
            return Err("空の文字定数のリテラルは生成できません");
        }
        let bs = jis_x_201::convert_kana_wide_full_to_half(s)
            .chars()
            .map(jis_x_201::convert_from_char)
            .collect::<Vec<_>>();
        let s = bs
            .iter()
            .map(|ch| jis_x_201::convert_to_char(*ch, true))
            .collect::<String>();
        let key = format!("'{}", s);
        if let Some((pos, _)) = self.literals_for_debug.get(&key) {
            return Ok(*pos);
        }
        if !self.enough_remain(bs.len()) {
            return Err("メモリ不足でリテラルを生成できません");
        }
        let pos = self.compile_pos;
        for (i, ch) in bs.iter().enumerate() {
            self.mem[pos + i] = *ch as u16;
        }
        self.compile_pos += bs.len();
        self.literals_for_debug.insert(key, (pos, Value::Str(s)));
        Ok(pos)
    }

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
    MemoryAccessOutOfBounds {
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

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NormalTermination { position } => {
                format!("正常終了(終了位置: #{:04X})", position).fmt(f)
            }
            Self::AbnormalTermination { position, op_code } => {
                format!("異常終了(終了位置: #{:04X} コード: {})", position, op_code).fmt(f)
            }
            Self::ExecutePermissionDenied { position } => format!(
                "許可されてない領域の実行が発生 (発生位置: #{:04X})",
                position
            )
            .fmt(f),
            Self::AccessPermissionDenied {
                position,
                op_code,
                address,
            } => format!(
                "許可されてないアドレスへのアクセスが発生 (発生位置: #{:04X} コード: {} 参照先: {})",
                position, op_code, address
            )
            .fmt(f),
            Self::MemoryAccessOutOfBounds {
                position,
                op_code,
                address,
            } => format!(
                "メモリの範囲外のアドレスへのアクセスが発生 (発生位置: #{:04X} コード: {} 参照先: {})",
                position, op_code, address
            )
            .fmt(f),
            Self::InvalidOpCode { position, op_code } => format!(
                "命令の割り当てがない値を命令として実行が発生 (発生位置: #{:04X} コード: {})",
                position, op_code
            )
            .fmt(f),
            Self::StackOverflow {
                position,
                op_code,
                stack_pointer,
            } => format!(
                "スタックのオーバーフローが発生 (発生位置: #{:04X} コード: {} スタック位置: {})",
                position, op_code, stack_pointer
            )
            .fmt(f),
            Self::StackEmpty { position, op_code } => format!(
                "スタックが空の状態でのPOPが発生 (発生位置: #{:04X} コード: {})",
                position, op_code
            )
            .fmt(f),
            Self::IoError(error) => format!("IOエラーが発生 ({})", error).fmt(f),
        }
    }
}

impl Emulator {
    fn step_through_code<R, W>(&mut self, stdin: &mut R, stdout: &mut W) -> Result<(), RuntimeError>
    where
        R: io::BufRead,
        W: io::Write,
    {
        let pr = self.program_register;
        if pr >= 0xFFFF {
            return Err(RuntimeError::ExecutePermissionDenied { position: pr });
        }
        self.execute_count[pr] += 1;
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
                op_code: get_op_code_form(op_code, None),
            });
        }

        if op_code_size == 1 {
            if !(0..8).contains(&r1) || !(0..8).contains(&r2) {
                return Err(RuntimeError::InvalidOpCode {
                    position: pr,
                    op_code: get_op_code_form(op_code, None),
                });
            }
            match ((op_code >> 8) & 0xFF) as u8 {
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
                            op_code: get_op_code_form(op_code, None),
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
                            op_code: get_op_code_form(op_code, None),
                        });
                    }
                    let adr = self.mem[self.stack_pointer] as usize;
                    if self
                        .program_stack
                        .last()
                        .filter(|(_, r)| *r == adr)
                        .is_some()
                    {
                        self.program_stack.pop();
                    } else {
                        self.wrong_ret.push((pr, self.step_count, adr));
                    }
                    self.stack_pointer += 1;
                    self.program_register = adr;
                    if adr == INITIAL_PROGRAM_REGISTER {
                        if self.stack_pointer == CALLSTACK_START_POSITION {
                            return Err(RuntimeError::NormalTermination { position: pr });
                        } else {
                            return Err(RuntimeError::AbnormalTermination {
                                position: pr,
                                op_code: get_op_code_form(op_code, None),
                            });
                        }
                    }
                }
                0x00 if r1 == 0 && r2 == 0 => {
                    // NOP
                }
                _ => {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, None),
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
                op_code: get_op_code_form(op_code, Some(adr)),
            });
        }

        match ((op_code >> 8) & 0xFF) as u8 {
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
                        op_code: get_op_code_form(op_code, Some(adr)),
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
                        op_code: get_op_code_form(op_code, Some(adr)),
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
                        op_code: get_op_code_form(op_code, Some(adr)),
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
                    if self.debug_mode {
                        write!(stdout, "[IN] ? ")?;
                    } else {
                        write!(stdout, "? ")?;
                    }
                    stdout.flush()?;
                    self.update_count[len] += 1;
                    let mut line = String::new();
                    match stdin.read_line(&mut line)? {
                        0 => self.mem[len] = (-1_i16) as u16,
                        _ => {
                            let line = line.lines().next().unwrap();
                            let line = jis_x_201::convert_kana_wide_full_to_half(&line);
                            self.mem[len] = line.chars().count().min(256) as u16;
                            for (i, ch) in line.chars().enumerate().take(256) {
                                if pos + i > 0xFFFF {
                                    return Err(RuntimeError::MemoryAccessOutOfBounds {
                                        position: pr,
                                        op_code: get_op_code_form(op_code, Some(adr)),
                                        address: pos + i,
                                    });
                                }
                                let ch = jis_x_201::convert_from_char(ch) as u16;
                                self.mem[pos + i] = ch;
                                self.update_count[pos + i] += 1;
                            }
                        }
                    }
                } else if value == 2 {
                    // OUT
                    let pos = self.general_registers[1] as usize;
                    let len = self.general_registers[2] as usize;
                    let len = self.mem[len] as usize;
                    self.access_count[len] += 1;
                    let mut line = String::new();
                    for i in 0..len {
                        if pos + i > 0xFFFF {
                            return Err(RuntimeError::MemoryAccessOutOfBounds {
                                position: pr,
                                op_code: get_op_code_form(op_code, Some(adr)),
                                address: len,
                            });
                        }
                        let ch = (self.mem[pos + i] & 0xFF) as u8;
                        line.push(jis_x_201::convert_to_char(ch, true));
                        self.access_count[pos + i] += 1;
                    }
                    if self.debug_mode {
                        writeln!(stdout, "[OUT] {}", line)?;
                    } else {
                        writeln!(stdout, "{}", line)?;
                    }
                    stdout.flush()?;
                }
                // SVC後のGR,FRは不定なので、値を保持しないという仕様にした(雑)
                self.general_registers[0] ^=
                    (self.general_registers[1] << 1) ^ (self.general_registers[2] >> 1);
                self.general_registers[1] ^=
                    (self.general_registers[2] << 1) ^ (self.general_registers[3] >> 1);
                self.general_registers[2] ^=
                    (self.general_registers[3] << 1) ^ (self.general_registers[4] >> 1);
                self.general_registers[3] ^=
                    (self.general_registers[4] << 1) ^ (self.general_registers[5] >> 1);
                self.general_registers[4] ^=
                    (self.general_registers[5] << 1) ^ (self.general_registers[6] >> 1);
                self.general_registers[5] ^=
                    (self.general_registers[6] << 1) ^ (self.general_registers[7] >> 1);
                self.general_registers[6] ^=
                    (self.general_registers[7] << 1) ^ (self.general_registers[0] >> 1);
                self.general_registers[7] ^=
                    (self.general_registers[0] << 1) ^ (self.general_registers[3] >> 1);
                self.overflow_flag = (self.general_registers[3] & 4) == 0;
                self.sign_flag = (self.general_registers[5] & 4) == 0;
                self.zero_flag = (self.general_registers[7] & 4) == 0;
                return Ok(());
            }
            0xE0 => {
                // DebugBasicStep id
                if r1 != 0 || r2 != 0 {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, Some(adr)),
                    });
                }
                self.basic_step = Some(adr);
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

        if access > 0xFFFF {
            return Err(RuntimeError::MemoryAccessOutOfBounds {
                position: pr,
                op_code: get_op_code_form(op_code, Some(adr)),
                address: access,
            });
        }

        match ((op_code >> 8) & 0xFF) as u8 {
            0x10 => {
                // LD r,adr,x (OF=0,SF,ZF)
                let value = self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x11 => {
                // ST r,adr,x
                let r1_value = self.general_registers[r1];
                self.mem[access] = r1_value;
                self.update_count[access] += 1;
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
                self.access_count[access] += 1;
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
                self.access_count[access] += 1;
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
                self.access_count[access] += 1;
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
                self.access_count[access] += 1;
                return Ok(());
            }
            0x30 => {
                // AND r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] & self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x31 => {
                // OR r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] | self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x32 => {
                // XOR r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] ^ self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x40 => {
                // CPA r,adr,x (OF=0,SF,ZF)
                let r1_value = self.general_registers[r1] as i16;
                let value = self.mem[access] as i16;
                self.overflow_flag = false;
                self.sign_flag = r1_value < value;
                self.zero_flag = r1_value == value;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x41 => {
                // CPL r,adr,x (OF=0,SF,ZF)
                let r1_value = self.general_registers[r1];
                let value = self.mem[access];
                self.overflow_flag = false;
                self.sign_flag = r1_value < value;
                self.zero_flag = r1_value == value;
                self.access_count[access] += 1;
                return Ok(());
            }
            _ => {}
        }

        if r1 != 0 {
            return Err(RuntimeError::InvalidOpCode {
                position: pr,
                op_code: get_op_code_form(op_code, Some(adr)),
            });
        }

        match ((op_code >> 8) & 0xFF) as u8 {
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
                        op_code: get_op_code_form(op_code, Some(adr)),
                        stack_pointer: self.stack_pointer,
                    });
                }
                self.stack_pointer = sp;
                let pr = self.program_register;
                self.mem[sp] = pr as u16;
                self.program_register = access;
                self.program_stack.push((access, pr));
            }
            _ => {
                // UNKNOWN CODE
                return Err(RuntimeError::InvalidOpCode {
                    position: pr,
                    op_code: get_op_code_form(op_code, Some(adr)),
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
                        self.save_casl2_src(&mut path, "EOF", &src, flags.statistics, false)?;
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
                        self.save_casl2_src(&mut path, "ALLOC", &src, flags.statistics, false)?;
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
                let overwrite = compiler::utils::is_valid_program_name(&name);
                self.save_casl2_src(&mut path, &name, &casl2_src, flags.statistics, overwrite)?;
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
        overwrite: bool,
    ) -> io::Result<()> {
        let file_name = format!("{}.cas", name);
        path.push(file_name);
        {
            self.loaded_files
                .insert(path.to_string_lossy().into_owned());

            if overwrite || !path.exists() {
                let mut dst_file = fs::File::create(&path)?;

                for stmt in casl2_src.iter() {
                    writeln!(&mut dst_file, "{}", stmt)?;
                }

                dst_file.flush()?;
                eprintln!("生成されたファイル: {}", path.display());
            }
        }
        path.pop();

        if statistics {
            let file_name = format!("{}.stat.txt", name);
            path.push(file_name);
            self.loaded_files
                .insert(path.to_string_lossy().into_owned());
            if overwrite || !path.exists() {
                let statistics = stat::analyze(casl2_src);
                fs::write(&path, statistics)?;
                eprintln!("生成されたファイル: {}", path.display());
            }
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

        if let Some(msg) = casl2::utils::find_syntax_error(&casl2_src) {
            eprintln!("SyntaxError{{ {} }}", msg);
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
                let overwrite = compiler::utils::is_valid_program_name(&name);
                self.save_casl2_src(&mut path, &name, &casl2_src, flags.statistics, overwrite)?;
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
        let mut code = Vec::new();
        let mut labels = HashMap::new();
        let mut literals = HashMap::new();
        let mut label_list = Vec::new();
        let mut end_label: Option<String> = None;

        for stmt in src {
            let pos = self.compile_pos;
            if let casl2::Statement::Code { label, command, .. } = &stmt {
                let len = command.len();
                if !self.enough_remain(len) {
                    return Err("メモリ不足でプログラムをロードできませんでした".into());
                }
                self.compile_pos += len;
                if let Some(label) = label {
                    match command {
                        casl2::Command::Start { .. } => {}
                        casl2::Command::End => {
                            end_label = Some(label.as_string().clone());
                        }
                        _ => {
                            labels.insert(label.as_string().clone(), pos);
                            label_list.push((format!("{}:{}", name, label.as_str()), pos));
                        }
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

        let mut lit_stmt = Vec::new();

        for (adr, pos) in literals.iter_mut() {
            match adr {
                casl2::Adr::LiteralDec(d) => {
                    if !self.enough_remain(1) {
                        return Err("メモリ不足でプログラムをロードできませんでした".into());
                    }
                    *pos = self.compile_pos;
                    self.mem[*pos] = *d as u16;
                    self.compile_pos += 1;
                    let stmt = format!(" DC {0} ; ={0}", d);
                    lit_stmt.push((*pos, casl2::parse(&stmt).unwrap().pop().unwrap()));
                }
                casl2::Adr::LiteralHex(h) => {
                    if !self.enough_remain(1) {
                        return Err("メモリ不足でプログラムをロードできませんでした".into());
                    }
                    *pos = self.compile_pos;
                    self.mem[*pos] = *h;
                    self.compile_pos += 1;
                    let stmt = format!(" DC #{0:04X} ; =#{0:04X}", h);
                    lit_stmt.push((*pos, casl2::parse(&stmt).unwrap().pop().unwrap()));
                }
                casl2::Adr::LiteralStr(ls) => {
                    let s = jis_x_201::convert_kana_wide_full_to_half(&ls);
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
                    let stmt = format!(" DC '{0}' ; ='{0}'", ls.replace('\'', "''"));
                    lit_stmt.push((*pos, casl2::parse(&stmt).unwrap().pop().unwrap()));
                }
                _ => {}
            }
        }

        if let Some(end_index) = code.iter().position(|(_, stmt)| {
            matches!(
                stmt,
                casl2::Statement::Code {
                    command: casl2::Command::End,
                    ..
                }
            )
        }) {
            let mut rest = code.split_off(end_index);
            code.extend(lit_stmt);
            let pos = self.compile_pos - 1;
            if let Some(label) = end_label {
                labels.insert(label.clone(), pos);
                label_list.push((format!("{}:{}", name, label.as_str()), pos));
            }
            rest.iter_mut().for_each(|(p, _)| *p = pos);
            code.extend(rest);
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
                        self.mem[*mempos + 18] = *label_pos as u16;
                    } else if let Some(label_pos) = self.program_labels.get(pos).copied() {
                        self.mem[*mempos + 18] = label_pos as u16;
                    } else {
                        self.unknown_labels
                            .entry(pos.clone())
                            .or_insert_with(Vec::new)
                            .push(*mempos + 18);
                    }
                    if let Some(label_pos) = labels.get(len) {
                        self.mem[*mempos + 20] = *label_pos as u16;
                    } else if let Some(label_pos) = self.program_labels.get(len).copied() {
                        self.mem[*mempos + 20] = label_pos as u16;
                    } else {
                        self.unknown_labels
                            .entry(len.clone())
                            .or_insert_with(Vec::new)
                            .push(*mempos + 20);
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

        let program_pos = *self.program_labels.get(&name).expect("BUG");

        if let Some(pos_list) = self.unknown_labels.remove(&name) {
            for pos in pos_list {
                self.mem[pos] = program_pos as u16;
            }
        }

        // 分かりにくい条件式…
        if label_list
            .first()
            .filter(|(_, pos)| *pos < program_pos)
            .is_none()
        {
            self.all_label_list.push((name.clone(), program_pos));
            self.all_label_list.extend(label_list);
        } else {
            for (label, pos) in label_list {
                if program_pos == pos {
                    self.all_label_list.push((name.clone(), program_pos));
                }
                self.all_label_list.push((label, pos));
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
                        PUSH 0,GR3
                        PUSH 0,GR4
                        PUSH 0,GR5
                        PUSH 0,GR6
                        PUSH 0,GR7
                        LD  GR1,GR0
                        PUSH 0,GR1
                        LAD GR1,0
                        LAD GR2,0
                        SVC 1
                        POP GR1
                        LD  GR0,GR1
                        POP GR7
                        POP GR6
                        POP GR5
                        POP GR4
                        POP GR3
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
                        PUSH 0,GR3
                        PUSH 0,GR4
                        PUSH 0,GR5
                        PUSH 0,GR6
                        PUSH 0,GR7
                        LD  GR1,GR0
                        PUSH 0,GR1
                        LAD GR1,0
                        LAD GR2,0
                        SVC 2
                        POP GR1
                        LD  GR0,GR1
                        POP GR7
                        POP GR6
                        POP GR5
                        POP GR4
                        POP GR3
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

fn get_op_code_form(op_code: u16, adr: Option<u16>) -> String {
    let r1 = (op_code >> 4) & 0xF;
    let r2 = op_code & 0xF;
    if !(0..8).contains(&r1) || !(0..8).contains(&r2) {
        if let Some(adr) = adr {
            return format!("[#{:04X} #{:04X}] UNKNOWN", op_code, adr);
        } else {
            return format!("[#{:04X}      ] UNKNOWN", op_code);
        }
    }
    if let Some(adr) = adr {
        match (op_code >> 8) & 0xFF {
            0xE0 => format!("[#{0:04X} #{1:04X}] DEBUGBASICSTEP {1}", op_code, adr),
            code if r2 == 0 => match code {
                0x12 => format!("[#{0:04X} #{2:04X}] LAD   GR{1},#{2:04X}", op_code, r1, adr),
                0x10 => format!("[#{0:04X} #{2:04X}] LD    GR{1},#{2:04X}", op_code, r1, adr),
                0x11 => format!("[#{0:04X} #{2:04X}] ST    GR{1},#{2:04X}", op_code, r1, adr),
                0x20 => format!("[#{0:04X} #{2:04X}] ADDA  GR{1},#{2:04X}", op_code, r1, adr),
                0x22 => format!("[#{0:04X} #{2:04X}] ADDL  GR{1},#{2:04X}", op_code, r1, adr),
                0x21 => format!("[#{0:04X} #{2:04X}] SUBA  GR{1},#{2:04X}", op_code, r1, adr),
                0x23 => format!("[#{0:04X} #{2:04X}] SUBL  GR{1},#{2:04X}", op_code, r1, adr),
                0x30 => format!("[#{0:04X} #{2:04X}] AND   GR{1},#{2:04X}", op_code, r1, adr),
                0x31 => format!("[#{0:04X} #{2:04X}] OR    GR{1},#{2:04X}", op_code, r1, adr),
                0x32 => format!("[#{0:04X} #{2:04X}] XOR   GR{1},#{2:04X}", op_code, r1, adr),
                0x40 => format!("[#{0:04X} #{2:04X}] CPA   GR{1},#{2:04X}", op_code, r1, adr),
                0x41 => format!("[#{0:04X} #{2:04X}] CPL   GR{1},#{2:04X}", op_code, r1, adr),
                0x50 => format!("[#{0:04X} #{2:04X}] SLA   GR{1},#{2:04X}", op_code, r1, adr),
                0x51 => format!("[#{0:04X} #{2:04X}] SRA   GR{1},#{2:04X}", op_code, r1, adr),
                0x52 => format!("[#{0:04X} #{2:04X}] SLL   GR{1},#{2:04X}", op_code, r1, adr),
                0x53 => format!("[#{0:04X} #{2:04X}] SRL   GR{1},#{2:04X}", op_code, r1, adr),
                _ if r1 != 0 => format!("[#{:04X} #{:04X}] UNKNOWN", op_code, adr),
                0x70 => format!("[#{0:04X} #{1:04X}] PUSH  #{1:04X}", op_code, adr),
                0xF0 => format!("[#{0:04X} #{1:04X}] SVC   #{1:04X}", op_code, adr),
                0x65 => format!("[#{0:04X} #{1:04X}] JPL   #{1:04X}", op_code, adr),
                0x61 => format!("[#{0:04X} #{1:04X}] JMI   #{1:04X}", op_code, adr),
                0x62 => format!("[#{0:04X} #{1:04X}] JNZ   #{1:04X}", op_code, adr),
                0x63 => format!("[#{0:04X} #{1:04X}] JZE   #{1:04X}", op_code, adr),
                0x66 => format!("[#{0:04X} #{1:04X}] JOV   #{1:04X}", op_code, adr),
                0x64 => format!("[#{0:04X} #{1:04X}] JUMP  #{1:04X}", op_code, adr),
                0x80 => format!("[#{0:04X} #{1:04X}] CALL  #{1:04X}", op_code, adr),
                _ => format!("[#{:04X} #{:04X}] UNKNOWN", op_code, adr),
            },
            0x12 => format!(
                "[#{0:04X} #{2:04X}] LAD   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x10 => format!(
                "[#{0:04X} #{2:04X}] LD    GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x11 => format!(
                "[#{0:04X} #{2:04X}] ST    GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x20 => format!(
                "[#{0:04X} #{2:04X}] ADDA  GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x22 => format!(
                "[#{0:04X} #{2:04X}] ADDL  GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x21 => format!(
                "[#{0:04X} #{2:04X}] SUBA  GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x23 => format!(
                "[#{0:04X} #{2:04X}] SUBL  GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x30 => format!(
                "[#{0:04X} #{2:04X}] AND   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x31 => format!(
                "[#{0:04X} #{2:04X}] OR    GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x32 => format!(
                "[#{0:04X} #{2:04X}] XOR   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x40 => format!(
                "[#{0:04X} #{2:04X}] CPA   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x41 => format!(
                "[#{0:04X} #{2:04X}] CPL   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x50 => format!(
                "[#{0:04X} #{2:04X}] SLA   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x51 => format!(
                "[#{0:04X} #{2:04X}] SRA   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x52 => format!(
                "[#{0:04X} #{2:04X}] SLL   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            0x53 => format!(
                "[#{0:04X} #{2:04X}] SRL   GR{1},#{2:04X},GR{3}",
                op_code, r1, adr, r2
            ),
            _ if r1 != 0 => format!("[#{:04X} #{:04X}] UNKNOWN", op_code, adr),
            0x70 => format!("[#{0:04X} #{1:04X}] PUSH  #{1:04X},GR{2}", op_code, adr, r2),
            0xF0 => format!("[#{0:04X} #{1:04X}] SVC   #{1:04X},GR{2}", op_code, adr, r2),
            0x65 => format!("[#{0:04X} #{1:04X}] JPL   #{1:04X},GR{2}", op_code, adr, r2),
            0x61 => format!("[#{0:04X} #{1:04X}] JMI   #{1:04X},GR{2}", op_code, adr, r2),
            0x62 => format!("[#{0:04X} #{1:04X}] JNZ   #{1:04X},GR{2}", op_code, adr, r2),
            0x63 => format!("[#{0:04X} #{1:04X}] JZE   #{1:04X},GR{2}", op_code, adr, r2),
            0x66 => format!("[#{0:04X} #{1:04X}] JOV   #{1:04X},GR{2}", op_code, adr, r2),
            0x64 => format!("[#{0:04X} #{1:04X}] JUMP  #{1:04X},GR{2}", op_code, adr, r2),
            0x80 => format!("[#{0:04X} #{1:04X}] CALL  #{1:04X},GR{2}", op_code, adr, r2),
            _ => format!("[#{:04X} #{:04X}] UNKNOWN", op_code, adr),
        }
    } else {
        match (op_code >> 8) & 0xFF {
            0x14 => format!("[#{:04X}      ] LD    GR{},GR{}", op_code, r1, r2),
            0x24 => format!("[#{:04X}      ] ADDA  GR{},GR{}", op_code, r1, r2),
            0x26 => format!("[#{:04X}      ] ADDL  GR{},GR{}", op_code, r1, r2),
            0x25 => format!("[#{:04X}      ] SUBA  GR{},GR{}", op_code, r1, r2),
            0x27 => format!("[#{:04X}      ] SUBL  GR{},GR{}", op_code, r1, r2),
            0x34 => format!("[#{:04X}      ] AND   GR{},GR{}", op_code, r1, r2),
            0x35 => format!("[#{:04X}      ] OR    GR{},GR{}", op_code, r1, r2),
            0x36 => format!("[#{:04X}      ] XOR   GR{},GR{}", op_code, r1, r2),
            0x44 => format!("[#{:04X}      ] CPA   GR{},GR{}", op_code, r1, r2),
            0x45 => format!("[#{:04X}      ] CPL   GR{},GR{}", op_code, r1, r2),
            _ if r2 != 0 => format!("[#{:04X}      ] UNKNOWN", op_code),
            0x71 => format!("[#{:04X}      ] POP   GR{}", op_code, r1),
            _ if r1 != 0 => format!("[#{:04X}      ] UNKNOWN", op_code),
            0x81 => format!("[#{:04X}      ] RET", op_code),
            0x00 => format!("[#{:04X}      ] NOP", op_code),
            _ => format!("[#{:04X}      ] UNKNOWN", op_code),
        }
    }
}

fn get_op_code_size(op_code: u16) -> usize {
    let r1 = (op_code >> 4) & 0xF;
    let r2 = op_code & 0xF;
    if !(0..8).contains(&r1) || !(0..8).contains(&r2) {
        return 0;
    }
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

        0x71 if r2 == 0 => 1, //format!("#{:04X}: POP GR{}", op_code, r1),
        0x81 if r1 == 0 && r2 == 0 => 1, //format!("#{:04X}: RET",op_code),
        0x00 if r1 == 0 && r2 == 0 => 1, //format!("#{:04X}: NOP",op_code),

        0xE0 if r1 == 0 && r2 == 0 => 2, //format!("#{:04X}: DEBUGBASICSTEP {}", op_code, adr),

        0x12 => 2, //format!("#{:04X}: LAD GR{},#{:04X},GR{}", op_code, r1, adr, r2),
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

        _ if r1 != 0 => 0,

        0x70 => 2, //format!("#{:04X}: PUSH #{:04X},GR{}",op_code,  adr, r2),
        0xF0 => 2, //format!("#{:04X}: SVC #{:04X},GR{}", op_code, adr, r2),
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
                // 0  0
                // 2  2 PUSH 0,GR1
                // 2  4 PUSH 0,GR2
                // 2  6 PUSH 0,GR3
                // 2  8 PUSH 0,GR4
                // 2 10 PUSH 0,GR5
                // 2 12 PUSH 0,GR6
                // 2 14 PUSH 0,GR7
                // 1 15 LD  GR1,GR0
                // 2 17 PUSH 0,GR1
                // 2 19 LAD GR1,BUF  (BUF offset 18)
                // 2 21 LAD GR2,LEN  (LEN offset 20)
                // 2 23 SVC 1
                // 1 24 POP GR1
                // 1 25 LD  GR0,GR1
                // 1 26 POP GR7
                // 1 27 POP GR6
                // 1 28 POP GR5
                // 1 29 POP GR4
                // 1 30 POP GR3
                // 1 31 POP GR2
                // 1 32 POP GR1
                32
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
