use super::*;

pub(super) fn parse_casl2_command(
    emu: &Emulator,
    cmd: &str,
    resolve_adr: bool,
) -> Result<Code, String> {
    use std::fmt::Write;
    let mut iter = cmd.splitn(2, ' ').map(|s| s.trim());
    let mut cmd = format!(" {} ", iter.next().unwrap().to_ascii_uppercase());
    let mut count = 0;
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
                    None => return Err("引数が不正です".to_string()),
                    Some(token) => {
                        write!(&mut cmd, "{}", token).unwrap();
                    }
                },
            }
            count += 1;
            if tokenizer.comma() {
                cmd.push(',');
            } else {
                break;
            }
        }
        cmd.push_str(&tokenizer.rest());
    }

    if resolve_adr {
        if let Some(param) = cmd.strip_prefix(" OUT ") {
            return if count == 2 {
                Ok(Code::Out(param.trim().to_string()))
            } else {
                Err("引数が不正です".to_string())
            };
        } else if let Some(param) = cmd.strip_prefix(" IN ") {
            return if count == 2 {
                Ok(Code::In(param.trim().to_string()))
            } else {
                Err("引数が不正です".to_string())
            };
        }
    }

    match casl2::parse(&cmd) {
        Ok(stmt) => {
            if let [casl2::Statement::Code { command, .. }] = stmt.as_slice() {
                Ok(Code::Casl2(command.clone()))
            } else {
                Err("引数が不正です".to_string())
            }
        }
        Err(error) => Err(format!("{:?}", error)),
    }
}

pub(super) fn parse_just_u16_value(s: &str) -> Option<u16> {
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

pub(super) fn get_program_name(file: &str) -> io::Result<Option<String>> {
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

pub(super) fn auto_resolve_files<R: BufRead, W: Write>(
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
                    return Ok(REQUEST_QUIT);
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
                        return Ok(REQUEST_QUIT);
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

pub(super) fn resolve_files<R: BufRead, W: Write>(
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
            return Ok(REQUEST_QUIT);
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
                return Ok(REQUEST_QUIT);
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
                    return Ok(REQUEST_QUIT);
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

pub(super) fn get_op_code_form(op_code: u16, adr: Option<u16>) -> String {
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

pub(super) fn get_op_code_size(op_code: u16) -> usize {
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

pub(super) fn print_arr_label<W: Write>(
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

pub(super) fn print_str_label<W: Write>(
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

pub(super) fn print_value_label<W: Write>(
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