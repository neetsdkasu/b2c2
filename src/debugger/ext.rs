use super::*;

pub(super) trait ResolveLabel<T> {
    fn resolve_label(&self, pg_label: &str, label: &T) -> Option<(usize, parser::VarType)>;
}

impl ResolveLabel<compiler::ValueLabel> for Emulator {
    fn resolve_label(
        &self,
        pg_label: &str,
        label: &compiler::ValueLabel,
    ) -> Option<(usize, parser::VarType)> {
        use compiler::ValueLabel::*;
        let local_labels = self.local_labels.get(pg_label)?;
        match label {
            VarBoolean(label) => {
                let pos = local_labels.get(label)?;
                Some((*pos, parser::VarType::Boolean))
            }
            VarInteger(label) => {
                let pos = local_labels.get(label)?;
                Some((*pos, parser::VarType::Integer))
            }
            VarRefBoolean(label) => {
                let pos = local_labels.get(label)?;
                let pos = self.mem[*pos] as usize;
                Some((pos, parser::VarType::Boolean))
            }
            VarRefInteger(label) => {
                let pos = local_labels.get(label)?;
                let pos = self.mem[*pos] as usize;
                Some((pos, parser::VarType::Integer))
            }
            MemBoolean(_) => todo!(),
            MemInteger(_) => todo!(),
            MemRefBoolean(_) => todo!(),
            MemRefInteger(_) => todo!(),
        }
    }
}

impl Value {
    // リテラル生成するので注意
    pub(super) fn take_all_values(
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
    pub(super) fn take_single_value(
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
    pub(super) fn take_all_values_without_literal(
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

    pub(super) fn take_single_value_without_literal(
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

impl<'a> casl2::Tokenizer<'a> {
    pub(super) fn extended_label(&mut self) -> Result<Option<ExtendedLabel>, &'static str> {
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
    pub(super) fn get_address(&self, emu: &Emulator) -> Result<usize, String> {
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

impl casl2::Command {
    // このやり方はちょっと厳しい・・・
    pub(super) fn op_code(&self) -> u16 {
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

    pub(super) fn first_word(&self) -> u16 {
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

    pub(super) fn len(&self) -> usize {
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
