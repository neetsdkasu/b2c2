// b2c2-debugger crate::ext
// author: Leonardone @ NEETSDKASU

use super::*;

type ResolveLabelAddress = (usize, Option<usize>, parser::VarType);

pub(super) trait ResolveLabel<T> {
    fn resolve_label(&self, pg_label: &str, label: &T) -> Option<ResolveLabelAddress>;
}

impl ResolveLabel<compiler::StrLabels> for Emulator {
    fn resolve_label(
        &self,
        pg_label: &str,
        label: &compiler::StrLabels,
    ) -> Option<ResolveLabelAddress> {
        use compiler::StrLabelType::*;
        let local_labels = self.local_labels.get(pg_label)?;
        match &label.label_type {
            Const(_) | Lit(_) | Temp => unreachable!("γγΆγ"),
            Var | ArgVal => {
                let len = local_labels.get(&label.len)?;
                len.checked_add(1).filter(|p| *p <= self.mem.len())?;
                let pos = local_labels.get(&label.pos)?;
                pos.checked_add(256).filter(|p| *p <= self.mem.len())?;
                Some((*len, Some(*pos), parser::VarType::String))
            }
            ArgRef => {
                let len = local_labels.get(&label.len)?;
                let len = self.mem[*len] as usize;
                len.checked_add(1).filter(|p| *p <= self.mem.len())?;
                let pos = local_labels.get(&label.pos)?;
                let pos = self.mem[*pos] as usize;
                pos.checked_add(256).filter(|p| *p <= self.mem.len())?;
                Some((len, Some(pos), parser::VarType::String))
            }
            MemVal(offset) => {
                let mem = local_labels.get("MEM")?;
                let mem = self.mem[*mem] as usize;
                let len = mem.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                len.checked_add(257).filter(|p| *p <= self.mem.len())?;
                Some((len, Some(len + 1), parser::VarType::String))
            }
            MemRef(offset) => {
                let mem = local_labels.get("MEM")?;
                let mem = self.mem[*mem] as usize;
                let len = mem.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                let pos = len.checked_add(1).filter(|p| *p < self.mem.len())?;
                let len = self.mem[len] as usize;
                len.checked_add(1).filter(|p| *p <= self.mem.len())?;
                let pos = self.mem[pos] as usize;
                pos.checked_add(256).filter(|p| *p <= self.mem.len())?;
                Some((len, Some(pos), parser::VarType::String))
            }
        }
    }
}

impl ResolveLabel<compiler::ArrayLabel> for Emulator {
    fn resolve_label(
        &self,
        pg_label: &str,
        label: &compiler::ArrayLabel,
    ) -> Option<ResolveLabelAddress> {
        use compiler::ArrayLabel::*;
        let local_labels = self.local_labels.get(pg_label)?;
        match label {
            TempArrayOfBoolean(..) | TempArrayOfInteger(..) => unreachable!("γγΆγ"),
            VarArrayOfBoolean(label, size) => {
                let pos = local_labels.get(label)?;
                pos.checked_add(*size).filter(|p| *p <= self.mem.len())?;
                Some((*pos, None, parser::VarType::ArrayOfBoolean(*size)))
            }
            VarArrayOfInteger(label, size) => {
                let pos = local_labels.get(label)?;
                pos.checked_add(*size).filter(|p| *p <= self.mem.len())?;
                Some((*pos, None, parser::VarType::ArrayOfInteger(*size)))
            }
            VarRefArrayOfBoolean(label, size) => {
                let pos = local_labels.get(label)?;
                let pos = self.mem[*pos] as usize;
                pos.checked_add(*size).filter(|p| *p <= self.mem.len())?;
                Some((pos, None, parser::VarType::ArrayOfBoolean(*size)))
            }
            VarRefArrayOfInteger(label, size) => {
                let pos = local_labels.get(label)?;
                let pos = self.mem[*pos] as usize;
                Some((pos, None, parser::VarType::ArrayOfInteger(*size)))
            }
            MemArrayOfBoolean { offset, size } => {
                let pos = local_labels.get("MEM")?;
                let pos = self.mem[*pos] as usize;
                let pos = pos.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                pos.checked_add(*size).filter(|p| *p <= self.mem.len())?;
                Some((pos, None, parser::VarType::ArrayOfBoolean(*size)))
            }
            MemArrayOfInteger { offset, size } => {
                let pos = local_labels.get("MEM")?;
                let pos = self.mem[*pos] as usize;
                let pos = pos.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                pos.checked_add(*size).filter(|p| *p <= self.mem.len())?;
                Some((pos, None, parser::VarType::ArrayOfInteger(*size)))
            }
            MemRefArrayOfBoolean { offset, size } => {
                let pos = local_labels.get("MEM")?;
                let pos = self.mem[*pos] as usize;
                let pos = pos.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                let pos = self.mem[pos] as usize;
                pos.checked_add(*size).filter(|p| *p <= self.mem.len())?;
                Some((pos, None, parser::VarType::ArrayOfBoolean(*size)))
            }
            MemRefArrayOfInteger { offset, size } => {
                let pos = local_labels.get("MEM")?;
                let pos = self.mem[*pos] as usize;
                let pos = pos.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                let pos = self.mem[pos] as usize;
                pos.checked_add(*size).filter(|p| *p <= self.mem.len())?;
                Some((pos, None, parser::VarType::ArrayOfInteger(*size)))
            }
        }
    }
}

impl ResolveLabel<compiler::ValueLabel> for Emulator {
    fn resolve_label(
        &self,
        pg_label: &str,
        label: &compiler::ValueLabel,
    ) -> Option<ResolveLabelAddress> {
        use compiler::ValueLabel::*;
        let local_labels = self.local_labels.get(pg_label)?;
        match label {
            VarBoolean(label) => {
                let pos = local_labels.get(label)?;
                Some((*pos, None, parser::VarType::Boolean))
            }
            VarInteger(label) => {
                let pos = local_labels.get(label)?;
                Some((*pos, None, parser::VarType::Integer))
            }
            VarRefBoolean(label) => {
                let pos = local_labels.get(label)?;
                let pos = self.mem[*pos] as usize;
                Some((pos, None, parser::VarType::Boolean))
            }
            VarRefInteger(label) => {
                let pos = local_labels.get(label)?;
                let pos = self.mem[*pos] as usize;
                Some((pos, None, parser::VarType::Integer))
            }
            MemBoolean(offset) => {
                let pos = local_labels.get("MEM")?;
                let pos = self.mem[*pos] as usize;
                let pos = pos.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                Some((pos, None, parser::VarType::Boolean))
            }
            MemInteger(offset) => {
                let pos = local_labels.get("MEM")?;
                let pos = self.mem[*pos] as usize;
                let pos = pos.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                Some((pos, None, parser::VarType::Integer))
            }
            MemRefBoolean(offset) => {
                let pos = local_labels.get("MEM")?;
                let pos = self.mem[*pos] as usize;
                let pos = pos.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                let pos = self.mem[pos] as usize;
                Some((pos, None, parser::VarType::Boolean))
            }
            MemRefInteger(offset) => {
                let pos = local_labels.get("MEM")?;
                let pos = self.mem[*pos] as usize;
                let pos = pos.checked_add(*offset).filter(|p| *p < self.mem.len())?;
                let pos = self.mem[pos] as usize;
                Some((pos, None, parser::VarType::Integer))
            }
        }
    }
}

impl Value {
    // γͺγγ©γ«ηζγγγ?γ§ζ³¨ζ
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
                    return Err("εΌζ°γδΈζ­£γ§γ".to_string());
                }
            }
        }
    }

    // γͺγγ©γ«ηζγγγ?γ§ζ³¨ζ
    pub(super) fn take_single_value(
        emu: &mut Emulator,
        tokenizer: &mut casl2::Tokenizer<'_>,
    ) -> Result<Value, String> {
        match tokenizer.extended_label()? {
            Some(label) => label.get_address(emu).map(|adr| Value::Int(adr as u16)),
            None => {
                let value = match tokenizer.ignore_case_value() {
                    Some(value) => value,
                    None => return Err("εΌζ°γδΈζ­£γ§γ".to_string()),
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
                        Ok((pos, _)) => Ok(Value::Int(pos as u16)),
                        Err(msg) => Err(msg.to_string()),
                    },
                }
            }
        }
    }

    // γͺγγ©γ«ηζγγγ?γ§ζ³¨ζ
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
                    return Err("εΌζ°γδΈζ­£γ§γ".to_string());
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
                    None => return Err("εΌζ°γδΈζ­£γ§γ".to_string()),
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
                    | casl2::Token::LitStr(..) => Err("γͺγγ©γ«γ―ζε?γ§γγΎγγ".to_string()),
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

pub(super) trait Casl2TokenizerExtension {
    fn extended_label(&mut self) -> Result<Option<ExtendedLabel>, &'static str>;
}
impl<'a> Casl2TokenizerExtension for casl2::Tokenizer<'a> {
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
                        return Err("εΌζ°γδΈζ­£γ§γ");
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
                        return Err("εΌζ°γδΈζ­£γ§γ");
                    }
                } else {
                    return Err("εΌζ°γδΈζ­£γ§γ");
                }
            } else if self.open_bracket() {
                op_stack.push(0);
                continue;
            } else {
                // εεε°ιγ?γΏε₯γ?Tokenγ?γ±γΌγΉγγγδ»₯ε€γ―γ¨γ©γΌ
                return if value_stack.is_empty() && op_stack.is_empty() {
                    Ok(None)
                } else {
                    Err("εΌζ°γδΈζ­£γ§γ")
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
                            return Err("εΌζ°γδΈζ­£γ§γ");
                        }
                        // +
                        let v2 = value_stack.pop().unwrap();
                        let v1 = value_stack.pop().unwrap();
                        value_stack.push(ExtendedLabel::Sum(Box::new(v1), Box::new(v2)));
                    }
                    2 => {
                        if value_stack.len() < 2 {
                            return Err("εΌζ°γδΈζ­£γ§γ");
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
                        return Err("εΌζ°γδΈζ­£γ§γ");
                    }
                } else {
                    return Err("εΌζ°γδΈζ­£γ§γ");
                }
            }
            if self.plus() {
                op_stack.push(1);
            } else if self.minus() {
                op_stack.push(2);
            } else {
                // ExtendedLabelγε?η΅γγ¦γγ°OK(δ»₯ιγ«ε₯TokenγηΆγγͺγ©)γγγδ»₯ε€γ―Err
                return if value_stack.len() == 1 && op_stack.is_empty() {
                    Ok(value_stack.pop())
                } else {
                    Err("εΌζ°γδΈζ­£γ§γ")
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
            Self::Load(adr) => format!("({})", adr).fmt(f),
            Self::Sum(adr1, adr2) => format!("{}+{}", adr1, adr2).fmt(f),
            Self::Diff(adr1, adr2) => format!("{}-{}", adr1, adr2).fmt(f),
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
                            None => Err(format!("γ©γγ«{}γθ¦γ€γγγΎγγ", label)),
                        },
                    },
                }
            }
            Self::LocalLabel(glabel, llabel) => {
                match emu.local_labels.get(&glabel.to_ascii_uppercase()) {
                    Some(label_map) => match label_map.get(&llabel.to_ascii_uppercase()) {
                        Some(adr) => Ok(*adr),
                        None => Err(format!(
                            "γγ­γ°γ©γ {}γ«γ©γγ«{}γθ¦γ€γγγΎγγ",
                            glabel, llabel
                        )),
                    },
                    None => Err(format!("γ©γγ«{}γθ¦γ€γγγΎγγ", glabel)),
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
                    Err(format!("{}γ―γͺγΌγγΌγγ­γΌγγΎγγ", exlabel))
                }
            }
            Self::Sum(ex_adr1, ex_adr2) => {
                let adr1 = ex_adr1.get_address(emu)?;
                let adr2 = ex_adr2.get_address(emu)?;
                if let Some(adr) = adr1.checked_add(adr2) {
                    if adr < emu.mem.len() {
                        Ok(adr)
                    } else {
                        Err(format!("{}γ―γͺγΌγγΌγγ­γΌγγΎγγ", self))
                    }
                } else {
                    Err(format!("{}γ―γͺγΌγγΌγγ­γΌγγΎγγ", self))
                }
            }
            Self::Diff(ex_adr1, ex_adr2) => {
                let adr1 = ex_adr1.get_address(emu)?;
                let adr2 = ex_adr2.get_address(emu)?;
                if let Some(adr) = adr1.checked_sub(adr2) {
                    if adr < emu.mem.len() {
                        Ok(adr)
                    } else {
                        Err(format!("{}γ―γͺγΌγγΌγγ­γΌγγΎγγ", self))
                    }
                } else {
                    Err(format!("{}γ―γͺγΌγγΌγγ­γΌγγΎγγ", self))
                }
            }
        }
    }
}

pub(super) trait Casl2CommandExtension {
    fn op_code(&self) -> u16;
    fn first_word(&self) -> u16;
    fn len(&self) -> usize;
}

impl Casl2CommandExtension for casl2::Command {
    // γγ?γγζΉγ―γ‘γγ£γ¨ε³γγγ»γ»γ»
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

impl From<io::Error> for RuntimeError {
    fn from(error: io::Error) -> Self {
        Self::IoError(error)
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NormalTermination { position } => {
                format!("ζ­£εΈΈη΅δΊ(η΅δΊδ½η½?: #{:04X})", position).fmt(f)
            }
            Self::AbnormalTermination { position, op_code } => {
                format!("η°εΈΈη΅δΊ(η΅δΊδ½η½?: #{:04X} γ³γΌγ: {})", position, op_code).fmt(f)
            }
            Self::ExecutePermissionDenied { position } => format!(
                "θ¨±ε―γγγ¦γͺγι εγ?ε?θ‘γηΊη (ηΊηδ½η½?: #{:04X})",
                position
            )
            .fmt(f),
            Self::MemoryAccessOutOfBounds {
                position,
                op_code,
                address,
            } => format!(
                "γ‘γ’γͺγ?η―ε²ε€γ?γ’γγ¬γΉγΈγ?γ’γ―γ»γΉγηΊη (ηΊηδ½η½?: #{:04X} γ³γΌγ: {} εη§ε: {})",
                position, op_code, address
            )
            .fmt(f),
            Self::InvalidOpCode { position, op_code } => format!(
                "ε½δ»€γ?ε²γε½γ¦γγͺγε€γε½δ»€γ¨γγ¦ε?θ‘γηΊη (ηΊηδ½η½?: #{:04X} γ³γΌγ: {})",
                position, op_code
            )
            .fmt(f),
            Self::StackOverflow {
                position,
                op_code,
                stack_pointer,
            } => format!(
                "γΉγΏγγ―γ?γͺγΌγγΌγγ­γΌγηΊη (ηΊηδ½η½?: #{:04X} γ³γΌγ: {} γΉγΏγγ―δ½η½?: {})",
                position, op_code, stack_pointer
            )
            .fmt(f),
            Self::StackEmpty { position, op_code } => format!(
                "γΉγΏγγ―γη©Ίγ?ηΆζγ§γ?POPγηΊη (ηΊηδ½η½?: #{:04X} γ³γΌγ: {})",
                position, op_code
            )
            .fmt(f),
            Self::IoError(error) => format!("IOγ¨γ©γΌγηΊη ({})", error).fmt(f),
        }
    }
}
