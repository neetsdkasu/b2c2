// crate::casl2::parser

use super::*;
use crate::SyntaxError;

type TokenError = String;

pub fn parse(src: &str) -> Result<Vec<Statement>, SyntaxError> {
    let mut ret = vec![];
    for (i, line) in src.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        let stmt = Statement::parse(line).ok_or_else(|| {
            SyntaxError::new(i + 1, 0, format!("invalid CASL2 statement: {{{}}}", line))
        })?;
        ret.push(stmt);
    }
    Ok(ret)
}

impl Statement {
    fn parse(src: &str) -> Option<Self> {
        let mut tokenizer = Tokenizer::new(src);

        let mut label: Option<Label> = None;
        if tokenizer.space() {
            if let Some(comment) = tokenizer.comment() {
                let indent = tokenizer.space_count;
                return Some(Statement::Comment {
                    indent,
                    text: comment,
                });
            }
        } else if let Some(word) = tokenizer.word() {
            let word = Label::from(word);
            if !word.is_valid() {
                return None;
            }
            if !tokenizer.space() {
                return None;
            }
            label = Some(word);
        } else if let Some(comment) = tokenizer.comment() {
            return Some(Statement::Comment {
                indent: 0,
                text: comment,
            });
        } else {
            return None;
        }

        let (command, comment) = Command::parse(tokenizer)?;

        Some(Statement::Code {
            label,
            command,
            comment,
        })
    }
}

impl Command {
    fn parse(mut tokenizer: Tokenizer) -> Option<(Self, Option<String>)> {
        let cmd_word = tokenizer.word()?;

        if !tokenizer.space() {
            let rest = tokenizer.rest();
            if !rest.is_empty() {
                return None;
            }
        }

        let values = tokenizer.values()?;

        if values.is_empty() {
            let command = match cmd_word.as_str() {
                "START" => Command::Start { entry_point: None },
                "RPUSH" => Command::Rpush,
                "RPOP" => Command::Rpop,
                "RET" => Command::Ret,
                "END" => Command::End,
                "NOP" => Command::Nop,
                _ => return None,
            };
            if !tokenizer.space() {
                if let Some(comment) = tokenizer.comment() {
                    return Some((command, Some(comment)));
                }
            }
            if tokenizer.rest().is_empty() {
                return Some((command, None));
            }
            return None;
        }

        let comment = if tokenizer.space() {
            if let Some(comment) = tokenizer.comment() {
                Some(comment)
            } else {
                let rest = tokenizer.rest();
                if rest.is_empty() {
                    None
                } else {
                    Some(rest)
                }
            }
        } else {
            let rest = tokenizer.rest();
            if rest.is_empty() {
                None
            } else {
                return None;
            }
        };

        let command = match cmd_word.as_str() {
            "LD" => Self::parse_r_or_a(R::Ld, A::Ld, &values)?,
            "ST" => Self::parse_a(A::St, &values)?,
            "LAD" => Self::parse_a(A::Lad, &values)?,
            "ADDA" => Self::parse_r_or_a(R::Adda, A::Adda, &values)?,
            "ADDL" => Self::parse_r_or_a(R::Addl, A::Addl, &values)?,
            "SUBA" => Self::parse_r_or_a(R::Suba, A::Suba, &values)?,
            "SUBL" => Self::parse_r_or_a(R::Subl, A::Subl, &values)?,
            "AND" => Self::parse_r_or_a(R::And, A::And, &values)?,
            "OR" => Self::parse_r_or_a(R::Or, A::Or, &values)?,
            "XOR" => Self::parse_r_or_a(R::Xor, A::Xor, &values)?,
            "CPA" => Self::parse_r_or_a(R::Cpa, A::Cpa, &values)?,
            "CPL" => Self::parse_r_or_a(R::Cpl, A::Cpl, &values)?,
            "SLA" => Self::parse_a(A::Sla, &values)?,
            "SRA" => Self::parse_a(A::Sra, &values)?,
            "SLL" => Self::parse_a(A::Sll, &values)?,
            "SRL" => Self::parse_a(A::Srl, &values)?,
            "JPL" => Self::parse_p(P::Jpl, &values)?,
            "JMI" => Self::parse_p(P::Jmi, &values)?,
            "JNZ" => Self::parse_p(P::Jnz, &values)?,
            "JZE" => Self::parse_p(P::Jze, &values)?,
            "JOV" => Self::parse_p(P::Jov, &values)?,
            "JUMP" => Self::parse_p(P::Jump, &values)?,
            "PUSH" => Self::parse_p(P::Push, &values)?,
            "CALL" => Self::parse_p(P::Call, &values)?,
            "SVC" => Self::parse_p(P::Svc, &values)?,
            "START" => Self::parse_start(&values)?,
            "POP" => Self::parse_pop(&values)?,
            "IN" => Self::parse_in(&values)?,
            "OUT" => Self::parse_out(&values)?,
            "DC" => Self::parse_dc(&values)?,
            "DS" => Self::parse_ds(&values)?,
            _ => return None,
        };

        Some((command, comment))
    }

    fn parse_dc(values: &[Token]) -> Option<Command> {
        let mut constants = vec![];
        for v in values {
            constants.push(Constant::parse(v)?);
        }
        Some(Command::Dc { constants })
    }

    fn parse_ds(values: &[Token]) -> Option<Command> {
        if let [Token::Dec(v)] = values {
            Some(Command::Ds { size: *v as u16 })
        } else {
            None
        }
    }

    fn parse_start(values: &[Token]) -> Option<Command> {
        if let [label] = values {
            let label = Label::parse(label)?;
            Some(Command::Start {
                entry_point: Some(label),
            })
        } else {
            None
        }
    }

    fn parse_a(code: A, values: &[Token]) -> Option<Command> {
        match values {
            [r, adr] => {
                let r = Register::parse(r)?;
                let adr = Adr::parse(adr)?;
                Some(Command::A {
                    code,
                    r,
                    adr,
                    x: None,
                })
            }
            [r, adr, x] => {
                let r = Register::parse(r)?;
                let adr = Adr::parse(adr)?;
                let x = IndexRegister::parse(x)?;
                Some(Command::A {
                    code,
                    r,
                    adr,
                    x: Some(x),
                })
            }
            _ => None,
        }
    }

    fn parse_p(code: P, values: &[Token]) -> Option<Command> {
        match values {
            [adr] => {
                let adr = Adr::parse(adr)?;
                Some(Command::P { code, adr, x: None })
            }
            [adr, x] => {
                let adr = Adr::parse(adr)?;
                let x = IndexRegister::parse(x)?;
                Some(Command::P {
                    code,
                    adr,
                    x: Some(x),
                })
            }
            _ => None,
        }
    }

    fn parse_pop(values: &[Token]) -> Option<Command> {
        if let [r] = values {
            let r = Register::parse(r)?;
            Some(Command::Pop { r })
        } else {
            None
        }
    }

    fn parse_in(values: &[Token]) -> Option<Command> {
        if let [pos, len] = values {
            let pos = Label::parse(pos)?;
            let len = Label::parse(len)?;
            Some(Command::In { pos, len })
        } else {
            None
        }
    }

    fn parse_out(values: &[Token]) -> Option<Command> {
        if let [pos, len] = values {
            let pos = Label::parse(pos)?;
            let len = Label::parse(len)?;
            Some(Command::Out { pos, len })
        } else {
            None
        }
    }

    fn parse_r_or_a(r: R, a: A, values: &[Token]) -> Option<Command> {
        if let Some(command) = Self::parse_a(a, values) {
            return Some(command);
        }
        if let [r1, r2] = values {
            let r1 = Register::parse(r1)?;
            let r2 = Register::parse(r2)?;
            Some(Command::R { code: r, r1, r2 })
        } else {
            None
        }
    }
}

impl Constant {
    fn parse(token: &Token) -> Option<Self> {
        let c = match token {
            word @ Token::Word(_) => Label::parse(word)?.into(),
            Token::Dec(v) => Self::Dec(*v),
            Token::Hex(v) => Self::Hex(*v),
            Token::Str(s) => Self::Str(s.clone()),
            Token::LitDec(_) | Token::LitHex(_) | Token::LitStr(_) => return None,
        };
        Some(c)
    }
}

impl Label {
    fn parse(token: &Token) -> Option<Self> {
        if let Token::Word(w) = token {
            let label = Self::from(w);
            if label.is_valid() {
                return Some(label);
            }
        }
        None
    }
}

impl Adr {
    fn parse(token: &Token) -> Option<Self> {
        let adr = match token {
            word @ Token::Word(_) => Label::parse(word)?.into(),
            Token::Dec(v) => Self::Dec(*v),
            Token::Hex(v) => Self::Hex(*v),
            Token::Str(_) => return None,
            Token::LitDec(v) => Self::LiteralDec(*v),
            Token::LitHex(v) => Self::LiteralHex(*v),
            Token::LitStr(s) => Self::LiteralStr(s.clone()),
        };
        Some(adr)
    }
}

impl Register {
    fn parse(token: &Token) -> Option<Self> {
        let s = if let Token::Word(w) = token {
            w
        } else {
            return None;
        };
        match s.as_str() {
            "GR0" => Some(Self::Gr0),
            "GR1" => Some(Self::Gr1),
            "GR2" => Some(Self::Gr2),
            "GR3" => Some(Self::Gr3),
            "GR4" => Some(Self::Gr4),
            "GR5" => Some(Self::Gr5),
            "GR6" => Some(Self::Gr6),
            "GR7" => Some(Self::Gr7),
            _ => None,
        }
    }
}

impl IndexRegister {
    fn parse(token: &Token) -> Option<Self> {
        let s = if let Token::Word(w) = token {
            w
        } else {
            return None;
        };
        match s.as_str() {
            "GR1" => Some(Self::Gr1),
            "GR2" => Some(Self::Gr2),
            "GR3" => Some(Self::Gr3),
            "GR4" => Some(Self::Gr4),
            "GR5" => Some(Self::Gr5),
            "GR6" => Some(Self::Gr6),
            "GR7" => Some(Self::Gr7),
            _ => None,
        }
    }
}

struct Tokenizer<'a> {
    chars: std::str::Chars<'a>,
    stack: Vec<char>,
    temp: String,
    space_count: usize,
}

enum Token {
    Word(String),
    Dec(i16),
    Hex(u16),
    Str(String),
    LitDec(i16),
    LitHex(u16),
    LitStr(String),
}

impl<'a> Tokenizer<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            chars: s.chars(),
            stack: Vec::new(),
            temp: String::new(),
            space_count: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        if let Some(ch) = self.stack.pop() {
            self.temp.push(ch);
            Some(ch)
        } else if let Some(ch) = self.chars.next() {
            self.temp.push(ch);
            Some(ch)
        } else {
            None
        }
    }

    fn back(&mut self) {
        if let Some(ch) = self.temp.pop() {
            self.stack.push(ch);
        }
    }

    fn recover(&mut self) {
        while let Some(ch) = self.temp.pop() {
            self.stack.push(ch);
        }
    }

    fn take(&mut self) -> String {
        self.temp.drain(..).collect()
    }

    fn clear(&mut self) {
        self.temp.clear();
    }

    fn value(&mut self) -> Option<Token> {
        if let Some(w) = self.word() {
            return Some(Token::Word(w));
        }
        if let Some(i) = self.integer() {
            return Some(Token::Dec(i));
        }
        if let Some(h) = self.hex() {
            return Some(Token::Hex(h));
        }
        if let Some(s) = self.string() {
            return Some(Token::Str(s));
        }
        if let Some(i) = self.lit_integer() {
            return Some(Token::LitDec(i));
        }
        if let Some(h) = self.lit_hex() {
            return Some(Token::LitHex(h));
        }
        if let Some(s) = self.lit_string() {
            return Some(Token::LitStr(s));
        }
        None
    }

    // 破壊的操作、recover不可能
    fn values(&mut self) -> Option<Vec<Token>> {
        let mut ret = vec![];
        if let Some(t) = self.value() {
            ret.push(t);
        } else {
            // オペランドなし (あるいはシンタックスエラー)
            return Some(ret);
        }
        while self.comma() {
            if let Some(t) = self.value() {
                ret.push(t);
            } else {
                // カンマのあとに値がないのはおかしいので
                return None;
            }
        }
        Some(ret)
    }

    fn comment(&mut self) -> Option<String> {
        if !matches!(self.next(), Some(';')) {
            self.recover();
            return None;
        }
        while self.next().is_some() {}
        let comment = if matches!(
            self.temp.chars().nth(1),
            Some(ch) if ch.is_ascii_whitespace()
        ) {
            self.temp.chars().skip(2).collect()
        } else {
            self.temp.chars().skip(1).collect()
        };
        self.clear();
        Some(comment)
    }

    fn rest(&mut self) -> String {
        while self.next().is_some() {}
        self.take()
    }

    fn word(&mut self) -> Option<String> {
        if !matches!(self.next(), Some(ch) if ch.is_ascii_uppercase()) {
            self.recover();
            return None;
        }
        while let Some(ch) = self.next() {
            if !ch.is_ascii_uppercase() && !ch.is_ascii_digit() {
                self.back();
                break;
            }
        }
        Some(self.take())
    }

    fn space(&mut self) -> bool {
        if !matches!(self.next(),Some(ch)if ch.is_ascii_whitespace()) {
            self.recover();
            return false;
        }
        while let Some(ch) = self.next() {
            if !ch.is_ascii_whitespace() {
                self.back();
                break;
            }
        }
        self.space_count = self.temp.chars().count();
        self.clear();
        true
    }

    fn integer(&mut self) -> Option<i16> {
        if !matches!(self.next(),
                Some(ch) if ch == '-' || ch.is_ascii_digit())
        {
            self.recover();
            return None;
        }
        while let Some(ch) = self.next() {
            if !ch.is_ascii_digit() {
                self.back();
                break;
            }
        }
        if let Ok(value) = self.temp.parse::<i64>() {
            self.clear();
            Some(value as i16)
        } else {
            self.recover();
            None
        }
    }

    fn lit_integer(&mut self) -> Option<i16> {
        if !matches!(self.next(), Some('=')) {
            self.recover();
            return None;
        }
        if !matches!(self.next(),
                Some(ch) if ch == '-' || ch.is_ascii_digit())
        {
            self.recover();
            return None;
        }
        while let Some(ch) = self.next() {
            if !ch.is_ascii_digit() {
                self.back();
                break;
            }
        }
        let s: String = self.temp.chars().skip(1).collect();
        if let Ok(value) = s.parse::<i64>() {
            self.clear();
            Some(value as i16)
        } else {
            self.recover();
            None
        }
    }

    fn lit_hex(&mut self) -> Option<u16> {
        if !matches!(self.next(), Some('=')) {
            self.recover();
            return None;
        }
        if !matches!(self.next(), Some('#')) {
            self.recover();
            return None;
        }
        for _ in 0..4 {
            if !matches!(
                self.next(),
                Some(ch) if ch.is_ascii_digit()
                            || (ch.is_ascii_uppercase() && ch.is_ascii_hexdigit())
            ) {
                self.recover();
                return None;
            }
        }
        let h: String = self.temp.chars().skip(2).collect();
        if let Ok(value) = u16::from_str_radix(&h, 16) {
            self.clear();
            Some(value)
        } else {
            self.recover();
            None
        }
    }

    fn hex(&mut self) -> Option<u16> {
        if !matches!(self.next(), Some('#')) {
            self.recover();
            return None;
        }
        for _ in 0..4 {
            if !matches!(
                self.next(),
                Some(ch) if ch.is_ascii_digit()
                            || (ch.is_ascii_uppercase() && ch.is_ascii_hexdigit())
            ) {
                self.recover();
                return None;
            }
        }
        let h: String = self.temp.chars().skip(1).collect();
        if let Ok(value) = u16::from_str_radix(&h, 16) {
            self.clear();
            Some(value)
        } else {
            self.recover();
            None
        }
    }

    fn string(&mut self) -> Option<String> {
        use crate::jis_x_201;
        if !matches!(self.next(), Some('\'')) {
            self.recover();
            return None;
        }
        let mut quote = false;
        let mut text = String::new();
        while let Some(ch) = self.next() {
            if !jis_x_201::contains(ch) {
                self.recover();
                return None;
            }
            if quote {
                if ch == '\'' {
                    quote = false;
                    text.push(ch);
                } else {
                    self.back();
                    break;
                }
            } else if ch == '\'' {
                quote = true;
            } else {
                text.push(ch);
            }
        }
        if quote {
            self.clear();
            Some(text)
        } else {
            self.recover();
            None
        }
    }

    fn lit_string(&mut self) -> Option<String> {
        if !matches!(self.next(), Some('=')) {
            self.recover();
            return None;
        }
        self.string()
    }

    fn comma(&mut self) -> bool {
        if matches!(self.next(), Some(',')) {
            self.clear();
            true
        } else {
            self.recover();
            false
        }
    }
}
