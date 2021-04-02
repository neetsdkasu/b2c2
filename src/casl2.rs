use std::fmt;

#[cfg(test)]
mod test;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement {
    Comment {
        indent: usize,
        text: String,
    },
    Code {
        label: Option<Label>,
        command: Command,
        comment: Option<String>,
    },
}

impl Statement {
    pub fn labeled(label: &str, command: Command) -> Self {
        Self::Code {
            label: Some(label.into()),
            command,
            comment: None,
        }
    }

    pub fn code(command: Command) -> Self {
        Self::Code {
            label: None,
            command,
            comment: None,
        }
    }

    pub fn labeled_with_comment(label: &str, command: Command, comment: &str) -> Self {
        Self::Code {
            label: Some(label.into()),
            command,
            comment: Some(comment.into()),
        }
    }

    pub fn code_with_comment(command: Command, comment: &str) -> Self {
        Self::Code {
            label: None,
            command,
            comment: Some(comment.into()),
        }
    }

    pub fn comment(comment: &str) -> Self {
        Self::Comment {
            indent: 0,
            text: comment.into(),
        }
    }

    pub fn comment_with_indent(indent: usize, comment: &str) -> Self {
        Self::Comment {
            indent,
            text: comment.into(),
        }
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, Self::Comment { .. })
    }

    pub fn is_code(&self) -> bool {
        !self.is_comment()
    }

    pub fn remove_comment(&self) -> Option<Self> {
        if let Self::Code {
            label,
            command,
            comment: _,
        } = self
        {
            Some(Self::Code {
                label: label.clone(),
                command: command.clone(),
                comment: None,
            })
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Label(String);

impl From<String> for Label {
    fn from(s: String) -> Self {
        Label(s)
    }
}

impl From<&str> for Label {
    fn from(s: &str) -> Self {
        Label(s.into())
    }
}

impl From<&String> for Label {
    fn from(s: &String) -> Self {
        Label(s.clone())
    }
}

impl Label {
    pub fn as_str(&self) -> &str {
        let Label(label) = self;
        label
    }

    pub fn is_valid(&self) -> bool {
        let Label(label) = self;
        let mut chars = label.chars();
        if chars.next().filter(|ch| ch.is_ascii_uppercase()).is_none() {
            return false;
        }
        if !chars.all(|ch| ch.is_ascii_uppercase() || ch.is_ascii_digit()) {
            return false;
        }
        if label.chars().count() > 8 {
            return false;
        }
        !matches!(
            label.as_str(),
            "GR0" | "GR1" | "GR2" | "GR3" | "GR4" | "GR5" | "GR6" | "GR7"
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Adr {
    Dec(i16),           // -32678 ~ 32677
    Hex(u16),           // #0000 ~ #FFFF
    Label(Label),       // 'text'
    LiteralDec(i16),    // =1234
    LiteralHex(u16),    // =#12AB
    LiteralStr(String), // ='text'
}

impl From<Label> for Adr {
    fn from(label: Label) -> Self {
        Self::Label(label)
    }
}

impl From<&Label> for Adr {
    fn from(label: &Label) -> Self {
        Self::Label(label.clone())
    }
}

impl Adr {
    pub fn label(label: &str) -> Self {
        Self::Label(label.into())
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Constant {
    Dec(i16),     // =1234
    Hex(u16),     // =#12AB
    Str(String),  // ='text'
    Label(Label), // 'text'
}

impl From<i16> for Constant {
    fn from(v: i16) -> Self {
        Self::Dec(v)
    }
}

impl From<u16> for Constant {
    fn from(v: u16) -> Self {
        Self::Hex(v)
    }
}

impl From<String> for Constant {
    fn from(v: String) -> Self {
        Self::Str(v)
    }
}

impl From<&str> for Constant {
    fn from(v: &str) -> Self {
        Self::Str(v.into())
    }
}

impl From<Label> for Constant {
    fn from(v: Label) -> Self {
        Self::Label(v)
    }
}

impl From<&Label> for Constant {
    fn from(v: &Label) -> Self {
        Self::Label(v.clone())
    }
}

impl Constant {
    pub fn label(label: &str) -> Self {
        Self::Label(label.into())
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Register {
    Gr0 = 0,
    Gr1 = 1,
    Gr2 = 2,
    Gr3 = 3,
    Gr4 = 4,
    Gr5 = 5,
    Gr6 = 6,
    Gr7 = 7,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IndexRegister {
    Gr1 = 1,
    Gr2 = 2,
    Gr3 = 3,
    Gr4 = 4,
    Gr5 = 5,
    Gr6 = 6,
    Gr7 = 7,
}

impl From<IndexRegister> for Register {
    fn from(r: IndexRegister) -> Self {
        use IndexRegister::*;
        match r {
            Gr1 => Self::Gr1,
            Gr2 => Self::Gr2,
            Gr3 => Self::Gr3,
            Gr4 => Self::Gr4,
            Gr5 => Self::Gr5,
            Gr6 => Self::Gr6,
            Gr7 => Self::Gr7,
        }
    }
}

impl std::convert::TryFrom<Register> for IndexRegister {
    type Error = i32;
    fn try_from(r: Register) -> Result<Self, Self::Error> {
        use Register::*;
        let ir = match r {
            Gr0 => return Err(0),
            Gr1 => Self::Gr1,
            Gr2 => Self::Gr2,
            Gr3 => Self::Gr3,
            Gr4 => Self::Gr4,
            Gr5 => Self::Gr5,
            Gr6 => Self::Gr6,
            Gr7 => Self::Gr7,
        };
        Ok(ir)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum R {
    Ld,
    Adda,
    Addl,
    Suba,
    Subl,
    And,
    Or,
    Xor,
    Cpa,
    Cpl,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum A {
    Ld,
    St,
    Lad,
    Adda,
    Addl,
    Suba,
    Subl,
    And,
    Or,
    Xor,
    Cpa,
    Cpl,
    Sla,
    Sra,
    Sll,
    Srl,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum P {
    Jpl,
    Jmi,
    Jnz,
    Jze,
    Jov,
    Jump,
    Push,
    Call,
    Svc,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Command {
    Start {
        entry_point: Option<Label>,
    },
    End,
    Ds {
        size: u16,
    },
    Dc {
        constants: Vec<Constant>,
    },
    In {
        pos: Label,
        len: Label,
    },
    Out {
        pos: Label,
        len: Label,
    },
    Rpush,
    Rpop,

    R {
        code: R,
        r1: Register,
        r2: Register,
    },
    A {
        code: A,
        r: Register,
        adr: Adr,
        x: Option<IndexRegister>,
    },
    P {
        code: P,
        adr: Adr,
        x: Option<IndexRegister>,
    },
    Pop {
        r: Register,
    },
    Ret,
    Nop,
}

#[derive(Clone, Debug)]
pub struct Program {
    name: String,
    statements: Vec<Statement>,
}

impl Program {
    pub fn remove_comments(&self) -> Self {
        Self {
            name: self.name.clone(),
            statements: self
                .statements
                .iter()
                .filter_map(Statement::remove_comment)
                .collect(),
        }
    }

    pub fn take_code(self) -> Vec<Statement> {
        self.statements
    }
}

#[derive(Clone, Debug)]
pub struct Builder {
    label: Option<Label>,
    program: Program,
}

impl Builder {
    pub fn new(name: &str) -> Self {
        let statements = vec![Statement::Code {
            label: Some(Label(name.into())),
            command: Command::Start { entry_point: None },
            comment: None,
        }];
        Self {
            label: None,
            program: Program {
                name: name.into(),
                statements,
            },
        }
    }

    pub fn new_with_comment(name: &str, comment: &str) -> Self {
        let statements = vec![Statement::Code {
            label: Some(Label(name.into())),
            command: Command::Start { entry_point: None },
            comment: Some(comment.into()),
        }];
        Self {
            label: None,
            program: Program {
                name: name.into(),
                statements,
            },
        }
    }

    pub fn start_at(name: &str, entry_point: &str) -> Self {
        let statements = vec![Statement::Code {
            label: Some(Label(name.into())),
            command: Command::Start {
                entry_point: Some(Label(entry_point.into())),
            },
            comment: None,
        }];
        Self {
            label: None,
            program: Program {
                name: name.into(),
                statements,
            },
        }
    }

    pub fn start_at_with_comment(name: &str, entry_point: &str, comment: &str) -> Self {
        let statements = vec![Statement::Code {
            label: Some(Label(name.into())),
            command: Command::Start {
                entry_point: Some(Label(entry_point.into())),
            },
            comment: Some(comment.into()),
        }];
        Self {
            label: None,
            program: Program {
                name: name.into(),
                statements,
            },
        }
    }

    pub fn label(mut self, label: &str) -> Self {
        self.label = Some(Label(label.into()));
        self
    }

    pub fn code(mut self, command: Command) -> Self {
        let label = self.label.take();
        self.program.statements.push(Statement::Code {
            label,
            command,
            comment: None,
        });
        self
    }

    pub fn code_with_comment(mut self, command: Command, comment: &str) -> Self {
        let label = self.label.take();
        self.program.statements.push(Statement::Code {
            label,
            command,
            comment: Some(comment.into()),
        });
        self
    }

    fn comment(mut self, comment: &str) -> Self {
        self.program.statements.push(Statement::Comment {
            indent: 0,
            text: comment.into(),
        });
        self
    }

    fn comment_with_indent(mut self, indent: usize, comment: &str) -> Self {
        self.program.statements.push(Statement::Comment {
            indent,
            text: comment.into(),
        });
        self
    }

    fn end(mut self) -> Program {
        let label = self.label.take();
        self.program.statements.push(Statement::Code {
            label,
            command: Command::End,
            comment: None,
        });
        self.program
    }

    fn end_with_comment(mut self, comment: &str) -> Program {
        let label = self.label.take();
        self.program.statements.push(Statement::Code {
            label,
            command: Command::End,
            comment: Some(comment.into()),
        });
        self.program
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in self.statements.iter() {
            writeln!(f, "{}", s.to_string())?;
        }
        Ok(())
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Label(label) = self;
        label.fmt(f)
    }
}

impl fmt::Display for Adr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Adr::Dec(v) => format!("{}", *v).fmt(f),
            Adr::Hex(v) => format!("#{:04X}", *v).fmt(f),
            Adr::Label(v) => format!("{}", v).fmt(f),
            Adr::LiteralDec(v) => format!("={}", *v).fmt(f),
            Adr::LiteralHex(v) => format!("=#{:04X}", *v).fmt(f),
            Adr::LiteralStr(v) => format!("='{}'", v.replace("'", "''")).fmt(f),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Dec(v) => format!("{}", *v).fmt(f),
            Constant::Hex(v) => format!("#{:04X}", *v).fmt(f),
            Constant::Str(v) => format!("'{}'", v.replace("'", "''")).fmt(f),
            Constant::Label(v) => format!("{}", v).fmt(f),
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format!("GR{}", *self as isize).fmt(f)
    }
}

impl fmt::Display for IndexRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format!("GR{}", *self as isize).fmt(f)
    }
}

impl fmt::Display for R {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use R::*;
        let s = match self {
            Ld => "LD",
            Adda => "ADDA",
            Addl => "ADDL",
            Suba => "SUBA",
            Subl => "SUBL",
            And => "AND",
            Or => "OR",
            Xor => "XOR",
            Cpa => "CPA",
            Cpl => "CPL",
        };
        s.fmt(f)
    }
}

impl fmt::Display for A {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use A::*;
        let s = match self {
            Ld => "LD",
            St => "ST",
            Lad => "LAD",
            Adda => "ADDA",
            Addl => "ADDL",
            Suba => "SUBA",
            Subl => "SUBL",
            And => "AND",
            Or => "OR",
            Xor => "XOR",
            Cpa => "CPA",
            Cpl => "CPL",
            Sla => "SLA",
            Sra => "SRA",
            Sll => "SLL",
            Srl => "SRL",
        };
        s.fmt(f)
    }
}

impl fmt::Display for P {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use P::*;
        let s = match self {
            Jpl => "JPL",
            Jmi => "JMI",
            Jnz => "JNZ",
            Jze => "JZE",
            Jov => "JOV",
            Jump => "JUMP",
            Push => "PUSH",
            Call => "CALL",
            Svc => "SVC",
        };
        s.fmt(f)
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Command::*;
        let s = match self {
            Start {
                entry_point: Some(entry),
            } => format!("{:<9} {}", "START", entry),
            Start { entry_point: None } => return "START".fmt(f),
            End => return "END".fmt(f),
            Ds { size } => format!("{:<9} {}", "DS", *size),
            Dc { constants } => {
                let mut s = format!("{:<9} ", "DC");
                for (i, c) in constants.iter().enumerate() {
                    if i > 0 {
                        s.push(',');
                    }
                    s.push_str(&c.to_string());
                }
                s
            }
            In { pos, len } => format!("{:<9} {},{}", "IN", pos, len),
            Out { pos, len } => format!("{:<9} {},{}", "OUT", pos, len),
            Rpush => return "RPUSH".fmt(f),
            Rpop => return "RPOP".fmt(f),

            R { code, r1, r2 } => format!("{:<9} {},{}", code.to_string(), r1, r2),
            A {
                code,
                r,
                adr,
                x: Some(x),
            } => format!("{:<9} {},{},{}", code.to_string(), r, adr, x),
            A {
                code,
                r,
                adr,
                x: None,
            } => format!("{:<9} {},{}", code.to_string(), r, adr),
            P {
                code,
                adr,
                x: Some(x),
            } => format!("{:<9} {},{}", code.to_string(), adr, x),
            P { code, adr, x: None } => format!("{:<9} {}", code.to_string(), adr),
            Pop { r } => format!("{:<9} {}", "POP", r),
            Ret => return "RET".fmt(f),
            Nop => return "NOP".fmt(f),
        };
        s.fmt(f)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Comment { indent, text } => format!("{0:1$}; {2}", "", indent, text),
            Self::Code {
                label,
                command,
                comment: Some(text),
            } => {
                let command = command.to_string();
                let count = command.chars().count();
                let width = (count + 5 - count % 5).max(25);
                if let Some(label) = label {
                    format!(
                        "{0:<9} {1:<2$}; {3}",
                        label.to_string(),
                        command,
                        width,
                        text
                    )
                } else {
                    format!("{0:<9} {1:<2$}; {3}", "", command, width, text)
                }
            }
            Self::Code {
                label,
                command,
                comment: None,
            } => {
                if let Some(label) = label {
                    format!("{:<9} {}", label.to_string(), command.to_string())
                } else {
                    format!("{:<9} {}", "", command.to_string())
                }
            }
        };
        s.fmt(f)
    }
}

pub use self::parser::parse;

mod parser {
    use super::*;
    use crate::SyntaxError;

    type TokenError = String;

    pub fn parse(src: &str) -> Result<Vec<Statement>, SyntaxError> {
        let mut ret = vec![];
        for (i, line) in src.lines().enumerate() {
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
}
