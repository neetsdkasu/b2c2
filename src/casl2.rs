use std::fmt;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement {
    Comment(String),
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
        Self::Comment(comment.into())
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, Self::Comment(_))
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
    GR0 = 0,
    GR1 = 1,
    GR2 = 2,
    GR3 = 3,
    GR4 = 4,
    GR5 = 5,
    GR6 = 6,
    GR7 = 7,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IndexRegister {
    GR1 = 1,
    GR2 = 2,
    GR3 = 3,
    GR4 = 4,
    GR5 = 5,
    GR6 = 6,
    GR7 = 7,
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
        let mut statements = vec![];
        statements.push(Statement::Code {
            label: Some(Label(name.into())),
            command: Command::Start { entry_point: None },
            comment: None,
        });
        Self {
            label: None,
            program: Program {
                name: name.into(),
                statements,
            },
        }
    }

    pub fn new_with_comment(name: &str, comment: &str) -> Self {
        let mut statements = vec![];
        statements.push(Statement::Code {
            label: Some(Label(name.into())),
            command: Command::Start { entry_point: None },
            comment: Some(comment.into()),
        });
        Self {
            label: None,
            program: Program {
                name: name.into(),
                statements,
            },
        }
    }

    pub fn start_at(name: &str, entry_point: &str) -> Self {
        let mut statements = vec![];
        statements.push(Statement::Code {
            label: Some(Label(name.into())),
            command: Command::Start {
                entry_point: Some(Label(entry_point.into())),
            },
            comment: None,
        });
        Self {
            label: None,
            program: Program {
                name: name.into(),
                statements,
            },
        }
    }

    pub fn start_at_with_comment(name: &str, entry_point: &str, comment: &str) -> Self {
        let mut statements = vec![];
        statements.push(Statement::Code {
            label: Some(Label(name.into())),
            command: Command::Start {
                entry_point: Some(Label(entry_point.into())),
            },
            comment: Some(comment.into()),
        });
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
        self.program
            .statements
            .push(Statement::Comment(comment.into()));
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
        write!(f, "{}", label)
    }
}

impl fmt::Display for Adr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Adr::Dec(v) => write!(f, "{}", *v),
            Adr::Hex(v) => write!(f, "#{:04X}", *v),
            Adr::Label(v) => write!(f, "{}", v),
            Adr::LiteralDec(v) => write!(f, "={}", *v),
            Adr::LiteralHex(v) => write!(f, "={:04X}", *v),
            Adr::LiteralStr(v) => write!(f, "='{}'", v.replace("'", "''")),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Dec(v) => write!(f, "{}", *v),
            Constant::Hex(v) => write!(f, "#{:04X}", *v),
            Constant::Str(v) => write!(f, "'{}'", v.replace("'", "''")),
            Constant::Label(v) => write!(f, "{}", v),
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GR{}", *self as isize)
    }
}

impl fmt::Display for IndexRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GR{}", *self as isize)
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
        write!(f, "{}", s)
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
        write!(f, "{}", s)
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
        write!(f, "{}", s)
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Command::*;
        match self {
            Start {
                entry_point: Some(entry),
            } => write!(f, "{:<9} {}", "START", entry),
            Start { entry_point: None } => write!(f, "START"),
            End => write!(f, "END"),
            Ds { size } => write!(f, "{:<9} {}", "DS", *size),
            Dc { constants } => {
                write!(f, "{:<9} ", "DC")?;
                for (i, c) in constants.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", c)?;
                }
                Ok(())
            }
            In { pos, len } => write!(f, "{:<9} {},{}", "IN", pos, len),
            Out { pos, len } => write!(f, "{:<9} {},{}", "OUT", pos, len),
            Rpush => write!(f, "RPUSH"),
            Rpop => write!(f, "RPOP"),

            R { code, r1, r2 } => write!(f, "{:<9} {},{}", code.to_string(), r1, r2),
            A {
                code,
                r,
                adr,
                x: Some(x),
            } => write!(f, "{:<9} {},{},{}", code.to_string(), r, adr, x),
            A {
                code,
                r,
                adr,
                x: None,
            } => write!(f, "{:<9} {},{}", code.to_string(), r, adr),
            P {
                code,
                adr,
                x: Some(x),
            } => write!(f, "{:<9} {},{}", code.to_string(), adr, x),
            P { code, adr, x: None } => write!(f, "{:<9} {}", code.to_string(), adr),
            Pop { r } => write!(f, "{:<9} {}", "POP", r),
            Ret => write!(f, "RET"),
            Nop => write!(f, "NOP"),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Comment(text) => write!(f, "; {}", text),
            Self::Code {
                label,
                command,
                comment: Some(text),
            } => {
                let command = command.to_string();
                let count = command.chars().count();
                let width = (count + 5 - count % 5).max(25);
                if let Some(label) = label {
                    write!(
                        f,
                        "{0:<9} {1:<2$}; {3}",
                        label.to_string(),
                        command,
                        width,
                        text
                    )
                } else {
                    write!(f, "{0:<9} {1:<2$}; {3}", "", command, width, text)
                }
            }
            Self::Code {
                label,
                command,
                comment: None,
            } => {
                if let Some(label) = label {
                    write!(f, "{:<9} {}", label.to_string(), command.to_string())
                } else {
                    write!(f, "{:<9} {}", "", command.to_string())
                }
            }
        }
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
            let stmt = Statement::parse(line).map_err(|s| SyntaxError::new(i + 1, 0, s))?;
            ret.push(stmt);
        }
        Ok(ret)
    }

    fn take_comment(src: &str) -> Option<String> {
        take_comment_with(src).or_else(|| {
            let s = src.trim_start();
            if s.is_empty() {
                None
            } else {
                Some(s.into())
            }
        })
    }

    fn take_comment_with(src: &str) -> Option<String> {
        let s = src.trim_start();
        if let Some(';') = s.chars().next() {
            if s.chars()
                .nth(1)
                .filter(|ch| ch.is_ascii_whitespace())
                .is_some()
            {
                Some(s.chars().skip(2).collect())
            } else {
                Some(s.chars().skip(1).collect())
            }
        } else {
            None
        }
    }

    impl Statement {
        fn parse(src: &str) -> Result<Self, TokenError> {
            if let Some(comment) = take_comment_with(src.trim_start()) {
                return Ok(Statement::Comment(comment));
            }
            let (label, rest) = if let Ok((label, rest)) = Label::take(src) {
                (Some(label), rest.trim_start().to_string())
            } else {
                if src
                    .chars()
                    .next()
                    .filter(|ch| ch.is_ascii_whitespace())
                    .is_none()
                {
                    return Err("invalid statement start".into());
                }
                (None, src.trim_start().to_string())
            };
            let (command, comment) = Command::take(&rest)?;
            Ok(Statement::Code {
                label,
                command,
                comment,
            })
        }
    }

    impl Label {
        fn take(src: &str) -> Result<(Self, String), TokenError> {
            let word: Vec<_> = src
                .chars()
                .take_while(|ch| !ch.is_ascii_whitespace() && *ch != ',')
                .collect();
            let label = Label(word.iter().collect());
            if label.is_valid() {
                let rest = src.chars().skip(word.len()).collect();
                Ok((label, rest))
            } else {
                Err("invalid Label".into())
            }
        }
    }

    enum Args {
        InOut(Label, Label),
        R(Register, Register),
        A(Register, Adr, Option<IndexRegister>),
        P(Adr, Option<IndexRegister>),
        Pop(Register),
        Constants(Vec<Constant>),
    }

    impl Args {
        fn take_label_and(
            label: Label,
            rest: String,
        ) -> Result<(Option<Self>, Option<String>), TokenError> {
            let mut chars = rest.chars();
            if chars.next().filter(|ch| *ch == ',').is_some() {
                let rest = chars.collect::<String>();
                if let Ok((s_len, rest)) = Label::take(&rest) {
                    Ok((Some(Self::InOut(label, s_len)), take_comment(&rest)))
                } else {
                    IndexRegister::take(&rest)
                        .map(|(idx, rest)| {
                            (
                                Some(Self::P(Adr::Label(label), Some(idx))),
                                take_comment(&rest),
                            )
                        })
                        .ok_or_else(|| "invalid Args".into())
                }
            } else {
                Ok((Some(Self::P(Adr::Label(label), None)), take_comment(&rest)))
            }
        }

        fn take_reg_and(
            r1: Register,
            rest: String,
        ) -> Result<(Option<Self>, Option<String>), TokenError> {
            let mut chars = rest.chars();
            if chars.next().filter(|ch| *ch == ',').is_some() {
                let rest = chars.collect::<String>();
                if let Some((r2, rest)) = Register::take(&rest) {
                    return Ok((Some(Self::R(r1, r2)), take_comment(&rest)));
                }
                let (adr, rest) = Adr::take(&rest).ok_or_else(|| "invalid Args".to_string())?;
                let mut chars = rest.chars();
                if chars.next().filter(|ch| *ch == ',').is_some() {
                    let rest = chars.collect::<String>();
                    IndexRegister::take(&rest)
                        .map(|(idx, rest)| (Some(Self::A(r1, adr, Some(idx))), take_comment(&rest)))
                        .ok_or_else(|| "invalid Args".into())
                } else {
                    Ok((Some(Self::A(r1, adr, None)), take_comment(&rest)))
                }
            } else {
                Ok((Some(Self::Pop(r1)), take_comment(&rest)))
            }
        }

        fn take(src: &str) -> Result<(Option<Self>, Option<String>), TokenError> {
            if let Ok((label, rest)) = Label::take(src) {
                return Self::take_label_and(label, rest);
            }
            if let Some((r1, rest)) = Register::take(src) {
                return Self::take_reg_and(r1, rest);
            }
            todo!()
        }

        fn r_or_a(self, r: R, a: A) -> Result<Command, TokenError> {
            match self {
                Self::R(r1, r2) => Ok(Command::R { code: r, r1, r2 }),
                Self::A(r, adr, x) => Ok(Command::A { code: a, r, adr, x }),
                _ => Err("invalid command".into()),
            }
        }

        fn a(self, a: A) -> Result<Command, TokenError> {
            if let Self::A(r, adr, x) = self {
                Ok(Command::A { code: a, r, adr, x })
            } else {
                Err("invalid command".into())
            }
        }

        fn p(self, p: P) -> Result<Command, TokenError> {
            if let Self::P(adr, x) = self {
                Ok(Command::P { code: p, adr, x })
            } else {
                Err("invalid command".into())
            }
        }

        fn pop(self) -> Result<Command, TokenError> {
            if let Self::Pop(r) = self {
                Ok(Command::Pop { r })
            } else {
                Err("invalid command".into())
            }
        }

        fn inout(self, inout: InOut) -> Result<Command, TokenError> {
            if let Self::InOut(pos, len) = self {
                if matches!(inout, InOut::In) {
                    Ok(Command::In { pos, len })
                } else {
                    Ok(Command::Out { pos, len })
                }
            } else {
                Err("invalid command".into())
            }
        }
    }

    enum InOut {
        In,
        Out,
    }

    impl Register {
        fn take(src: &str) -> Option<(Self, String)> {
            use Register::*;
            let word: Vec<_> = src
                .chars()
                .take_while(|ch| !ch.is_ascii_whitespace())
                .collect();
            match word.as_slice() {
                ['G', 'R', ch] if ch.is_ascii_digit() => {
                    let n = ch.to_digit(8)?;
                    let rest = src.chars().skip(word.len()).collect();
                    let gr = [GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7];
                    Some((gr[n as usize], rest))
                }
                _ => None,
            }
        }
    }

    impl IndexRegister {
        fn take(src: &str) -> Option<(IndexRegister, String)> {
            use IndexRegister::*;
            let (reg, rest) = Register::take(src)?;
            match reg as usize {
                i @ 1..=7 => {
                    let gr = [GR1, GR2, GR3, GR4, GR5, GR6, GR7];
                    Some((gr[i - 1], rest))
                }
                _ => None,
            }
        }
    }

    impl Adr {
        fn take(src: &str) -> Option<(Self, String)> {
            todo!()
        }
    }

    impl Command {
        fn take(src: &str) -> Result<(Self, Option<String>), TokenError> {
            let cmd_word: String = src
                .chars()
                .take_while(|ch| !ch.is_ascii_whitespace())
                .collect();
            let rest: String = src
                .chars()
                .skip(cmd_word.len())
                .skip_while(|ch| ch.is_ascii_whitespace())
                .collect();
            let (args, comment) = Args::take(&rest)?;
            let args = if let Some(args) = args {
                args
            } else {
                let command = match cmd_word.as_str() {
                    "START" => Command::Start { entry_point: None },
                    "RPUSH" => Command::Rpush,
                    "RPOP" => Command::Rpop,
                    "RET" => Command::Ret,
                    "END" => Command::End,
                    "NOP" => Command::Nop,
                    _ => return Err("invalid command".into()),
                };
                return Ok((command, comment));
            };
            let command = match cmd_word.as_str() {
                "LD" => args.r_or_a(R::Ld, A::Ld)?,
                "ST" => args.a(A::St)?,
                "LAD" => args.a(A::Lad)?,
                "ADDA" => args.r_or_a(R::Adda, A::Adda)?,
                "ADDL" => args.r_or_a(R::Addl, A::Addl)?,
                "SUBA" => args.r_or_a(R::Suba, A::Suba)?,
                "SUBL" => args.r_or_a(R::Subl, A::Subl)?,
                "AND" => args.r_or_a(R::And, A::And)?,
                "OR" => args.r_or_a(R::Or, A::Or)?,
                "XOR" => args.r_or_a(R::Xor, A::Xor)?,
                "CPA" => args.r_or_a(R::Cpa, A::Cpa)?,
                "CPL" => args.r_or_a(R::Cpl, A::Cpl)?,
                "SLA" => args.a(A::Sla)?,
                "SRA" => args.a(A::Sra)?,
                "SLL" => args.a(A::Sll)?,
                "SRL" => args.a(A::Srl)?,
                "JPL" => args.p(P::Jpl)?,
                "JMI" => args.p(P::Jmi)?,
                "JNZ" => args.p(P::Jnz)?,
                "JZE" => args.p(P::Jze)?,
                "JOV" => args.p(P::Jov)?,
                "JUMP" => args.p(P::Jump)?,
                "PUSH" => args.p(P::Push)?,
                "CALL" => args.p(P::Call)?,
                "SVC" => args.p(P::Svc)?,
                // "START"
                "POP" => args.pop()?,
                "IN" => args.inout(InOut::In)?,
                "OUT" => args.inout(InOut::Out)?,
                // "DC"
                // "DS"
                _ => todo!(),
            };
            Ok((command, comment))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const SRC: &str = "\
MUL       START
; MULTIPLY
; GR0 = GR1 * GR2
; SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)
          PUSH      0,GR1
          PUSH      0,GR2
          XOR       GR0,GR0        ; GR0 = 0
LOOP      SRL       GR1,1          ; GR1 >>= 1
          JOV       ADD
          JZE       NEXT
          POP       GR2
          POP       GR1
          RET
ADD       ADDL      GR0,GR2        ; GR0 += GR2
NEXT      SLL       GR2,1          ; GR2 <<= 1
          JUMP      LOOP
          END
";

    #[test]
    fn it_works() {
        let statements = get_statements();
        let program = get_program();

        assert_eq!(statements, program.statements);

        assert_eq!(SRC, &program.to_string());
    }

    fn get_statements() -> Vec<Statement> {
        use super::Command as Cmd;
        use super::IndexRegister as Idx;
        use super::Register as Reg;
        use super::Statement as Stmt;

        vec![
            // MUL       START
            Stmt::labeled("MUL", Cmd::Start { entry_point: None }),
            // ; MULTIPLY
            Stmt::comment("MULTIPLY"),
            // ; GR0 = GR1 * GR2
            Stmt::comment("GR0 = GR1 * GR2"),
            // ; SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)
            Stmt::comment("SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)"),
            // PUSH      0,GR1
            Stmt::code(Cmd::P {
                code: P::Push,
                adr: Adr::Dec(0),
                x: Some(Idx::GR1),
            }),
            // PUSH      0,GR2
            Stmt::code(Cmd::P {
                code: P::Push,
                adr: Adr::Dec(0),
                x: Some(Idx::GR2),
            }),
            // XOR       GR0,GR0        ; GR0 = 0
            Stmt::code_with_comment(
                Cmd::R {
                    code: R::Xor,
                    r1: Reg::GR0,
                    r2: Reg::GR0,
                },
                "GR0 = 0",
            ),
            // LOOP      SRL       GR1,1          ; GR1 >>= 1
            Stmt::labeled_with_comment(
                "LOOP",
                Cmd::A {
                    code: A::Srl,
                    r: Reg::GR1,
                    adr: Adr::Dec(1),
                    x: None,
                },
                "GR1 >>= 1",
            ),
            // JOV       ADD
            Stmt::code(Cmd::P {
                code: P::Jov,
                adr: Adr::label("ADD"),
                x: None,
            }),
            // JZE       NEXT
            Stmt::code(Cmd::P {
                code: P::Jze,
                adr: Adr::label("NEXT"),
                x: None,
            }),
            // POP       GR2
            Stmt::code(Cmd::Pop { r: Reg::GR2 }),
            // POP       GR1
            Stmt::code(Cmd::Pop { r: Reg::GR1 }),
            // RET
            Stmt::code(Cmd::Ret),
            // ADD       ADDL      GR0,GR2        ; GR0 += GR2
            Stmt::labeled_with_comment(
                "ADD",
                Cmd::R {
                    code: R::Addl,
                    r1: Reg::GR0,
                    r2: Reg::GR2,
                },
                "GR0 += GR2",
            ),
            // NEXT      SLL       GR2,1          ; GR2 <<= 1
            Stmt::labeled_with_comment(
                "NEXT",
                Cmd::A {
                    code: A::Sll,
                    r: Reg::GR2,
                    adr: Adr::Dec(1),
                    x: None,
                },
                "GR2 <<= 1",
            ),
            // JUMP      LOOP
            Stmt::code(Cmd::P {
                code: P::Jump,
                adr: Adr::label("LOOP"),
                x: None,
            }),
            // END
            Stmt::code(Cmd::End),
        ]
    }

    fn get_program() -> Program {
        use super::Command as Cmd;
        use super::IndexRegister as Idx;
        use super::Register as Reg;

        // MUL    START
        Builder::new("MUL")
            // ; MULTIPLY
            .comment("MULTIPLY")
            // ; GR0 = GR1 * GR2
            .comment("GR0 = GR1 * GR2")
            // ; SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)
            .comment("SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)")
            // PUSH 0,GR1
            .code(Cmd::P {
                code: P::Push,
                adr: Adr::Dec(0),
                x: Some(Idx::GR1),
            })
            // PUSH 0,GR2
            .code(Cmd::P {
                code: P::Push,
                adr: Adr::Dec(0),
                x: Some(Idx::GR2),
            })
            // XOR  GR0,GR0    ; GR0 = 0
            .code_with_comment(
                Cmd::R {
                    code: R::Xor,
                    r1: Reg::GR0,
                    r2: Reg::GR0,
                },
                "GR0 = 0",
            )
            // LOOP      SRL       GR1,1          ; GR1 >>= 1
            .label("LOOP")
            .code_with_comment(
                Cmd::A {
                    code: A::Srl,
                    r: Reg::GR1,
                    adr: Adr::Dec(1),
                    x: None,
                },
                "GR1 >>= 1",
            )
            // JOV       ADD
            .code(Cmd::P {
                code: P::Jov,
                adr: Adr::label("ADD"),
                x: None,
            })
            // JZE       NEXT
            .code(Cmd::P {
                code: P::Jze,
                adr: Adr::label("NEXT"),
                x: None,
            })
            // POP       GR2
            .code(Cmd::Pop { r: Reg::GR2 })
            // POP       GR1
            .code(Cmd::Pop { r: Reg::GR1 })
            // RET
            .code(Cmd::Ret)
            // ADD       ADDL      GR0,GR2        ; GR0 += GR2
            .label("ADD")
            .code_with_comment(
                Cmd::R {
                    code: R::Addl,
                    r1: Reg::GR0,
                    r2: Reg::GR2,
                },
                "GR0 += GR2",
            )
            // NEXT      SLL       GR2,1          ; GR2 <<= 1
            .label("NEXT")
            .code_with_comment(
                Cmd::A {
                    code: A::Sll,
                    r: Reg::GR2,
                    adr: Adr::Dec(1),
                    x: None,
                },
                "GR2 <<= 1",
            )
            // JUMP      LOOP
            .code(Cmd::P {
                code: P::Jump,
                adr: Adr::label("LOOP"),
                x: None,
            })
            // END
            .end()
    }
}
