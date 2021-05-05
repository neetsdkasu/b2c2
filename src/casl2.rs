use std::fmt;

pub use self::parser::{parse, Token, Tokenizer};

mod parser;
pub mod utils;

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

    #[cfg(test)]
    pub fn labeled_with_comment(label: &str, command: Command, comment: &str) -> Self {
        Self::Code {
            label: Some(label.into()),
            command,
            comment: Some(comment.into()),
        }
    }

    #[cfg(test)]
    pub fn code_with_comment(command: Command, comment: &str) -> Self {
        Self::Code {
            label: None,
            command,
            comment: Some(comment.into()),
        }
    }

    #[cfg(test)]
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
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
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

    pub fn as_string(&self) -> &String {
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

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
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
    type Error = ();
    fn try_from(r: Register) -> Result<Self, Self::Error> {
        use Register::*;
        let ir = match r {
            Gr0 => return Err(()),
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

impl std::convert::TryFrom<&str> for IndexRegister {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let s = s.to_ascii_uppercase();
        let ir = match s.as_str() {
            "GR1" => Self::Gr1,
            "GR2" => Self::Gr2,
            "GR3" => Self::Gr3,
            "GR4" => Self::Gr4,
            "GR5" => Self::Gr5,
            "GR6" => Self::Gr6,
            "GR7" => Self::Gr7,
            _ => return Err(()),
        };
        Ok(ir)
    }
}
impl std::convert::TryFrom<&str> for Register {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let s = s.to_ascii_uppercase();
        let ir = match s.as_str() {
            "GR0" => Self::Gr0,
            "GR1" => Self::Gr1,
            "GR2" => Self::Gr2,
            "GR3" => Self::Gr3,
            "GR4" => Self::Gr4,
            "GR5" => Self::Gr5,
            "GR6" => Self::Gr6,
            "GR7" => Self::Gr7,
            _ => return Err(()),
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
    DebugBasicStep {
        id: usize,
    },
}

#[cfg(test)]
#[derive(Clone, Debug)]
pub struct Program {
    name: String,
    statements: Vec<Statement>,
}

#[cfg(test)]
#[derive(Clone, Debug)]
pub struct Builder {
    label: Option<Label>,
    program: Program,
}

#[cfg(test)]
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

    fn end(mut self) -> Program {
        let label = self.label.take();
        self.program.statements.push(Statement::Code {
            label,
            command: Command::End,
            comment: None,
        });
        self.program
    }
}

#[cfg(test)]
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
            DebugBasicStep { id } => format!("; DEBUGBASICSTEP {}", id),
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
