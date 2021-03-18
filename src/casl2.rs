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
    GR0,
    GR1,
    GR2,
    GR3,
    GR4,
    GR5,
    GR6,
    GR7,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IndexRegister {
    GR1,
    GR2,
    GR3,
    GR4,
    GR5,
    GR6,
    GR7,
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
        write!(f, "GR{}", *self as isize + 1)
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
          PUSH      0,GR3
          XOR       GR0,GR0        ; GR0 = 0
LOOP      LAD       GR3,0,GR1      ; GR3 = GR1
          AND       GR3,=1         ; GR3 &= 1
          JZE       SHIFT          ; if GR3 == 0 then goto SHIFT
          ADDL      GR0,GR2        ; GR0 += GR2
SHIFT     SLL       GR2,=1         ; GR2 <<= 1
          SRL       GR1,=1         ; GR1 >>= 1
          JNZ       LOOP           ; if GR1 != 0 then goto LOOP
          POP       GR3
          POP       GR2
          POP       GR1
          RET
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
            // PUSH      0,GR3
            Stmt::code(Cmd::P {
                code: P::Push,
                adr: Adr::Dec(0),
                x: Some(Idx::GR3),
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
            // LOOP      LAD       GR3,0,GR1      ; GR3 = GR1
            Stmt::labeled_with_comment(
                "LOOP",
                Cmd::A {
                    code: A::Lad,
                    r: Reg::GR3,
                    adr: Adr::Dec(0),
                    x: Some(Idx::GR1),
                },
                "GR3 = GR1",
            ),
            // AND       GR3,=1         ; GR3 &= 1
            Stmt::code_with_comment(
                Cmd::A {
                    code: A::And,
                    r: Reg::GR3,
                    adr: Adr::LiteralDec(1),
                    x: None,
                },
                "GR3 &= 1",
            ),
            // JZE       SHIFT          ; if GR3 == 0 then goto SHIFT
            Stmt::code_with_comment(
                Cmd::P {
                    code: P::Jze,
                    adr: Adr::label("SHIFT"),
                    x: None,
                },
                "if GR3 == 0 then goto SHIFT",
            ),
            // ADDL      GR0,GR2        ; GR0 += GR2
            Stmt::code_with_comment(
                Cmd::R {
                    code: R::Addl,
                    r1: Reg::GR0,
                    r2: Reg::GR2,
                },
                "GR0 += GR2",
            ),
            // SHIFT     SLL       GR2,=1         ; GR2 <<= 1
            Stmt::labeled_with_comment(
                "SHIFT",
                Cmd::A {
                    code: A::Sll,
                    r: Reg::GR2,
                    adr: Adr::LiteralDec(1),
                    x: None,
                },
                "GR2 <<= 1",
            ),
            // SRL       GR1,=1         ; GR1 >>= 1
            Stmt::code_with_comment(
                Cmd::A {
                    code: A::Srl,
                    r: Reg::GR1,
                    adr: Adr::LiteralDec(1),
                    x: None,
                },
                "GR1 >>= 1",
            ),
            // JNZ       LOOP           ; if GR1 != 0 then goto LOOP
            Stmt::code_with_comment(
                Cmd::P {
                    code: P::Jnz,
                    adr: Adr::label("LOOP"),
                    x: None,
                },
                "if GR1 != 0 then goto LOOP",
            ),
            // POP       GR3
            Stmt::code(Cmd::Pop { r: Reg::GR3 }),
            // POP       GR2
            Stmt::code(Cmd::Pop { r: Reg::GR2 }),
            // POP       GR1
            Stmt::code(Cmd::Pop { r: Reg::GR1 }),
            // RET
            Stmt::code(Cmd::Ret),
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
            // PUSH 0,GR3
            .code(Cmd::P {
                code: P::Push,
                adr: Adr::Dec(0),
                x: Some(Idx::GR3),
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
            // LOOP   LAD  GR3,0,GR1  ; GR3 = GR1
            .label("LOOP")
            .code_with_comment(
                Cmd::A {
                    code: A::Lad,
                    r: Reg::GR3,
                    adr: Adr::Dec(0),
                    x: Some(Idx::GR1),
                },
                "GR3 = GR1",
            )
            // AND  GR3,=1     ; GR3 &= 1
            .code_with_comment(
                Cmd::A {
                    code: A::And,
                    r: Reg::GR3,
                    adr: Adr::LiteralDec(1),
                    x: None,
                },
                "GR3 &= 1",
            )
            // JZE  SHIFT      ; if GR3 == 0 then goto SHIFT
            .code_with_comment(
                Cmd::P {
                    code: P::Jze,
                    adr: Adr::label("SHIFT"),
                    x: None,
                },
                "if GR3 == 0 then goto SHIFT",
            )
            // ADDL GR0,GR2    ; GR0 += GR2
            .code_with_comment(
                Cmd::R {
                    code: R::Addl,
                    r1: Reg::GR0,
                    r2: Reg::GR2,
                },
                "GR0 += GR2",
            )
            // SHIFT  SLL  GR2,=1     ; GR2 <<= 1
            .label("SHIFT")
            .code_with_comment(
                Cmd::A {
                    code: A::Sll,
                    r: Reg::GR2,
                    adr: Adr::LiteralDec(1),
                    x: None,
                },
                "GR2 <<= 1",
            )
            // SRL  GR1,=1     ; GR1 >>= 1
            .code_with_comment(
                Cmd::A {
                    code: A::Srl,
                    r: Reg::GR1,
                    adr: Adr::LiteralDec(1),
                    x: None,
                },
                "GR1 >>= 1",
            )
            // JNZ  LOOP       ; if GR1 != 0 then goto LOOP
            .code_with_comment(
                Cmd::P {
                    code: P::Jnz,
                    adr: Adr::label("LOOP"),
                    x: None,
                },
                "if GR1 != 0 then goto LOOP",
            )
            // POP  GR3
            .code(Cmd::Pop { r: Reg::GR3 })
            // POP  GR2
            .code(Cmd::Pop { r: Reg::GR2 })
            // POP  GR1
            .code(Cmd::Pop { r: Reg::GR1 })
            // RET
            .code(Cmd::Ret)
            // END
            .end()
    }
}
