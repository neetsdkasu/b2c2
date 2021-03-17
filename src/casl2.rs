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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Label(String);

impl Label {
    pub fn is_valid(&self) -> bool {
        let Label(label) = self;
        let mut chars = label.chars();
        if chars.next().filter(|ch| ch.is_ascii_uppercase()).is_none() {
            return false;
        }
        if !chars.all(|ch| ch.is_ascii_uppercase() || ch.is_ascii_digit()) {
            return false;
        }
        label.chars().count() <= 8
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Adr {
    Dec(i32),           // -32678 ~ 32677
    Hex(i32),           // #0000 ~ #FFFF
    Label(Label),       // 'text'
    LiteralDec(i32),    // =1234
    LiteralHex(i32),    // =#12AB
    LiteralStr(String), // ='text'
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Constant {
    Dec(i32),     // =1234
    Hex(i32),     // =#12AB
    Str(String),  // ='text'
    Label(Label), // 'text'
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
        size: i32,
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

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Label(label) = self;
        write!(f, "{}", label)
    }
}

impl fmt::Display for Adr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Adr::Dec(v) => write!(f, "{}", *v as i16),
            Adr::Hex(v) => write!(f, "#{:04X}", *v as u16),
            Adr::Label(v) => write!(f, "{}", v),
            Adr::LiteralDec(v) => write!(f, "={}", *v as i16),
            Adr::LiteralHex(v) => write!(f, "={:04X}", *v as u16),
            Adr::LiteralStr(v) => write!(f, "='{}'", v),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Dec(v) => write!(f, "{}", *v as i16),
            Constant::Hex(v) => write!(f, "#{:04X}", *v as u16),
            Constant::Str(v) => write!(f, "'{}'", v),
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
            Ds { size } => write!(f, "{:<9} {}", "DS", *size as u16),
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
            In { pos, len } => write!(f, "{:<9} {}, {}", "IN", pos, len),
            Out { pos, len } => write!(f, "{:<9} {}, {}", "OUT", pos, len),
            Rpush => write!(f, "RPUSH"),
            Rpop => write!(f, "RPOP"),

            R { code, r1, r2 } => write!(f, "{:<9} {}, {}", code.to_string(), r1, r2),
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
            Statement::Comment(text) => write!(f, "; {}", text),
            Statement::Code {
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
            Statement::Code {
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

    #[test]
    fn it_works() {
        let src = vec![
            Statement::Code {
                label: Some(Label("COUNT1".into())),
                command: Command::Start { entry_point: None },
                comment: Some("".into()),
            },
            Statement::Comment(format!("{:<7} {:<7} {}", "", "入力", "GR1:検索する語")),
            Statement::Comment(format!(
                "{:<7} {:<7} {}",
                "", "処理", "GR1中の'1'のビットの個数を求める"
            )),
            Statement::Comment(format!(
                "{:<7} {:<7} {}",
                "", "出力", "GR0:GR1中の'1'のビットの個数"
            )),
            Statement::Code {
                label: None,
                command: Command::P {
                    code: P::Push,
                    adr: Adr::Dec(0),
                    x: Some(IndexRegister::GR1),
                },
                comment: Some("".into()),
            },
            Statement::Code {
                label: None,
                command: Command::P {
                    code: P::Push,
                    adr: Adr::Dec(0),
                    x: Some(IndexRegister::GR2),
                },
                comment: Some("".into()),
            },
            Statement::Code {
                label: None,
                command: Command::R {
                    code: R::Suba,
                    r1: Register::GR2,
                    r2: Register::GR2,
                },
                comment: Some("Count = 0".into()),
            },
            Statement::Code {
                label: None,
                command: Command::R {
                    code: R::And,
                    r1: Register::GR1,
                    r2: Register::GR1,
                },
                comment: Some("全部のビットが'0'？".into()),
            },
            Statement::Code {
                label: None,
                command: Command::P {
                    code: P::Jze,
                    adr: Adr::Label(Label("RETURN".into())),
                    x: None,
                },
                comment: Some("全部のビットが'0'なら終了".into()),
            },
            Statement::Code {
                label: Some(Label("MORE".into())),
                command: Command::A {
                    code: A::Lad,
                    r: Register::GR2,
                    adr: Adr::Dec(1),
                    x: Some(IndexRegister::GR2),
                },
                comment: Some("Count = Count + 1".into()),
            },
            Statement::Code {
                label: None,
                command: Command::A {
                    code: A::Lad,
                    r: Register::GR1,
                    adr: Adr::Dec(-1),
                    x: Some(IndexRegister::GR1),
                },
                comment: Some("最下位の'1'のビット１個を".into()),
            },
            Statement::Code {
                label: None,
                command: Command::R {
                    code: R::And,
                    r1: Register::GR1,
                    r2: Register::GR0,
                },
                comment: Some("　'0'に変える".into()),
            },
            Statement::Code {
                label: None,
                command: Command::P {
                    code: P::Jnz,
                    adr: Adr::Label(Label("MORE".into())),
                    x: None,
                },
                comment: Some("'1'のビットが残っていれば繰返し".into()),
            },
            Statement::Code {
                label: Some(Label("RETURN".into())),
                command: Command::R {
                    code: R::Ld,
                    r1: Register::GR0,
                    r2: Register::GR2,
                },
                comment: Some("GR0 = Count".into()),
            },
            Statement::Code {
                label: None,
                command: Command::Pop { r: Register::GR2 },
                comment: Some("".into()),
            },
            Statement::Code {
                label: None,
                command: Command::Pop { r: Register::GR1 },
                comment: Some("".into()),
            },
            Statement::Code {
                label: None,
                command: Command::Ret,
                comment: Some("呼び出しプログラムへ戻る".into()),
            },
            Statement::Code {
                label: None,
                command: Command::End,
                comment: Some("".into()),
            },
        ];
        for s in src.iter() {
            eprintln!("{}", s);
        }
    }
}
