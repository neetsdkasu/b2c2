use crate::casl2::*;
use std::collections::{BTreeMap, HashMap};

pub fn analyze(statements: &[Statement]) -> String {
    let mut comment_lines: usize = 0;
    let mut with_comment_lines: usize = 0;
    let mut labels = HashMap::<String, usize>::new();
    let mut commands = HashMap::<String, usize>::new();
    let mut total_ds_size: usize = 0;
    let mut total_dc_size: usize = 0;
    let mut total_dc_item_count: usize = 0;
    let mut registers = vec![0_usize; 8];
    let mut index_registers = vec![0_usize; 8];
    let mut literals = HashMap::<String, usize>::new();
    let mut r_codes = HashMap::<String, usize>::new();
    let mut a_codes = HashMap::<String, usize>::new();
    let mut p_codes = HashMap::<String, usize>::new();
    let mut name: String = String::new();

    for stmt in statements.iter() {
        match stmt {
            Statement::Comment { .. } => {
                comment_lines += 1;
            }
            Statement::Code {
                label,
                command,
                comment,
            } => {
                if comment.is_some() {
                    with_comment_lines += 1;
                }
                if let Some(label) = label {
                    let key = label.as_str().to_string();
                    labels.entry(key).or_insert(0);
                }
                match command {
                    Command::Start { entry_point } => {
                        if let Some(label) = label {
                            name.push_str(label.as_str());
                        }
                        if let Some(label) = entry_point {
                            let key = label.as_str().to_string();
                            *labels.entry(key).or_insert(0) += 1;
                        }
                        *commands.entry("START".to_string()).or_insert(0) += 1;
                    }
                    Command::End => *commands.entry("END".to_string()).or_insert(0) += 1,
                    Command::Ds { size } => {
                        total_ds_size += *size as usize;
                        *commands.entry("DS".to_string()).or_insert(0) += 1;
                    }
                    Command::Dc { constants } => {
                        total_dc_item_count += constants.len();
                        total_dc_size += constants.len();
                        for cnst in constants.iter() {
                            match cnst {
                                Constant::Label(label) => {
                                    let key = label.as_str().to_string();
                                    *labels.entry(key).or_insert(0) += 1;
                                }
                                Constant::Str(s) => {
                                    total_dc_size += s.chars().count();
                                    total_dc_size -= 1;
                                }
                                _ => {}
                            }
                        }
                        *commands.entry("DC".to_string()).or_insert(0) += 1;
                    }
                    Command::In { pos, len } => {
                        *labels.entry(pos.as_str().to_string()).or_insert(0) += 1;
                        *labels.entry(len.as_str().to_string()).or_insert(0) += 1;
                        *commands.entry("IN".to_string()).or_insert(0) += 1;
                    }
                    Command::Out { pos, len } => {
                        *labels.entry(pos.as_str().to_string()).or_insert(0) += 1;
                        *labels.entry(len.as_str().to_string()).or_insert(0) += 1;
                        *commands.entry("OUT".to_string()).or_insert(0) += 1;
                    }
                    Command::Rpush => *commands.entry("RPUSH".to_string()).or_insert(0) += 1,
                    Command::Rpop => *commands.entry("RPOP".to_string()).or_insert(0) += 1,
                    Command::R { code, r1, r2 } => {
                        registers[*r1 as usize] += 1;
                        registers[*r2 as usize] += 1;
                        use R::*;
                        let s = match code {
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
                        *r_codes.entry(s.to_string()).or_insert(0) += 1;
                    }
                    Command::A { code, r, adr, x } => {
                        registers[*r as usize] += 1;
                        if let Some(reg) = x {
                            index_registers[*reg as usize] += 1;
                        }
                        if let Adr::Label(label) = adr {
                            let key = label.as_str().to_string();
                            *labels.entry(key).or_insert(0) += 1;
                        } else if matches!(
                            adr,
                            Adr::LiteralDec(_) | Adr::LiteralHex(_) | Adr::LiteralStr(_)
                        ) {
                            *literals.entry(adr.to_string()).or_insert(0) += 1;
                        }
                        use A::*;
                        let s = match code {
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
                        *a_codes.entry(s.to_string()).or_insert(0) += 1;
                    }
                    Command::P { code, adr, x } => {
                        if let Some(reg) = x {
                            index_registers[*reg as usize] += 1;
                        }
                        if let Adr::Label(label) = adr {
                            let key = label.as_str().to_string();
                            *labels.entry(key).or_insert(0) += 1;
                        } else if matches!(
                            adr,
                            Adr::LiteralDec(_) | Adr::LiteralHex(_) | Adr::LiteralStr(_)
                        ) {
                            *literals.entry(adr.to_string()).or_insert(0) += 1;
                        }
                        use P::*;
                        let s = match code {
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
                        *p_codes.entry(s.to_string()).or_insert(0) += 1;
                    }
                    Command::Pop { r } => {
                        registers[*r as usize] += 1;
                        *commands.entry("POP".to_string()).or_insert(0) += 1;
                    }
                    Command::Ret => *commands.entry("RET".to_string()).or_insert(0) += 1,
                    Command::Nop => *commands.entry("NOP".to_string()).or_insert(0) += 1,
                }
            }
        }
    }
    format!(
        r#"
STATISTICS

name: {name}

lines: {lines}
    comment: {comments}
    code: {codes}
        with_comment: {with_comment}

total_ds_size: {total_ds_size}

total_dc_item_count: {total_dc_item_count}
total_dc_size: {total_dc_size}

labels:
    total: {total_label}
    unused: {unused_label}
    {labels}

registers:
    [GR0,GR1,GR2,GR3,GR4,GR5,GR6,GR7]
    {registers}

index registers:
    [---,GR1,GR2,GR3,GR4,GR5,GR6,GR7]
    {index_registers}

literals:
    {literals}

commands:
    {commands}
    {p_codes}

commands(r1,r2):
    {r_codes}

commands(r,adr,x):
    {a_codes}

"#,
        name = name,
        lines = statements.len(),
        comments = comment_lines,
        codes = statements.len() - comment_lines,
        with_comment = with_comment_lines,
        labels = format!("{:?}", labels.iter().collect::<BTreeMap<_, _>>()),
        total_label = labels.len(),
        unused_label = labels.iter().filter(|(_, c)| **c == 0).count(),
        commands = format!("{:?}", commands.iter().collect::<BTreeMap<_, _>>()),
        p_codes = format!("{:?}", p_codes.iter().collect::<BTreeMap<_, _>>()),
        total_ds_size = total_ds_size,
        total_dc_item_count = total_dc_item_count,
        total_dc_size = total_dc_size,
        registers = format!("{:?}", registers),
        index_registers = format!("{:?}", index_registers),
        literals = format!("{:?}", literals.iter().collect::<BTreeMap<_, _>>()),
        r_codes = format!("{:?}", r_codes.iter().collect::<BTreeMap<_, _>>()),
        a_codes = format!("{:?}", a_codes.iter().collect::<BTreeMap<_, _>>())
    )
}
