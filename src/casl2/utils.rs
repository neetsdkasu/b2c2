use super::*;

pub fn is_program(statements: &[Statement]) -> bool {
    let mut iter = statements.iter().filter(|stmt| stmt.is_code());

    match iter.next() {
        Some(Statement::Code {
            label: Some(_),
            command: Command::Start { .. },
            ..
        }) => {}
        _ => return false,
    }

    let mut has_ret = false;

    while let Some(stmt) = iter.next() {
        match stmt {
            Statement::Code {
                command: Command::Start { .. },
                ..
            } => return false,
            Statement::Code {
                command: Command::End,
                ..
            } => return has_ret && iter.next().is_none(),
            Statement::Code {
                command: Command::Ret,
                ..
            } => has_ret = true,
            _ => {}
        }
    }

    false
}

pub fn get_program_name(statements: &[Statement]) -> Option<&str> {
    statements.iter().find_map(|stmt| {
        if let Statement::Code {
            label: Some(label),
            command: Command::Start { .. },
            ..
        } = stmt
        {
            Some(label.as_str())
        } else {
            None
        }
    })
}

pub fn change_label(
    statements: &[Statement],
    old_label: &str,
    new_label: &str,
) -> Option<Vec<Statement>> {
    let mut ret = vec![];

    for stmt in statements.iter() {
        match stmt {
            Statement::Code {
                label: Some(label), ..
            } if label.as_str() == new_label => return None,
            Statement::Code {
                label: Some(label),
                command,
                comment,
            } if label.as_str() == old_label => ret.push(Statement::Code {
                label: Some(new_label.into()),
                command: command.clone(),
                comment: comment.clone(),
            }),
            _ => ret.push(stmt.clone()),
        }
    }

    for command in ret.iter_mut().filter_map(|stmt| {
        if let Statement::Code { command, .. } = stmt {
            Some(command)
        } else {
            None
        }
    }) {
        match command {
            Command::A {
                adr: Adr::Label(label),
                ..
            }
            | Command::P {
                adr: Adr::Label(label),
                ..
            } => {
                if label.as_str() == old_label {
                    *label = new_label.into();
                } else if label.as_str() == new_label {
                    return None;
                }
            }
            Command::Start {
                entry_point: Some(label),
            } => {
                if label.as_str() == old_label {
                    *label = new_label.into();
                } else if label.as_str() == new_label {
                    return None;
                }
            }
            Command::In { pos, len } | Command::Out { pos, len } => {
                if pos.as_str() == old_label {
                    *pos = new_label.into();
                } else if pos.as_str() == new_label {
                    return None;
                }
                if len.as_str() == old_label {
                    *len = new_label.into();
                } else if len.as_str() == new_label {
                    return None;
                }
            }
            Command::Dc { constants } => {
                for c in constants.iter_mut() {
                    if let Constant::Label(label) = c {
                        if label.as_str() == old_label {
                            *label = new_label.into();
                        } else if label.as_str() == new_label {
                            return None;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    Some(ret)
}
