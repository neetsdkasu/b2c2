use super::*;

// コメントの除去
pub fn remove_comment(statements: &[casl2::Statement]) -> Vec<casl2::Statement> {
    let mut ret = vec![];

    for stmt in statements.iter() {
        if let casl2::Statement::Code { label, command, .. } = stmt {
            ret.labeled(label.clone(), command.clone());
        }
    }

    ret
}

// NOPの除去
pub(super) fn remove_nop(statements: &[casl2::Statement]) -> Vec<casl2::Statement> {
    let mut ret: Vec<casl2::Statement> = vec![];
    let mut marge_label: Option<&str> = None;
    let mut mapped_label = HashMap::<&str, &str>::new();

    for stmt in statements.iter() {
        match stmt {
            casl2::Statement::Code {
                label: None,
                command: casl2::Command::Nop,
                comment,
            } => {
                if let Some(comment) = comment {
                    ret.comment(comment.clone());
                }
            }
            casl2::Statement::Code {
                label: Some(label),
                command: casl2::Command::Nop,
                comment,
            } => {
                if let Some(comment) = comment {
                    ret.comment(comment.clone());
                }
                if let Some(key) = marge_label {
                    mapped_label.insert(label.as_str(), key);
                } else {
                    marge_label = Some(label.as_str());
                }
            }
            casl2::Statement::Code {
                label,
                command,
                comment,
            } => {
                if let Some(key) = marge_label.take() {
                    if let Some(label) = label {
                        mapped_label.insert(label.as_str(), key);
                    }
                    ret.push(casl2::Statement::Code {
                        label: Some(key.into()),
                        command: command.clone(),
                        comment: comment.clone(),
                    });
                } else {
                    ret.push(stmt.clone());
                }
            }
            casl2::Statement::Comment { .. } => ret.push(stmt.clone()),
        }
    }

    assert!(marge_label.is_none());

    for command in ret.iter_mut().filter_map(|stmt| {
        if let casl2::Statement::Code { command, .. } = stmt {
            Some(command)
        } else {
            None
        }
    }) {
        match command {
            casl2::Command::Start { entry_point } => {
                let new_label = entry_point
                    .as_ref()
                    .and_then(|label| mapped_label.get(label.as_str()));
                if let Some(&label) = new_label {
                    *entry_point = Some(label.into());
                }
            }
            casl2::Command::Dc { constants } => {
                for label in constants.iter_mut().filter_map(|cnst| {
                    if let casl2::Constant::Label(label) = cnst {
                        Some(label)
                    } else {
                        None
                    }
                }) {
                    if let Some(&new_label) = mapped_label.get(label.as_str()) {
                        *label = new_label.into();
                    }
                }
            }
            casl2::Command::In { pos, len } | casl2::Command::Out { pos, len } => {
                if let Some(&label) = mapped_label.get(pos.as_str()) {
                    *pos = label.into();
                }
                if let Some(&label) = mapped_label.get(len.as_str()) {
                    *len = label.into();
                }
            }
            casl2::Command::A { adr, .. } | casl2::Command::P { adr, .. } => {
                if let casl2::Adr::Label(label) = adr {
                    if let Some(&new_label) = mapped_label.get(label.as_str()) {
                        *label = new_label.into();
                    }
                }
            }
            casl2::Command::End
            | casl2::Command::Ds { .. }
            | casl2::Command::Rpush
            | casl2::Command::Rpop
            | casl2::Command::R { .. }
            | casl2::Command::Pop { .. }
            | casl2::Command::Ret => {}
            casl2::Command::Nop => unreachable!("BUG"),
        }
    }

    ret
}

// 未参照ラベルの除去
pub fn remove_unreferenced_label(statements: &[casl2::Statement]) -> Vec<casl2::Statement> {
    let mut ret: Vec<casl2::Statement> = vec![];
    let mut set = std::collections::HashSet::<&str>::new();

    for command in statements.iter().filter_map(|stmt| {
        if let casl2::Statement::Code { command, .. } = stmt {
            Some(command)
        } else {
            None
        }
    }) {
        match command {
            casl2::Command::Start { entry_point } => {
                if let Some(label) = entry_point {
                    set.insert(label.as_str());
                }
            }
            casl2::Command::Dc { constants } => {
                for cnst in constants.iter() {
                    if let casl2::Constant::Label(label) = cnst {
                        set.insert(label.as_str());
                    }
                }
            }
            casl2::Command::In { pos, len } | casl2::Command::Out { pos, len } => {
                set.insert(pos.as_str());
                set.insert(len.as_str());
            }
            casl2::Command::A { adr, .. } | casl2::Command::P { adr, .. } => {
                if let casl2::Adr::Label(label) = adr {
                    set.insert(label.as_str());
                }
            }
            casl2::Command::End
            | casl2::Command::Ds { .. }
            | casl2::Command::Rpush
            | casl2::Command::Rpop
            | casl2::Command::R { .. }
            | casl2::Command::Pop { .. }
            | casl2::Command::Ret
            | casl2::Command::Nop => {}
        }
    }

    for stmt in statements.iter() {
        match stmt {
            casl2::Statement::Code {
                label: Some(label),
                command,
                comment,
            } if !matches!(command, casl2::Command::Start { .. })
                && !set.contains(label.as_str()) =>
            {
                ret.push(casl2::Statement::Code {
                    label: None,
                    command: command.clone(),
                    comment: comment.clone(),
                });
            }
            _ => ret.push(stmt.clone()),
        }
    }

    ret
}

// 組み込みルーチンを分離する
pub(super) fn split_subroutines(
    mut statements: Vec<casl2::Statement>,
) -> Vec<(String, Vec<casl2::Statement>)> {
    let mut indexes: Vec<(String, usize)> = vec![];

    for (i, stmt) in statements.iter().enumerate() {
        let label = if let casl2::Statement::Code {
            label: Some(label), ..
        } = stmt
        {
            label
        } else {
            continue;
        };
        let label = label.as_str();
        if !(label.chars().count() >= 2
            && label.starts_with('C')
            && label.chars().skip(1).all(|ch| ch.is_ascii_digit()))
        {
            continue;
        }
        let label = label.to_string();
        if let Some(casl2::Statement::Comment { .. }) = statements.get(i - 1) {
            indexes.push((label, i - 1));
        } else {
            indexes.push((label, i));
        }
    }

    let mut ret = Vec::<(String, Vec<casl2::Statement>)>::new();

    while let Some((name_label, i)) = indexes.pop() {
        let mut routine = statements.split_off(i);
        if !matches!(
            routine.last(),
            Some(casl2::Statement::Code {
                command: casl2::Command::End,
                ..
            })
        ) {
            routine.code(casl2::Command::End);
        }
        for stmt in routine.iter_mut() {
            if let casl2::Statement::Code { label, .. } = stmt {
                if matches!(label, Some(label) if label.as_str() == name_label.as_str()) {
                    *label = None;
                    break;
                }
            }
        }
        routine.insert(
            0,
            casl2::Statement::labeled(&name_label, casl2::Command::Start { entry_point: None }),
        );
        ret.push((name_label, routine));
    }

    if !matches!(
        statements.last(),
        Some(casl2::Statement::Code {
            command: casl2::Command::End,
            ..
        })
    ) {
        statements.code(casl2::Command::End);
    }

    let program_name = statements
        .iter()
        .find_map(|stmt| {
            if let casl2::Statement::Code {
                label: Some(label),
                command: casl2::Command::Start { .. },
                ..
            } = stmt
            {
                Some(label.as_str().to_string())
            } else {
                None
            }
        })
        .expect("BUG");

    ret.push((program_name, statements));

    ret
}
