// b2c2-compiler crate::optimize
// author: Leonardone @ NEETSDKASU

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
pub fn remove_nop(statements: &[casl2::Statement]) -> Vec<casl2::Statement> {
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
            casl2::Command::DebugBasicStep { .. } => {}
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
            | casl2::Command::Nop
            | casl2::Command::DebugBasicStep { .. } => {}
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

// スニペット化できるステートメントの判定用
trait Casl2StatementExtension {
    fn is_jump_code(&self) -> bool;
    fn is_push_code(&self) -> bool;
    fn is_pop_code(&self) -> bool;
}

impl Casl2StatementExtension for casl2::Statement {
    fn is_jump_code(&self) -> bool {
        if let casl2::Statement::Code {
            command: casl2::Command::P { code, .. },
            ..
        } = self
        {
            use casl2::P::*;
            match code {
                Jpl | Jmi | Jnz | Jze | Jov | Jump => true,
                Push | Call | Svc => false,
            }
        } else {
            false
        }
    }

    fn is_push_code(&self) -> bool {
        matches!(
            self,
            casl2::Statement::Code {
                command: casl2::Command::P {
                    code: casl2::P::Push,
                    ..
                },
                ..
            }
        )
    }

    fn is_pop_code(&self) -> bool {
        matches!(
            self,
            casl2::Statement::Code {
                command: casl2::Command::Pop { .. },
                ..
            }
        )
    }
}

fn can_include_snippet(stmt: &casl2::Statement) -> bool {
    stmt.is_code() && !stmt.is_jump_code()
}

// 共通コードのスニペットをサブルーチンとしてまとめる
pub fn collect_duplicates(mut statements: Vec<casl2::Statement>) -> Vec<casl2::Statement> {
    let mut snippets = vec![];
    let mut id = 0;

    loop {
        let end = {
            let mut end = statements.len();
            for (i, stmt) in statements.iter().enumerate() {
                if let casl2::Statement::Code {
                    command: casl2::Command::Ret,
                    ..
                } = stmt
                {
                    end = i;
                    break;
                }
            }
            end
        };

        let mut hset = std::collections::HashSet::new();

        let mut best: Option<(usize, usize, Vec<usize>)> = None;

        for i in 0..end {
            if !can_include_snippet(&statements[i]) {
                continue;
            }
            let mut stack: i32 = 0;
            if statements[i].is_pop_code() {
                continue;
            } else if statements[i].is_push_code() {
                stack += 1;
            }
            for r in i + 1..end {
                if !can_include_snippet(&statements[r]) {
                    break;
                }
                if statements[r].is_pop_code() {
                    stack -= 1;
                    if stack < 0 {
                        break;
                    }
                } else if statements[r].is_push_code() {
                    stack += 1;
                }
                if stack != 0 {
                    continue;
                }
                let len = r - i + 1;
                if hset.contains(&(i, len)) {
                    continue;
                }
                let mut dups = vec![i];
                for k in r + 1..end - len {
                    if statements[i..=r] != statements[k..k + len] {
                        continue;
                    }
                    if hset.insert((k, len)) {
                        dups.push(k);
                    }
                }
                if dups.len() < 2 {
                    continue;
                }
                let before_cost = len * dups.len();
                let after_cost = len + 1 + dups.len();
                if after_cost >= before_cost {
                    continue;
                }
                let score = before_cost - after_cost;
                if let Some((best_score, best_len, _)) = best.as_ref() {
                    if *best_score > score {
                        continue;
                    }
                    if *best_score == score && *best_len <= len {
                        continue;
                    }
                }
                best = Some((score, len, dups));
            }
        }

        if let Some((_, len, mut dups)) = best {
            id += 1;
            let name = format!("F{:03}", id);
            let mut snippet = None;
            while let Some(pos) = dups.pop() {
                let rest = statements.split_off(pos + len);
                if snippet.is_none() {
                    let code = statements.split_off(pos);
                    snippet = Some(code);
                } else {
                    statements.truncate(pos);
                }
                statements.code(format!(" CALL {}", name));
                statements.extend(rest);
            }
            if let Some(mut snippet) = snippet {
                if let Some(casl2::Statement::Code { label, .. }) = snippet.first_mut() {
                    *label = Some(name.into());
                }
                snippet.code(casl2::Command::Ret);
                snippets.push(snippet);
            }
        } else {
            break;
        }
    }

    let rest = {
        let mut end = statements.len();
        for (i, stmt) in statements.iter().enumerate() {
            if let casl2::Statement::Code {
                command: casl2::Command::Ret,
                ..
            } = stmt
            {
                end = i + 1;
                break;
            }
        }
        statements.split_off(end)
    };

    for snippet in snippets {
        statements.extend(snippet);
    }

    statements.extend(rest);

    statements
}
