// b2c2-compiler crate::utils
// author: Leonardone @ NEETSDKASU

use super::*;

// 組み込みルーチンを分離する
pub fn split_subroutines(
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
        let routine = statements.split_off(i);
        let routine = to_external(&name_label, routine);
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

    let program_name = casl2::utils::get_program_name(&statements)
        .unwrap()
        .to_string();

    ret.push((program_name, statements));

    ret
}

// 分割保存対象のサブルーチンのコードの先頭と末尾にSTARTとENDを付与するための処理
pub fn to_external(name: &str, mut statements: Vec<casl2::Statement>) -> Vec<casl2::Statement> {
    if !matches!(
        statements.last(),
        Some(casl2::Statement::Code {
            command: casl2::Command::End,
            ..
        })
    ) {
        statements.code(casl2::Command::End);
    }

    for stmt in statements.iter_mut() {
        if let casl2::Statement::Code { label, .. } = stmt {
            if matches!(label, Some(label) if label.as_str() == name) {
                *label = None;
                break;
            }
        }
    }

    statements.insert(
        0,
        casl2::Statement::labeled(name, casl2::Command::Start { entry_point: None }),
    );

    statements
}
