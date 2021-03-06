// b2c2-compiler-common crate
// author: Leonardone @ NEETSDKASU

use b2c2_casl2 as casl2;

pub const MAX_ALLOCATION_SIZE: usize = 30000;
pub const MAX_ARRAY_SIZE: usize = 256;

// プログラム名のラベルとしての正当性チェック
pub fn is_valid_program_name(program_name: &str) -> bool {
    // 予約済みラベル
    // EOF    Inputステートメント、EOF()関数で使用
    // EXIT   Endステートメントで使用
    // MEM    Allocatorが確保したメモリのアドレスを保持する領域
    // ALLOC  Allocatorのサブルーチン名
    // 自動生成のラベルとの重複を避けるチェックが必要
    // B** 真理値変数
    // I** 整数変数
    // T** 式展開時の一時変数(真理値/整数で共有)(主にForループの終点とステップで使用)
    // V** 組み込みサブルーチンのローカル変数
    // J** ループや条件分岐に使うジャンプ先ラベルのId
    // C** 組み込みサブルーチンの入り口のラベル
    // F** スニペットのサブルーチン
    // SL** 文字列変数の長さ
    // SB** 文字列変数の内容位置
    // BA** 真理値配列
    // IA** 整数配列
    // LL** IN/OUTで使用の文字列定数の長さ
    // LB** IN/OUTで使用の文字列定数の内容位置
    // TL** 式展開時の一時的な文字列変数の長さ
    // TB** 式展開時の一時的な文字列変数の内容位置
    // ARG* プログラム引数
    casl2::Label::from(program_name).is_valid()
        && !(matches!(program_name, "EOF" | "EXIT" | "MEM" | "ALLOC")
            || ((program_name.chars().count() >= 2)
                && ["B", "I", "T", "V", "J", "C", "F"]
                    .iter()
                    .any(|prefix| program_name.starts_with(prefix))
                && program_name.chars().skip(1).all(|ch| ch.is_ascii_digit()))
            || ((program_name.chars().count() >= 3)
                && ["SL", "SB", "BA", "IA", "LL", "LB", "TL", "TB"]
                    .iter()
                    .any(|prefix| program_name.starts_with(prefix))
                && program_name.chars().skip(2).all(|ch| ch.is_ascii_digit()))
            || ((program_name.chars().count() == 4)
                && program_name.starts_with("ARG")
                && program_name.chars().skip(3).all(|ch| ch.is_ascii_digit())))
}
