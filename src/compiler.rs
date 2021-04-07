use self::ext::*;
use crate::casl2;
use crate::parser;
use crate::tokenizer;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::TryFrom;
use std::fmt::Write;

mod ext;
mod subroutine;

#[cfg(test)]
mod test;

type CompileError = String;

// 正しい src が来ることを前提とする (不正な src の判定は面倒くさい)
pub fn compile(
    program_name: &str,
    src: &[parser::Statement],
) -> Result<Vec<casl2::Statement>, CompileError> {
    let mut compiler = Compiler::new(program_name)?;

    for stmt in src.iter() {
        compiler.compile(stmt);
    }

    Ok(compiler.finish())
}

#[derive(Clone, Debug)]
pub struct Flag {
    // コメントを除去
    pub remove_comment: bool,
    // NOPを除去
    pub remove_nop: bool,
    // 未参照ラベルを除去
    pub remove_unreferenced_label: bool,
    // サブルーチンを分割
    pub split_subroutines: bool,
}

// 条件付き(?)コンパイル
pub fn compile_with_flag(
    program_name: &str,
    src: &[parser::Statement],
    flag: Flag,
) -> Result<Vec<(String, Vec<casl2::Statement>)>, CompileError> {
    let mut statements = compile(program_name, src)?;

    if flag.remove_comment {
        statements = remove_comment(&statements);
    }

    if flag.remove_nop {
        statements = remove_nop(&statements);
    }

    if flag.remove_unreferenced_label {
        statements = remove_unreferenced_label(&statements);
    }

    if !flag.split_subroutines {
        return Ok(vec![(program_name.to_string(), statements)]);
    }

    Ok(split_subroutines(program_name, statements))
}

struct Compiler {
    // ソース定義の変数のId (真理値/整数/文字列の全体で一意)
    var_id: usize,

    // IN/OUTで使用する文字列定数のId
    lit_id: usize,

    // 組み込みサブルーチンのローカル変数・定数のId (名前重複を防ぐが目的) (DS/DCは各サブルーチン内で定義する)
    local_var_id: usize,

    // 式展開時などの一時変数のId
    temp_int_var_id: usize,
    temp_str_var_id: usize,

    // ループや条件分岐に使うジャンプ先ラベルのId (全体で一意)
    jump_id: usize,

    // 真理値変数のラベル対応を保持 (変数名, ラベル)
    bool_var_labels: HashMap<String, String>,

    // 整数変数のラベル対応を保持 (変数名, ラベル)
    int_var_labels: HashMap<String, String>,

    // 文字列変数のラベル対応を保持 (変数名, (長さラベル, 内容位置ラベル))
    str_var_labels: HashMap<String, StrLabels>,

    // 真理値配列のラベル対応を保持 (配列名, (ラベル, 配列サイズ))
    bool_arr_labels: HashMap<String, (String, usize)>,

    // 整数配列のラベル対応を保持 (配列名, (ラベル, 配列サイズ))
    int_arr_labels: HashMap<String, (String, usize)>,

    // IN/OUTで使用する文字列定数のラベル対応を保持 (文字列定数, (長さラベル, 内容位置ラベル)))
    lit_str_labels: HashMap<String, StrLabels>,

    // 式展開時の一時変数(整数/真理値)のラベルを保持 (スタック的利用) (ラベル) (主にForループの終点とステップで使用)
    temp_int_var_labels: Vec<String>,

    // 式展開時の一時変数(文字列)のラベルを保持 (スタック的利用) (長さラベル, 内容位置ラベル)
    temp_str_var_labels: Vec<StrLabels>,

    // 組み込みサブルーチンのコードを保持 (サブルーチンの固有名, コード)
    subroutine_codes: HashMap<subroutine::Id, Vec<casl2::Statement>>,

    // Continueで参照する継続ラベル対応を保持 (exit_id, 継続ラベル)
    loop_labels: HashMap<usize, String>,

    // Exitステートメントで参照する脱出ラベル対応を保持 (exit_id, 脱出ラベル)
    exit_labels: HashMap<usize, String>,

    // 式展開でレジスタの使用状況把握
    //  最下位ビットがGR0, 最上位ビットがGR7
    //    (1 << GR0),  (1 << GR7)
    //    (ただし、実際にはGR0の使用状況は管理しないでGR1～7のみ管理)
    //  ビットが立っている場合は使用中(register_dequeにある)
    //  ビットが立っていない場合はアイドル中、番号の大きいレジスタから使用していく
    registers_used: u8,

    // 式展開でレジスタの使用状況把握
    //  使用するレジスタを後尾に追加し、使用し終わったら後尾から取り出す
    //  アイドルレジスタが１つもない場合、
    //     先頭のレジスタを取り出して使用する(後尾に追加する)
    //     このレジスタの内容はコールスタックに積んでおき、必要のタイミングでスタックから取り出す
    //  先頭：　現在の式展開中の箇所から"遠い"箇所で使用してるレジスタ
    //  後尾: 現在の式展開中の箇所から"近い"箇所で使用してるレジスタ
    used_registers_order: Vec<casl2::Register>,
    stacked_registers: Vec<casl2::Register>,

    // 生成するCASL2コード本体
    statements: Vec<casl2::Statement>,

    // Input命令またはEOF関数があるときに設定される領域を持つかどうか
    has_eof: bool,

    // 変数領域の総サイズ
    var_total_size: usize,
}

// 文字列ラベルのタイプ判定に使う
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum StrLabelType {
    Const,       // IN/OUTの定数 LB**
    Lit(String), // リテラル ='abc'
    Temp,        // 一時変数 TB**
    Var,         // 文字列変数 SB**
}

// 文字列のラベル
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
struct StrLabels {
    pos: String,
    len: String,
    label_type: StrLabelType,
}

impl subroutine::Gen for Compiler {
    fn var_label(&mut self) -> String {
        self.get_new_local_var_label()
    }

    fn jump_label(&mut self) -> String {
        self.get_new_jump_label()
    }
}

impl Compiler {
    // プログラム名のラベルとしての正当性チェック
    fn is_valid_program_name(program_name: &str) -> bool {
        casl2::Label::from(program_name).is_valid()

        // 予約済みラベル
        // EOF Inputステートメント、EOF()関数で使用
        && !matches!(program_name, "EOF")

        // 自動生成のラベルとの重複を避けるチェックが必要
        // B** 真理値変数
        // I** 整数変数
        // T** 式展開時の一時変数(真理値/整数で共有)(主にForループの終点とステップで使用)
        // V** 組み込みサブルーチンのローカル変数
        // J** ループや条件分岐に使うジャンプ先ラベルのId
        // C** 組み込みサブルーチンの入り口のラベル
        && (program_name.len() < 2 || !{
            let (head, tail) = program_name.split_at(1);
            matches!(head, "B"|"I"|"T"|"V"|"J"|"C")
            && tail.chars().all(|ch| ch.is_ascii_digit())
        })
        // SL** 文字列変数の長さ
        // SB** 文字列変数の内容位置
        // BA** 真理値配列
        // IA** 整数配列
        // LL** IN/OUTで使用の文字列定数の長さ
        // LB** IN/OUTで使用の文字列定数の内容位置
        // TL** 式展開時の一時的な文字列変数の長さ
        // TB** 式展開時の一時的な文字列変数の内容位置
        && (program_name.len() < 3 || !{
            let (head, tail) = program_name.split_at(2);
            matches!(head, "SL"|"SB"|"BA"|"IA"|"LL"|"LB"|"TL"|"TB")
            && tail.chars().all(|ch| ch.is_ascii_digit())
        })
    }

    // コンパイラの初期化
    fn new(program_name: &str) -> Result<Self, CompileError> {
        if !Self::is_valid_program_name(program_name) {
            return Err(format!("invalid Program Name: {}", program_name));
        }

        Ok(Self {
            var_id: 0,
            lit_id: 0,
            local_var_id: 0,
            temp_int_var_id: 0,
            temp_str_var_id: 0,
            jump_id: 0,
            bool_var_labels: HashMap::new(),
            int_var_labels: HashMap::new(),
            str_var_labels: HashMap::new(),
            bool_arr_labels: HashMap::new(),
            int_arr_labels: HashMap::new(),
            lit_str_labels: HashMap::new(),
            temp_int_var_labels: Vec::new(),
            temp_str_var_labels: Vec::new(),
            subroutine_codes: HashMap::new(),
            loop_labels: HashMap::new(),
            exit_labels: HashMap::new(),
            registers_used: 0,
            used_registers_order: {
                use casl2::Register::*;
                vec![Gr7, Gr6, Gr5, Gr4, Gr3, Gr2, Gr1]
            },
            stacked_registers: Vec::new(),
            statements: vec![casl2::Statement::labeled(
                program_name,
                casl2::Command::Start { entry_point: None },
            )],
            has_eof: false,
            var_total_size: 0,
        })
    }

    // 組み込みサブルーチンのローカル変数の新規ラベル生成
    fn get_new_local_var_label(&mut self) -> String {
        self.local_var_id += 1;
        format!("V{}", self.local_var_id)
    }

    // ループや条件分岐に使うジャンプ先ラベル生成
    fn get_new_jump_label(&mut self) -> String {
        self.jump_id += 1;
        format!("J{}", self.jump_id)
    }

    // Continueなど繰り返しの先頭で使うラベルの取得、無い場合は新規登録
    fn get_loop_label(&mut self, exit_id: usize) -> String {
        if let Some(label) = self.loop_labels.get(&exit_id) {
            label.clone()
        } else {
            let label = self.get_new_jump_label();
            self.loop_labels.insert(exit_id, label.clone());
            label
        }
    }

    // Exitなどブロック脱出で使うラベルの取得、無い場合は新規登録
    fn get_exit_label(&mut self, exit_id: usize) -> String {
        if let Some(label) = self.exit_labels.get(&exit_id) {
            label.clone()
        } else {
            let label = self.get_new_jump_label();
            self.exit_labels.insert(exit_id, label.clone());
            label
        }
    }

    // 式展開時の一時変数(整数/真理値)のラベル取得・生成
    fn get_temp_int_var_label(&mut self) -> String {
        if let Some(label) = self.temp_int_var_labels.pop() {
            return label;
        }
        self.temp_int_var_id += 1;
        format!("T{}", self.temp_int_var_id)
    }

    // 式展開時の一時変数(整数/真理値)のラベルの返却
    fn return_temp_int_var_label(&mut self, label: String) {
        self.temp_int_var_labels.push(label);
    }

    // 式展開時の一時変数(文字列)のラベル取得・生成
    fn get_temp_str_var_label(&mut self) -> StrLabels {
        if let Some(labels) = self.temp_str_var_labels.pop() {
            return labels;
        }
        self.temp_str_var_id += 1;
        StrLabels {
            len: format!("TL{}", self.temp_str_var_id),
            pos: format!("TB{}", self.temp_str_var_id),
            label_type: StrLabelType::Temp,
        }
    }

    // 式展開時の一時変数(文字列)のラベルの返却
    fn return_temp_str_var_label(&mut self, labels: StrLabels) {
        if matches!(labels.label_type, StrLabelType::Temp) {
            self.temp_str_var_labels.push(labels);
        }
    }

    // IN/OUTで使用する文字列定数のラベル生成
    fn get_lit_str_labels(&mut self, literal: &str) -> StrLabels {
        if let Some(labels) = self.lit_str_labels.get(literal) {
            return labels.clone();
        }
        self.lit_id += 1;
        let labels = StrLabels {
            len: format!("LL{}", self.lit_id),
            pos: format!("LB{}", self.lit_id),
            label_type: StrLabelType::Const,
        };
        self.lit_str_labels.insert(literal.into(), labels.clone());
        labels
    }

    // 文字列リテラル取得、もしIN/OUTで使用する文字列定数のラベルがあればそれを返す
    fn get_lit_str_label_if_exists(&mut self, literal: &str) -> StrLabels {
        if let Some(labels) = self.lit_str_labels.get(literal) {
            return labels.clone();
        }
        if literal.is_empty() {
            StrLabels {
                len: "=0".to_string(),
                pos: "=0".to_string(),
                label_type: StrLabelType::Lit(literal.to_string()),
            }
        } else {
            StrLabels {
                len: format!("={}", literal.chars().count()),
                pos: format!("='{}'", literal.replace('\'', "''")),
                label_type: StrLabelType::Lit(literal.to_string()),
            }
        }
    }

    // サブルーチンのソースコードを登録する
    fn load_subroutine(&mut self, req_id: subroutine::Id) -> String {
        let mut loading_ids = vec![req_id];
        while let Some(id) = loading_ids.pop() {
            if self.subroutine_codes.contains_key(&id) {
                continue;
            }
            let subroutine::Src {
                mut dependencies,
                statements,
            } = subroutine::get_src(self, id);
            loading_ids.append(&mut dependencies);
            self.subroutine_codes.insert(id, statements);
        }
        req_id.label()
    }

    // レジスタのアイドル状態を取得
    fn is_idle_register(&self, reg: casl2::Register) -> bool {
        (self.registers_used & (1 << reg as isize)) == 0
    }

    // アイドル中のレジスタを取得
    fn get_idle_register(&mut self) -> casl2::Register {
        let len = self.used_registers_order.len();
        let reg = self.used_registers_order[len - 7];
        if !self.is_idle_register(reg) {
            self.stacked_registers.push(reg);
            self.code(casl2::Command::P {
                code: casl2::P::Push,
                adr: casl2::Adr::Dec(0),
                x: Some(TryFrom::try_from(reg).expect("BUG")),
            });
        }
        self.registers_used |= 1 << reg as isize;
        self.used_registers_order.push(reg);
        reg
    }

    // アイドル化している場合にコールスタックに積まれてる値を戻す
    fn restore_register(&mut self, reg: casl2::Register) {
        if !self.is_idle_register(reg) {
            return;
        }
        self.registers_used |= 1 << reg as isize;
        self.code(casl2::Command::Pop { r: reg });
        let poped = self.stacked_registers.pop();
        assert_eq!(poped, Some(reg));
    }

    // アイドル中のレスジスタを使用中に変更
    fn set_register_used(&mut self, reg: casl2::Register) {
        assert!(self.is_idle_register(reg));
        self.registers_used |= 1 << reg as isize;
        self.used_registers_order.push(reg);
    }

    // 使用中のレジスタをアイドル扱いにする
    fn set_register_idle(&mut self, reg: casl2::Register) {
        assert!(!self.is_idle_register(reg));
        self.registers_used ^= 1 << reg as isize;
        let poped = self.used_registers_order.pop();
        assert_eq!(poped, Some(reg));
    }

    // subrutine引数のレジスタなどの一時利用のとき
    // 一時退避するためのソースコードと復帰するためのソースコードを生成
    fn get_save_registers_src(&mut self, regs: &[casl2::Register]) -> (String, String) {
        let mut saves = String::new();
        let mut recovers = String::new();

        for reg in regs.iter() {
            if !self.is_idle_register(*reg) {
                writeln!(&mut saves, " PUSH 0,{}", reg).unwrap();
                writeln!(&mut recovers, " POP {}", reg).unwrap();
            }
        }

        (saves, recovers)
    }

    // コンパイル最終工程
    fn finish(mut self) -> Vec<casl2::Statement> {
        if self.var_total_size > 1 || (self.var_total_size == 1 && self.has_eof) {
            self.load_subroutine(subroutine::Id::UtilFill);
        }
        let Self {
            bool_var_labels,
            int_var_labels,
            str_var_labels,
            bool_arr_labels,
            int_arr_labels,
            lit_str_labels,
            temp_int_var_labels,
            temp_str_var_labels,
            subroutine_codes,
            mut statements,
            has_eof,
            mut var_total_size,
            ..
        } = self;

        // RET ステートメント
        statements.code(casl2::Command::Ret);

        let mut first_var_label: Option<String> = None;

        // 真理値変数 B**
        for (label, var_name) in bool_var_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            if first_var_label.is_none() {
                first_var_label = Some(label.clone());
            }
            statements.comment(format!("Dim {} As Boolean", var_name));
            statements.labeled(label, casl2::Command::Ds { size: 1 });
        }

        // 整数変数 I**
        for (label, var_name) in int_var_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            if first_var_label.is_none() {
                first_var_label = Some(label.clone());
            }
            statements.comment(format!("Dim {} As Integer", var_name));
            statements.labeled(label, casl2::Command::Ds { size: 1 });
        }

        // 文字列変数 SL** SB**
        for (labels, var_name) in str_var_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            if first_var_label.is_none() {
                first_var_label = Some(labels.len.clone());
            }
            let StrLabels { pos, len, .. } = labels;
            statements.comment(format!("Dim {} As String", var_name));
            statements.labeled(len, casl2::Command::Ds { size: 1 });
            statements.labeled(pos, casl2::Command::Ds { size: 256 });
        }

        // 真理値配列(固定長) BA**
        for ((label, size), var_name) in bool_arr_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            if first_var_label.is_none() {
                first_var_label = Some(label.clone());
            }
            statements.comment(format!("Dim {}({}) As Boolean", var_name, size - 1));
            statements.labeled(label, casl2::Command::Ds { size: size as u16 });
        }

        // 整数配列(固定長) IA**
        for ((label, size), var_name) in int_arr_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            if first_var_label.is_none() {
                first_var_label = Some(label.clone());
            }
            statements.comment(format!("Dim {}({}) As Integer", var_name, size - 1));
            statements.labeled(label, casl2::Command::Ds { size: size as u16 });
        }

        // EOFを扱う場合
        if has_eof {
            var_total_size += 1;
            if first_var_label.is_none() {
                first_var_label = Some("EOF".to_string());
            }
            statements.labeled("EOF", casl2::Command::Ds { size: 1 });
        }

        match first_var_label {
            Some(label) if var_total_size == 1 => {
                let mut src = casl2::parse(&format!(
                    r#" XOR   GR0,GR0
                        ST    GR0,{label}"#,
                    label = label
                ))
                .unwrap();
                src.reverse();
                for stmt in src {
                    statements.insert(1, stmt);
                }
            }
            Some(label) => {
                let mut src = casl2::parse(&format!(
                    r#" LAD   GR1,{start}
                        XOR   GR2,GR2
                        LAD   GR3,{size}
                        CALL  {fill}"#,
                    start = label,
                    size = var_total_size,
                    fill = subroutine::Id::UtilFill.label()
                ))
                .unwrap();
                src.reverse();
                for stmt in src {
                    statements.insert(1, stmt);
                }
            }
            _ => {}
        }

        // 式展開等で使う一時変数(整数/真理値で共有) T**
        for label in temp_int_var_labels.into_iter().collect::<BTreeSet<_>>() {
            statements.labeled(label, casl2::Command::Ds { size: 1 });
        }

        // 式展開等で使う一時変数(文字列) TL** TB**
        for labels in temp_str_var_labels.into_iter().collect::<BTreeSet<_>>() {
            let StrLabels { pos, len, .. } = labels;
            statements.labeled(len, casl2::Command::Ds { size: 1 });
            statements.labeled(pos, casl2::Command::Ds { size: 256 });
        }

        // IN/OUTで使用する文字列定数 LL** LB**
        for (labels, literal) in lit_str_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            let StrLabels { pos, len, .. } = labels;
            statements.labeled(
                len,
                casl2::Command::Dc {
                    constants: vec![casl2::Constant::Dec(literal.chars().count() as i16)],
                },
            );
            if literal.is_empty() {
                statements.labeled(pos, casl2::Command::Ds { size: 0 });
            } else {
                statements.labeled(
                    pos,
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str(literal.clone())],
                    },
                );
            }
        }

        // 組み込みサブルーチンのコード
        for (_, code) in subroutine_codes.into_iter().collect::<BTreeMap<_, _>>() {
            statements.code(code);
        }

        // END ステートメント
        statements.code(casl2::Command::End);

        statements
    }

    // ステートメントをコンパイルする
    fn compile(&mut self, stmt: &parser::Statement) {
        use parser::Statement::*;
        match stmt {
            End => self.compile_end(),
            AssignAddInto { var_name, value } => self.compile_assign_add_into(var_name, value),
            AssignAddIntoElement {
                var_name,
                index,
                value,
            } => self.compile_assign_add_into_element(var_name, index, value),
            AssignBoolean { var_name, value } => self.compile_assign_boolean(var_name, value),
            AssignBooleanElement {
                var_name,
                index,
                value,
            } => self.compile_assign_boolean_element(var_name, index, value),
            AssignIntegerElement {
                var_name,
                index,
                value,
            } => self.compile_assign_integer_element(var_name, index, value),
            AssignCharacterElement {
                var_name,
                index,
                value,
            } => self.compile_assign_character_element(var_name, index, value),
            AssignInteger { var_name, value } => self.compile_assign_integer(var_name, value),
            AssignString { var_name, value } => self.compile_assign_string(var_name, value),
            AssignSubInto { var_name, value } => self.compile_assign_sub_into(var_name, value),
            AssignSubIntoElement {
                var_name,
                index,
                value,
            } => self.compile_assign_sub_into_element(var_name, index, value),
            ContinueDo { exit_id } => self.compile_continue_loop(*exit_id, "Do"),
            ContinueFor { exit_id } => self.compile_continue_loop(*exit_id, "For"),
            Dim { var_name, var_type } => self.compile_dim(var_name, var_type),
            Mid {
                var_name,
                offset,
                length,
                value,
            } => self.compile_mid(var_name, offset, length, value),
            DoLoop { exit_id, block } => self.compile_do_loop(*exit_id, block),
            DoLoopUntil {
                exit_id,
                condition,
                block,
            } => self.compile_do_loop_until(*exit_id, condition, block),
            DoLoopWhile {
                exit_id,
                condition,
                block,
            } => self.compile_do_loop_while(*exit_id, condition, block),
            DoUntilLoop {
                exit_id,
                condition,
                block,
            } => self.compile_do_until_loop(*exit_id, condition, block),
            DoWhileLoop {
                exit_id,
                condition,
                block,
            } => self.compile_do_while_loop(*exit_id, condition, block),
            ExitDo { exit_id } => self.compile_exit_block(*exit_id, "Do"),
            ExitFor { exit_id } => self.compile_exit_block(*exit_id, "For"),
            ExitSelect { exit_id } => self.compile_exit_block(*exit_id, "Select"),
            For { step: None, .. } => self.compile_for_with_literal_step(stmt, 1),
            For {
                step: Some(parser::Expr::LitInteger(step)),
                ..
            } => self.compile_for_with_literal_step(stmt, *step),
            For { .. } => self.compile_for(stmt),
            If {
                condition,
                block,
                else_blocks,
            } => self.compile_if(condition, block, else_blocks),
            SelectInteger {
                exit_id,
                value,
                case_blocks,
            } => self.compile_select_integer(*exit_id, value, case_blocks),
            SelectString {
                exit_id,
                value,
                case_blocks,
            } => self.compile_select_string(*exit_id, value, case_blocks),
            InputElementInteger { var_name, index } => {
                self.compile_input_element_integer(var_name, index)
            }
            InputInteger { var_name } => self.compile_input_integer(var_name),
            InputString { var_name } => self.compile_input_string(var_name),
            PrintLitBoolean { value } => self.compile_print_lit_boolean(*value),
            PrintLitInteger { value } => self.compile_print_lit_integer(*value),
            PrintLitString { value } => self.compile_print_lit_string(value),
            PrintVarString { var_name } => self.compile_print_var_string(var_name),
            PrintExprBoolan { value } => self.compile_print_expr_boolean(value),
            PrintExprInteger { value } => self.compile_print_expr_integer(value),
            PrintExprString { value } => self.compile_print_expr_string(value),

            // IfやSelectの内側で処理する
            ElseIf { .. }
            | Else { .. }
            | CaseInteger { .. }
            | CaseString { .. }
            | CaseElse { .. } => unreachable!("BUG"),

            // Provisionals unreachable
            ProvisionalDo { .. }
            | ProvisionalFor { .. }
            | ProvitionalIf { .. }
            | ProvisionalElseIf { .. }
            | ProvisionalElse
            | ProvisionalSelectInteger { .. }
            | ProvisionalCaseInteger { .. }
            | ProvisionalSelectString { .. }
            | ProvisionalCaseString { .. }
            | ProvisionalCaseElse => unreachable!("BUG"),
        }
    }

    // End ステートメント
    fn compile_end(&mut self) {
        assert!(self.stacked_registers.is_empty());
        // たぶん、コールスタックは初期状態のはず…
        //  (ブロック(If/For/Do/Select)でPUSH/POP汚染はやってないはず…)
        // RET でプログラム終了できるはずだが
        self.comment("End");
        self.code(casl2::Command::Ret);
    }

    // Midステートメント
    // Mid(<var_str>,<offset>) = <str_expr>
    // Mid(<var_str>,<offset>,<length>) = <str_expr>
    fn compile_mid(
        &mut self,
        var_name: &str,
        offset: &parser::Expr,
        length: &Option<parser::Expr>,
        value: &parser::Expr,
    ) {
        assert!(matches!(offset.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::String));

        let partialcopy = self.load_subroutine(subroutine::Id::UtilCopyToOffsetStr);

        if let Some(length) = length {
            assert!(matches!(length.return_type(), parser::ExprType::Integer));
            self.comment(format!(
                "Mid( {name}, {offset}, {length} ) = {value}",
                name = var_name,
                offset = offset,
                length = length,
                value = value
            ));
            let var_labels = self.str_var_labels.get(var_name).cloned().expect("BUG");
            let value_labels = self.compile_str_expr(value);
            let offset_reg = self.compile_int_expr(offset);
            let length_reg = self.compile_int_expr(length);
            self.restore_register(offset_reg);

            let (saves, recovers) = {
                use casl2::Register::*;
                let mut regs = vec![Gr1, Gr2, Gr3, Gr4, Gr5];
                if matches!(offset_reg, Gr1) {
                    regs.retain(|r| !matches!(r, Gr1));
                }
                if !matches!(length_reg, Gr6) {
                    regs.push(Gr6);
                }
                self.get_save_registers_src(&regs)
            };

            let (length_line1, length_line2) = if matches!(length_reg, casl2::Register::Gr1) {
                if matches!(offset_reg, casl2::Register::Gr6) {
                    (" LD GR0,GR1".to_string(), " LD GR6,GR0".to_string())
                } else {
                    (" LD GR6,GR1".to_string(), "".to_string())
                }
            } else {
                (
                    "".to_string(),
                    if matches!(length_reg, casl2::Register::Gr6) {
                        "".to_string()
                    } else {
                        format!(" LD GR6,{length}", length = length_reg)
                    },
                )
            };

            let offset_line = if matches!(offset_reg, casl2::Register::Gr1) {
                "".to_string()
            } else {
                format!(" LD GR1,{offset}", offset = offset_reg)
            };

            self.code(saves);
            self.code(format!(
                r#" {length_line1}
                    {offset_line}
                    {length_line2}
                    LAD   GR5,{dstpos}
                    LD    GR2,{dstlen}
                    LAD   GR3,{srcpos}
                    LD    GR4,{srclen}
                    CALL  {copy}"#,
                length_line1 = length_line1,
                offset_line = offset_line,
                length_line2 = length_line2,
                dstpos = var_labels.pos,
                dstlen = var_labels.len,
                srcpos = value_labels.pos,
                srclen = value_labels.len,
                copy = partialcopy
            ));
            self.code(recovers);

            self.set_register_idle(length_reg);
            self.set_register_idle(offset_reg);
            self.return_temp_str_var_label(value_labels);
        } else {
            self.comment(format!(
                "Mid( {name}, {offset} ) = {value}",
                name = var_name,
                offset = offset,
                value = value
            ));
            let var_labels = self.str_var_labels.get(var_name).cloned().expect("BUG");
            let value_labels = self.compile_str_expr(value);
            let offset_reg = self.compile_int_expr(offset);

            let (saves, recovers) = {
                use casl2::Register::*;
                let mut regs = vec![Gr1, Gr2, Gr3, Gr4, Gr5, Gr6];
                if matches!(offset_reg, Gr1) {
                    regs.retain(|r| !matches!(r, Gr1));
                }
                self.get_save_registers_src(&regs)
            };

            let offset_line = if matches!(offset_reg, casl2::Register::Gr1) {
                "".to_string()
            } else {
                format!(" LD GR1,{offset}", offset = offset_reg)
            };

            self.code(saves);
            self.code(format!(
                r#" {offset_line}
                    LAD   GR5,{dstpos}
                    LD    GR2,{dstlen}
                    LAD   GR3,{srcpos}
                    LD    GR4,{srclen}
                    LD    GR6,GR2
                    CALL  {copy}"#,
                offset_line = offset_line,
                dstpos = var_labels.pos,
                dstlen = var_labels.len,
                srcpos = value_labels.pos,
                srclen = value_labels.len,
                copy = partialcopy
            ));
            self.code(recovers);

            self.set_register_idle(offset_reg);
            self.return_temp_str_var_label(value_labels);
        }
    }

    // Select Case <string> ステートメント
    fn compile_select_string(
        &mut self,
        exit_id: usize,
        value: &parser::Expr,
        case_blocks: &[parser::Statement],
    ) {
        assert!(matches!(value.return_type(), parser::ExprType::String));

        self.comment(format!("Select Case {value}", value = value));

        let exit_label = self.get_exit_label(exit_id);

        let label_and_blocks: Vec<_> = case_blocks
            .iter()
            .map(|b| (self.get_new_jump_label(), b))
            .collect();

        let value_labels = self.compile_str_expr(value);

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };

        self.code(saves);
        self.code(format!(
            r#" LAD  GR1,{pos}
                LD   GR2,{len}"#,
            pos = value_labels.pos,
            len = value_labels.len
        ));

        self.return_temp_str_var_label(value_labels);

        let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);

        for (label, case_stmt) in label_and_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseString { values, .. } => {
                    for lit in values.iter() {
                        let lit_labels = self.get_lit_str_label_if_exists(lit);
                        self.code(format!(
                            r#" LAD   GR3,{pos}
                                LAD   GR4,{len}
                                CALL  {cmp}
                                AND   GR0,GR0
                                JZE   {label}"#,
                            pos = lit_labels.pos,
                            len = lit.chars().count(),
                            cmp = cmpstr,
                            label = label
                        ));
                    }
                }
                parser::Statement::CaseElse { .. } => {
                    self.code(&recovers);
                    self.code(format!(" JUMP {label}", label = label));
                }
                _ => unreachable!("BUG"),
            }
        }

        if !matches!(case_blocks.last(), Some(parser::Statement::CaseElse { .. })) {
            self.code(&recovers);
            self.code(format!(" JUMP {exit}", exit = exit_label));
        }

        for (label, case_stmt) in label_and_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseString { values, block } => {
                    self.comment(format!(
                        "Case {}",
                        values
                            .iter()
                            .map(|s| format!("'{}'", s.replace('\'', "''")))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                    self.labeled(label, casl2::Command::Nop);
                    self.code(&recovers);
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                }
                parser::Statement::CaseElse { block } => {
                    self.comment("Case Else");
                    self.labeled(label, casl2::Command::Nop);
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                }
                _ => unreachable!("BUG"),
            }
            self.code(format!(" JUMP {exit}", exit = exit_label));
        }

        assert_eq!(
            self.statements.pop(),
            Some(casl2::Statement::code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            }))
        );

        self.comment("End Select");
        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Do While <condition>
    // Loop
    // ステートメント
    fn compile_do_while_loop(
        &mut self,
        exit_id: usize,
        condition: &parser::Expr,
        block: &[parser::Statement],
    ) {
        assert!(matches!(condition.return_type(), parser::ExprType::Boolean));

        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.comment(format!("Do While {cond}", cond = condition));
        self.labeled(&loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.code(format!(
            r#" AND {reg},{reg}
                JZE {exit}"#,
            reg = cond_reg,
            exit = exit_label
        ));

        self.set_register_idle(cond_reg);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment("Loop");
        self.code(format!(" JUMP {next}", next = loop_label));
        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Do Until <condition>
    // Loop
    // ステートメント
    fn compile_do_until_loop(
        &mut self,
        exit_id: usize,
        condition: &parser::Expr,
        block: &[parser::Statement],
    ) {
        assert!(matches!(condition.return_type(), parser::ExprType::Boolean));

        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.comment(format!("Do Until {cond}", cond = condition));
        self.labeled(&loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.code(format!(
            r#" AND {reg},{reg}
                JNZ {exit}"#,
            reg = cond_reg,
            exit = exit_label
        ));

        self.set_register_idle(cond_reg);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment("Loop");
        self.code(format!(" JUMP {next}", next = loop_label));
        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Do
    // Loop While <condition>
    // ステートメント
    fn compile_do_loop_while(
        &mut self,
        exit_id: usize,
        condition: &parser::Expr,
        block: &[parser::Statement],
    ) {
        assert!(matches!(condition.return_type(), parser::ExprType::Boolean));

        self.comment("Do");

        let top_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.labeled(&top_label, casl2::Command::Nop);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment(format!("Loop While {cond}", cond = condition));
        self.labeled(loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.code(format!(
            r#" AND {reg},{reg}
                JNZ {next}"#,
            reg = cond_reg,
            next = top_label
        ));

        self.set_register_idle(cond_reg);

        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Do
    // Loop Until <condition>
    // ステートメント
    fn compile_do_loop_until(
        &mut self,
        exit_id: usize,
        condition: &parser::Expr,
        block: &[parser::Statement],
    ) {
        assert!(matches!(condition.return_type(), parser::ExprType::Boolean));

        self.comment("Do");

        let top_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.labeled(&top_label, casl2::Command::Nop);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment(format!("Loop Until {cond}", cond = condition));
        self.labeled(loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.code(format!(
            r#" AND {reg},{reg}
                JZE {next}"#,
            reg = cond_reg,
            next = top_label
        ));

        self.set_register_idle(cond_reg);

        self.labeled(exit_label, casl2::Command::Nop);
    }

    // If ステートメント
    fn compile_if(
        &mut self,
        condition: &parser::Expr,
        block: &[parser::Statement],
        else_blocks: &[parser::Statement],
    ) {
        self.comment(format!("If {} Then", condition));

        let end_label = self.get_new_jump_label();

        let labels: Vec<_> = (0..else_blocks.len())
            .map(|_| self.get_new_jump_label())
            .chain(vec![end_label.clone()])
            .collect();

        let condition_reg = self.compile_int_expr(condition);

        self.set_register_idle(condition_reg);

        self.code(format!(
            r#" AND {reg},{reg}
                JZE {next}"#,
            reg = condition_reg,
            next = labels.first().expect("BUG")
        ));

        for stmt in block.iter() {
            self.compile(stmt);
        }

        let label_iter = labels.iter().zip(labels.iter().skip(1));

        for (else_stmt, (head, next)) in else_blocks.iter().zip(label_iter) {
            self.code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&end_label),
                x: None,
            });

            match else_stmt {
                parser::Statement::ElseIf { condition, block } => {
                    self.comment(format!("ElseIf {} Then", condition));
                    self.labeled(head, casl2::Command::Nop);

                    let condition_reg = self.compile_int_expr(condition);

                    self.set_register_idle(condition_reg);

                    self.code(format!(
                        r#" AND {reg},{reg}
                            JZE {next}"#,
                        reg = condition_reg,
                        next = next
                    ));

                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                }
                parser::Statement::Else { block } => {
                    self.comment("Else");
                    self.labeled(head, casl2::Command::Nop);

                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                }
                _ => unreachable!("BUG"),
            }
        }

        self.comment("End If");
        self.labeled(end_label, casl2::Command::Nop);
    }

    // Continue {Do/For}
    fn compile_continue_loop(&mut self, exit_id: usize, keyword: &str) {
        let loop_label = self.get_loop_label(exit_id);
        self.comment(format!("Continue {}", keyword));
        // JUMP {loop}
        self.code(casl2::Command::P {
            code: casl2::P::Jump,
            adr: casl2::Adr::label(&loop_label),
            x: None,
        });
    }

    // Exit {Do/For/Select}
    fn compile_exit_block(&mut self, exit_id: usize, keyword: &str) {
        let exit_label = self.get_exit_label(exit_id);
        self.comment(format!("Exit {}", keyword));
        // JUMP {exit}
        self.code(casl2::Command::P {
            code: casl2::P::Jump,
            adr: casl2::Adr::label(&exit_label),
            x: None,
        });
    }

    // Assign Sub Into Element ステートメント
    // int_arr(index) -= int_expr
    fn compile_assign_sub_into_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!(
            "{var}( {index} ) -= {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.int_arr_labels.get(var_name).cloned().expect("BUG");

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD   GR1,{index}
                LAD  GR2,{size}
                CALL {fit}"#,
            index = index_reg,
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        let reg = self.get_idle_register();

        self.code(format!(
            r#" LD    {reg},{arr},{index}
                SUBA  {reg},{value}
                ST    {reg},{arr},{index}"#,
            reg = reg,
            value = value_reg,
            arr = arr_label,
            index = index_reg
        ));

        self.set_register_idle(reg);
        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Sub Into ステートメント
    // int_var -= int_expr
    fn compile_assign_sub_into(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!("{var} -= {value}", var = var_name, value = value));

        let var_label = self.int_var_labels.get(var_name).cloned().expect("BUG");

        let value_reg = self.compile_int_expr(value);

        let reg = self.get_idle_register();

        self.code(format!(
            r#" LD    {reg},{var}
                SUBA  {reg},{value}
                ST    {reg},{var}"#,
            reg = reg,
            var = var_label,
            value = value_reg
        ));

        self.set_register_idle(reg);
        self.set_register_idle(value_reg);
    }

    // Assign Add Into Element ステートメント
    // int_arr(index) += int_expr
    fn compile_assign_add_into_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!(
            "{var}( {index} ) += {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.int_arr_labels.get(var_name).cloned().expect("BUG");

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD   GR1,{index}
                LAD  GR2,{size}
                CALL {fit}"#,
            index = index_reg,
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ADDA  {value},{arr},{index}
                ST    {value},{arr},{index}"#,
            value = value_reg,
            arr = arr_label,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Add Into ステートメント
    // int_var += int_expr
    fn compile_assign_add_into(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!("{var} += {value}", var = var_name, value = value));

        let value_reg = self.compile_int_expr(value);

        let var_label = self.int_var_labels.get(var_name).cloned().expect("BUG");

        self.code(format!(
            r#" ADDA  {reg},{var}
                ST    {reg},{var}"#,
            reg = value_reg,
            var = var_label
        ));

        self.set_register_idle(value_reg);
    }

    // Assign Boolean Element ステートメント
    // bool_arr(index) = bool_expr
    fn compile_assign_boolean_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let (arr_label, arr_size) = self.bool_arr_labels.get(var_name).cloned().expect("BUG");

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ST {value},{arr},{index}"#,
            value = value_reg,
            arr = arr_label,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Integer Element ステートメント
    // int_arr(index) = int_expr
    fn compile_assign_integer_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let (arr_label, arr_size) = self.int_arr_labels.get(var_name).cloned().expect("BUG");

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ST {value},{arr},{index}"#,
            value = value_reg,
            arr = arr_label,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Character Element ステートメント
    // str_var(index) = int_expr
    fn compile_assign_character_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let str_labels = self.str_var_labels.get(var_name).cloned().expect("BUG");

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                LD    GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = str_labels.len,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        // 仮に文字列長が0の文字列変数であったとしても
        // index=0は文字列変数のバッファ予約領域なので書き込んでも無問題

        self.code(format!(
            r#" ST {value},{arr},{index}"#,
            value = value_reg,
            arr = str_labels.pos,
            index = casl2::IndexRegister::try_from(index_reg).expect("BUG")
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Boolean ステートメント
    // bool_var = bool_expr
    fn compile_assign_boolean(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.comment(format!("{var} = {value}", var = var_name, value = value));

        let reg = self.compile_int_expr(value);
        let var_label = self.bool_var_labels.get(var_name).expect("BUG");
        let adr = casl2::Adr::label(var_label);

        // ST {reg},{var}
        self.code(casl2::Command::A {
            code: casl2::A::St,
            r: reg,
            adr,
            x: None,
        });

        self.set_register_idle(reg);
    }

    // Assign String ステートメント
    // str_var = str_expr
    fn compile_assign_string(&mut self, var_name: &str, value: &parser::Expr) {
        self.comment(format!("{var} = {value}", var = var_name, value = value));

        let value_label = self.compile_str_expr(value);
        let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
        let var_label = self.str_var_labels.get(var_name).expect("BUG");

        let src = if let StrLabels {
            pos,
            label_type: StrLabelType::Lit(s),
            ..
        } = &value_label
        {
            format!(
                r#" LAD   GR1,{dstpos}
                    LAD   GR2,{dstlen}
                    LAD   GR3,{srcpos}
                    LAD   GR4,{srclen}
                    CALL  {copystr}"#,
                dstpos = var_label.pos,
                dstlen = var_label.len,
                srcpos = pos,
                srclen = s.chars().count(),
                copystr = copystr
            )
        } else {
            format!(
                r#" LAD   GR1,{dstpos}
                    LAD   GR2,{dstlen}
                    LAD   GR3,{srcpos}
                    LD    GR4,{srclen}
                    CALL  {copystr}"#,
                dstpos = var_label.pos,
                dstlen = var_label.len,
                srcpos = value_label.pos,
                srclen = value_label.len,
                copystr = copystr
            )
        };

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };
        self.code(saves);
        self.code(src);
        self.code(recovers);

        self.return_temp_str_var_label(value_label);
    }

    // Assign Integer ステートメント
    // int_var = int_expr
    fn compile_assign_integer(&mut self, var_name: &str, value: &parser::Expr) {
        self.comment(format!("{var} = {value}", var = var_name, value = value));
        let reg = self.compile_int_expr(value);
        let var_label = self.int_var_labels.get(var_name).expect("BUG");
        let adr = casl2::Adr::label(var_label);

        // ST {reg},{var}
        self.code(casl2::Command::A {
            code: casl2::A::St,
            r: reg,
            adr,
            x: None,
        });

        self.set_register_idle(reg);
    }

    // For ステートメント (Stepが定数)
    fn compile_for_with_literal_step(&mut self, for_stmt: &parser::Statement, step: i32) {
        let (exit_id, counter, init, end, block) = if let parser::Statement::For {
            exit_id,
            counter,
            init,
            end,
            step: _,
            block,
        } = for_stmt
        {
            (*exit_id, counter, init, end, block)
        } else {
            unreachable!("BUG");
        };

        self.comment(format!(
            "For {counter} = {init} To {end} Step {step}",
            counter = counter,
            init = init,
            end = end,
            step = step
        ));

        // calc {end}
        let end_var = if let parser::Expr::LitInteger(_) = end {
            None
        } else {
            let end_var = self.get_temp_int_var_label();
            // 想定では GR7
            let end_reg = self.compile_int_expr(end);
            self.code(casl2::Command::A {
                code: casl2::A::St,
                r: end_reg,
                adr: casl2::Adr::label(&end_var),
                x: None,
            });
            self.set_register_idle(end_reg); // GR7 解放のはず
            Some(end_var)
        };

        // カウンタの準備
        let counter_var = self.int_var_labels.get(counter).expect("BUG").clone();

        // calc {init} and assign to {counter}
        // 想定では GR7
        let init_reg = self.compile_int_expr(init);
        self.code(casl2::Command::A {
            code: casl2::A::St,
            r: init_reg,
            adr: casl2::Adr::label(&counter_var),
            x: None,
        });
        self.set_register_idle(init_reg); // GR7 解放のはず

        // ラベルの準備
        let condition_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        // ループ継続の判定部分

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::Gr1));

        self.code(format!("{cond} NOP", cond = condition_label));
        self.code(saves);
        if let Some(end_var) = end_var.as_ref() {
            self.code(format!(
                r#" LD    GR1,{counter}
                    CPA   GR1,{end}"#,
                counter = counter_var,
                end = end_var
            ));
        } else if let parser::Expr::LitInteger(end) = end {
            self.code(format!(
                r#" LD    GR1,{counter}
                    CPA   GR1,={end}"#,
                counter = counter_var,
                end = *end as i16
            ));
        } else {
            unreachable!("BUG");
        }
        self.code(recovers);

        if step < 0 {
            self.code(format!(" JMI {exit}", exit = exit_label));
        } else {
            self.code(format!(" JPL {exit}", exit = exit_label));
        }

        // ループ内のコードを実行
        for stmt in block.iter() {
            self.compile(stmt);
        }

        // ループ末尾 (カウンタの更新など)

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::Gr1));

        self.comment(format!("Next {counter}", counter = counter));

        self.code(format!("{next} NOP", next = loop_label));
        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{counter}
                LAD   GR1,{step},GR1
                ST    GR1,{counter}"#,
            counter = counter_var,
            step = step
        ));
        self.code(recovers);
        self.code(format!(" JUMP {cond}", cond = condition_label));
        self.code(format!("{exit} NOP", exit = exit_label));

        if let Some(end_var) = end_var {
            self.return_temp_int_var_label(end_var);
        }
    }

    // For ステートメント
    fn compile_for(&mut self, for_stmt: &parser::Statement) {
        let (exit_id, counter, init, end, step, block) = if let parser::Statement::For {
            exit_id,
            counter,
            init,
            end,
            step: Some(step),
            block,
        } = for_stmt
        {
            (*exit_id, counter, init, end, step, block)
        } else {
            unreachable!("BUG");
        };

        self.comment(format!(
            "For {counter} = {init} To {end} Step {step}",
            counter = counter,
            init = init,
            end = end,
            step = step
        ));

        // calc {step}
        let step_var = self.get_temp_int_var_label();
        // 想定では GR7
        let step_reg = self.compile_int_expr(step);
        self.code(casl2::Command::A {
            code: casl2::A::St,
            r: step_reg,
            adr: casl2::Adr::label(&step_var),
            x: None,
        });
        self.set_register_idle(step_reg); // GR7 解放のはず

        // calc {end}
        let end_var = if let parser::Expr::LitInteger(_) = end {
            None
        } else {
            let end_var = self.get_temp_int_var_label();
            // 想定では GR7
            let end_reg = self.compile_int_expr(end);
            self.code(casl2::Command::A {
                code: casl2::A::St,
                r: end_reg,
                adr: casl2::Adr::label(&end_var),
                x: None,
            });
            self.set_register_idle(end_reg); // GR7 解放のはず
            Some(end_var)
        };

        // カウンタの準備
        let counter_var = self.int_var_labels.get(counter).expect("BUG").clone();

        // calc {init} and assign to {counter}
        // 想定では GR7
        let init_reg = self.compile_int_expr(init);
        self.code(casl2::Command::A {
            code: casl2::A::St,
            r: init_reg,
            adr: casl2::Adr::label(&counter_var),
            x: None,
        });
        self.set_register_idle(init_reg); // GR7 解放のはず

        // ラベルの準備
        let condition_label = self.get_new_jump_label();
        let negastep_label = self.get_new_jump_label();
        let blockhead_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        // ループ継続の判定部分

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::Gr1));

        self.code(format!("{cond} NOP", cond = condition_label));
        self.code(saves);
        if let Some(end_var) = end_var.as_ref() {
            self.code(format!(
                r#" LD    GR1,{step}
                JMI   {nega}
                LD    GR1,{counter}
                CPA   GR1,{end}
                JUMP  {block}
{nega}          LD    GR1,{end}
                CPA   GR1,{counter}
{block}         NOP"#,
                counter = counter_var,
                step = step_var,
                nega = negastep_label,
                end = end_var,
                block = blockhead_label
            ));
        } else if let parser::Expr::LitInteger(end) = end {
            self.code(format!(
                r#" LD    GR1,{step}
                JMI   {nega}
                LD    GR1,{counter}
                CPA   GR1,={end}
                JUMP  {block}
{nega}          LAD    GR1,{end}
                CPA   GR1,{counter}
{block}         NOP"#,
                counter = counter_var,
                step = step_var,
                nega = negastep_label,
                end = *end as i16,
                block = blockhead_label
            ));
        } else {
            unreachable!("BUG");
        }
        self.code(recovers);
        self.code(format!(" JPL {exit}", exit = exit_label));

        // ループ内のコードを実行
        for stmt in block.iter() {
            self.compile(stmt);
        }

        // ループ末尾 (カウンタの更新など)

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::Gr1));

        self.comment(format!("Next {counter}", counter = counter));
        self.code(format!("{next} NOP", next = loop_label));
        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{counter}
                ADDA  GR1,{step}
                ST    GR1,{counter}"#,
            counter = counter_var,
            step = step_var
        ));
        self.code(recovers);
        self.code(format!(" JUMP {cond}", cond = condition_label));
        self.code(format!("{exit} NOP", exit = exit_label));

        if let Some(end_var) = end_var {
            self.return_temp_int_var_label(end_var);
        }
        self.return_temp_int_var_label(step_var);
    }

    // Select Case <integer> ステートメント
    fn compile_select_integer(
        &mut self,
        exit_id: usize,
        value: &parser::Expr,
        case_blocks: &[parser::Statement],
    ) {
        assert!(
            {
                let mut iter = case_blocks.iter();
                iter.next_back();
                iter.all(|stmt| matches!(stmt, parser::Statement::CaseInteger { .. }))
            },
            "BUG"
        );

        let has_else = matches!(case_blocks.last(), Some(parser::Statement::CaseElse { .. }));

        let case_blocks: Vec<_> = case_blocks
            .iter()
            .map(|case_stmt| (case_stmt, self.get_new_jump_label()))
            .collect();

        self.comment(format!("Select Case {}", value));

        // 想定では GR7
        let value_reg = self.compile_int_expr(value);

        for (case_stmt, label) in case_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseInteger { values, .. } => {
                    for case_value in values.iter() {
                        use parser::CaseIntegerItem::*;
                        let adr = match case_value {
                            Integer(value) => casl2::Adr::LiteralDec(*value as i16),
                            Character(ch) => casl2::Adr::LiteralStr(ch.to_string()),
                        };
                        self.code(casl2::Command::A {
                            code: casl2::A::Cpa,
                            r: value_reg,
                            adr,
                            x: None,
                        });
                        self.code(casl2::Command::P {
                            code: casl2::P::Jze,
                            adr: casl2::Adr::label(label),
                            x: None,
                        });
                    }
                }
                parser::Statement::CaseElse { .. } => {
                    self.code(casl2::Command::P {
                        code: casl2::P::Jump,
                        adr: casl2::Adr::label(label),
                        x: None,
                    });
                }
                _ => unreachable!("BUG"),
            }
        }

        self.set_register_idle(value_reg); // GR7 解放のはず

        let exit_label = self.get_exit_label(exit_id);

        if !has_else {
            self.code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            });
        }

        for (case_stmt, label) in case_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseInteger { values, block } => {
                    self.comment(format!(
                        "Case {}",
                        values
                            .iter()
                            .map(|v| v.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                    self.labeled(label, casl2::Command::Nop);
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                }
                parser::Statement::CaseElse { block } => {
                    self.comment("Case Else");
                    self.labeled(label, casl2::Command::Nop);
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                }
                _ => unreachable!("BUG"),
            }
            self.code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            });
        }

        assert_eq!(
            self.statements.pop(),
            Some(casl2::Statement::code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            }))
        );
        self.comment("End Select");
        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Dim ステートメント
    fn compile_dim(&mut self, var_name: &str, var_type: &parser::VarType) {
        use parser::VarType;
        self.var_id += 1;
        match var_type {
            VarType::Boolean => {
                let label = format!("B{}", self.var_id);
                self.bool_var_labels.insert(var_name.into(), label);
                self.var_total_size += 1;
            }
            VarType::Integer => {
                let label = format!("I{}", self.var_id);
                self.int_var_labels.insert(var_name.into(), label);
                self.var_total_size += 1;
            }
            VarType::String => {
                let len_label = format!("SL{}", self.var_id);
                let pos_label = format!("SB{}", self.var_id);
                let labels = StrLabels {
                    len: len_label,
                    pos: pos_label,
                    label_type: StrLabelType::Var,
                };
                self.str_var_labels.insert(var_name.into(), labels);
                self.var_total_size += 257;
            }
            VarType::ArrayOfBoolean(size) => {
                let label = format!("BA{}", self.var_id);
                self.bool_arr_labels.insert(var_name.into(), (label, *size));
                self.var_total_size += size;
            }
            VarType::ArrayOfInteger(size) => {
                let label = format!("IA{}", self.var_id);
                self.int_arr_labels.insert(var_name.into(), (label, *size));
                self.var_total_size += size;
            }
        }
    }

    // Do ~ Loop ステートメント
    fn compile_do_loop(&mut self, exit_id: usize, block: &[parser::Statement]) {
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.comment("Do");
        self.labeled(&loop_label, casl2::Command::Nop);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment("Loop");
        self.code(casl2::Command::P {
            code: casl2::P::Jump,
            adr: casl2::Adr::label(&loop_label),
            x: None,
        });

        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Input ステートメント
    // 整数配列の要素へのコンソール入力
    fn compile_input_element_integer(&mut self, var_name: &str, index: &parser::Expr) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        self.comment(format!(
            "Input {arr}( {index} )",
            arr = var_name,
            index = index
        ));

        self.has_eof = true;

        // 想定では GR7
        let index_reg = self.compile_int_expr(index);

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
        let cint_label = self.load_subroutine(subroutine::Id::FuncCInt);
        let s_labels = self.get_temp_str_var_label();
        let (arr_label, arr_size) = self.int_arr_labels.get(var_name).cloned().expect("BUG");
        let label = self.get_new_jump_label();

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}
                LD    GR3,GR0
                IN    {pos},{len}
                LD    GR2,{len}
                JPL   {ok}
                JZE   {ok}
                ST    GR2,EOF
                XOR   GR2,GR2
{ok}            LAD   GR1,{pos}
                CALL  {cint}
                ST    GR0,{arr},GR3"#,
            index = index_reg,
            size = arr_size,
            fit = safe_index,
            pos = s_labels.pos,
            len = s_labels.len,
            ok = label,
            cint = cint_label,
            arr = arr_label
        ));
        self.code(recovers);

        self.set_register_idle(index_reg); // GR7 解放のはず
        self.return_temp_str_var_label(s_labels);
    }

    // Input ステートメント
    // 整数変数へのコンソール入力
    fn compile_input_integer(&mut self, var_name: &str) {
        let cint_label = self.load_subroutine(subroutine::Id::FuncCInt);
        let s_labels = self.get_temp_str_var_label();
        let var_label = self.int_var_labels.get(var_name).cloned().expect("BUG");
        let label = self.get_new_jump_label();

        self.has_eof = true;

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.comment(format!("Input {}", var_name));
        self.code(saves);
        self.code(format!(
            r#" IN    {pos},{len}
                LD    GR2,{len}
                JPL   {ok}
                JZE   {ok}
                ST    GR2,EOF
                XOR   GR2,GR2
{ok}            LAD   GR1,{pos}
                CALL  {cint}
                ST    GR0,{var}"#,
            pos = s_labels.pos,
            len = s_labels.len,
            ok = label,
            cint = cint_label,
            var = var_label
        ));
        self.code(recovers);

        self.return_temp_str_var_label(s_labels);
    }

    // Input ステートメント
    // 文字列変数へのコンソール入力
    fn compile_input_string(&mut self, var_name: &str) {
        let StrLabels { len, pos, .. } = self.str_var_labels.get(var_name).cloned().expect("BUG");
        let label = self.get_new_jump_label();
        self.has_eof = true;
        self.comment(format!("Input {}", var_name));
        // IN {var_pos},{var_len}
        self.code(format!(
            r#" IN   {pos},{len}
                LD   GR0,{len}
                JPL  {ok}
                JZE  {ok}
                ST   GR0,EOF
                XOR  GR0,GR0
                ST   GR0,{len}
{ok}            NOP"#,
            pos = pos,
            len = len,
            ok = label
        ));
    }

    // Print ステートメント
    // 真理値リテラルの画面出力
    fn compile_print_lit_boolean(&mut self, value: bool) {
        let s = if value { "True" } else { "False" };
        let StrLabels { len, pos, .. } = self.get_lit_str_labels(s);
        self.comment(format!("Print {}", s));
        // OUT {lit_pos},{lit_len}
        self.code(casl2::Command::Out {
            pos: pos.into(),
            len: len.into(),
        });
    }

    // Print ステートメント
    // 数字リテラルの画面出力
    fn compile_print_lit_integer(&mut self, value: i32) {
        let StrLabels { len, pos, .. } = self.get_lit_str_labels(&value.to_string());
        self.comment(format!("Print {}", value));
        self.code(casl2::Command::Out {
            pos: pos.into(),
            len: len.into(),
        });
    }

    // Print ステートメント
    // 文字列リテラルの画面出力
    fn compile_print_lit_string(&mut self, value: &str) {
        let StrLabels { len, pos, .. } = self.get_lit_str_labels(value);
        self.comment(format!(r#"Print "{}""#, value.replace('"', r#""""#)));
        self.code(casl2::Command::Out {
            pos: pos.into(),
            len: len.into(),
        });
    }

    // Print ステートメント
    // 文字列変数の画面出力
    fn compile_print_var_string(&mut self, var_name: &str) {
        let StrLabels { len, pos, .. } = self.str_var_labels.get(var_name).cloned().expect("BUG");
        self.comment(format!("Print {}", var_name));
        self.code(casl2::Command::Out {
            pos: pos.into(),
            len: len.into(),
        });
    }

    // Print ステートメント
    // 真理値の演算結果の画面出力
    fn compile_print_expr_boolean(&mut self, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.comment(format!("Print {}", value));

        // 想定では GR7
        let reg = self.compile_int_expr(value);
        let labels = self.get_temp_str_var_label();
        let cstr = self.load_subroutine(subroutine::Id::FuncCStrArgBool);

        // 想定では、
        //  reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR3,{reg}
                LAD   GR1,{pos}
                LAD   GR2,{len}
                CALL  {cstr}
                OUT   {pos},{len}"#,
            reg = reg,
            pos = labels.pos,
            len = labels.len,
            cstr = cstr
        ));
        self.code(recovers);

        self.set_register_idle(reg); // GR7 解放のはず
        self.return_temp_str_var_label(labels);
    }

    // Print ステートメント
    // 文字列の演算結果の画面出力
    fn compile_print_expr_string(&mut self, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::String));

        self.comment(format!("Print {}", value));

        let labels = self.compile_str_expr(value);

        let labels = if let StrLabels {
            label_type: StrLabelType::Lit(s),
            ..
        } = labels
        {
            self.get_lit_str_labels(&s)
        } else {
            labels
        };

        self.code(format!(
            r#" OUT  {pos},{len}"#,
            pos = labels.pos,
            len = labels.len
        ));

        self.return_temp_str_var_label(labels);
    }

    // Print ステートメント
    // 整数の計算結果の画面出力
    fn compile_print_expr_integer(&mut self, value: &parser::Expr) {
        self.comment(format!("Print {}", value));

        // 想定では GR7
        let value_reg = self.compile_int_expr(value);
        let call_label = self.load_subroutine(subroutine::Id::FuncCStrArgInt);
        let str_labels = self.get_temp_str_var_label();

        // 想定では、
        //  value_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR3,{value}
                LAD   GR1,{pos}
                LAD   GR2,{len}
                CALL  {cstr}
                OUT   {pos},{len}"#,
            value = value_reg,
            pos = &str_labels.pos,
            len = &str_labels.len,
            cstr = call_label
        ));
        self.code(recovers);

        self.set_register_idle(value_reg); // GR7 解放のはず
        self.return_temp_str_var_label(str_labels);
    }

    // 式の展開 (戻り値が文字列)
    fn compile_str_expr(&mut self, expr: &parser::Expr) -> StrLabels {
        use parser::Expr::*;
        match expr {
            BinaryOperatorString(op, lhs, rhs) => self.compile_bin_op_string(*op, lhs, rhs),
            FunctionString(func, param) => self.compile_function_string(*func, param),
            LitString(lit_str) => self.get_lit_str_label_if_exists(lit_str),
            VarString(var_name) => self.str_var_labels.get(var_name).cloned().expect("BUG"),

            // 戻り値が文字列ではないもの
            BinaryOperatorBoolean(..)
            | BinaryOperatorInteger(..)
            | CharOfLitString(..)
            | CharOfVarString(..)
            | FunctionBoolean(..)
            | FunctionInteger(..)
            | LitBoolean(..)
            | LitInteger(..)
            | LitCharacter(..)
            | UnaryOperatorInteger(..)
            | UnaryOperatorBoolean(..)
            | VarBoolean(..)
            | VarInteger(..)
            | VarArrayOfBoolean(..)
            | VarArrayOfInteger(..)
            | ParamList(_) => unreachable!("BUG"),
        }
    }

    // 文字列を返す二項演算子の処理
    fn compile_bin_op_string(
        &mut self,
        op: tokenizer::Operator,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> StrLabels {
        use tokenizer::Operator::*;

        match op {
            Concat => self.compile_bin_op_string_concat(lhs, rhs),

            // 文字列を返さないもの、あるいは二項演算子ではないもの
            And | Xor | Or | NotEqual | LessOrEequal | GreaterOrEqual | Equal | LessThan
            | GreaterThan | ShiftLeft | ShiftRight | Add | Sub | Mul | Div | Mod | Not
            | AddInto | SubInto | OpenBracket | CloseBracket | Comma => unreachable!("BUG"),
        }
    }

    // 文字列を返す二項演算子の処理
    // 文字列の結合 ( & )
    fn compile_bin_op_string_concat(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> StrLabels {
        assert!(matches!(lhs.return_type(), parser::ExprType::String));
        assert_eq!(lhs.return_type(), rhs.return_type());

        let lhs_labels = self.compile_str_expr(lhs);
        let rhs_labels = self.compile_str_expr(rhs);

        // レジスタを退避
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };

        self.code(saves);

        let lhs_labels = if !matches!(lhs_labels.label_type, StrLabelType::Temp) {
            let temp_labels = self.get_temp_str_var_label();
            let copy = self.load_subroutine(subroutine::Id::UtilCopyStr);
            self.code(format!(
                r#" LAD   GR1,{tmppos}
                    LAD   GR2,{tmplen}
                    LAD   GR3,{srcpos}
                    LD    GR4,{srclen}
                    CALL  {copy}"#,
                tmppos = temp_labels.pos,
                tmplen = temp_labels.len,
                srcpos = lhs_labels.pos,
                srclen = lhs_labels.len,
                copy = copy
            ));
            temp_labels
        } else {
            lhs_labels
        };

        let concat = self.load_subroutine(subroutine::Id::UtilConcatStr);
        self.code(format!(
            r#" LAD   GR1,{lhspos}
                LAD   GR2,{lhslen}
                LAD   GR3,{rhspos}
                LD    GR4,{rhslen}
                CALL  {concat}"#,
            lhspos = lhs_labels.pos,
            lhslen = lhs_labels.len,
            rhspos = rhs_labels.pos,
            rhslen = rhs_labels.len,
            concat = concat
        ));

        self.code(recovers);

        self.return_temp_str_var_label(rhs_labels);
        lhs_labels
    }

    // 戻り値が文字列の関数の処理
    fn compile_function_string(
        &mut self,
        func: tokenizer::Function,
        param: &parser::Expr,
    ) -> StrLabels {
        use tokenizer::Function::*;
        match func {
            Chr => self.call_function_chr(param),
            CStr => self.call_function_cstr(param),
            Mid => self.call_function_mid(param),
            Space => self.call_function_space(param),

            // 戻り値が文字列ではないもの
            Abs | Asc | CInt | Eof | Len | Max | Min | CBool => unreachable!("BUG"),
        }
    }

    // Mid関数
    // Mid(<string>,<integer>)
    // Mid(<string>,<integer>,<integer>)
    fn call_function_mid(&mut self, param: &parser::Expr) -> StrLabels {
        let param = if let parser::Expr::ParamList(list) = param {
            list
        } else {
            unreachable!("BUG");
        };

        assert!((2..=3).contains(&param.len()));

        let src = param.get(0).expect("BUG");
        assert!(matches!(src.return_type(), parser::ExprType::String));

        let offset = param.get(1).expect("BUG");
        assert!(matches!(offset.return_type(), parser::ExprType::Integer));

        let length = param.get(2);

        assert!(
            !matches!(length, Some(expr) if !matches!(expr.return_type(), parser::ExprType::Integer))
        );

        let partialcopy = self.load_subroutine(subroutine::Id::UtilCopyFromOffsetStr);

        let dst_labels = self.get_temp_str_var_label();
        let src_labels = self.compile_str_expr(src);
        let offset_reg = self.compile_int_expr(offset);

        if let Some(length) = length {
            let length_reg = self.compile_int_expr(length);
            self.restore_register(offset_reg);

            // レジスタを退避
            let (saves, recovers) = {
                use casl2::Register::*;
                let mut regs = vec![Gr1, Gr2, Gr3, Gr4, Gr5];
                if matches!(offset_reg, Gr1) {
                    regs.retain(|r| !matches!(r, Gr1));
                }
                if !matches!(length_reg, Gr6) {
                    regs.push(Gr6);
                }
                self.get_save_registers_src(&regs)
            };

            let (length_line1, length_line2) = if matches!(length_reg, casl2::Register::Gr1) {
                if matches!(offset_reg, casl2::Register::Gr6) {
                    (" LD GR0,GR1".to_string(), " LD GR6,GR0".to_string())
                } else {
                    (" LD GR6,GR1".to_string(), "".to_string())
                }
            } else {
                (
                    "".to_string(),
                    if matches!(length_reg, casl2::Register::Gr6) {
                        "".to_string()
                    } else {
                        format!(" LD GR6,{length}", length = length_reg)
                    },
                )
            };

            let offset_line = if matches!(offset_reg, casl2::Register::Gr1) {
                "".to_string()
            } else {
                format!(" LD GR1,{offset}", offset = offset_reg)
            };

            self.code(saves);
            self.code(format!(
                r#" {length_line1}
                    {offset_line}
                    {length_line2}
                    LAD   GR5,{dstpos}
                    LAD   GR3,{srcpos}
                    LD    GR4,{srclen}
                    LD    GR2,GR4
                    CALL  {copy}
                    ST    GR0,{dstlen}"#,
                length_line1 = length_line1,
                offset_line = offset_line,
                length_line2 = length_line2,
                dstpos = dst_labels.pos,
                dstlen = dst_labels.len,
                srcpos = src_labels.pos,
                srclen = src_labels.len,
                copy = partialcopy
            ));
            self.code(recovers);

            self.set_register_idle(length_reg);
        } else {
            // レジスタを退避
            let (saves, recovers) = {
                use casl2::Register::*;
                let mut regs = vec![Gr1, Gr2, Gr3, Gr4, Gr5, Gr6];
                if matches!(offset_reg, Gr1) {
                    regs.retain(|r| !matches!(r, Gr1));
                }
                self.get_save_registers_src(&regs)
            };
            let offset_line = if matches!(offset_reg, casl2::Register::Gr1) {
                "".to_string()
            } else {
                format!(" LD GR1,{offset}", offset = offset_reg)
            };
            self.code(saves);
            self.code(format!(
                r#" {offset_line}
                    LAD   GR5,{dstpos}
                    LAD   GR3,{srcpos}
                    LD    GR4,{srclen}
                    LD    GR2,GR4
                    LD    GR6,GR4
                    CALL  {copy}
                    ST    GR0,{dstlen}"#,
                offset_line = offset_line,
                dstpos = dst_labels.pos,
                dstlen = dst_labels.len,
                srcpos = src_labels.pos,
                srclen = src_labels.len,
                copy = partialcopy
            ));
            self.code(recovers);
        }

        self.set_register_idle(offset_reg);
        self.return_temp_str_var_label(src_labels);
        dst_labels
    }

    // Chr(<integer>)
    fn call_function_chr(&mut self, param: &parser::Expr) -> StrLabels {
        assert!(matches!(param.return_type(), parser::ExprType::Integer));

        let reg = self.compile_int_expr(param);

        let labels = self.get_temp_str_var_label();

        self.code(format!(
            r#" ST   {reg},{pos}
                LAD  {reg},1
                ST   {reg},{len}"#,
            reg = reg,
            pos = labels.pos,
            len = labels.len
        ));

        self.set_register_idle(reg);

        labels
    }

    // Space(<integer>)
    fn call_function_space(&mut self, param: &parser::Expr) -> StrLabels {
        assert!(matches!(param.return_type(), parser::ExprType::Integer));

        let space = self.load_subroutine(subroutine::Id::FuncSpace);

        let size_reg = self.compile_int_expr(param);

        let labels = self.get_temp_str_var_label();

        // レジスタを退避
        let (saves, recovers) = {
            use casl2::Register::*;
            let mut regs = vec![Gr1, Gr2, Gr3];
            if matches!(size_reg, Gr3) {
                regs.pop();
            }
            self.get_save_registers_src(&regs)
        };

        let size_line = if matches!(size_reg, casl2::Register::Gr3) {
            "".to_string()
        } else {
            format!(" LD GR3,{size}", size = size_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {size_line}
                LAD  GR1,{pos}
                LAD  GR2,{len}
                CALL {space}"#,
            size_line = size_line,
            pos = labels.pos,
            len = labels.len,
            space = space
        ));
        self.code(recovers);

        self.set_register_idle(size_reg);

        labels
    }

    // CStr(<boolean>/<integer>) 関数
    fn call_function_cstr(&mut self, param: &parser::Expr) -> StrLabels {
        assert!(matches!(
            param.return_type(),
            parser::ExprType::Boolean | parser::ExprType::Integer
        ));

        let id = match param.return_type() {
            parser::ExprType::Boolean => subroutine::Id::FuncCStrArgBool,
            parser::ExprType::Integer => subroutine::Id::FuncCStrArgInt,
            parser::ExprType::String | parser::ExprType::ParamList => unreachable!("BUG"),
        };

        let call_label = self.load_subroutine(id);

        let value_reg = self.compile_int_expr(param);

        let t_labels = self.get_temp_str_var_label();

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            let mut regs = vec![Gr1, Gr2, Gr3];
            if matches!(value_reg, Gr3) {
                regs.pop();
            }
            self.get_save_registers_src(&regs)
        };

        let value_line = if matches!(value_reg, casl2::Register::Gr3) {
            "".to_string()
        } else {
            format!(" LD GR3,{value}", value = value_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {value_line}
                LAD   GR1,{pos}
                LAD   GR2,{len}
                CALL  {call}"#,
            value_line = value_line,
            len = t_labels.len,
            pos = t_labels.pos,
            call = call_label
        ));
        self.code(recovers);

        self.set_register_idle(value_reg);

        t_labels
    }

    // 式の展開 (戻り値が整数or真理値)
    fn compile_int_expr(&mut self, expr: &parser::Expr) -> casl2::Register {
        use parser::Expr::*;
        match expr {
            BinaryOperatorBoolean(op, lhs, rhs) => self.compile_bin_op_boolean(*op, lhs, rhs),
            BinaryOperatorInteger(op, lhs, rhs) => self.compile_bin_op_integer(*op, lhs, rhs),
            CharOfLitString(lit_str, index) => self.compile_character_of_literal(lit_str, index),
            CharOfVarString(var_name, index) => self.compile_character_of_variable(var_name, index),
            FunctionBoolean(func, param) => self.compile_function_boolean(*func, param),
            FunctionInteger(func, param) => self.compile_function_integer(*func, param),
            LitBoolean(lit_bool) => self.compile_literal_boolean(*lit_bool),
            LitInteger(lit_int) => self.compile_literal_integer(*lit_int),
            LitCharacter(lit_char) => self.compile_literal_character(*lit_char),
            UnaryOperatorInteger(op, value) => self.compile_unary_op_integer(*op, value),
            UnaryOperatorBoolean(op, value) => self.compile_unary_op_boolean(*op, value),
            VarBoolean(var_name) => self.compile_variable_boolean(var_name),
            VarInteger(var_name) => self.compile_variable_integer(var_name),
            VarArrayOfBoolean(arr_name, index) => {
                self.compile_variable_array_of_boolean(arr_name, index)
            }
            VarArrayOfInteger(arr_name, index) => {
                self.compile_variable_array_of_integer(arr_name, index)
            }

            // 戻り値が整数でも真理値でもないもの
            BinaryOperatorString(..)
            | FunctionString(..)
            | LitString(..)
            | VarString(..)
            | ParamList(_) => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 整数を返す単項演算子の処理
    fn compile_unary_op_integer(
        &mut self,
        op: tokenizer::Operator,
        value: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        match op {
            Not => {
                assert!(matches!(value.return_type(), parser::ExprType::Integer));
                let reg = self.compile_int_expr(value);
                self.code(format!(" XOR {reg},=#FFFF", reg = reg));
                reg
            }

            Sub => {
                assert!(matches!(value.return_type(), parser::ExprType::Integer));
                let reg = self.compile_int_expr(value);
                self.code(format!(
                    r#" XOR {reg},=#FFFF
                        LAD {reg},1,{reg}"#,
                    reg = reg
                ));
                reg
            }

            // 真理値を返さないもの、または単項演算子ではないもの
            And | Xor | Or | NotEqual | LessOrEequal | GreaterOrEqual | Equal | LessThan
            | GreaterThan | ShiftLeft | ShiftRight | Add | Mul | Div | Mod | AddInto | SubInto
            | Concat | OpenBracket | CloseBracket | Comma => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 真理値を返す関数の処理
    fn compile_function_boolean(
        &mut self,
        func: tokenizer::Function,
        param: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Function::*;
        match func {
            CBool => self.call_function_cbool(param),
            Eof => self.call_function_eof(param),

            // 戻り値が真理値ではないもの
            Abs | Asc | Chr | CInt | Len | Max | Mid | Min | CStr | Space => unreachable!("BUG"),
        }
    }

    // EOF()
    fn call_function_eof(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param, parser::Expr::LitInteger(0)));
        self.has_eof = true;
        let reg = self.get_idle_register();
        self.code(format!(" LD {reg},EOF", reg = reg));
        reg
    }

    // CBool(<integer>)
    fn call_function_cbool(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param.return_type(), parser::ExprType::Integer));

        let reg = self.compile_int_expr(param);
        let label = self.get_new_jump_label();

        self.code(format!(
            r#" AND  {reg},{reg}
                JZE  {ok}
                LAD  {reg},#FFFF
{ok}            NOP"#,
            reg = reg,
            ok = label
        ));

        reg
    }

    // (式展開の処理の一部)
    // 真理値を返す単項演算子の処理
    fn compile_unary_op_boolean(
        &mut self,
        op: tokenizer::Operator,
        value: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        match op {
            Not => {
                assert!(matches!(value.return_type(), parser::ExprType::Boolean));
                let reg = self.compile_int_expr(value);
                self.code(format!(" XOR {reg},=#FFFF", reg = reg));
                reg
            }

            // 真理値を返さないもの、または単項演算子ではないもの
            And | Xor | Or | NotEqual | LessOrEequal | GreaterOrEqual | Equal | LessThan
            | GreaterThan | ShiftLeft | ShiftRight | Add | Sub | Mul | Div | Mod | AddInto
            | SubInto | Concat | OpenBracket | CloseBracket | Comma => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 文字リテラルを返す
    fn compile_literal_character(&mut self, lit_char: char) -> casl2::Register {
        let reg = self.get_idle_register();

        if lit_char == '\'' {
            self.code(format!(r#" LD {reg},=''''"#, reg = reg));
        } else {
            self.code(format!(r#" LD {reg},='{ch}'"#, reg = reg, ch = lit_char));
        }

        reg
    }

    // (式展開の処理の一部)
    // 文字列リテラルの文字を取り出す
    fn compile_character_of_literal(
        &mut self,
        lit_str: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        // サイズ0の文字列…(parserでコンパイルエラーにすべきな気が…)
        if lit_str.is_empty() {
            let reg = self.get_idle_register();
            self.code(format!(" XOR {reg},{reg}", reg = reg));
            return reg;
        }

        let str_labels = self.get_lit_str_label_if_exists(lit_str);

        // リテラル文字列の一部をリテラル整数で指定する、だと…？
        if let parser::Expr::LitInteger(index) = index {
            let index = ((*index).max(0) as usize).min(lit_str.chars().count() - 1);
            let ch = lit_str.chars().nth(index).unwrap();
            return self.compile_literal_character(ch);
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                LD    GR2,{size}
                CALL  {fit}"#,
            index_line = index_line,
            size = str_labels.len,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" LD    {index},GR0
                LD    {index},{lit},{index}"#,
            index = index_reg,
            lit = str_labels.pos
        ));

        index_reg
    }

    // (式展開の処理の一部)
    // 文字列変数の文字を取り出す
    fn compile_character_of_variable(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let load_elem = self.load_subroutine(subroutine::Id::UtilLoadElement);

        let index_reg = self.compile_int_expr(index);

        let str_labels = self.str_var_labels.get(var_name).cloned().expect("BUG");

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2, Gr3])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2, Gr3])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        /*
           考察

           サブルーチンを一切使用しない場合
           新規レジスタは無いが、11行でラベルを4つ消費…(ラベルコストが高すぎる、10行あたり2個が理想)
                 AND   {index},{index}
                 JPL   {ok1}
                 XOR   {index},{index}
          {ok1}  CPL   {index},{len}
                 JMI   {ok3}
                 LD    {index},{len}
                 JNZ   {ok2}
                 XOR   {index},{index}
                 JUMP  {ok4}
         {ok2}   LAD   {index},-1,{index}
         {ok3}   LD    {index},{var},{index}
         {ok4}   NOP

                 7～12行でラベルを1つ消費(ただしサブルーチン行コスト13がある)
                 (大半のケースは8行と思われ)(複雑な深いネストの計算式でもないとGR2あたりまで使用しない)
                 (8行呼び出し5回以上でサブルーチンコストはチャラになる)
                 (5回以上呼び出すケースはレアそうだが,INTSORTなど)
                 (ラベルコストは4回以上でチャラ)
                 ( 11 * 1 = 11　[ 4],  8 * 1 + 13 = 21 [1+4 = 5])
                 ( 11 * 2 = 22 [ 8],  8 * 2 + 13 = 29 [2+4 = 6])
                 ( 11 * 3 = 33 [12],  8 * 3 + 13 = 37 [3+4 = 7])
                 ( 11 * 4 = 44 [16],  8 * 4 + 13 = 45 [4+4 = 8])
                 ( 11 * 5 = 55 [20],  8 * 5 + 13 = 53 [5+4 = 9])
                 ( 11 * 6 = 66 [24],  8 * 6 + 13 = 61 [6+4 =10])
                 ( 11 * 7 = 77 [28],  8 * 7 + 13 = 69 [7+4 =11])

                 [ PUSH  0,GR1 ]
                 [ PUSH  0,GR2 ]
                 [ LD    GR1,{index} ]
                   LD    GR2,{len}
                   CALL  {fit}
                 [ POP   GR2 ]
                 [ POP   GR1 ]
                   LD    {index},GR0
                   LD    {index},{var},{index}
                   LD    GR0,{len}
                   JNZ   {ok}
                   XOR   {index},{index}
         {ok}      NOP

                 SafeIndexサブルーチン(13行)
         {fit}     AND    GR2,GR2
                   JNZ    {lbound}
                   XOR    GR0,GR0
                   RET
         {lbound}  LD     GR0,GR1
                   JPL    {ubound}
                   XOR    GR0,GR0
                   RET
         {ubound}  CPL    GR0,GR2
                   JMI    {ret}
                   LAD    GR0,-1
                   ADDL   GR0,GR2
         {ret}     RET



                 いっそ要素参照までをサブルーチン化は？
                 4～11行のコスト、ラベルコストはない
                 (大半は5行となると思う)(複雑な深いネストの計算式でもないとGR3あたりまで使用しない)
                 サブルーチンコストは
                    行コスト 13+10=23
                    ラベルコスト 4+2=6
                 4回以上呼び出しでチャラだが…
                 (長さ0の文字列変数があるればの恩恵で、長さ1以上が確定の固定長整数配列などにはあまり意味が無いSafeIndexだけでよいが)
                 ( 11 * 1 = 11　[ 4],  5 * 1 + 23 = 28 [6])
                 ( 11 * 2 = 22 [ 8],  5 * 2 + 23 = 33 [6])
                 ( 11 * 3 = 33 [12],  5 * 3 + 23 = 38 [6])
                 ( 11 * 4 = 44 [16],  5 * 4 + 23 = 43 [6])
                 ( 11 * 5 = 55 [20],  5 * 5 + 23 = 48 [6])
                 ( 11 * 6 = 66 [24],  5 * 6 + 23 = 53 [6])
                 ( 11 * 7 = 77 [28],  5 * 7 + 23 = 58 [6])

                 [ PUSH 0,GR1 ]
                 [ PUSH 0,GR2 ]
                 [ PUSH 0,GR3 ]
                 [ LD   GR1,{index} ]
                   LD   GR2,{len}
                   LAD  GR3,{var}
                   CALL {load}
                 [ POP  GR3 ]
                 [ POP  GR2 ]
                 [ POP  GR1 ]
                   LD   {index},GR0

               LoadElementサブルーチン(10行)
        {load}     AND  GR2,GR2
                   JNZ  {ok}
                   XOR  GR0,GR0
                   RET
        {ok}       CALL {fit}
                   PUSH 0,GR3
                   ADDL GR3,GR0
                   LD   GR0,0,GR3
                   POP  GR3
                   RET

         文字列変数は長さ0がありうるからそのチェックが必要だが
         真理値・整数配列なら固定長配列で長さ1以上が保証されてるから…
         素で書くと8行コストのラベル2個か…
                 AND   {index},{index}
                 JPL   {ok1}
                 XOR   {index},{index}
          {ok1}  CPL   {index},{len}
                 JMI   {ok2}
                 LAD   {index},-1
                 ADDL  {index},{len}
          {ok2}  LD    {index},{arr},{index}

          SafeIndexサブルーチンを使用すると4～9行(大半は5行と思われ)、ラベルコストなし
          (LoadElementサブルーチンを使う利点は皆無…最大行が増えるリスクが発生してしまう)
                 [ PUSH  0,GR1 ]
                 [ PUSH  0,GR2 ]
                 [ LD    GR1,{index} ]
                   LD    GR2,{len}
                   CALL  {fit}
                 [ POP   GR2 ]
                 [ POP   GR1 ]
                   LD    {index},GR0
                   LD    {index},{arr},{index}

         */

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                LD    GR2,{len}
                LAD   GR3,{pos}
                CALL  {load}"#,
            index_line = index_line,
            len = str_labels.len,
            pos = str_labels.pos,
            load = load_elem
        ));
        self.code(recovers);
        self.code(format!(r#" LD {index},GR0"#, index = index_reg));

        index_reg
    }

    // (式展開の処理の一部)
    // 真理値配列の要素を取り出す
    fn compile_variable_array_of_boolean(
        &mut self,
        arr_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let (arr_label, arr_size) = self.bool_arr_labels.get(arr_name).cloned().expect("BUG");

        assert!(arr_size > 0);

        // インデックスがリテラル整数で指定…
        if let parser::Expr::LitInteger(index) = index {
            let index = ((*index).max(0) as usize).min(arr_size - 1);
            let reg = self.get_idle_register();
            self.code(format!(
                r#" LAD {reg},{index}
                    LD  {reg},{arr},{reg}"#,
                reg = reg,
                index = index,
                arr = arr_label
            ));
            return reg;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index_line = index_line,
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" LD    {index},GR0
                LD    {index},{arr},{index}"#,
            index = index_reg,
            arr = arr_label
        ));

        index_reg
    }

    // (式展開の処理の一部)
    // 整数配列の要素を取り出す
    fn compile_variable_array_of_integer(
        &mut self,
        arr_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        let (arr_label, arr_size) = self.int_arr_labels.get(arr_name).cloned().expect("BUG");

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index_line = index_line,
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" LD    {index},GR0
                LD    {index},{arr},{index}"#,
            index = index_reg,
            arr = arr_label
        ));

        index_reg
    }

    // (式展開の処理の一部)
    // 真理値を返す二項演算子の処理
    fn compile_bin_op_boolean(
        &mut self,
        op: tokenizer::Operator,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        let cmd = match op {
            // lhs,rhsは真理値のみ
            And => "AND",
            Xor => "XOR",
            Or => "OR",

            // lhs,rhsは真理値、整数、文字列のいずれか
            NotEqual => return self.compile_bin_op_boolean_not_equal(lhs, rhs),
            LessOrEequal => return self.compile_bin_op_boolean_less_or_equal(lhs, rhs),
            GreaterOrEqual => return self.compile_bin_op_boolean_greater_or_equal(lhs, rhs),
            Equal => return self.compile_bin_op_boolean_equal(lhs, rhs),
            LessThan => return self.compile_bin_op_boolean_less_than(lhs, rhs),
            GreaterThan => return self.compile_bin_op_boolean_greater_than(lhs, rhs),

            // 二項演算子ではないものや、真理値を返さないもの
            ShiftLeft | ShiftRight | Add | Sub | Mul | Div | Mod | Not | AddInto | SubInto
            | Concat | OpenBracket | CloseBracket | Comma => {
                unreachable!("BUG")
            }
        };

        assert!(matches!(lhs.return_type(), parser::ExprType::Boolean));
        assert!(matches!(rhs.return_type(), parser::ExprType::Boolean));

        let lhs_reg = self.compile_int_expr(lhs);
        let rhs_reg = self.compile_int_expr(rhs);

        self.code(format!(
            r#" {cmd} {lhs},{rhs}"#,
            cmd = cmd,
            lhs = lhs_reg,
            rhs = rhs_reg
        ));

        self.set_register_idle(rhs_reg);
        lhs_reg
    }

    // (式展開の処理の一部)
    // 比較演算子( <> )
    fn compile_bin_op_boolean_not_equal(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert_eq!(lhs.return_type(), rhs.return_type());

        match lhs.return_type() {
            parser::ExprType::Boolean => {
                let lhs_reg = self.compile_int_expr(lhs);
                let rhs_reg = self.compile_int_expr(rhs);
                self.restore_register(lhs_reg);
                self.code(format!(r#" XOR {lhs},{rhs}"#, lhs = lhs_reg, rhs = rhs_reg));
                self.set_register_idle(rhs_reg);
                lhs_reg
            }
            parser::ExprType::Integer => {
                let lhs_reg = self.compile_int_expr(lhs);
                let rhs_reg = self.compile_int_expr(rhs);
                self.restore_register(lhs_reg);
                let label = self.get_new_jump_label();
                self.code(format!(
                    r#" SUBA  {lhs},{rhs}
                        JZE   {ok}
                        LAD   {lhs},#FFFF
{ok}                    NOP"#,
                    lhs = lhs_reg,
                    rhs = rhs_reg,
                    ok = label
                ));
                self.set_register_idle(rhs_reg);
                lhs_reg
            }
            parser::ExprType::String => {
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_labels = self.compile_str_expr(lhs);
                let rhs_labels = self.compile_str_expr(rhs);
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" LAD   GR1,{lhspos}
                        LD    GR2,{lhslen}
                        LAD   GR3,{rhspos}
                        LD    GR4,{rhslen}
                        CALL  {cmpstr}"#,
                    lhspos = lhs_labels.pos,
                    lhslen = lhs_labels.len,
                    rhspos = rhs_labels.pos,
                    rhslen = rhs_labels.len,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SLL   GR0,15
                        SRA   GR0,15
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                self.return_temp_str_var_label(lhs_labels);
                self.return_temp_str_var_label(rhs_labels);
                self.set_register_used(reg);
                reg
            }
            parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( <= )
    fn compile_bin_op_boolean_less_or_equal(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert_eq!(lhs.return_type(), rhs.return_type());

        match lhs.return_type() {
            parser::ExprType::Integer => {
                let lhs_reg = self.compile_int_expr(lhs);
                let rhs_reg = self.compile_int_expr(rhs);
                self.restore_register(lhs_reg);
                let label = self.get_new_jump_label();
                self.code(format!(
                    r#" XOR   GR0,GR0
                        CPA   {lhs},{rhs}
                        JPL   {ok}
                        LAD   GR0,#FFFF
{ok}                    LD    {lhs},GR0"#,
                    lhs = lhs_reg,
                    rhs = rhs_reg,
                    ok = label
                ));
                self.set_register_idle(rhs_reg);
                lhs_reg
            }
            parser::ExprType::String => {
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_labels = self.compile_str_expr(lhs);
                let rhs_labels = self.compile_str_expr(rhs);
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" LAD   GR3,{lhspos}
                        LD    GR4,{lhslen}
                        LAD   GR1,{rhspos}
                        LD    GR2,{rhslen}
                        CALL  {cmpstr}"#,
                    lhspos = lhs_labels.pos,
                    lhslen = lhs_labels.len,
                    rhspos = rhs_labels.pos,
                    rhslen = rhs_labels.len,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SRA   GR0,1
                        XOR   GR0,=#FFFF
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                self.return_temp_str_var_label(lhs_labels);
                self.return_temp_str_var_label(rhs_labels);
                self.set_register_used(reg);
                reg
            }
            parser::ExprType::Boolean | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( >= )
    fn compile_bin_op_boolean_greater_or_equal(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert_eq!(lhs.return_type(), rhs.return_type());

        match lhs.return_type() {
            parser::ExprType::Integer => {
                let lhs_reg = self.compile_int_expr(lhs);
                let rhs_reg = self.compile_int_expr(rhs);
                self.restore_register(lhs_reg);
                let label = self.get_new_jump_label();
                self.code(format!(
                    r#" XOR   GR0,GR0
                        CPA   {lhs},{rhs}
                        JMI   {ok}
                        LAD   GR0,#FFFF
{ok}                    LD    {lhs},GR0"#,
                    lhs = lhs_reg,
                    rhs = rhs_reg,
                    ok = label
                ));
                self.set_register_idle(rhs_reg);
                lhs_reg
            }
            parser::ExprType::String => {
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_labels = self.compile_str_expr(lhs);
                let rhs_labels = self.compile_str_expr(rhs);
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" LAD   GR1,{lhspos}
                        LD    GR2,{lhslen}
                        LAD   GR3,{rhspos}
                        LD    GR4,{rhslen}
                        CALL  {cmpstr}"#,
                    lhspos = lhs_labels.pos,
                    lhslen = lhs_labels.len,
                    rhspos = rhs_labels.pos,
                    rhslen = rhs_labels.len,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SRA   GR0,15
                        XOR   GR0,=#FFFF
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                self.return_temp_str_var_label(lhs_labels);
                self.return_temp_str_var_label(rhs_labels);
                self.set_register_used(reg);
                reg
            }
            parser::ExprType::Boolean | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( > )
    fn compile_bin_op_boolean_greater_than(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert_eq!(lhs.return_type(), rhs.return_type());

        match lhs.return_type() {
            parser::ExprType::Integer => {
                let lhs_reg = self.compile_int_expr(lhs);
                let rhs_reg = self.compile_int_expr(rhs);
                self.restore_register(lhs_reg);
                let label = self.get_new_jump_label();
                self.code(format!(
                    r#" LAD   GR0,#FFFF
                        CPA   {lhs},{rhs}
                        JPL   {ok}
                        XOR   GR0,GR0
{ok}                    LD    {lhs},GR0"#,
                    lhs = lhs_reg,
                    rhs = rhs_reg,
                    ok = label
                ));
                self.set_register_idle(rhs_reg);
                lhs_reg
            }
            parser::ExprType::String => {
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_labels = self.compile_str_expr(lhs);
                let rhs_labels = self.compile_str_expr(rhs);
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" LAD   GR3,{lhspos}
                        LD    GR4,{lhslen}
                        LAD   GR1,{rhspos}
                        LD    GR2,{rhslen}
                        CALL  {cmpstr}"#,
                    lhspos = lhs_labels.pos,
                    lhslen = lhs_labels.len,
                    rhspos = rhs_labels.pos,
                    rhslen = rhs_labels.len,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SRA   GR0,15
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                self.return_temp_str_var_label(lhs_labels);
                self.return_temp_str_var_label(rhs_labels);
                self.set_register_used(reg);
                reg
            }
            parser::ExprType::Boolean | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( < )
    fn compile_bin_op_boolean_less_than(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert_eq!(lhs.return_type(), rhs.return_type());

        match lhs.return_type() {
            parser::ExprType::Integer => {
                let lhs_reg = self.compile_int_expr(lhs);
                let rhs_reg = self.compile_int_expr(rhs);
                self.restore_register(lhs_reg);
                let label = self.get_new_jump_label();
                self.code(format!(
                    r#" LAD   GR0,#FFFF
                        CPA   {lhs},{rhs}
                        JMI   {ok}
                        XOR   GR0,GR0
{ok}                    LD    {lhs},GR0"#,
                    lhs = lhs_reg,
                    rhs = rhs_reg,
                    ok = label
                ));
                self.set_register_idle(rhs_reg);
                lhs_reg
            }
            parser::ExprType::String => {
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_labels = self.compile_str_expr(lhs);
                let rhs_labels = self.compile_str_expr(rhs);
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" LAD   GR1,{lhspos}
                        LD    GR2,{lhslen}
                        LAD   GR3,{rhspos}
                        LD    GR4,{rhslen}
                        CALL  {cmpstr}"#,
                    lhspos = lhs_labels.pos,
                    lhslen = lhs_labels.len,
                    rhspos = rhs_labels.pos,
                    rhslen = rhs_labels.len,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SRA   GR0,15
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                self.return_temp_str_var_label(lhs_labels);
                self.return_temp_str_var_label(rhs_labels);
                self.set_register_used(reg);
                reg
            }
            parser::ExprType::Boolean | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( = )
    fn compile_bin_op_boolean_equal(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert_eq!(lhs.return_type(), rhs.return_type());

        match lhs.return_type() {
            parser::ExprType::Boolean => {
                let lhs_reg = self.compile_int_expr(lhs);
                let rhs_reg = self.compile_int_expr(rhs);
                self.restore_register(lhs_reg);
                self.code(format!(
                    r#" XOR {lhs},{rhs}
                        XOR {lhs},=#FFFF"#,
                    lhs = lhs_reg,
                    rhs = rhs_reg
                ));
                self.set_register_idle(rhs_reg);
                lhs_reg
            }
            parser::ExprType::Integer => {
                let lhs_reg = self.compile_int_expr(lhs);
                let rhs_reg = self.compile_int_expr(rhs);
                self.restore_register(lhs_reg);
                let label = self.get_new_jump_label();
                self.code(format!(
                    r#" SUBA  {lhs},{rhs}
                        JZE   {ok}
                        LAD   {lhs},#FFFF
{ok}                    XOR   {lhs},=#FFFF"#,
                    lhs = lhs_reg,
                    rhs = rhs_reg,
                    ok = label
                ));
                self.set_register_idle(rhs_reg);
                lhs_reg
            }
            parser::ExprType::String => {
                let ret_reg = self.get_idle_register();
                self.set_register_idle(ret_reg);
                let lhs_str = self.compile_str_expr(lhs);
                let rhs_str = self.compile_str_expr(rhs);
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" LAD   GR1,{lhspos}
                        LD    GR2,{lhslen}
                        LAD   GR3,{rhspos}
                        LD    GR4,{rhslen}
                        CALL  {cmpstr}"#,
                    lhspos = lhs_str.pos,
                    lhslen = lhs_str.len,
                    rhspos = rhs_str.pos,
                    rhslen = rhs_str.len,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SLL   GR0,15
                        SRA   GR0,15
                        XOR   GR0,=#FFFF
                        LD    {reg},GR0"#,
                    reg = ret_reg
                ));
                self.return_temp_str_var_label(lhs_str);
                self.return_temp_str_var_label(rhs_str);
                self.set_register_used(ret_reg);
                ret_reg
            }
            parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 整数を返す二項演算子の処理
    fn compile_bin_op_integer(
        &mut self,
        op: tokenizer::Operator,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        let lhs_reg = self.compile_int_expr(lhs);

        // rhsを直接埋め込める場合
        let with_lit_src = match rhs {
            parser::Expr::LitInteger(value) => match op {
                And => Some(format!(" AND {},={}", lhs_reg, value)),
                Xor => Some(format!(" XOR {},={}", lhs_reg, value)),
                Or => Some(format!(" OR {},={}", lhs_reg, value)),
                ShiftLeft => Some(format!(" SLA {},{}", lhs_reg, value)),
                ShiftRight => Some(format!(" SRA {},{}", lhs_reg, value)),
                Add => Some(format!(" LAD {0},{1},{0}", lhs_reg, value)),
                Sub => Some(format!(" SUBA {},={}", lhs_reg, value)),
                _ => None,
            },
            parser::Expr::LitCharacter(value) => match op {
                And => Some(format!(" AND {},='{}'", lhs_reg, value)),
                Xor => Some(format!(" XOR {},='{}'", lhs_reg, value)),
                Or => Some(format!(" OR {},='{}'", lhs_reg, value)),
                Add => Some(format!(" ADDA {},='{}'", lhs_reg, value)),
                Sub => Some(format!(" SUBA {},='{}'", lhs_reg, value)),
                _ => None,
            },
            _ => None,
        };
        if let Some(src) = with_lit_src {
            self.code(src);
            return lhs_reg;
        }

        // rhsを計算する場合

        let rhs_reg = self.compile_int_expr(rhs);

        assert_ne!(lhs_reg, rhs_reg); // たぶん、大丈夫…

        self.restore_register(lhs_reg);

        let src = match op {
            And => format!(" AND {},{}", lhs_reg, rhs_reg),
            Xor => format!(" XOR {},{}", lhs_reg, rhs_reg),
            Or => format!(" OR {},{}", lhs_reg, rhs_reg),
            ShiftLeft => format!(" SLA {},0,{}", lhs_reg, rhs_reg),
            ShiftRight => format!(" SRA {},0,{}", lhs_reg, rhs_reg),
            Add => format!(" ADDA {},{}", lhs_reg, rhs_reg),
            Sub => format!(" SUBA {},{}", lhs_reg, rhs_reg),

            // 組み込みサブルーチンで処理するもの
            Mul => return self.compile_bin_op_integer_mul(lhs_reg, rhs_reg),
            Div => return self.compile_bin_op_integer_div(lhs_reg, rhs_reg),
            Mod => return self.compile_bin_op_integer_mod(lhs_reg, rhs_reg),

            // 二項演算子ではないものや、整数を返さないもの
            Not | NotEqual | LessOrEequal | GreaterOrEqual | AddInto | SubInto | Equal
            | LessThan | GreaterThan | Concat | OpenBracket | CloseBracket | Comma => {
                unreachable!("BUG")
            }
        };

        self.code(src);

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // 整数を返す二項演算子( Mul (*) )の処理
    fn compile_bin_op_integer_mul(
        &mut self,
        lhs_reg: casl2::Register,
        rhs_reg: casl2::Register,
    ) -> casl2::Register {
        let mul_label = self.load_subroutine(subroutine::Id::UtilMul);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            let mut regs = vec![Gr1];
            if !matches!(lhs_reg, Gr2) {
                regs.push(Gr2);
            }
            if !matches!(rhs_reg, Gr3) {
                regs.push(Gr3);
            }
            self.get_save_registers_src(&regs)
        };

        // lhs=GR2 rhs=GR3
        //    no code
        //    no code
        //    no code
        // lhs=GR2 rhs=GR*
        //    no code
        //    LD GR3,{rhs}
        //    no code
        // lhs=GR* rhs=GR3
        //    no code
        //    no code
        //    LD GR2,{lhs}
        // lhs=GR3 rhs=GR2
        //    LD GR0,{lhs}
        //    LD GR3,{rhs}
        //    LD GR2,GR0
        // lhs=GR3 rhs=GR*
        //    LD GR2,{lhs}
        //    LD GR3,{rhs}
        //    no code
        // lhs=GR* rhs=GR2
        //    no code
        //    LD GR3,{rhs}
        //    LD GR2,{lhs}
        // lhs=GR* rhs=GR*
        //    no code
        //    LD GR3,{rhs}
        //    LD GR2,{lhs}

        let (lhs_line1, lhs_line2) = if matches!(lhs_reg, casl2::Register::Gr3) {
            if matches!(rhs_reg, casl2::Register::Gr2) {
                (" LD GR0,GR3".to_string(), " LD GR2,GR0".to_string())
            } else {
                (" LD GR2,GR3".to_string(), "".to_string())
            }
        } else {
            (
                "".to_string(),
                if matches!(lhs_reg, casl2::Register::Gr2) {
                    "".to_string()
                } else {
                    format!(" LD GR2,{lhs}", lhs = lhs_reg)
                },
            )
        };

        let rhs_line = if matches!(rhs_reg, casl2::Register::Gr3) {
            "".to_string()
        } else {
            format!(" LD GR3,{rhs}", rhs = rhs_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {lhs_line1}
                {rhs_line}
                {lhs_line2}
                CALL  {mul}"#,
            rhs_line = rhs_line,
            lhs_line1 = lhs_line1,
            lhs_line2 = lhs_line2,
            mul = mul_label
        ));

        self.code(recovers);

        self.code(format!(" LD {lhs},GR0", lhs = lhs_reg));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // 整数を返す二項演算子( Div (\) )の処理
    fn compile_bin_op_integer_div(
        &mut self,
        lhs_reg: casl2::Register,
        rhs_reg: casl2::Register,
    ) -> casl2::Register {
        let divmod_label = self.load_subroutine(subroutine::Id::UtilDivMod);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            let mut regs = vec![Gr1];
            if !matches!(lhs_reg, Gr2) {
                regs.push(Gr2);
            }
            if !matches!(rhs_reg, Gr3) {
                regs.push(Gr3);
            }
            self.get_save_registers_src(&regs)
        };

        let (lhs_line1, lhs_line2) = if matches!(lhs_reg, casl2::Register::Gr3) {
            if matches!(rhs_reg, casl2::Register::Gr2) {
                (" LD GR0,GR3".to_string(), " LD GR2,GR0".to_string())
            } else {
                (" LD GR2,GR3".to_string(), "".to_string())
            }
        } else {
            (
                "".to_string(),
                if matches!(lhs_reg, casl2::Register::Gr2) {
                    "".to_string()
                } else {
                    format!(" LD GR2,{lhs}", lhs = lhs_reg)
                },
            )
        };

        let rhs_line = if matches!(rhs_reg, casl2::Register::Gr3) {
            "".to_string()
        } else {
            format!(" LD GR3,{rhs}", rhs = rhs_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {lhs_line1}
                {rhs_line}
                {lhs_line2}
                CALL  {divmod}"#,
            rhs_line = rhs_line,
            lhs_line1 = lhs_line1,
            lhs_line2 = lhs_line2,
            divmod = divmod_label
        ));
        self.code(recovers);
        self.code(format!(" LD {lhs},GR0", lhs = lhs_reg));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // 整数を返す二項演算子( Mod )の処理
    fn compile_bin_op_integer_mod(
        &mut self,
        lhs_reg: casl2::Register,
        rhs_reg: casl2::Register,
    ) -> casl2::Register {
        let divmod_label = self.load_subroutine(subroutine::Id::UtilDivMod);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            let mut regs = vec![Gr1];
            if !matches!(lhs_reg, Gr2) {
                regs.push(Gr2);
            }
            if !matches!(rhs_reg, Gr3) {
                regs.push(Gr3);
            }
            self.get_save_registers_src(&regs)
        };

        let (lhs_line1, lhs_line2) = if matches!(lhs_reg, casl2::Register::Gr3) {
            if matches!(rhs_reg, casl2::Register::Gr2) {
                (" LD GR0,GR3".to_string(), " LD GR2,GR0".to_string())
            } else {
                (" LD GR2,GR3".to_string(), "".to_string())
            }
        } else {
            (
                "".to_string(),
                if matches!(lhs_reg, casl2::Register::Gr2) {
                    "".to_string()
                } else {
                    format!(" LD GR2,{lhs}", lhs = lhs_reg)
                },
            )
        };

        let rhs_line = if matches!(rhs_reg, casl2::Register::Gr3) {
            "".to_string()
        } else {
            format!(" LD GR3,{rhs}", rhs = rhs_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {lhs_line1}
                {rhs_line}
                {lhs_line2}
                CALL  {divmod}
                LD    GR0,GR1"#,
            rhs_line = rhs_line,
            lhs_line1 = lhs_line1,
            lhs_line2 = lhs_line2,
            divmod = divmod_label
        ));
        self.code(recovers);
        self.code(format!(" LD {lhs},GR0", lhs = lhs_reg));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // 整数変数の読み込み
    fn compile_variable_integer(&mut self, var_name: &str) -> casl2::Register {
        let reg = self.get_idle_register();
        let var_label = self.int_var_labels.get(var_name).expect("BUG");
        let adr = casl2::Adr::label(var_label);

        // LD REG,VAR
        self.code(casl2::Command::A {
            code: casl2::A::Ld,
            r: reg,
            adr,
            x: None,
        });

        reg
    }

    // (式展開の処理の一部)
    // 整数リテラルの読み込み
    fn compile_literal_integer(&mut self, value: i32) -> casl2::Register {
        let reg = self.get_idle_register();

        // LAD REG,VALUE
        self.code(casl2::Command::A {
            code: casl2::A::Lad,
            r: reg,
            adr: casl2::Adr::Dec(value as i16),
            x: None,
        });

        reg
    }

    // (式展開の処理の一部)
    // 真理値変数の読み込み
    fn compile_variable_boolean(&mut self, var_name: &str) -> casl2::Register {
        let reg = self.get_idle_register();
        let var_label = self.bool_var_labels.get(var_name).expect("BUG");
        let adr = casl2::Adr::label(var_label);

        // LD REG,VAR
        self.code(casl2::Command::A {
            code: casl2::A::Ld,
            r: reg,
            adr,
            x: None,
        });

        reg
    }

    // (式展開の処理の一部)
    // 真理値リテラルの読み込み
    fn compile_literal_boolean(&mut self, value: bool) -> casl2::Register {
        let reg = self.get_idle_register();
        let value = if value { 0xFFFF } else { 0x0000 };

        // LAD REG,VALUE
        self.code(casl2::Command::A {
            code: casl2::A::Lad,
            r: reg,
            adr: casl2::Adr::Hex(value),
            x: None,
        });

        reg
    }

    // (式展開の処理の一部)
    // Function integer
    // 戻り値が整数の関数 (※引数の型とかは関数による…オーバーロードもあるか？)
    fn compile_function_integer(
        &mut self,
        func: tokenizer::Function,
        param: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Function::*;
        match func {
            Abs => self.call_function_abs(param),
            Asc => self.call_function_asc(param),
            CInt => self.call_function_cint(param),
            Len => self.call_function_len(param),
            Max => self.call_function_max(param),
            Min => self.call_function_min(param),
            CBool | Chr | CStr | Eof | Mid | Space => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // Min(<integer>,<integer>)の処理
    fn call_function_min(&mut self, param: &parser::Expr) -> casl2::Register {
        let list = if let parser::Expr::ParamList(list) = param {
            list
        } else {
            unreachable!("BUG");
        };

        let (lhs, rhs) = match list.as_slice() {
            [lhs, rhs]
                if matches!(lhs.return_type(), parser::ExprType::Integer)
                    && matches!(rhs.return_type(), parser::ExprType::Integer) =>
            {
                (lhs, rhs)
            }
            _ => unreachable!("BUG"),
        };

        let lhs_reg = self.compile_int_expr(lhs);

        let rhs_reg = self.compile_int_expr(rhs);

        self.restore_register(lhs_reg);

        let ok_label = self.get_new_jump_label();

        self.code(format!(
            r#" CPA  {lhs},{rhs}
                JMI  {ok}
                LD   {lhs},{rhs}
{ok}            NOP
"#,
            lhs = lhs_reg,
            rhs = rhs_reg,
            ok = ok_label
        ));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // Max(<integer>,<integer>)の処理
    fn call_function_max(&mut self, param: &parser::Expr) -> casl2::Register {
        let list = if let parser::Expr::ParamList(list) = param {
            list
        } else {
            unreachable!("BUG");
        };

        let (lhs, rhs) = match list.as_slice() {
            [lhs, rhs]
                if matches!(lhs.return_type(), parser::ExprType::Integer)
                    && matches!(rhs.return_type(), parser::ExprType::Integer) =>
            {
                (lhs, rhs)
            }
            _ => unreachable!("BUG"),
        };

        let lhs_reg = self.compile_int_expr(lhs);

        let rhs_reg = self.compile_int_expr(rhs);

        self.restore_register(lhs_reg);

        let ok_label = self.get_new_jump_label();

        self.code(format!(
            r#" CPA  {lhs},{rhs}
                JPL  {ok}
                LD   {lhs},{rhs}
{ok}            NOP
"#,
            lhs = lhs_reg,
            rhs = rhs_reg,
            ok = ok_label
        ));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // Asc<string>) の処理
    fn call_function_asc(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param.return_type(), parser::ExprType::String));

        let reg = self.get_idle_register();
        self.set_register_idle(reg);

        let labels = self.compile_str_expr(param);

        let ok = self.get_new_jump_label();

        self.set_register_used(reg);

        self.code(format!(
            r#" LD   {reg},{len}
                JZE  {ok}
                LD   {reg},{pos}
{ok}            NOP"#,
            reg = reg,
            len = labels.len,
            pos = labels.pos,
            ok = ok
        ));

        self.return_temp_str_var_label(labels);
        reg
    }

    // (式展開の処理の一部)
    // Abs<integer>) の処理
    fn call_function_abs(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param.return_type(), parser::ExprType::Integer));

        /*
            考察

            素で書くと4行1ラベル
                  LD    GR0,{reg}
                  JPL   {ok}
                  XOR   {reg},{reg}
                  SUBA  {reg},GR0
            {ok}  NOP

            サブルーチン利用だと2～5行0ラベル(大半3行)
                [ PUSH  0,GR1     ]
                [ LD    GR1,{reg} ]
                  CALL  {abs}
                [ POP   GR1       ]
                  LD    {reg},GR0
            Absサブルーチン6行2ラベル
            {abs} LD    GR0,GR1
                  JMI   {mi}
                  RET
            {mi}  XOR   GR0,GR0
                  SUBA  GR0,GR1
                  RET

            6~7回以上でチャラだが、普通そんなにAbsを多用せんだろJK...
            (4 * 1 =  4 [1*1=1], 3 * 1 + 6 =  9 [2])
            (4 * 2 =  8 [1*2=2], 3 * 2 + 6 = 12 [2])
            (4 * 3 = 12 [1*3=3], 3 * 3 + 6 = 15 [2])
            (4 * 4 = 16 [1*4=4], 3 * 4 + 6 = 18 [2])
            (4 * 5 = 20 [1*5=5], 3 * 5 + 6 = 21 [2])
            (4 * 6 = 24 [1*6=6], 3 * 6 + 6 = 24 [2])
            (4 * 7 = 28 [1*7=7], 3 * 7 + 6 = 27 [2])
        */

        let reg = self.compile_int_expr(param);

        let ok_label = self.get_new_jump_label();

        self.code(format!(
            r#" LD    GR0,{reg}
                JPL   {ok}
                XOR   {reg},{reg}
                SUBA  {reg},GR0
{ok}            NOP
"#,
            reg = reg,
            ok = ok_label
        ));

        reg
    }

    // (式展開の処理の一部)
    // Len(<string>) の処理
    fn call_function_len(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param.return_type(), parser::ExprType::String));

        let reg = self.get_idle_register();
        self.set_register_idle(reg);

        let str_labels = self.compile_str_expr(param);

        self.set_register_used(reg);

        self.code(format!(" LD {reg},{len}", reg = reg, len = str_labels.len));

        self.return_temp_str_var_label(str_labels);

        reg
    }

    // (式展開の処理の一部)
    // CInt(<boolean>/<string>) の処理
    fn call_function_cint(&mut self, param: &parser::Expr) -> casl2::Register {
        match param.return_type() {
            parser::ExprType::Boolean => self.compile_int_expr(param),
            parser::ExprType::String => {
                let ret_reg = self.get_idle_register();
                self.set_register_idle(ret_reg);
                let arg_str = self.compile_str_expr(param);
                let cint = self.load_subroutine(subroutine::Id::FuncCInt);
                // レジスタの退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    let mut regs = vec![Gr1, Gr2];
                    regs.retain(|r| *r != ret_reg);
                    self.get_save_registers_src(&regs)
                };
                self.code(saves);
                self.code(format!(
                    r#" LAD   GR1,{strpos}
                        LD    GR2,{strlen}
                        CALL  {cint}"#,
                    strpos = arg_str.pos,
                    strlen = arg_str.len,
                    cint = cint
                ));
                self.code(recovers);
                self.code(format!(" LD {reg},GR0", reg = ret_reg));
                self.return_temp_str_var_label(arg_str);
                self.set_register_used(ret_reg);
                ret_reg
            }
            parser::ExprType::Integer | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // call function
    //    2 arguments (int/bool)
    //    1 return (int/bool)
    // レジスタに載るサイズの引数が２つ (GR1,GR2)
    // レジスタに載る戻り値１つ (GR0)
    //  の関数の処理をサブルーチンへ橋渡しする処理
    fn call_function_2_int_args_int_ret(
        &mut self,
        param: &parser::Expr,
        id: subroutine::Id,
    ) -> casl2::Register {
        let list = match param {
            parser::Expr::ParamList(list) if list.len() == 2 => list,
            _ => unreachable!("BUG"),
        };
        let (lhs, rhs) = match list.as_slice() {
            [lhs, rhs] => (lhs, rhs),
            _ => unreachable!("BUG"),
        };
        self.call_subroutine_2_int_args_int_ret(lhs, rhs, id)
    }

    // (式展開の処理の一部)
    // call subroutine
    //    2 arguments (int/bool)
    //    1 return (int/bool)
    // レジスタに載るサイズの引数が２つ (GR1,GR2)
    // レジスタに載る戻り値１つ (GR0)
    // 　のサブルーチンを呼び出す処理を加える
    fn call_subroutine_2_int_args_int_ret(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
        id: subroutine::Id,
    ) -> casl2::Register {
        let lhs_reg = self.compile_int_expr(lhs);
        let rhs_reg = self.compile_int_expr(rhs);

        assert_ne!(lhs_reg, rhs_reg); // たぶん、大丈夫…

        self.restore_register(lhs_reg);

        let sub_label = self.load_subroutine(id);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);

        self.code(format!(
            r#" PUSH  0,{rhs}
                LD    GR1,{lhs}
                POP   GR2
                CALL  {sub}"#,
            lhs = lhs_reg,
            rhs = rhs_reg,
            sub = sub_label
        ));

        self.code(recovers);

        self.code(format!(" LD {lhs},GR0", lhs = lhs_reg));

        self.set_register_idle(rhs_reg);
        lhs_reg
    }
}

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
fn remove_nop(statements: &[casl2::Statement]) -> Vec<casl2::Statement> {
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
fn split_subroutines(
    program_name: &str,
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
        let (head, tail) = label.as_str().split_at(1);
        if !matches!(head, "C") || !tail.chars().all(|ch| ch.is_ascii_digit()) {
            continue;
        }
        let label = label.as_str().to_string();
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

    ret.push((program_name.to_string(), statements));

    ret
}
