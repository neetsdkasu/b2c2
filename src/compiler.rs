use self::ext::*;
pub use self::utils::is_valid_program_name;
use crate::casl2;
use crate::parser;
use crate::tokenizer;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::TryFrom;
use std::fmt::Write;

mod assign_elem_stmt;
mod assign_stmt;
mod bool_arr_func;
mod bool_func;
mod cmp_bin_op;
mod do_stmt;
mod exprs;
mod ext;
mod fill_stmt;
mod for_stmt;
mod input_stmt;
mod int_arr_func;
mod int_bin_op;
mod int_func;
mod optimize;
mod print_stmt;
mod select_stmt;
mod stmts;
mod str_bin_op;
mod str_func;
pub mod subroutine;
mod utils;

#[cfg(test)]
mod test;

pub const MAX_ARRAY_SIZE: usize = 256;

type CompileError = String;

// 正しい src が来ることを前提とする (不正な src の判定は面倒くさい)
pub fn compile(
    program_name: Option<String>,
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

    pub program_name: Option<String>,
}

// 条件付き(?)コンパイル
pub fn compile_with_flag(
    src: &[parser::Statement],
    flag: Flag,
) -> Result<Vec<(String, Vec<casl2::Statement>)>, CompileError> {
    let mut statements = compile(flag.program_name.clone(), src)?;

    if flag.remove_comment {
        statements = optimize::remove_comment(&statements);
    }

    if flag.remove_nop {
        statements = optimize::remove_nop(&statements);
    }

    if flag.remove_unreferenced_label {
        statements = optimize::remove_unreferenced_label(&statements);
    }

    if !flag.split_subroutines {
        let name = statements
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
        return Ok(vec![(name, statements)]);
    }

    Ok(optimize::split_subroutines(statements))
}

// 文字列ラベルのタイプ判定に使う
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum StrLabelType {
    Const(String), // IN/OUTの定数 LB**/LL**
    Lit(String),   // リテラル ='abc'/=3
    Temp,          // 一時変数 TB**/TL**
    Var,           // 文字列変数 SB**/SL**
    ArgRef,        // 引数(参照型) ARG*/ARG*
    ArgVal,        // 引数 ARG*/ARG*
}

// 文字列のラベル
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
struct StrLabels {
    pos: String,
    len: String,
    label_type: StrLabelType,
}

// 配列参照
// (一時的な配列は一時的な文字列変数の文字領域(pos)を借用する(長さ領域(len)の値は使用しない))
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum ArrayLabel {
    TempArrayOfBoolean(StrLabels, usize),
    TempArrayOfInteger(StrLabels, usize),
    VarArrayOfBoolean(String, usize),
    VarArrayOfInteger(String, usize),
    VarRefArrayOfBoolean(String, usize),
    VarRefArrayOfInteger(String, usize),
}

struct Compiler {
    // プログラム名
    program_name: Option<String>,
    original_program_name: Option<String>,

    // プログラム引数リスト
    arguments: Vec<parser::ArgumentInfo>,

    // プログラム引数のラベル (真理値/整数/真理値配列/整数配列) (引数名, (ラベル, 引数情報))
    argument_labels: HashMap<String, (String, parser::ArgumentInfo)>,

    // プログラム引数のラベル (文字列) (引数名, (ラベル, 引数情報))
    str_argument_labels: HashMap<String, (StrLabels, parser::ArgumentInfo)>,

    // 外部プログラム呼び出し情報
    callables: HashMap<String, Vec<parser::ArgumentInfo>>,

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

    // プログラム実行前のレジスタを保持するかどうか
    option_restore_registers: bool,

    // プログラム開始時に変数領域を0で初期化するかどうか
    option_initialize_variables: bool,

    // EOF情報の保持場所が外部プログラムかどうか
    option_external_eof: bool,
}

// コンパイラの初期化
impl Compiler {
    // コンパイラの初期化
    fn new(program_name: Option<String>) -> Result<Self, CompileError> {
        if let Some(name) = program_name.as_ref() {
            if !is_valid_program_name(name) {
                return Err(format!("invalid Program Name: {}", name));
            }
        }

        Ok(Self {
            program_name,
            original_program_name: None,
            arguments: Vec::new(),
            argument_labels: HashMap::new(),
            str_argument_labels: HashMap::new(),
            callables: HashMap::new(),
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
            statements: Vec::new(),
            has_eof: false,
            var_total_size: 0,
            option_restore_registers: true,
            option_initialize_variables: true,
            option_external_eof: false,
        })
    }
}

// subroutine::Genの実装
impl subroutine::Gen for Compiler {
    fn var_label(&mut self) -> String {
        self.get_new_local_var_label()
    }

    fn jump_label(&mut self) -> String {
        self.get_new_jump_label()
    }
}

// 細かい関心の分離処理？
impl Compiler {
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
            label_type: StrLabelType::Const(literal.to_string()),
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

    // 整数変数のラベル取得
    fn get_int_var_label(&self, var_name: &str) -> String {
        self.int_var_labels
            .get(var_name)
            .cloned()
            .unwrap_or_else(|| {
                let (label, arg) = self.argument_labels.get(var_name).expect("BUG");
                assert_eq!(arg.var_name, var_name);
                assert!(matches!(arg.var_type, parser::VarType::Integer));
                label.clone()
            })
    }

    // 整数変数(参照型)のラベル取得
    fn get_ref_int_var_label(&self, var_name: &str) -> String {
        let (label, arg) = self.argument_labels.get(var_name).expect("BUG");
        assert_eq!(arg.var_name, var_name);
        assert!(matches!(arg.var_type, parser::VarType::RefInteger));
        label.clone()
    }

    // 真理値変数のラベル取得
    fn get_bool_var_label(&self, var_name: &str) -> String {
        self.bool_var_labels
            .get(var_name)
            .cloned()
            .unwrap_or_else(|| {
                let (label, arg) = self.argument_labels.get(var_name).expect("BUG");
                assert_eq!(arg.var_name, var_name);
                assert!(matches!(arg.var_type, parser::VarType::Boolean));
                label.clone()
            })
    }

    // 真理値変数(参照型)のラベル取得
    fn get_ref_bool_var_label(&self, var_name: &str) -> String {
        let (label, arg) = self.argument_labels.get(var_name).expect("BUG");
        assert_eq!(arg.var_name, var_name);
        assert!(matches!(arg.var_type, parser::VarType::RefBoolean));
        label.clone()
    }

    // 文字列変数のラベル取得
    fn get_str_var_labels(&self, var_name: &str) -> StrLabels {
        self.str_var_labels
            .get(var_name)
            .cloned()
            .unwrap_or_else(|| {
                let (labels, arg) = self.str_argument_labels.get(var_name).expect("BUG");
                assert_eq!(arg.var_name, var_name);
                assert!(matches!(arg.var_type, parser::VarType::String));
                assert!(matches!(labels.label_type, StrLabelType::ArgVal));
                labels.clone()
            })
    }

    // 文字列変数(参照型)のラベル取得
    fn get_ref_str_var_labels(&self, var_name: &str) -> StrLabels {
        let (labels, arg) = self.str_argument_labels.get(var_name).expect("BUG");
        assert_eq!(arg.var_name, var_name);
        assert!(matches!(arg.var_type, parser::VarType::RefString));
        assert!(matches!(labels.label_type, StrLabelType::ArgRef));
        labels.clone()
    }

    // 真理値配列のラベル取得
    fn get_bool_arr_label(&self, var_name: &str) -> (String, usize) {
        self.bool_arr_labels
            .get(var_name)
            .cloned()
            .unwrap_or_else(|| {
                let (label, arg) = self.argument_labels.get(var_name).expect("BUG");
                assert_eq!(arg.var_name, var_name);
                if let parser::VarType::ArrayOfBoolean(size) = arg.var_type {
                    assert!((1..=MAX_ARRAY_SIZE).contains(&size));
                    (label.clone(), size)
                } else {
                    unreachable!("BUG");
                }
            })
    }

    // 真理値配列(参照型)のラベル取得
    fn get_ref_bool_arr_label(&self, var_name: &str) -> (String, usize) {
        let (label, arg) = self.argument_labels.get(var_name).expect("BUG");
        assert_eq!(arg.var_name, var_name);
        if let parser::VarType::RefArrayOfBoolean(size) = arg.var_type {
            assert!((1..=MAX_ARRAY_SIZE).contains(&size));
            (label.clone(), size)
        } else {
            unreachable!("BUG");
        }
    }

    // 整数配列のラベル取得
    fn get_int_arr_label(&self, var_name: &str) -> (String, usize) {
        self.int_arr_labels
            .get(var_name)
            .cloned()
            .unwrap_or_else(|| {
                let (label, arg) = self.argument_labels.get(var_name).expect("BUG");
                assert_eq!(arg.var_name, var_name);
                if let parser::VarType::ArrayOfInteger(size) = arg.var_type {
                    assert!((1..=MAX_ARRAY_SIZE).contains(&size));
                    (label.clone(), size)
                } else {
                    unreachable!("BUG");
                }
            })
    }

    // 整数配列(参照型)のラベル取得
    fn get_ref_int_arr_label(&self, var_name: &str) -> (String, usize) {
        let (label, arg) = self.argument_labels.get(var_name).expect("BUG");
        assert_eq!(arg.var_name, var_name);
        if let parser::VarType::RefArrayOfInteger(size) = arg.var_type {
            assert!((1..=MAX_ARRAY_SIZE).contains(&size));
            (label.clone(), size)
        } else {
            unreachable!("BUG");
        }
    }
}

// コンパイル最終工程
impl Compiler {
    // コンパイル最終工程
    fn finish(mut self) -> Vec<casl2::Statement> {
        if self.var_total_size > 1 {
            self.load_subroutine(subroutine::Id::UtilFill);
        }
        let Self {
            program_name,
            arguments,
            argument_labels,
            str_argument_labels,
            callables,
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
            var_total_size,
            option_restore_registers,
            option_initialize_variables,
            option_external_eof,
            ..
        } = self;

        // プログラム終了ポイント
        statements.labeled("EXIT", casl2::Command::Nop);

        // プログラム開始時のレジスタの状態の復帰
        if option_restore_registers {
            statements.code(casl2::Command::Rpop);
        }

        // プログラムの終了
        statements.code(casl2::Command::Ret);

        // 引数の値を保持する領域の設定 ARG*
        for arg in arguments.iter() {
            statements.comment(arg.to_string());
            match arg.var_type {
                parser::VarType::Boolean
                | parser::VarType::RefBoolean
                | parser::VarType::Integer
                | parser::VarType::RefInteger
                | parser::VarType::RefArrayOfBoolean(_)
                | parser::VarType::RefArrayOfInteger(_) => {
                    let (label, _) = argument_labels.get(&arg.var_name).expect("BUG");
                    statements.labeled(label.clone(), casl2::Command::Ds { size: 1 });
                }
                parser::VarType::ArrayOfBoolean(size) | parser::VarType::ArrayOfInteger(size) => {
                    let (label, _) = argument_labels.get(&arg.var_name).expect("BUG");
                    statements.labeled(label.clone(), casl2::Command::Ds { size: size as u16 });
                }
                parser::VarType::String => {
                    let (labels, _) = str_argument_labels.get(&arg.var_name).expect("BUG");
                    statements.labeled(&labels.len, casl2::Command::Ds { size: 1 });
                    statements.labeled(&labels.pos, casl2::Command::Ds { size: 256 });
                }
                parser::VarType::RefString => {
                    let (labels, _) = str_argument_labels.get(&arg.var_name).expect("BUG");
                    statements.labeled(&labels.len, casl2::Command::Ds { size: 1 });
                    statements.labeled(&labels.pos, casl2::Command::Ds { size: 1 });
                }
            }
        }

        // 初期化が必要な変数領域の最初のラベル
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
        if has_eof && !option_external_eof {
            statements.labeled("EOF", casl2::Command::Ds { size: 1 });
        }

        // プログラム冒頭のコードをまとめる用
        let mut temp_statements = Vec::<casl2::Statement>::new();

        // プログラムの開始点(START)の設定
        if let Some(name) = program_name {
            temp_statements.code(casl2::Statement::Code {
                label: Some(name.into()),
                command: casl2::Command::Start { entry_point: None },
                comment: None,
            });
        } else {
            let mut name = "MAIN".to_string();
            if callables.contains_key(&name) {
                for i in 0.. {
                    name = format!("MAIN{}", i);
                    if !callables.contains_key(&name) {
                        break;
                    }
                }
            }
            temp_statements.code(casl2::Statement::Code {
                label: Some(name.into()),
                command: casl2::Command::Start { entry_point: None },
                comment: None,
            });
        }

        // プログラム開始時のレジスタの状態の保存
        if option_restore_registers {
            temp_statements.code(casl2::Command::Rpush);
        }

        // 引数であるレジスタの値の保存
        for arg in arguments.iter() {
            temp_statements.comment(format!("Argument {}", arg.var_name));
            match arg.var_type {
                parser::VarType::Boolean
                | parser::VarType::RefBoolean
                | parser::VarType::Integer
                | parser::VarType::RefInteger
                | parser::VarType::RefArrayOfBoolean(_)
                | parser::VarType::ArrayOfBoolean(_)
                | parser::VarType::ArrayOfInteger(_)
                | parser::VarType::RefArrayOfInteger(_) => {
                    let (label, _) = argument_labels.get(&arg.var_name).expect("BUG");
                    temp_statements.code(format!(
                        r#" ST {reg},{label}"#,
                        reg = arg.register1,
                        label = label
                    ));
                }
                parser::VarType::String | parser::VarType::RefString => {
                    let (labels, _) = str_argument_labels.get(&arg.var_name).expect("BUG");
                    temp_statements.code(format!(
                        r#" ST {reglen},{len}
                            ST {regpos},{pos}"#,
                        reglen = arg.register1,
                        len = labels.len,
                        regpos = arg.register2.expect("BUG"),
                        pos = labels.pos
                    ));
                }
            }
        }

        // ByValの配列/文字列の引数の中身コピー処理
        for arg in arguments.iter() {
            match arg.var_type {
                parser::VarType::Boolean
                | parser::VarType::RefBoolean
                | parser::VarType::Integer
                | parser::VarType::RefInteger
                | parser::VarType::RefArrayOfBoolean(_)
                | parser::VarType::RefArrayOfInteger(_)
                | parser::VarType::RefString => {}

                parser::VarType::ArrayOfBoolean(size) | parser::VarType::ArrayOfInteger(size) => {
                    temp_statements.comment(format!("Copy Into {}", arg.var_name));
                    let (label, _) = argument_labels.get(&arg.var_name).expect("BUG");
                    temp_statements.code(format!(
                        r#" LAD   GR1,{label}
                            LAD   GR2,{label}
                            LD    GR3,{label}
                            LAD   GR4,{size}
                            CALL  {copy}"#,
                        label = label,
                        size = size,
                        copy = subroutine::Id::UtilCopyStr.label()
                    ));
                }
                parser::VarType::String => {
                    temp_statements.comment(format!("Copy Into {}", arg.var_name));
                    let (labels, _) = str_argument_labels.get(&arg.var_name).expect("BUG");
                    temp_statements.code(format!(
                        r#" LAD   GR1,{pos}
                            LAD   GR2,{len}
                            LD    GR3,{pos}
                            LD    GR4,{len}
                            CALL  {copy}"#,
                        pos = labels.pos,
                        len = labels.len,
                        copy = subroutine::Id::UtilCopyStr.label()
                    ));
                }
            }
        }

        // 変数領域の初期化処理のコード
        if option_initialize_variables {
            match first_var_label {
                Some(label) if var_total_size == 1 => {
                    temp_statements.comment("Init Variable");
                    temp_statements.extend(
                        casl2::parse(&format!(
                            r#" XOR   GR0,GR0
                            ST    GR0,{label}"#,
                            label = label
                        ))
                        .unwrap(),
                    );
                }
                Some(label) => {
                    temp_statements.comment("Init Variables");
                    temp_statements.extend(
                        casl2::parse(&format!(
                            r#" LAD   GR1,{start}
                            XOR   GR2,GR2
                            LAD   GR3,{size}
                            CALL  {fill}"#,
                            start = label,
                            size = var_total_size,
                            fill = subroutine::Id::UtilFill.label()
                        ))
                        .unwrap(),
                    );
                }
                _ => {}
            }
        }

        // プログラム冒頭のコードをマージ
        temp_statements.extend(statements);
        let mut statements = temp_statements;

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

        // END ステートメントの挿入 (CASL2ソースコード末尾)
        statements.code(casl2::Command::End);

        statements
    }
}

// ステートメントをコンパイルする
impl Compiler {
    // ステートメントをコンパイルする
    fn compile(&mut self, stmt: &parser::Statement) {
        use parser::Statement::*;
        match stmt {
            CompileOption { option } => self.set_option(option),
            ProgramName { name } => self.compile_program_name(name),
            Argument { arguments } => self.compile_argument(arguments),
            Call { name, arguments } => self.compile_call_exterun_sub(name, arguments),
            ExitProgram => self.compile_exit_program(),
            ExternSub { name, arguments } => self.compile_extern_sub(name, arguments),
            AssignAddInto { var_name, value } => self.compile_assign_add_into(var_name, value),
            AssignRefAddInto { var_name, value } => {
                self.compile_assign_ref_add_into(var_name, value)
            }
            AssignAddIntoElement {
                var_name,
                index,
                value,
            } => self.compile_assign_add_into_element(var_name, index, value),
            AssignRefAddIntoElement {
                var_name,
                index,
                value,
            } => self.compile_assign_ref_add_into_element(var_name, index, value),
            AssignBoolean { var_name, value } => self.compile_assign_boolean(var_name, value),
            AssignRefBoolean { var_name, value } => {
                self.compile_assign_ref_boolean(var_name, value)
            }
            AssignBooleanElement {
                var_name,
                index,
                value,
            } => self.compile_assign_boolean_element(var_name, index, value),
            AssignRefBooleanElement {
                var_name,
                index,
                value,
            } => self.compile_assign_ref_boolean_element(var_name, index, value),
            AssignIntegerElement {
                var_name,
                index,
                value,
            } => self.compile_assign_integer_element(var_name, index, value),
            AssignRefIntegerElement {
                var_name,
                index,
                value,
            } => self.compile_assign_ref_integer_element(var_name, index, value),
            AssignCharacterElement {
                var_name,
                index,
                value,
            } => self.compile_assign_character_element(var_name, index, value),
            AssignRefCharacterElement {
                var_name,
                index,
                value,
            } => self.compile_assign_ref_character_element(var_name, index, value),
            AssignInteger { var_name, value } => self.compile_assign_integer(var_name, value),
            AssignRefInteger { var_name, value } => {
                self.compile_assign_ref_integer(var_name, value)
            }
            AssignString { var_name, value } => self.compile_assign_string(var_name, value),
            AssignRefString { var_name, value } => self.compile_assign_ref_string(var_name, value),
            AssignSubInto { var_name, value } => self.compile_assign_sub_into(var_name, value),
            AssignRefSubInto { var_name, value } => {
                self.compile_assign_ref_sub_into(var_name, value)
            }
            AssignSubIntoElement {
                var_name,
                index,
                value,
            } => self.compile_assign_sub_into_element(var_name, index, value),
            AssignRefSubIntoElement {
                var_name,
                index,
                value,
            } => self.compile_assign_ref_sub_into_element(var_name, index, value),
            AssignBooleanArray { var_name, value } => {
                self.compile_assign_boolean_array(var_name, value)
            }
            AssignRefBooleanArray { var_name, value } => {
                self.compile_assign_ref_boolean_array(var_name, value)
            }
            AssignIntegerArray { var_name, value } => {
                self.compile_assign_integer_array(var_name, value)
            }
            AssignRefIntegerArray { var_name, value } => {
                self.compile_assign_ref_integer_array(var_name, value)
            }
            ContinueDo { exit_id } => self.compile_continue_loop(*exit_id, "Do"),
            ContinueFor { exit_id } => self.compile_continue_loop(*exit_id, "For"),
            Dim { var_name, var_type } => self.compile_dim(var_name, var_type),
            Mid {
                var_name,
                var_is_ref,
                offset,
                length,
                value,
            } => self.compile_mid(var_name, *var_is_ref, offset, length, value),
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
            InputRefElementInteger { var_name, index } => {
                self.compile_input_ref_element_integer(var_name, index)
            }
            InputRefInteger { var_name } => self.compile_input_ref_integer(var_name),
            InputRefString { var_name } => self.compile_input_ref_string(var_name),
            PrintLitBoolean { value } => self.compile_print_lit_boolean(*value),
            PrintLitInteger { value } => self.compile_print_lit_integer(*value),
            PrintLitString { value } => self.compile_print_lit_string(value),
            PrintVarString { var_name } => self.compile_print_var_string(var_name),
            PrintExprBoolan { value } => self.compile_print_expr_boolean(value),
            PrintExprInteger { value } => self.compile_print_expr_integer(value),
            PrintExprString { value } => self.compile_print_expr_string(value),
            FillArrayOfBoolean { var_name, value } => {
                self.compile_fill_boolean_array(var_name, value)
            }
            FillRefArrayOfBoolean { var_name, value } => {
                self.compile_fill_ref_boolean_array(var_name, value)
            }
            FillArrayOfInteger { var_name, value } => {
                self.compile_fill_integer_array(var_name, value)
            }
            FillRefArrayOfInteger { var_name, value } => {
                self.compile_fill_ref_integer_array(var_name, value)
            }
            FillString { var_name, value } => self.compile_fill_string(var_name, value),
            FillRefString { var_name, value } => self.compile_fill_ref_string(var_name, value),

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
}

impl ArrayLabel {
    fn label(&self) -> String {
        use ArrayLabel::*;
        match self {
            TempArrayOfBoolean(labels, _) | TempArrayOfInteger(labels, _) => labels.pos.clone(),
            VarArrayOfBoolean(label, _)
            | VarArrayOfInteger(label, _)
            | VarRefArrayOfBoolean(label, _)
            | VarRefArrayOfInteger(label, _) => label.clone(),
        }
    }

    fn lad_pos(&self, reg: casl2::Register) -> String {
        use ArrayLabel::*;
        match self {
            TempArrayOfBoolean(labels, _) | TempArrayOfInteger(labels, _) => {
                format!(r#" LAD  {reg},{pos}"#, reg = reg, pos = labels.pos)
            }
            VarArrayOfBoolean(label, _) | VarArrayOfInteger(label, _) => {
                format!(r#" LAD  {reg},{pos}"#, reg = reg, pos = label)
            }
            VarRefArrayOfBoolean(label, _) | VarRefArrayOfInteger(label, _) => {
                format!(r#" LD   {reg},{pos}"#, reg = reg, pos = label)
            }
        }
    }

    fn size(&self) -> usize {
        use ArrayLabel::*;
        match self {
            TempArrayOfBoolean(_, size)
            | TempArrayOfInteger(_, size)
            | VarArrayOfBoolean(_, size)
            | VarArrayOfInteger(_, size)
            | VarRefArrayOfBoolean(_, size)
            | VarRefArrayOfInteger(_, size) => *size,
        }
    }

    fn release(self) -> Option<StrLabels> {
        use ArrayLabel::*;
        match self {
            TempArrayOfBoolean(labels, _) | TempArrayOfInteger(labels, _) => Some(labels),
            VarArrayOfBoolean(..)
            | VarArrayOfInteger(..)
            | VarRefArrayOfBoolean(..)
            | VarRefArrayOfInteger(..) => None,
        }
    }

    fn element_type(&self) -> parser::ExprType {
        use ArrayLabel::*;
        match self {
            TempArrayOfBoolean(..) | VarArrayOfBoolean(..) | VarRefArrayOfBoolean(..) => {
                parser::ExprType::Boolean
            }
            TempArrayOfInteger(..) | VarArrayOfInteger(..) | VarRefArrayOfInteger(..) => {
                parser::ExprType::Integer
            }
        }
    }
}

impl StrLabels {
    fn lad_pos(&self, reg: casl2::Register) -> String {
        match &self.label_type {
            StrLabelType::Const(_)
            | StrLabelType::Lit(_)
            | StrLabelType::Temp
            | StrLabelType::Var
            | StrLabelType::ArgVal => {
                format!(r#" LAD {reg},{pos}"#, reg = reg, pos = self.pos)
            }
            StrLabelType::ArgRef => {
                format!(r#" LD {reg},{pos}"#, reg = reg, pos = self.pos)
            }
        }
    }

    fn lad_len(&self, reg: casl2::Register) -> String {
        match &self.label_type {
            StrLabelType::Const(_)
            | StrLabelType::Lit(_)
            | StrLabelType::Temp
            | StrLabelType::Var
            | StrLabelType::ArgVal => {
                format!(r#" LAD {reg},{len}"#, reg = reg, len = self.len)
            }
            StrLabelType::ArgRef => {
                format!(r#" LD {reg},{len}"#, reg = reg, len = self.len)
            }
        }
    }

    fn ld_len(&self, reg: casl2::Register) -> String {
        match &self.label_type {
            StrLabelType::Const(s) | StrLabelType::Lit(s) => {
                if s.is_empty() {
                    format!(r#" XOR {reg},{reg}"#, reg = reg)
                } else {
                    format!(r#" LAD {reg},{len}"#, reg = reg, len = s.chars().count())
                }
            }
            StrLabelType::Temp | StrLabelType::Var | StrLabelType::ArgVal => {
                format!(r#" LD {reg},{len}"#, reg = reg, len = self.len)
            }
            StrLabelType::ArgRef => {
                format!(
                    r#" LD {reg},{len}
                        LD {reg},0,{reg}"#,
                    reg = reg,
                    len = self.len
                )
            }
        }
    }
}
