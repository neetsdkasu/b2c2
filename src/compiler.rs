use crate::casl2;
use crate::parser;
use crate::tokenizer;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

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

struct Compiler {
    // ソース定義の変数のID (真理値/整数/文字列の全体で一意)
    var_id: usize,

    // IN/OUTで使用する文字列定数のID
    lit_id: usize,

    // 組み込みサブルーチンのローカル変数・定数のID (名前重複を防ぐが目的) (DS/DCは各サブルーチン内で定義する)
    local_var_id: usize,

    // 式展開時などの一時変数のID (真理値/整数/文字列の全体で一意)
    temp_var_id: usize,

    // ループや条件分岐に使うジャンプ先ラベルのID (全体で一意)
    jump_id: usize,

    // 真理値変数のラベル対応を保持 (変数名, ラベル)
    bool_var_labels: HashMap<String, String>,

    // 整数変数のラベル対応を保持 (変数名, ラベル)
    int_var_labels: HashMap<String, String>,

    // 文字列変数のラベル対応を保持 (変数名, (長さラベル, 内容位置ラベル))
    str_var_labels: HashMap<String, (String, String)>,

    // 真理値配列のラベル対応を保持 (配列名, (ラベル, 配列サイズ))
    bool_arr_labels: HashMap<String, (String, usize)>,

    // 整数配列のラベル対応を保持 (配列名, (ラベル, 配列サイズ))
    int_arr_labels: HashMap<String, (String, usize)>,

    // IN/OUTで使用する文字列定数のラベル対応を保持 (文字列定数, (長さラベル, 内容位置ラベル)))
    lit_str_labels: HashMap<String, (String, String)>,

    // 式展開時の一時変数(整数/真理値)のラベルを保持 (スタック的利用) (ラベル) (主にForループの終点とステップで使用)
    temp_int_var_labels: Vec<String>,

    // 式展開時の一時変数(文字列)のラベルを保持 (スタック的利用) (長さラベル, 内容位置ラベル)
    temp_str_var_labels: Vec<(String, String)>,

    // 組み込みサブルーチンのコードを保持 (サブルーチンの固有名, コード)
    subroutine_codes: HashMap<subroutine::ID, Vec<casl2::Statement>>,

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
    registers_deque: VecDeque<casl2::Register>,

    // ループや条件分岐の脱出先のコードにラベルをつけるためその脱出ラベルを保持
    next_statement_label: Option<casl2::Label>,

    // 次のcasl2ステートメントにコメントをつける
    next_statement_comment: Option<String>,

    // 生成するCASL2コード本体
    statements: Vec<casl2::Statement>,
}

impl Compiler {
    // プログラム名のラベルとしての正当性チェック
    fn is_valid_program_name(program_name: &str) -> bool {
        casl2::Label::from(program_name).is_valid()

        // 自動生成のラベルとの重複を避けるチェックが必要
        // B** 真理値変数
        // I** 整数変数
        // T** 式展開時の一時変数(真理値/整数で共有)(主にForループの終点とステップで使用)
        // V** 組み込みサブルーチンのローカル変数
        // J** ループや条件分岐に使うジャンプ先ラベルのID
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
            temp_var_id: 0,
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
            registers_deque: VecDeque::with_capacity(7),
            next_statement_label: None,
            next_statement_comment: None,
            statements: vec![casl2::Statement::labeled(
                program_name,
                casl2::Command::Start { entry_point: None },
            )],
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

    // 式展開時の一時変数(整数/真理値)のラベル取得・生成
    fn get_temp_int_var_label(&mut self) -> String {
        if let Some(label) = self.temp_int_var_labels.pop() {
            return label;
        }
        self.temp_var_id += 1;
        format!("T{}", self.temp_var_id)
    }

    // 式展開時の一時変数(整数/真理値)のラベルの返却
    fn return_temp_int_var_label(&mut self, label: String) {
        self.temp_int_var_labels.push(label);
    }

    // 式展開時の一時変数(文字列)のラベル取得・生成
    fn get_temp_str_var_label(&mut self) -> (String, String) {
        if let Some(labels) = self.temp_str_var_labels.pop() {
            return labels;
        }
        self.temp_var_id += 1;
        let t_len = format!("TL{}", self.temp_var_id);
        let t_buf = format!("TB{}", self.temp_var_id);
        (t_len, t_buf)
    }

    // 式展開時の一時変数(文字列)のラベルの返却
    fn return_temp_str_var_label(&mut self, labels: (String, String)) {
        self.temp_str_var_labels.push(labels);
    }

    // IN/OUTで使用する文字列定数のラベル生成
    fn get_lit_str_labels(&mut self, literal: &str) -> (String, String) {
        if let Some(labels) = self.lit_str_labels.get(literal) {
            return labels.clone();
        }
        self.lit_id += 1;
        let len_label = format!("LL{}", self.lit_id);
        let buf_label = format!("LB{}", self.lit_id);
        let labels = (len_label, buf_label);
        self.lit_str_labels.insert(literal.into(), labels.clone());
        labels
    }

    fn load_subroutine(&mut self, req_id: subroutine::ID) -> String {
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

    // コンパイル最終工程
    fn finish(self) -> Vec<casl2::Statement> {
        let Self {
            bool_var_labels,
            int_var_labels,
            str_var_labels,
            bool_arr_labels,
            int_arr_labels,
            lit_str_labels,
            temp_int_var_labels,
            temp_str_var_labels,
            next_statement_label,
            subroutine_codes,
            mut statements,
            ..
        } = self;

        // RET ステートメント
        statements.push(casl2::Statement::Code {
            label: next_statement_label,
            command: casl2::Command::Ret,
            comment: None,
        });

        // 組み込みサブルーチンのコード
        for (_, mut code) in subroutine_codes.into_iter().collect::<BTreeMap<_, _>>() {
            statements.append(&mut code);
        }

        // 真理値変数 B**
        for (label, var_name) in bool_var_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled_with_comment(
                &label,
                casl2::Command::Ds { size: 1 },
                &var_name,
            ));
        }

        // 整数変数 I**
        for (label, var_name) in int_var_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled_with_comment(
                &label,
                casl2::Command::Ds { size: 1 },
                &var_name,
            ));
        }

        // 文字列変数 SL** SB**
        for ((len_label, buf_label), var_name) in str_var_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled_with_comment(
                &len_label,
                casl2::Command::Ds { size: 1 },
                &var_name,
            ));
            statements.push(casl2::Statement::labeled(
                &buf_label,
                casl2::Command::Ds { size: 256 },
            ));
        }

        // 真理値配列(固定長) BA**
        for ((label, size), var_name) in bool_arr_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled_with_comment(
                &label,
                casl2::Command::Ds { size: size as u16 },
                &var_name,
            ));
        }

        // 整数配列(固定長) IA**
        for ((label, size), var_name) in int_arr_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled_with_comment(
                &label,
                casl2::Command::Ds { size: size as u16 },
                &var_name,
            ));
        }

        // 式展開等で使う一時変数(整数/真理値で共有) T**
        for label in temp_int_var_labels.into_iter().collect::<BTreeSet<_>>() {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: 1 },
            ));
        }

        // 式展開等で使う一時変数(文字列) TL** TB**
        for (len_label, buf_label) in temp_str_var_labels.into_iter().collect::<BTreeSet<_>>() {
            statements.push(casl2::Statement::labeled(
                &len_label,
                casl2::Command::Ds { size: 1 },
            ));
            statements.push(casl2::Statement::labeled(
                &buf_label,
                casl2::Command::Ds { size: 256 },
            ));
        }

        // IN/OUTで使用する文字列定数 LL** LB**
        for ((len_label, buf_label), literal) in lit_str_labels
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &len_label,
                casl2::Command::Dc {
                    constants: vec![casl2::Constant::Dec(literal.chars().count() as i16)],
                },
            ));
            statements.push(casl2::Statement::labeled(
                &buf_label,
                casl2::Command::Dc {
                    constants: vec![casl2::Constant::Str(literal.clone())],
                },
            ));
        }

        // END ステートメント
        statements.push(casl2::Statement::code(casl2::Command::End));

        statements
    }

    fn compile(&mut self, stmt: &parser::Statement) {
        use parser::Statement::*;
        match stmt {
            AssignAddInto {
                var_name: _,
                value: _,
            } => todo!(),
            AssignAddIntoElement {
                var_name: _,
                index: _,
                value: _,
            } => todo!(),
            AssignBoolean {
                var_name: _,
                value: _,
            } => todo!(),
            AssignElement {
                var_name: _,
                index: _,
                value: _,
            } => todo!(),
            AssignInteger { var_name, value } => self.compile_assign_integer(var_name, value),
            AssignString {
                var_name: _,
                value: _,
            } => todo!(),
            AssignSubInto {
                var_name: _,
                value: _,
            } => todo!(),
            AssignSubIntoElement {
                var_name: _,
                index: _,
                value: _,
            } => todo!(),
            ContinueDo { exit_id: _ } => todo!(),
            ContinueFor { exit_id: _ } => todo!(),
            Dim { var_name, var_type } => self.compile_dim(var_name, var_type),
            DoLoop {
                exit_id: _,
                block: _,
            } => todo!(),
            DoLoopUntil {
                exit_id: _,
                condition: _,
                block: _,
            } => todo!(),
            DoLoopWhile {
                exit_id: _,
                condition: _,
                block: _,
            } => todo!(),
            DoUntilLoop {
                exit_id: _,
                condition: _,
                block: _,
            } => todo!(),
            DoWhileLoop {
                exit_id: _,
                condition: _,
                block: _,
            } => todo!(),
            ExitDo { exit_id: _ } => todo!(),
            ExitFor { exit_id: _ } => todo!(),
            ExitSelect { exit_id: _ } => todo!(),
            For {
                exit_id: _,
                counter: _,
                init: _,
                end: _,
                step: _,
                block: _,
            } => todo!(),
            If {
                condition: _,
                block: _,
                else_blocks: _,
            } => todo!(),
            ElseIf {
                condition: _,
                block: _,
            } => todo!(),
            Else { block: _ } => todo!(),
            SelectInteger {
                exit_id: _,
                value: _,
                case_blocks: _,
            } => todo!(),
            CaseInteger {
                values: _,
                block: _,
            } => todo!(),
            SelectString {
                exit_id: _,
                value: _,
                case_blocks: _,
            } => todo!(),
            CaseString {
                values: _,
                block: _,
            } => todo!(),
            CaseElse { block: _ } => todo!(),
            InputElementInteger {
                var_name: _,
                index: _,
            } => todo!(),
            InputInteger { var_name } => self.compile_input_integer(var_name),
            InputString { var_name } => self.compile_input_string(var_name),
            PrintLitBoolean { value } => self.compile_print_lit_boolean(*value),
            PrintLitInteger { value } => self.compile_print_lit_integer(*value),
            PrintLitString { value } => self.compile_print_lit_string(value),
            PrintVarBoolean { var_name: _ } => todo!(),
            PrintVarInteger { var_name: _ } => todo!(),
            PrintVarString { var_name } => self.compile_print_var_string(var_name),
            PrintExprBoolan { value: _ } => todo!(),
            PrintExprInteger { value: _ } => todo!(),
            PrintExprString { value: _ } => todo!(),

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

    // Assign Integer ステートメント
    // int_var = int_expr
    fn compile_assign_integer(&mut self, var_name: &str, value: &parser::Expr) {
        self.next_statement_comment = Some(format!("{} = {{value}}", var_name));
        let reg = self.compile_expr(value);
        let var_label = self.int_var_labels.get(var_name).expect("BUG");

        self.statements
            .push(casl2::Statement::code(casl2::Command::A {
                code: casl2::A::St,
                r: reg,
                adr: casl2::Adr::label(var_label),
                x: None,
            }));

        self.set_register_idle(reg);
    }

    // Dim ステートメント
    fn compile_dim(&mut self, var_name: &str, var_type: &parser::VarType) {
        use parser::VarType;
        self.var_id += 1;
        match var_type {
            VarType::Boolean => {
                let label = format!("B{}", self.var_id);
                self.bool_var_labels.insert(var_name.into(), label);
            }
            VarType::Integer => {
                let label = format!("I{}", self.var_id);
                self.int_var_labels.insert(var_name.into(), label);
            }
            VarType::String => {
                let len_label = format!("SL{}", self.var_id);
                let buf_label = format!("SB{}", self.var_id);
                let labels = (len_label, buf_label);
                self.str_var_labels.insert(var_name.into(), labels);
            }
            VarType::ArrayOfBoolean(size) => {
                let label = format!("BA{}", self.var_id);
                self.bool_arr_labels.insert(var_name.into(), (label, *size));
            }
            VarType::ArrayOfInteger(size) => {
                let label = format!("IA{}", self.var_id);
                self.int_arr_labels.insert(var_name.into(), (label, *size));
            }
        }
    }

    // Input ステートメント
    // 整数変数へのコンソール入力
    fn compile_input_integer(&mut self, var_name: &str) {
        let cint_label = self.load_subroutine(subroutine::ID::FuncCInt);
        let s_labels = self.get_temp_str_var_label();
        let (ref s_pos, ref s_len) = &s_labels;
        let var_label = self.int_var_labels.get(var_name).expect("BUG");

        let code = casl2::parse(
            format!(
                r#"
    IN {pos},{len}  ; {comment}
    LAD GR1,{pos}
    LD GR2,{len}
    CALL {cint}
    ST GR0,{var}
"#,
                pos = s_pos,
                len = s_len,
                cint = cint_label,
                var = var_label,
                comment = format!("Input {}", var_name)
            )
            .trim_start_matches('\n'),
        )
        .unwrap();

        self.statements.extend(code);

        self.return_temp_str_var_label(s_labels);
    }

    // Input ステートメント
    // 文字列変数へのコンソール入力
    fn compile_input_string(&mut self, var_name: &str) {
        let label = self.next_statement_label.take();
        let (len_label, buf_label) = self.str_var_labels.get(var_name).expect("BUG");
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::In {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: Some(format!("Input {}", var_name)),
        });
    }

    // Print ステートメント
    // 真理値リテラルの画面出力
    fn compile_print_lit_boolean(&mut self, value: bool) {
        let label = self.next_statement_label.take();
        let s = if value { "True" } else { "False" };
        let (len_label, buf_label) = self.get_lit_str_labels(s);
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::Out {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: Some(format!("Print {}", s)),
        });
    }

    // Print ステートメント
    // 数字リテラルの画面出力
    fn compile_print_lit_integer(&mut self, value: i32) {
        let label = self.next_statement_label.take();
        let (len_label, buf_label) = self.get_lit_str_labels(&value.to_string());
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::Out {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: Some(format!("Print {}", value)),
        });
    }

    // Print ステートメント
    // 文字列リテラルの画面出力
    fn compile_print_lit_string(&mut self, value: &str) {
        let label = self.next_statement_label.take();
        let (len_label, buf_label) = self.get_lit_str_labels(value);
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::Out {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: Some(format!(r#"Print "{}""#, value.replace('"', r#""""#))),
        });
    }

    // Print ステートメント
    // 文字列変数の画面出力
    fn compile_print_var_string(&mut self, var_name: &str) {
        let label = self.next_statement_label.take();
        let (len_label, buf_label) = self.str_var_labels.get(var_name).expect("BUG");
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::Out {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: Some(format!("Print {}", var_name)),
        });
    }

    // レジスタのアイドル状態を取得
    fn is_idle_register(&self, reg: casl2::Register) -> bool {
        (self.registers_used & (1 << reg as isize)) == 0
    }

    // アイドル中のレジスタを取得
    fn get_idle_register(&mut self) -> casl2::Register {
        use casl2::Register::{self, *};
        use std::convert::TryFrom;
        const REG: [Register; 7] = [GR7, GR6, GR5, GR4, GR3, GR2, GR1];
        for &reg in REG.iter() {
            if self.is_idle_register(reg) {
                self.set_register_used(reg);
                return reg;
            }
        }
        self.registers_deque.rotate_left(1);
        let reg = *self.registers_deque.back().expect("BUG");
        self.statements
            .push(casl2::Statement::code(casl2::Command::P {
                code: casl2::P::Push,
                adr: casl2::Adr::Dec(0),
                x: Some(TryFrom::try_from(reg).expect("BUG")),
            }));
        reg
    }

    // アイドル化している場合にコールスタックに積まれてる値を戻す
    fn restore_register(&mut self, reg: casl2::Register) {
        if !self.is_idle_register(reg) {
            return;
        }
        self.set_register_used(reg);
        self.statements
            .push(casl2::Statement::code(casl2::Command::Pop { r: reg }));
    }

    // アイドル中のレスジスタを使用中に変更
    fn set_register_used(&mut self, reg: casl2::Register) {
        assert!(self.is_idle_register(reg));
        self.registers_used |= 1 << reg as isize;
        self.registers_deque.push_back(reg);
    }

    // 使用中のレジスタをアイドル扱いにする
    fn set_register_idle(&mut self, reg: casl2::Register) {
        assert!(!self.is_idle_register(reg));
        self.registers_used ^= 1 << reg as isize;
        let _popped = self.registers_deque.pop_back().expect("BUG");
        assert_eq!(_popped, reg);
    }

    // 式の展開
    fn compile_expr(&mut self, expr: &parser::Expr) -> casl2::Register {
        use parser::Expr::*;
        match expr {
            BinaryOperatorBoolean(_op, _lhs, _rhs) => todo!(),
            BinaryOperatorInteger(_op, _lhs, _rhs) => todo!(),
            BinaryOperatorString(_op, _lhs, _rhs) => todo!(),
            CharOfLitString(_lit_str, _index) => todo!(),
            CharOfVarString(_var_name, _index) => todo!(),
            FunctionBoolean(_func, _param) => todo!(),
            FunctionInteger(func, param) => self.compile_function_integer(func, param),
            FunctionString(_func, _param) => todo!(),
            LitBoolean(_lit_bool) => todo!(),
            LitInteger(lit_int) => self.compile_literal_integer(*lit_int),
            LitString(_lit_str) => todo!(),
            UnaryOperatorInteger(_op, _value) => todo!(),
            UnaryOperatorBoolean(_op, _value) => todo!(),
            VarBoolean(_var_name) => todo!(),
            VarInteger(var_name) => self.compile_variable_integer(var_name),
            VarString(_var_name) => todo!(),
            VarArrayOfBoolean(_arr_name, _index) => todo!(),
            VarArrayOfInteger(_arr_name, _index) => todo!(),
            ParamList(_) => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 整数変数の読み込み
    fn compile_variable_integer(&mut self, var_name: &str) -> casl2::Register {
        let label = self.next_statement_label.take();
        let comment = self.next_statement_comment.take();
        let reg = self.get_idle_register();
        let var_label = self.int_var_labels.get(var_name).expect("BUG");

        // LD REG,VAR
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::A {
                code: casl2::A::Ld,
                r: reg,
                adr: casl2::Adr::label(var_label),
                x: None,
            },
            comment,
        });

        reg
    }

    // (式展開の処理の一部)
    // 整数リテラルの読み込み
    fn compile_literal_integer(&mut self, value: i32) -> casl2::Register {
        let label = self.next_statement_label.take();
        let comment = self.next_statement_comment.take();
        let reg = self.get_idle_register();

        // LAD REG,VALUE
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::A {
                code: casl2::A::Lad,
                r: reg,
                adr: casl2::Adr::Dec(value as i16),
                x: None,
            },
            comment,
        });

        reg
    }

    // (式展開の処理の一部)
    // Function integer
    // 戻り値が整数の関数 (※引数の型とかは関数による…オーバーロードもあるか？)
    fn compile_function_integer(
        &mut self,
        func: &tokenizer::Function,
        param: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Function::*;
        match func {
            CInt => todo!(),
            Max => self.call_function_2_int_args_int_ret(param, subroutine::ID::FuncMax),
            Min => self.call_function_2_int_args_int_ret(param, subroutine::ID::FuncMin),
            CBool | CStr | Len => unreachable!("BUG"),
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
        id: subroutine::ID,
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
        id: subroutine::ID,
    ) -> casl2::Register {
        use std::fmt::Write;

        let lhs_reg = self.compile_expr(lhs);
        let rhs_reg = self.compile_expr(rhs);

        assert_ne!(lhs_reg, rhs_reg); // たぶん、大丈夫…

        self.restore_register(lhs_reg);

        let sub_label = self.load_subroutine(id);

        let mut src = String::new();

        if !self.is_idle_register(casl2::Register::GR1) {
            writeln!(&mut src, " PUSH 0,GR1").unwrap();
        }
        if !self.is_idle_register(casl2::Register::GR2) {
            writeln!(&mut src, " PUSH 0,GR2").unwrap();
        }

        writeln!(
            &mut src,
            r#"   LD GR1,{lhs}
                  LD GR2,{rhs}
                  CALL {sub}"#,
            lhs = lhs_reg,
            rhs = rhs_reg,
            sub = sub_label
        )
        .unwrap();

        if !self.is_idle_register(casl2::Register::GR2) {
            writeln!(&mut src, " PUSH 0,GR2").unwrap();
        }
        if !self.is_idle_register(casl2::Register::GR1) {
            writeln!(&mut src, " PUSH 0,GR1").unwrap();
        }

        writeln!(&mut src, " LD {lhs},GR0", lhs = lhs_reg).unwrap();

        let code = casl2::parse(&src).unwrap();

        self.statements.extend(code);

        self.set_register_idle(rhs_reg);
        lhs_reg
    }
}

impl subroutine::Gen for Compiler {
    fn var_label(&mut self) -> String {
        self.get_new_local_var_label()
    }

    fn jump_label(&mut self) -> String {
        self.get_new_jump_label()
    }
}

mod subroutine {
    use crate::casl2;

    #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
    pub enum ID {
        FuncCInt,
        FuncMax,
        FuncMin,
    }

    impl ID {
        pub fn label(&self) -> String {
            format!("C{}", *self as isize)
        }
    }

    pub struct Src {
        pub dependencies: Vec<ID>,
        pub statements: Vec<casl2::Statement>,
    }

    pub trait Gen {
        fn var_label(&mut self) -> String;
        fn jump_label(&mut self) -> String;
    }

    pub fn get_src<T: Gen>(gen: &mut T, id: ID) -> Src {
        match id {
            ID::FuncCInt => get_func_cint(gen),
            ID::FuncMax => get_func_max(gen),
            ID::FuncMin => get_func_min(gen),
        }
    }

    fn get_func_max<T: Gen>(gen: &mut T) -> Src {
        let id = ID::FuncMax;
        let mi_label = gen.jump_label();
        // GR1 .. value1
        // GR2 .. value2
        // GR0 .. ret = max(value1, value2)
        Src {
            dependencies: Vec::new(),
            statements: casl2::parse(
                format!(
                    r#"
{prog} CPA GR1,GR2    ; {comment}
       JMI {mi}
       LD GR0,GR1
       RET
{mi}   LD GR0,GR2
       RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    mi = mi_label
                )
                .trim_start_matches('\n'),
            )
            .unwrap(),
        }
    }

    fn get_func_min<T: Gen>(gen: &mut T) -> Src {
        let id = ID::FuncMin;
        let mi_label = gen.jump_label();
        // GR1 .. value1
        // GR2 .. value2
        // GR0 .. ret = min(value1, value2)
        Src {
            dependencies: Vec::new(),
            statements: casl2::parse(
                format!(
                    r#"
{prog} CPA GR1,GR2    ; {comment}
       JMI {mi}
       LD GR0,GR2
       RET
{mi}   LD GR0,GR1
       RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    mi = mi_label
                )
                .trim_start_matches('\n'),
            )
            .unwrap(),
        }
    }

    fn get_func_cint<T: Gen>(gen: &mut T) -> Src {
        let id = ID::FuncCInt;
        let mi_label = gen.jump_label();
        let read_label = gen.jump_label();
        let ret_label = gen.jump_label();
        // GR1 .. adr of s_buf
        // GR2 .. s_len
        // GR0 .. ret
        Src {
            dependencies: Vec::new(),
            statements: casl2::parse(
                format!(
                    r#"
{prog} PUSH 0,GR1    ; {comment}
       PUSH 0,GR2
       PUSH 0,GR3
       PUSH 0,GR4
       PUSH 0,GR5
       ADDL GR2,GR1
       XOR GR0,GR0
       XOR GR4,GR4
       CPL GR1,GR2
       JZE {ret}
       LD GR3,0,GR1
       CPL GR3,='+'
       JNZ {mi}
       LAD GR1,1,GR1
       JUMP {read}
{mi}   CPL GR3,='-'
       JNZ {read}
       LAD GR4,-1
       LAD GR1,1,GR1
{read} CPL GR1,GR2
       JZE {ret}
       LD GR3,0,GR1
       SUBL GR3,='0'
       JMI {ret}
       CPL GR3,=9
       JPL {ret}
       LD GR5,GR0
       SLL GR0,3
       ADDL GR0,GR5
       ADDL GR0,GR5
       ADDL GR0,GR3
       LAD GR1,1,GR1
       JUMP {read}
{ret}  XOR GR0,GR4
       SUBL GR0,GR4
       POP GR5
       POP GR4
       POP GR3
       POP GR2
       POP GR1
       RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    ret = ret_label,
                    read = read_label,
                    mi = mi_label
                )
                .trim_start_matches('\n'),
            )
            .unwrap(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_works() {
        let src = r#"
Dim i As Integer
Dim c As Integer
Print "Limit?"
Input c
c = Max(1, Min(100, c))
For i = 1 To c Step 1
   Select Case i Mod 15
       Case 0
           Print "FizzBuzz"
       Case 3, 6, 9, 12
           Print "Fizz"
       Case 5, 10
           Print "Buzz"
       Case Else
           Print i
   End Select
Next i
"#;
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("FIZZBUZZ", &code[..5]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        assert!(!statements.is_empty()); // dummy assert
    }

    #[test]
    fn compiler_is_valid_program_name_works() {
        assert!(Compiler::is_valid_program_name("TEST"));
        assert!(Compiler::is_valid_program_name("X123"));
        assert!(Compiler::is_valid_program_name("B"));
        assert!(Compiler::is_valid_program_name("B123XY"));

        assert!(!Compiler::is_valid_program_name("GR3")); // register name is BAD
        assert!(!Compiler::is_valid_program_name("")); // empty is BAD
        assert!(!Compiler::is_valid_program_name("FOOBARBAZ")); // too long (require len <= 8)
        assert!(!Compiler::is_valid_program_name("Test")); // lowercase is BAD
        assert!(!Compiler::is_valid_program_name("123TEST")); // digit start is BAD
        assert!(!Compiler::is_valid_program_name("TEST$")); // all chars must be ascii digits or ascii uppercases

        // compiler using names
        assert!(!Compiler::is_valid_program_name("B123"));
        assert!(!Compiler::is_valid_program_name("I123"));
        assert!(!Compiler::is_valid_program_name("SL123"));
        assert!(!Compiler::is_valid_program_name("SB123"));
        assert!(!Compiler::is_valid_program_name("BA123"));
        assert!(!Compiler::is_valid_program_name("IA123"));
    }

    #[test]
    fn compiler_get_lit_str_labels_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        assert_eq!(
            compiler.get_lit_str_labels("-123"),
            ("LL1".into(), "LB1".into())
        );
        assert_eq!(
            compiler.get_lit_str_labels("A b c"),
            ("LL2".into(), "LB2".into())
        );
        assert_eq!(
            compiler.get_lit_str_labels("XYZ"),
            ("LL3".into(), "LB3".into())
        );
        assert_eq!(
            compiler.get_lit_str_labels("Test@1234"),
            ("LL4".into(), "LB4".into())
        );
        assert_eq!(
            compiler.get_lit_str_labels("A b c"),
            ("LL2".into(), "LB2".into())
        );
        assert_eq!(
            compiler.get_lit_str_labels("XYZ"),
            ("LL3".into(), "LB3".into())
        );

        assert_eq!(
            compiler.finish(),
            vec![
                casl2::Statement::labeled("TEST", casl2::Command::Start { entry_point: None }),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled(
                    "LL1",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("-123".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB1",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("-123".into()),]
                    }
                ),
                casl2::Statement::labeled(
                    "LL2",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("A b c".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB2",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("A b c".into()),]
                    }
                ),
                casl2::Statement::labeled(
                    "LL3",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("XYZ".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB3",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("XYZ".into()),]
                    }
                ),
                casl2::Statement::labeled(
                    "LL4",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("Test@1234".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB4",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("Test@1234".into()),]
                    }
                ),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }

    #[test]
    fn compiler_compile_dim_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_dim("strVar1", &parser::VarType::String);
        compiler.compile_dim("boolVar1", &parser::VarType::Boolean);
        compiler.compile_dim("intVar2", &parser::VarType::Integer);
        compiler.compile_dim("boolArr1", &parser::VarType::ArrayOfBoolean(32));
        compiler.compile_dim("boolVar2", &parser::VarType::Boolean);
        compiler.compile_dim("strVar2", &parser::VarType::String);
        compiler.compile_dim("intArr1", &parser::VarType::ArrayOfInteger(155));
        compiler.compile_dim("intVar1", &parser::VarType::Integer);

        assert_eq!(
            compiler.finish(),
            vec![
                casl2::Statement::labeled("TEST", casl2::Command::Start { entry_point: None }),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled_with_comment(
                    "B2",
                    casl2::Command::Ds { size: 1 },
                    "boolVar1"
                ),
                casl2::Statement::labeled_with_comment(
                    "B5",
                    casl2::Command::Ds { size: 1 },
                    "boolVar2"
                ),
                casl2::Statement::labeled_with_comment(
                    "I3",
                    casl2::Command::Ds { size: 1 },
                    "intVar2"
                ),
                casl2::Statement::labeled_with_comment(
                    "I8",
                    casl2::Command::Ds { size: 1 },
                    "intVar1"
                ),
                casl2::Statement::labeled_with_comment(
                    "SL1",
                    casl2::Command::Ds { size: 1 },
                    "strVar1"
                ),
                casl2::Statement::labeled("SB1", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled_with_comment(
                    "SL6",
                    casl2::Command::Ds { size: 1 },
                    "strVar2"
                ),
                casl2::Statement::labeled("SB6", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled_with_comment(
                    "BA4",
                    casl2::Command::Ds { size: 32 },
                    "boolArr1"
                ),
                casl2::Statement::labeled_with_comment(
                    "IA7",
                    casl2::Command::Ds { size: 155 },
                    "intArr1"
                ),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }

    #[test]
    fn compiler_compile_print_lit_boolean_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_print_lit_boolean(true);
        compiler.compile_print_lit_boolean(false);
        compiler.compile_print_lit_boolean(false);
        compiler.compile_print_lit_boolean(true);

        assert_eq!(
            compiler.finish(),
            vec![
                casl2::Statement::labeled("TEST", casl2::Command::Start { entry_point: None }),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB1".into(),
                        len: "LL1".into()
                    },
                    "Print True"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB2".into(),
                        len: "LL2".into()
                    },
                    "Print False"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB2".into(),
                        len: "LL2".into()
                    },
                    "Print False"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB1".into(),
                        len: "LL1".into()
                    },
                    "Print True"
                ),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled(
                    "LL1",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("True".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB1",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("True".into()),]
                    }
                ),
                casl2::Statement::labeled(
                    "LL2",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("False".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB2",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("False".into()),]
                    }
                ),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }

    #[test]
    fn compiler_compile_print_lit_integer_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_print_lit_integer(1234);
        compiler.compile_print_lit_integer(999);
        compiler.compile_print_lit_integer(-100);
        compiler.compile_print_lit_integer(1234);

        assert_eq!(
            compiler.finish(),
            vec![
                casl2::Statement::labeled("TEST", casl2::Command::Start { entry_point: None }),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB1".into(),
                        len: "LL1".into()
                    },
                    "Print 1234"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB2".into(),
                        len: "LL2".into()
                    },
                    "Print 999"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB3".into(),
                        len: "LL3".into()
                    },
                    "Print -100"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB1".into(),
                        len: "LL1".into()
                    },
                    "Print 1234"
                ),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled(
                    "LL1",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("1234".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB1",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("1234".into()),]
                    }
                ),
                casl2::Statement::labeled(
                    "LL2",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("999".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB2",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("999".into()),]
                    }
                ),
                casl2::Statement::labeled(
                    "LL3",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("-100".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB3",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("-100".into()),]
                    }
                ),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }

    #[test]
    fn compiler_compile_print_lit_string_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_print_lit_string("ABCD");
        compiler.compile_print_lit_string("hey you!");
        compiler.compile_print_lit_string("");
        compiler.compile_print_lit_string("ABCD");

        assert_eq!(
            compiler.finish(),
            vec![
                casl2::Statement::labeled("TEST", casl2::Command::Start { entry_point: None }),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB1".into(),
                        len: "LL1".into()
                    },
                    r#"Print "ABCD""#
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB2".into(),
                        len: "LL2".into()
                    },
                    r#"Print "hey you!""#
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB3".into(),
                        len: "LL3".into()
                    },
                    r#"Print """#
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "LB1".into(),
                        len: "LL1".into()
                    },
                    r#"Print "ABCD""#
                ),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled(
                    "LL1",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("ABCD".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB1",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("ABCD".into()),]
                    }
                ),
                casl2::Statement::labeled(
                    "LL2",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("hey you!".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB2",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("hey you!".into()),]
                    }
                ),
                casl2::Statement::labeled(
                    "LL3",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Dec("".chars().count() as i16),]
                    }
                ),
                casl2::Statement::labeled(
                    "LB3",
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str("".into()),]
                    }
                ),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }

    #[test]
    fn compiler_compile_print_var_string_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_dim("strVar1", &parser::VarType::String);
        compiler.compile_dim("strVar2", &parser::VarType::String);
        compiler.compile_dim("strVar3", &parser::VarType::String);
        compiler.compile_print_var_string("strVar3");
        compiler.compile_print_var_string("strVar2");
        compiler.compile_print_var_string("strVar1");

        assert_eq!(
            compiler.finish(),
            vec![
                casl2::Statement::labeled("TEST", casl2::Command::Start { entry_point: None }),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "SB3".into(),
                        len: "SL3".into()
                    },
                    "Print strVar3"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "SB2".into(),
                        len: "SL2".into()
                    },
                    "Print strVar2"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::Out {
                        pos: "SB1".into(),
                        len: "SL1".into()
                    },
                    "Print strVar1"
                ),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled_with_comment(
                    "SL1",
                    casl2::Command::Ds { size: 1 },
                    "strVar1"
                ),
                casl2::Statement::labeled("SB1", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled_with_comment(
                    "SL2",
                    casl2::Command::Ds { size: 1 },
                    "strVar2"
                ),
                casl2::Statement::labeled("SB2", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled_with_comment(
                    "SL3",
                    casl2::Command::Ds { size: 1 },
                    "strVar3"
                ),
                casl2::Statement::labeled("SB3", casl2::Command::Ds { size: 256 }),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }

    #[test]
    fn compiler_compile_input_string_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_dim("strVar1", &parser::VarType::String);
        compiler.compile_dim("strVar2", &parser::VarType::String);
        compiler.compile_dim("strVar3", &parser::VarType::String);
        compiler.compile_input_string("strVar3");
        compiler.compile_input_string("strVar2");
        compiler.compile_input_string("strVar1");

        assert_eq!(
            compiler.finish(),
            vec![
                casl2::Statement::labeled("TEST", casl2::Command::Start { entry_point: None }),
                casl2::Statement::code_with_comment(
                    casl2::Command::In {
                        pos: "SB3".into(),
                        len: "SL3".into()
                    },
                    "Input strVar3"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::In {
                        pos: "SB2".into(),
                        len: "SL2".into()
                    },
                    "Input strVar2"
                ),
                casl2::Statement::code_with_comment(
                    casl2::Command::In {
                        pos: "SB1".into(),
                        len: "SL1".into()
                    },
                    "Input strVar1"
                ),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled_with_comment(
                    "SL1",
                    casl2::Command::Ds { size: 1 },
                    "strVar1"
                ),
                casl2::Statement::labeled("SB1", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled_with_comment(
                    "SL2",
                    casl2::Command::Ds { size: 1 },
                    "strVar2"
                ),
                casl2::Statement::labeled("SB2", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled_with_comment(
                    "SL3",
                    casl2::Command::Ds { size: 1 },
                    "strVar3"
                ),
                casl2::Statement::labeled("SB3", casl2::Command::Ds { size: 256 }),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }

    #[test]
    fn compiler_compile_input_integer_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_dim("intVar1", &parser::VarType::Integer);

        compiler.compile_input_integer("intVar1");

        assert_eq!(compiler.subroutine_codes.len(), 1);
        assert_eq!(compiler.temp_str_var_labels.len(), 1);

        let statements = compiler.finish();

        // statements.iter().for_each(|line| {
        // eprintln!("{}", line);
        // });

        assert!(!statements.is_empty()); // dummy assert
    }
}
