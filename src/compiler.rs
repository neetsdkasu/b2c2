use crate::casl2;
use crate::parser;
use crate::tokenizer;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt::Write;

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

    // 式展開時などの一時変数のID
    temp_int_var_id: usize,
    temp_str_var_id: usize,

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
    fn get_temp_str_var_label(&mut self) -> (String, String) {
        if let Some(labels) = self.temp_str_var_labels.pop() {
            return labels;
        }
        self.temp_str_var_id += 1;
        let t_len = format!("TL{}", self.temp_str_var_id);
        let t_buf = format!("TB{}", self.temp_str_var_id);
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

    // サブルーチンのソースコードを登録する
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
                &format!("Dim {} As Boolean", var_name),
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
                &format!("Dim {} As Integer", var_name),
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
                &format!("Dim {} As String", var_name),
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
                &format!("Dim {}({}) As Boolean", var_name, size - 1),
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
                &format!("Dim {}({}) As Integer", var_name, size - 1),
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
            if literal.is_empty() {
                statements.push(casl2::Statement::labeled(
                    &buf_label,
                    casl2::Command::Ds { size: 0 },
                ));
            } else {
                statements.push(casl2::Statement::labeled(
                    &buf_label,
                    casl2::Command::Dc {
                        constants: vec![casl2::Constant::Str(literal.clone())],
                    },
                ));
            }
        }

        // END ステートメント
        statements.push(casl2::Statement::code(casl2::Command::End));

        statements
    }

    // ステートメントをコンパイルする
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
            For { step: None, .. } => self.compile_for_with_literal_step(stmt, 1),
            For {
                step: Some(parser::Expr::LitInteger(step)),
                ..
            } => self.compile_for_with_literal_step(stmt, *step),
            For { .. } => self.compile_for(stmt),
            If {
                condition: _,
                block: _,
                else_blocks: _,
            } => todo!(),
            SelectInteger {
                exit_id,
                value,
                case_blocks,
            } => self.compile_select_integer(*exit_id, value, case_blocks),
            SelectString {
                exit_id: _,
                value: _,
                case_blocks: _,
            } => todo!(),
            InputElementInteger {
                var_name: _,
                index: _,
            } => todo!(),
            InputInteger { var_name } => self.compile_input_integer(var_name),
            InputString { var_name } => self.compile_input_string(var_name),
            PrintLitBoolean { value } => self.compile_print_lit_boolean(*value),
            PrintLitInteger { value } => self.compile_print_lit_integer(*value),
            PrintLitString { value } => self.compile_print_lit_string(value),
            PrintVarString { var_name } => self.compile_print_var_string(var_name),
            PrintExprBoolan { value: _ } => todo!(),
            PrintExprInteger { value: _ } => todo!(),
            PrintExprString { value: _ } => todo!(),

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

    // Assign Integer ステートメント
    // int_var = int_expr
    fn compile_assign_integer(&mut self, var_name: &str, value: &parser::Expr) {
        self.next_statement_comment =
            Some(format!("{var} = {value}", var = var_name, value = value));
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

        self.next_statement_comment = Some(format!(
            "For {counter} = {init} To {end} Step {step}",
            counter = counter,
            init = init,
            end = end,
            step = step
        ));

        // calc {end}
        let end_var = self.get_temp_int_var_label();
        let end_reg = self.compile_expr(end);
        self.statements
            .push(casl2::Statement::code(casl2::Command::A {
                code: casl2::A::St,
                r: end_reg,
                adr: casl2::Adr::label(&end_var),
                x: None,
            }));
        self.set_register_idle(end_reg);

        // カウンタの準備
        let counter_var = self.int_var_labels.get(counter).expect("BUG").clone();

        // calc {init} and assign to {counter}
        let init_reg = self.compile_expr(init);
        self.statements
            .push(casl2::Statement::code(casl2::Command::A {
                code: casl2::A::St,
                r: init_reg,
                adr: casl2::Adr::label(&counter_var),
                x: None,
            }));
        self.set_register_idle(init_reg);

        // ラベルの準備
        let condition_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        // ループ継続の判定部分

        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::GR1));

        let mut condition_src = String::new();

        writeln!(&mut condition_src, "{cond} NOP", cond = condition_label).unwrap();

        condition_src.push_str(&saves);

        writeln!(
            &mut condition_src,
            r#" LD    GR1,{counter}
                CPA   GR1,{end}"#,
            counter = counter_var,
            end = end_var
        )
        .unwrap();

        condition_src.push_str(&recovers);

        if step < 0 {
            writeln!(&mut condition_src, " JMI {exit}", exit = exit_label).unwrap();
        } else {
            writeln!(&mut condition_src, " JPL {exit}", exit = exit_label).unwrap();
        }

        let condition_code = casl2::parse(&condition_src).unwrap();

        self.statements.extend(condition_code);

        // ループ内のコードを実行
        for stmt in block.iter() {
            self.compile(stmt);
        }

        // ループ末尾 (カウンタの更新など)

        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::GR1));

        let mut tail_src = String::new();

        writeln!(&mut tail_src, "{next} NOP", next = loop_label).unwrap();

        tail_src.push_str(&saves);

        writeln!(
            &mut tail_src,
            r#" LD    GR1,{counter}
                LAD   GR1,{step},GR1
                ST    GR1,{counter}"#,
            counter = counter_var,
            step = step
        )
        .unwrap();

        tail_src.push_str(&recovers);

        writeln!(
            &mut tail_src,
            " JUMP {cond} ; Next {counter}",
            cond = condition_label,
            counter = counter
        )
        .unwrap();
        writeln!(&mut tail_src, "{exit} NOP", exit = exit_label).unwrap();

        let tail_code = casl2::parse(&tail_src).unwrap();

        self.statements.extend(tail_code);

        self.return_temp_int_var_label(end_var);
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

        self.next_statement_comment = Some(format!(
            "For {counter} = {init} To {end} Step {step}",
            counter = counter,
            init = init,
            end = end,
            step = step
        ));

        // calc {step}
        let step_var = self.get_temp_int_var_label();
        let step_reg = self.compile_expr(step);
        self.statements
            .push(casl2::Statement::code(casl2::Command::A {
                code: casl2::A::St,
                r: step_reg,
                adr: casl2::Adr::label(&step_var),
                x: None,
            }));
        self.set_register_idle(step_reg);

        // calc {end}
        let end_var = self.get_temp_int_var_label();
        let end_reg = self.compile_expr(end);
        self.statements
            .push(casl2::Statement::code(casl2::Command::A {
                code: casl2::A::St,
                r: end_reg,
                adr: casl2::Adr::label(&end_var),
                x: None,
            }));
        self.set_register_idle(end_reg);

        // カウンタの準備
        let counter_var = self.int_var_labels.get(counter).expect("BUG").clone();

        // calc {init} and assign to {counter}
        let init_reg = self.compile_expr(init);
        self.statements
            .push(casl2::Statement::code(casl2::Command::A {
                code: casl2::A::St,
                r: init_reg,
                adr: casl2::Adr::label(&counter_var),
                x: None,
            }));
        self.set_register_idle(init_reg);

        // ラベルの準備
        let condition_label = self.get_new_jump_label();
        let negastep_label = self.get_new_jump_label();
        let blockhead_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        // ループ継続の判定部分

        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::GR1));

        let mut condition_src = String::new();

        writeln!(&mut condition_src, "{cond} NOP", cond = condition_label).unwrap();

        condition_src.push_str(&saves);

        writeln!(
            &mut condition_src,
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
        )
        .unwrap();

        condition_src.push_str(&recovers);

        writeln!(&mut condition_src, " JPL {exit}", exit = exit_label).unwrap();

        let condition_code = casl2::parse(&condition_src).unwrap();

        self.statements.extend(condition_code);

        // ループ内のコードを実行
        for stmt in block.iter() {
            self.compile(stmt);
        }

        // ループ末尾 (カウンタの更新など)

        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::GR1));

        let mut tail_src = String::new();

        writeln!(&mut tail_src, "{next} NOP", next = loop_label).unwrap();

        tail_src.push_str(&saves);

        writeln!(
            &mut tail_src,
            r#" LD    GR1,{counter}
                ADDA  GR1,{step}
                ST    GR1,{counter}"#,
            counter = counter_var,
            step = step_var
        )
        .unwrap();

        tail_src.push_str(&recovers);

        writeln!(
            &mut tail_src,
            " JUMP {cond} ; Next {counter}",
            cond = condition_label,
            counter = counter
        )
        .unwrap();
        writeln!(&mut tail_src, "{exit} NOP", exit = exit_label).unwrap();

        let tail_code = casl2::parse(&tail_src).unwrap();

        self.statements.extend(tail_code);
        self.return_temp_int_var_label(end_var);
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

        self.next_statement_comment = Some(format!("Select Case {}", value));

        let value_reg = self.compile_expr(value);

        for (case_stmt, label) in case_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseInteger { values, .. } => {
                    let mut comment = Some(format!(
                        "Case {}",
                        values
                            .iter()
                            .map(|v| v.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                    for case_value in values.iter() {
                        use parser::CaseIntegerItem::*;
                        let adr = match case_value {
                            Integer(value) => casl2::Adr::LiteralDec(*value as i16),
                            Character(ch) => casl2::Adr::LiteralStr(ch.to_string()),
                        };
                        self.statements.push(casl2::Statement::Code {
                            label: None,
                            command: casl2::Command::A {
                                code: casl2::A::Cpa,
                                r: value_reg,
                                adr,
                                x: None,
                            },
                            comment: comment.take(),
                        });
                        self.statements
                            .push(casl2::Statement::code(casl2::Command::P {
                                code: casl2::P::Jze,
                                adr: casl2::Adr::label(label),
                                x: None,
                            }));
                    }
                }
                parser::Statement::CaseElse { .. } => {
                    self.statements.push(casl2::Statement::code_with_comment(
                        casl2::Command::P {
                            code: casl2::P::Jump,
                            adr: casl2::Adr::label(label),
                            x: None,
                        },
                        "Case Else",
                    ));
                }
                _ => unreachable!("BUG"),
            }
        }

        self.set_register_idle(value_reg);

        let exit_label = self.get_exit_label(exit_id);

        if !has_else {
            self.statements
                .push(casl2::Statement::code(casl2::Command::P {
                    code: casl2::P::Jump,
                    adr: casl2::Adr::label(&exit_label),
                    x: None,
                }));
        }

        for (case_stmt, label) in case_blocks.iter() {
            self.statements
                .push(casl2::Statement::labeled(&label, casl2::Command::Nop));
            match case_stmt {
                parser::Statement::CaseInteger { block, .. }
                | parser::Statement::CaseElse { block } => {
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                }
                _ => unreachable!("BUG"),
            }
            self.statements
                .push(casl2::Statement::code(casl2::Command::P {
                    code: casl2::P::Jump,
                    adr: casl2::Adr::label(&exit_label),
                    x: None,
                }));
        }

        let last_stmt = self.statements.last_mut().expect("BUG");
        assert_eq!(
            last_stmt,
            &casl2::Statement::code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            })
        );
        *last_stmt = casl2::Statement::labeled(&exit_label, casl2::Command::Nop);
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

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[GR1, GR2])
        };

        let var_label = self.int_var_labels.get(var_name).expect("BUG");

        let mut src = saves;

        writeln!(
            &mut src,
            r#" IN    {pos},{len}  ; {comment}
                LAD   GR1,{pos}
                LD    GR2,{len}
                CALL  {cint}"#,
            pos = s_pos,
            len = s_len,
            cint = cint_label,
            comment = format!("Input {}", var_name)
        )
        .unwrap();

        src.push_str(&recovers);

        writeln!(&mut src, " ST GR0,{var}", var = var_label).unwrap();

        let code = casl2::parse(&src).unwrap();

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

    // subrutine引数のレジスタなどの一時利用のとき
    // 一時退避するためのソースコードと復帰するためのソースコードを生成
    fn get_save_registers_src(&self, regs: &[casl2::Register]) -> (String, String) {
        let mut saves = String::new();
        let mut recovers = String::new();

        // next_statement_comment や next_statement_label をどうするか…

        for reg in regs.iter() {
            if !self.is_idle_register(*reg) {
                writeln!(&mut saves, " PUSH 0,{}", reg).unwrap();
                writeln!(&mut recovers, " POP {}", reg).unwrap();
            }
        }

        (saves, recovers)
    }

    // 式の展開
    fn compile_expr(&mut self, expr: &parser::Expr) -> casl2::Register {
        use parser::Expr::*;
        match expr {
            BinaryOperatorBoolean(_op, _lhs, _rhs) => todo!(),
            BinaryOperatorInteger(op, lhs, rhs) => self.compile_bin_op_integer(*op, lhs, rhs),
            BinaryOperatorString(_op, _lhs, _rhs) => todo!(),
            CharOfLitString(_lit_str, _index) => todo!(),
            CharOfVarString(_var_name, _index) => todo!(),
            FunctionBoolean(_func, _param) => todo!(),
            FunctionInteger(func, param) => self.compile_function_integer(func, param),
            FunctionString(_func, _param) => todo!(),
            LitBoolean(_lit_bool) => todo!(),
            LitInteger(lit_int) => self.compile_literal_integer(*lit_int),
            LitString(_lit_str) => todo!(),
            LitCharacter(_lit_char) => todo!(),
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
    // 整数を返す二項演算子の処理
    fn compile_bin_op_integer(
        &mut self,
        op: tokenizer::Operator,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        let lhs_reg = self.compile_expr(lhs);

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
            let code = casl2::parse(&src).unwrap();
            self.statements.extend(code);
            return lhs_reg;
        }

        // rhsを計算する場合

        let rhs_reg = self.compile_expr(rhs);

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

        let code = casl2::parse(&src).unwrap();

        self.statements.extend(code);

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
        let mul_label = self.load_subroutine(subroutine::ID::UtilMul);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[GR1, GR2, GR3])
        };

        let mut src = saves;

        writeln!(
            &mut src,
            r#" PUSH  0,{rhs}
                LD    GR2,{lhs}
                POP   GR3
                CALL  {mul}"#,
            rhs = rhs_reg,
            lhs = lhs_reg,
            mul = mul_label
        )
        .unwrap();

        src.push_str(&recovers);

        writeln!(&mut src, " LD {lhs},GR0", lhs = lhs_reg).unwrap();

        let code = casl2::parse(&src).unwrap();

        self.statements.extend(code);

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
        let divmod_label = self.load_subroutine(subroutine::ID::UtilDivMod);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[GR1, GR2, GR3])
        };

        let mut src = saves;

        writeln!(
            &mut src,
            r#" PUSH  0,{rhs}
                LD    GR2,{lhs}
                POP   GR3
                CALL  {divmod}"#,
            rhs = rhs_reg,
            lhs = lhs_reg,
            divmod = divmod_label
        )
        .unwrap();

        src.push_str(&recovers);

        writeln!(&mut src, " LD {lhs},GR0", lhs = lhs_reg).unwrap();

        let code = casl2::parse(&src).unwrap();

        self.statements.extend(code);

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
        let divmod_label = self.load_subroutine(subroutine::ID::UtilDivMod);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[GR1, GR2, GR3])
        };

        let mut src = saves;

        writeln!(
            &mut src,
            r#" PUSH  0,{rhs}
                LD    GR2,{lhs}
                POP   GR3
                CALL  {divmod}
                LD    GR0,GR1"#,
            rhs = rhs_reg,
            lhs = lhs_reg,
            divmod = divmod_label
        )
        .unwrap();

        src.push_str(&recovers);

        writeln!(&mut src, " LD {lhs},GR0", lhs = lhs_reg).unwrap();

        let code = casl2::parse(&src).unwrap();

        self.statements.extend(code);

        self.set_register_idle(rhs_reg);

        lhs_reg
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
            Len => todo!(),
            Max => self.call_function_2_int_args_int_ret(param, subroutine::ID::FuncMax),
            Min => self.call_function_2_int_args_int_ret(param, subroutine::ID::FuncMin),
            CBool | CStr => unreachable!("BUG"),
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
        let lhs_reg = self.compile_expr(lhs);
        let rhs_reg = self.compile_expr(rhs);

        assert_ne!(lhs_reg, rhs_reg); // たぶん、大丈夫…

        self.restore_register(lhs_reg);

        let sub_label = self.load_subroutine(id);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[GR1, GR2])
        };

        let mut src = saves;

        writeln!(
            &mut src,
            r#" PUSH  0,{rhs}
                LD    GR1,{lhs}
                POP   GR2
                CALL  {sub}"#,
            lhs = lhs_reg,
            rhs = rhs_reg,
            sub = sub_label
        )
        .unwrap();

        src.push_str(&recovers);

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
        FuncAbs,
        FuncCInt,
        FuncMax,
        FuncMin,
        UtilDivMod,
        UtilMul,
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

    // サブルーチンのソースコードと依存関係を取得
    pub fn get_src<T: Gen>(gen: &mut T, id: ID) -> Src {
        match id {
            ID::FuncAbs => get_func_abs(gen, id),
            ID::FuncCInt => get_func_cint(gen, id),
            ID::FuncMax => get_func_max(gen, id),
            ID::FuncMin => get_func_min(gen, id),
            ID::UtilDivMod => get_util_div_mod(gen, id),
            ID::UtilMul => get_util_mul(gen, id),
        }
    }

    // Abs
    fn get_func_abs<T: Gen>(gen: &mut T, id: ID) -> Src {
        // GR1 .. value1
        // GR0 .. ret = abs(value1)
        Src {
            dependencies: Vec::new(),
            statements: casl2::parse(
                format!(
                    r#"
{prog} LD    GR0,GR1    ; {comment}
       JMI   {mi}
       RET
{mi}   SUBA  GR0,GR1
       SUBA  GR0,GR1
       RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    mi = gen.jump_label()
                )
                .trim_start_matches('\n'),
            )
            .unwrap(),
        }
    }

    // Max
    fn get_func_max<T: Gen>(gen: &mut T, id: ID) -> Src {
        // GR1 .. value1
        // GR2 .. value2
        // GR0 .. ret = max(value1, value2)
        Src {
            dependencies: Vec::new(),
            statements: casl2::parse(
                format!(
                    r#"
{prog} CPA   GR1,GR2    ; {comment}
       JMI   {mi}
       LD    GR0,GR1
       RET
{mi}   LD    GR0,GR2
       RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    mi = gen.jump_label()
                )
                .trim_start_matches('\n'),
            )
            .unwrap(),
        }
    }

    // Min
    fn get_func_min<T: Gen>(gen: &mut T, id: ID) -> Src {
        // GR1 .. value1
        // GR2 .. value2
        // GR0 .. ret = min(value1, value2)
        Src {
            dependencies: Vec::new(),
            statements: casl2::parse(
                format!(
                    r#"
{prog} CPA   GR1,GR2    ; {comment}
       JMI   {mi}
       LD    GR0,GR2
       RET
{mi}   LD    GR0,GR1
       RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    mi = gen.jump_label()
                )
                .trim_start_matches('\n'),
            )
            .unwrap(),
        }
    }

    // CInt
    fn get_func_cint<T: Gen>(gen: &mut T, id: ID) -> Src {
        // GR1 .. adr of s_buf
        // GR2 .. s_len
        // GR0 .. ret
        Src {
            dependencies: Vec::new(),
            statements: casl2::parse(
                format!(
                    r#"
{prog} PUSH  0,GR1    ; {comment}
       PUSH  0,GR2
       PUSH  0,GR3
       PUSH  0,GR4
       PUSH  0,GR5
       ADDL  GR2,GR1
       XOR   GR0,GR0
       XOR   GR4,GR4
       CPL   GR1,GR2
       JZE   {ret}
       LD    GR3,0,GR1
       CPL   GR3,='+'
       JNZ   {mi}
       LAD   GR1,1,GR1
       JUMP  {read}
{mi}   CPL   GR3,='-'
       JNZ   {read}
       LAD   GR4,-1
       LAD   GR1,1,GR1
{read} CPL   GR1,GR2
       JZE   {ret}
       LD    GR3,0,GR1
       SUBL  GR3,='0'
       JMI   {ret}
       CPL   GR3,=9
       JPL   {ret}
       LD    GR5,GR0
       SLL   GR0,3
       ADDL  GR0,GR5
       ADDL  GR0,GR5
       ADDL  GR0,GR3
       LAD   GR1,1,GR1
       JUMP  {read}
{ret}  XOR   GR0,GR4
       SUBL  GR0,GR4
       POP   GR5
       POP   GR4
       POP   GR3
       POP   GR2
       POP   GR1
       RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    ret = gen.jump_label(),
                    read = gen.jump_label(),
                    mi = gen.jump_label()
                )
                .trim_start_matches('\n'),
            )
            .unwrap(),
        }
    }

    // Div Mod
    fn get_util_div_mod<T: Gen>(gen: &mut T, id: ID) -> Src {
        // GR2 割られる数 (分子)
        // GR3 割る数 (分母)
        // GR0 商    = GR2 \ GR3
        // GR1 余り   = GR2 Mod GR3
        Src {
            dependencies: vec![ID::FuncAbs, ID::UtilMul],
            statements: casl2::parse(
                format!(
                    r#"
{prog}  AND   GR3,GR3     ; {comment}
        JNZ   {ok}
        XOR   GR0,GR0
        LAD   GR1,-1
        RET
{ok}    PUSH  0,GR2
        PUSH  0,GR3
        PUSH  0,GR4
        PUSH  0,GR5
        LD    GR4,GR2     ; GR4: dividend
        LD    GR1,GR2
        CALL  {abs}
        LD    GR5,GR0     ; GR5: Abs(dividend)
        LD    GR1,GR3
        CALL  {abs}
        LD    GR1,GR0     ; GR1: Abs(divisor)
        LAD   GR0,1
{shift} ADDL  GR1,GR1
        JOV   {pre}
        ADDL  GR0,GR0
        JUMP  {shift}
{pre}   SRL   GR1,1
        LAD   GR1,#8000,GR1
        XOR   GR2,GR2     ; GR2: Abs(quotient)
{cycle} CPL   GR5,GR1
        JMI   {next}
        SUBL  GR5,GR1
        ADDL  GR2,GR0
{next}  SRL   GR0,1
        JZE   {ret}
        SRL   GR1,1
        JUMP  {cycle}
{ret}   LD    GR5,GR4
        XOR   GR5,GR3
        SRA   GR5,15      ; GR5: sign of quotient
        XOR   GR2,GR5
        SUBA  GR2,GR5     ; GR2: quotient
        CALL  {mul}
        LD    GR1,GR4
        SUBA  GR1,GR0     ; GR1: remainder
        LD    GR0,GR2     ; GR0: quotient
        POP   GR5
        POP   GR4
        POP   GR3
        POP   GR2
        RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    abs = ID::FuncAbs.label(),
                    mul = ID::UtilMul.label(),
                    ok = gen.jump_label(),
                    shift = gen.jump_label(),
                    pre = gen.jump_label(),
                    cycle = gen.jump_label(),
                    next = gen.jump_label(),
                    ret = gen.jump_label()
                )
                .trim_start_matches('\n'),
            )
            .unwrap(),
        }
    }

    // Mul
    fn get_util_mul<T: Gen>(gen: &mut T, id: ID) -> Src {
        // GR2 * GR3
        // GR0 積の下位16ビット  (GR2 * GR3) & 0x0000FFFF
        // GR1 積の上位16ビット ((GR2 * GR3) & 0xFFFF0000) >> 16
        Src {
            dependencies: vec![ID::FuncAbs, ID::UtilMul],
            statements: casl2::parse(
                format!(
                    r#"
{prog}   PUSH  0,GR2     ; {comment}
         PUSH  0,GR3
         PUSH  0,GR4
         PUSH  0,GR5
         XOR   GR0,GR0
         XOR   GR1,GR1
         LD    GR4,GR2
         LD    GR5,GR3
{cycle1} SRL   GR2,1
         JOV   {add1}
         JNZ   {next1}
         JUMP  {cycle2}
{add1}   ADDL  GR0,GR3
         JOV   {raise1}
         JUMP  {next1}
{raise1} LAD   GR1,1,GR1
{next1}  SLL   GR3,1
         JUMP  {cycle1}
{cycle2} SRL   GR5,1
         SLL   GR4,1
         JOV   {add2}
         JNZ   {cycle2}
         JUMP  {ret}
{add2}   ADDL  GR1,GR5
         JUMP  {cycle2}
{ret}    POP   GR5
         POP   GR4
         POP   GR3
         POP   GR2
         RET
"#,
                    prog = id.label(),
                    comment = format!("{:?}", id),
                    cycle1 = gen.jump_label(),
                    add1 = gen.jump_label(),
                    raise1 = gen.jump_label(),
                    next1 = gen.jump_label(),
                    cycle2 = gen.jump_label(),
                    add2 = gen.jump_label(),
                    ret = gen.jump_label()
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
            Rem TEST PROGRAM
            Dim bool1 As Boolean
            Dim int1 As Integer
            Dim str1 As String
            Dim i As Integer
            Dim j As Integer
            Input int1
            Input str1
            Print False
            Print 1234
            Print "Text"
            ' Print bool1
            ' Print int1
            Print str1
            ' Print 1 + 2 + 3 + int1
            Let int1 = (1 + int1) + 2
            Let int1 = (1 - int1) - 2
            Let int1 = (1 << int1) << 2
            Let int1 = (1 >> int1) >> 2
            Let int1 = (1 And int1) And 2
            Let int1 = (1 Or int1) Or 2
            Let int1 = (1 Xor int1) Xor 2
            Let int1 = (1 * int1) * 2
            Let int1 = (1 \ int1) \ 2
            Let int1 = (1 Mod int1) Mod 2
            ' Let int1 = - (-int1 + -1)
            ' Let int1 = Not (Not int1 + Not 1)
            ' Let int1 = Len(str1)
            ' Let int1 = CInt(bool1)
            ' Let int1 = CInt(str1)
            ' Let bool1 = CBool(int1)
            ' Let str1 = CStr(bool1)
            ' Let str1 = CStr(int1)
            ' Let str1 = "prifix" & (str1 & "suffix")
            For i = 1 To 10
                Print "X"
            Next i
            For i = int1 To int1 Step -5
                Print "X"
            Next i
            For i = int1 - 3 To (int1 << 4) - 2 Step int1 + 1
                Print "X"
            Next i
            For i = 1 To 10
                For j = 1 To 10
                    Print "X"
                Next j
            Next i
            Select Case i
            Case 1, 2
                Print "X"
            Case 3, 4, "A"C, "B"C, 55
                Print "X"
            Case Else
                Print "X"
            End Select
            Select Case i
            Case 10, 11, 12
                Print "X"
            End Select
            Select Case i + 10
            Case Else
                Print "X"
            End Select
            Select Case i * i
            End Select
            Select i
            Case 1, 2, 3
                Print "X"
            End Select
            Select i << 1
            Case 1, 2, 4
                Print "X"
            Case Else
                Print "X"
            End Select
        "#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("TEST", &code[..]).unwrap();

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
            casl2::parse(
                r#"
TEST   START
       RET
LL1    DC     4
LB1    DC     '-123'
LL2    DC     5
LB2    DC     'A b c'
LL3    DC     3
LB3    DC     'XYZ'
LL4    DC     9
LB4    DC     'Test@1234'
       END
            "#
                .trim()
            )
            .unwrap()
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
            casl2::parse(
                r#"
TEST   START
       RET
B2     DS     1     ; Dim boolVar1 As Boolean
B5     DS     1     ; Dim boolVar2 As Boolean
I3     DS     1     ; Dim intVar2 As Integer
I8     DS     1     ; Dim intVar1 As Integer
SL1    DS     1     ; Dim strVar1 As String
SB1    DS     256
SL6    DS     1     ; Dim strVar2 As String
SB6    DS     256
BA4    DS     32    ; Dim boolArr1(31) As Boolean
IA7    DS     155   ; Dim intArr1(154) As Integer
       END
            "#
                .trim()
            )
            .unwrap()
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
            casl2::parse(
                r#"
TEST   START
       OUT    LB1,LL1     ; Print True
       OUT    LB2,LL2     ; Print False
       OUT    LB2,LL2     ; Print False
       OUT    LB1,LL1     ; Print True
       RET
LL1    DC     4
LB1    DC     'True'
LL2    DC     5
LB2    DC     'False'
       END
            "#
                .trim()
            )
            .unwrap()
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
            casl2::parse(
                r#"
TEST   START
       OUT    LB1,LL1     ; Print 1234
       OUT    LB2,LL2     ; Print 999
       OUT    LB3,LL3     ; Print -100
       OUT    LB1,LL1     ; Print 1234
       RET
LL1    DC     4
LB1    DC     '1234'
LL2    DC     3
LB2    DC     '999'
LL3    DC     4
LB3    DC     '-100'
       END
            "#
                .trim()
            )
            .unwrap()
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
            casl2::parse(
                r#"
TEST   START
       OUT    LB1,LL1     ; Print "ABCD"
       OUT    LB2,LL2     ; Print "hey you!"
       OUT    LB3,LL3     ; Print ""
       OUT    LB1,LL1     ; Print "ABCD"
       RET
LL1    DC     4
LB1    DC     'ABCD'
LL2    DC     8
LB2    DC     'hey you!'
LL3    DC     0
LB3    DS     0
       END
            "#
                .trim()
            )
            .unwrap()
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
            casl2::parse(
                r#"
TEST   START
       OUT    SB3,SL3     ; Print strVar3
       OUT    SB2,SL2     ; Print strVar2
       OUT    SB1,SL1     ; Print strVar1
       RET
SL1    DS     1           ; Dim strVar1 As String
SB1    DS     256
SL2    DS     1           ; Dim strVar2 As String
SB2    DS     256
SL3    DS     1           ; Dim strVar3 As String
SB3    DS     256
       END
            "#
                .trim()
            )
            .unwrap()
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
            casl2::parse(
                r#"
TEST   START
       IN     SB3,SL3     ; Input strVar3
       IN     SB2,SL2     ; Input strVar2
       IN     SB1,SL1     ; Input strVar1
       RET
SL1    DS     1           ; Dim strVar1 As String
SB1    DS     256
SL2    DS     1           ; Dim strVar2 As String
SB2    DS     256
SL3    DS     1           ; Dim strVar3 As String
SB3    DS     256
       END
            "#
                .trim()
            )
            .unwrap()
        );
    }

    #[test]
    fn compiler_compile_input_integer_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_dim("intVar1", &parser::VarType::Integer);

        compiler.compile_input_integer("intVar1");

        assert_eq!(compiler.subroutine_codes.len(), 1);
        assert_eq!(compiler.temp_str_var_labels.len(), 1);

        struct T {
            v: Vec<&'static str>,
        }

        impl subroutine::Gen for T {
            fn jump_label(&mut self) -> String {
                self.v.pop().unwrap().to_string()
            }
            fn var_label(&mut self) -> String {
                unreachable!()
            }
        }

        let id = subroutine::ID::FuncCInt;
        let mut t = T {
            v: vec!["J3", "J2", "J1"],
        };

        let mut statements = casl2::parse(
            format!(
                r#"
TEST   START
       IN     TB1,TL1     ; Input intVar1
       LAD    GR1,TB1
       LD     GR2,TL1
       CALL   {}
       ST     GR0,I1
       RET
            "#,
                id.label()
            )
            .trim(),
        )
        .unwrap();

        statements.extend(subroutine::get_src(&mut t, id).statements);

        statements.extend(
            casl2::parse(
                r#"
I1     DS     1           ; Dim intVar1 As Integer
TL1    DS     1
TB1    DS     256
       END
            "#
                .trim(),
            )
            .unwrap(),
        );

        assert_ne!(compiler.finish(), statements);
    }

    #[test]
    fn for_statement_without_step_works() {
        let src = r#"
            Dim i As Integer
            For i = 1 To 10
            Next i
        "#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("TEST", &code[..]).unwrap();

        assert_eq!(
            statements,
            casl2::parse(
                r#"TEST  START
                     LAD    GR7,10     ; For i = 1 To 10 Step 1
                     ST     GR7,T1
                     LAD    GR7,1
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,T1
                     JPL    J3
J2                   NOP
                     LD     GR1,I1
                     LAD    GR1,1,GR1
                     ST     GR1,I1
                     JUMP   J1         ; Next i
J3                   NOP
                     RET
I1                   DS 1              ; Dim i As Integer
T1                   DS 1
                     END
"#
            )
            .unwrap()
        );
    }

    #[test]
    fn for_statement_positive_step_works() {
        let src = r#"
            Dim i As Integer
            For i = 1 To 10 Step 1
            Next i
        "#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("TEST", &code[..]).unwrap();

        assert_eq!(
            statements,
            casl2::parse(
                r#"TEST  START
                     LAD    GR7,10     ; For i = 1 To 10 Step 1
                     ST     GR7,T1
                     LAD    GR7,1
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,T1
                     JPL    J3
J2                   NOP
                     LD     GR1,I1
                     LAD    GR1,1,GR1
                     ST     GR1,I1
                     JUMP   J1         ; Next i
J3                   NOP
                     RET
I1                   DS 1              ; Dim i As Integer
T1                   DS 1
                     END
"#
            )
            .unwrap()
        );
    }

    #[test]
    fn for_statement_negative_step_works() {
        let src = r#"
            Dim i As Integer
            For i = 24 To 8 Step -2
            Next i
        "#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("TEST", &code[..]).unwrap();

        assert_eq!(
            statements,
            casl2::parse(
                r#"TEST  START
                     LAD    GR7,8      ; For i = 24 To 8 Step -2
                     ST     GR7,T1
                     LAD    GR7,24
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,T1
                     JMI    J3
J2                   NOP
                     LD     GR1,I1
                     LAD    GR1,-2,GR1
                     ST     GR1,I1
                     JUMP   J1         ; Next i
J3                   NOP
                     RET
I1                   DS 1              ; Dim i As Integer
T1                   DS 1
                     END
"#
            )
            .unwrap()
        );
    }

    #[test]
    fn for_statement_expr_step_works() {
        let src = r#"
            Dim S As Integer
            Dim I As Integer
            For I = 1 To 10 Step S
            Next I
        "#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("TEST", &code[..]).unwrap();

        assert_eq!(
            statements,
            casl2::parse(
                r#"TEST  START
                     LD     GR7,I1     ; For I = 1 To 10 Step S
                     ST     GR7,T1
                     LAD    GR7,10
                     ST     GR7,T2
                     LAD    GR7,1
                     ST     GR7,I2
J1                   NOP
                     LD     GR1,T1
                     JMI    J2
                     LD     GR1,I2
                     CPA    GR1,T2
                     JUMP   J3
J2                   LD     GR1,T2
                     CPA    GR1,I2
J3                   NOP
                     JPL    J5
J4                   NOP
                     LD     GR1,I2
                     ADDA   GR1,T1
                     ST     GR1,I2
                     JUMP   J1         ; Next I
J5                   NOP
                     RET
I1                   DS 1              ; Dim S As Integer
I2                   DS 1              ; Dim I As Integer
T1                   DS 1
T2                   DS 1
                     END
"#
            )
            .unwrap()
        );
    }

    #[test]
    fn expr_add_literal_int_rhs_works() {
        let src = r#"
            Dim x As Integer
            x = 11 + 22
        "#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("TEST", &code[..]).unwrap();

        assert_eq!(
            statements,
            casl2::parse(
                r#"TEST  START
                     LAD    GR7,11     ; x = (11 + 22)
                     LAD    GR7,22,GR7
                     ST     GR7,I1
                     RET
I1                   DS 1              ; Dim x As Integer
                     END
"#
            )
            .unwrap()
        );
    }

    #[test]
    fn expr_add_variable_rhs_works() {
        let src = r#"
            Dim x As Integer
            Dim y As Integer
            x = 11 + y
        "#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("TEST", &code[..]).unwrap();

        assert_eq!(
            statements,
            casl2::parse(
                r#"TEST  START
                     LAD    GR7,11     ; x = (11 + y)
                     LD     GR6,I2
                     ADDA   GR7,GR6
                     ST     GR7,I1
                     RET
I1                   DS 1              ; Dim x As Integer
I2                   DS 1              ; Dim y As Integer
                     END
"#
            )
            .unwrap()
        );
    }

    #[test]
    fn fizzbuzz_1_works() {
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
'            Print i
    End Select
Next i
"#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("FIZZBUZZ", &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        assert!(!statements.is_empty()); // dummy assert
    }

    #[test]
    fn fizzbuzz_2_works() {
        let src = r#"
Dim s As String
Dim n As Integer
'Do
    Print "Number?"
    Input s
'    If s = "end" Then
'        Exit Do
'    End If
'    n = CInt(s)
'    If n < 1 Then
        Print "Invalid Input"
'        Continue Do
'    End If
'    If n Mod 15 = 0 Then
'        s = "FizzBuzz"
'    ElseIf n Mod 3 = 0 Then
'        s = "Fizz"
'    ElseIf n Mod 5 = 0 Then
'        s = "Buzz"
'    End If
    Print s
'Loop
"#;

        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile("FIZZBUZZ", &code[..]).unwrap();

        assert!(!statements.is_empty()); // dummy assert
    }
}
