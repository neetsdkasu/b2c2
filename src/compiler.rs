use crate::casl2;
use crate::parser;
use std::collections::{BTreeSet, HashMap};

type CompileError = String;

// 正しい src が来ることを前提とする (不正な src の判定は面倒くさい)
pub fn compile(
    program_name: &str,
    src: Vec<parser::Statement>,
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

    // 組み込みサブルーチンのローカル変数のID (真理値は整数として扱う)
    local_int_var_id: usize,

    // 組み込みサブルーチンのローカル変数のID (文字列のみ)
    local_str_var_id: usize,

    // 式展開時などの一時変数のID (真理値/整数/文字列の全体で一意)
    temp_var_id: usize,

    // ループや条件分岐に使うジャンプ先ラベルのID (全体で一意)
    jump_id: usize,

    // 組み込みサブルーチンの入り口のラベルID
    call_id: usize,

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

    // 式展開時の一時変数(整数/真理値)のラベルを保持 (スタック的利用) (ラベル)
    temp_int_var_labels: Vec<String>,

    // 式展開時の一時変数(文字列)のラベルを保持 (スタック的利用) (長さラベル, 内容位置ラベル)
    temp_str_var_labels: Vec<(String, String)>,

    // 組み込みサブルーチンの入り口のラベル対応を保持 (サブルーチンの固有名, 入り口ラベル)
    subroutine_labels: HashMap<String, String>,

    // 組み込みサブルーチンのコードを保持
    subroutine_codes: Vec<Vec<casl2::Statement>>,

    // ループや条件分岐の脱出先のラベルを保持 (まだ暫定的)
    next_statement_label: Option<casl2::Label>,

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
        // T** 式展開時の一時変数(真理値/整数で共有)
        // V** 組み込みサブルーチンのローカル変数(真理値/整数で共有)
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
        // VL** 組み込みサブルーチンのローカル変数の文字列変数の長さ
        // VB** 組み込みサブルーチンのローカル変数の文字列変数の内容位置
        && (program_name.len() < 3 || !{
            let (head, tail) = program_name.split_at(2);
            matches!(head, "SL"|"SB"|"BA"|"IA"|"LL"|"LB"|"TL"|"TB"|"VL"|"VB")
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
            local_int_var_id: 0,
            local_str_var_id: 0,
            temp_var_id: 0,
            jump_id: 0,
            call_id: 0,
            bool_var_labels: HashMap::new(),
            int_var_labels: HashMap::new(),
            str_var_labels: HashMap::new(),
            bool_arr_labels: HashMap::new(),
            int_arr_labels: HashMap::new(),
            lit_str_labels: HashMap::new(),
            temp_int_var_labels: Vec::new(),
            temp_str_var_labels: Vec::new(),
            subroutine_labels: HashMap::new(),
            subroutine_codes: Vec::new(),
            next_statement_label: None,
            statements: vec![casl2::Statement::labeled(
                program_name,
                casl2::Command::Start { entry_point: None },
            )],
        })
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

    // コンパイル最終工程
    fn finish(self) -> Vec<casl2::Statement> {
        let Self {
            local_int_var_id,
            local_str_var_id,
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
        for mut code in subroutine_codes.into_iter() {
            statements.append(&mut code);
        }
        // 組み込みサブルーチン内で使う整数変数
        for id in 1..=local_int_var_id {
            statements.push(casl2::Statement::labeled(
                &format!("T{}", id),
                casl2::Command::Ds { size: 1 },
            ));
        }
        // 組み込みサブルーチン内で使う文字列変数
        for id in 1..=local_str_var_id {
            statements.push(casl2::Statement::labeled(
                &format!("TL{}", id),
                casl2::Command::Ds { size: 1 },
            ));
            statements.push(casl2::Statement::labeled(
                &format!("TB{}", id),
                casl2::Command::Ds { size: 256 },
            ));
        }

        // 真理値変数
        for label in bool_var_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: 1 },
            ));
        }

        // 整数変数
        for label in int_var_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: 1 },
            ));
        }

        // 文字列変数
        for (len_label, buf_label) in str_var_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &len_label,
                casl2::Command::Ds { size: 1 },
            ));
            statements.push(casl2::Statement::labeled(
                &buf_label,
                casl2::Command::Ds { size: 256 },
            ));
        }

        // 真理値配列(固定長)
        for (label, size) in bool_arr_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: size as u16 },
            ));
        }

        // 整数配列(固定長)
        for (label, size) in int_arr_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: size as u16 },
            ));
        }

        // 式展開等で使う一時変数(整数/真理値で共有)
        for label in temp_int_var_labels.into_iter().collect::<BTreeSet<_>>() {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: 1 },
            ));
        }

        // 式展開等で使う一時変数(文字列)
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

        // IN/OUTで使用する文字列定数
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
            AssignInteger {
                var_name: _,
                value: _,
            } => todo!(),
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
            InputInteger { var_name: _ } => todo!(),
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

    fn compile_input_string(&mut self, var_name: &str) {
        let label = self.next_statement_label.take();
        let (len_label, buf_label) = self.str_var_labels.get(var_name).expect("BUG");
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::In {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: None,
        });
    }

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
            comment: None,
        });
    }

    fn compile_print_lit_integer(&mut self, value: i32) {
        let label = self.next_statement_label.take();
        let (len_label, buf_label) = self.get_lit_str_labels(&value.to_string());
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::Out {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: None,
        });
    }

    fn compile_print_lit_string(&mut self, value: &str) {
        let label = self.next_statement_label.take();
        let (len_label, buf_label) = self.get_lit_str_labels(value);
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::Out {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: None,
        });
    }

    fn compile_print_var_string(&mut self, var_name: &str) {
        let label = self.next_statement_label.take();
        let (len_label, buf_label) = self.str_var_labels.get(var_name).expect("BUG");
        self.statements.push(casl2::Statement::Code {
            label,
            command: casl2::Command::Out {
                pos: buf_label.into(),
                len: len_label.into(),
            },
            comment: None,
        });
    }

    fn compile_expr(&mut self, expr: &parser::Expr) {
        use parser::Expr::*;
        match expr {
            BinaryOperatorBoolean(_op, _lhs, _rhs) => todo!(),
            BinaryOperatorInteger(_op, _lhs, _rhs) => todo!(),
            BinaryOperatorString(_op, _lhs, _rhs) => todo!(),
            CharOfLitString(_lit_str, _index) => todo!(),
            CharOfVarString(_var_name, _index) => todo!(),
            FunctionBoolean(_func, _param) => todo!(),
            FunctionInteger(_func, _param) => todo!(),
            FunctionString(_func, _param) => todo!(),
            LitBoolean(_lit_bool) => todo!(),
            LitInteger(_lit_int) => todo!(),
            LitString(_lit_str) => todo!(),
            UnaryOperatorInteger(_op, _value) => todo!(),
            UnaryOperatorBoolean(_op, _value) => todo!(),
            VarBoolean(_var_name) => todo!(),
            VarInteger(_var_name) => todo!(),
            VarString(_var_name) => todo!(),
            VarArrayOfBoolean(_arr_name, _index) => todo!(),
            VarArrayOfInteger(_arr_name, _index) => todo!(),
            ParamList(_param_list) => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_works() {
        assert!(true);
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
                casl2::Statement::labeled("B2", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("B5", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("I3", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("I8", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("SL1", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("SB1", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled("SL6", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("SB6", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled("BA4", casl2::Command::Ds { size: 32 }),
                casl2::Statement::labeled("IA7", casl2::Command::Ds { size: 155 }),
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
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB1".into(),
                    len: "LL1".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB2".into(),
                    len: "LL2".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB2".into(),
                    len: "LL2".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB1".into(),
                    len: "LL1".into()
                }),
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
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB1".into(),
                    len: "LL1".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB2".into(),
                    len: "LL2".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB3".into(),
                    len: "LL3".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB1".into(),
                    len: "LL1".into()
                }),
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
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB1".into(),
                    len: "LL1".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB2".into(),
                    len: "LL2".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB3".into(),
                    len: "LL3".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "LB1".into(),
                    len: "LL1".into()
                }),
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
                casl2::Statement::code(casl2::Command::Out {
                    pos: "SB3".into(),
                    len: "SL3".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "SB2".into(),
                    len: "SL2".into()
                }),
                casl2::Statement::code(casl2::Command::Out {
                    pos: "SB1".into(),
                    len: "SL1".into()
                }),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled("SL1", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("SB1", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled("SL2", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("SB2", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled("SL3", casl2::Command::Ds { size: 1 }),
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
                casl2::Statement::code(casl2::Command::In {
                    pos: "SB3".into(),
                    len: "SL3".into()
                }),
                casl2::Statement::code(casl2::Command::In {
                    pos: "SB2".into(),
                    len: "SL2".into()
                }),
                casl2::Statement::code(casl2::Command::In {
                    pos: "SB1".into(),
                    len: "SL1".into()
                }),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled("SL1", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("SB1", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled("SL2", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("SB2", casl2::Command::Ds { size: 256 }),
                casl2::Statement::labeled("SL3", casl2::Command::Ds { size: 1 }),
                casl2::Statement::labeled("SB3", casl2::Command::Ds { size: 256 }),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }
}
