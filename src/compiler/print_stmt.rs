use super::*;

impl Compiler {
    // Print <lit_bool> ステートメント
    // 真理値リテラルの画面出力
    pub(super) fn compile_print_lit_boolean(&mut self, value: bool) {
        let s = if value { "True" } else { "False" };
        let StrLabels { len, pos, .. } = self.get_lit_str_labels(s);
        self.comment(format!("Print {}", s));
        // OUT {lit_pos},{lit_len}
        self.code(casl2::Command::Out {
            pos: pos.into(),
            len: len.into(),
        });
    }

    // Print <lit_int>ステートメント
    // 数字リテラルの画面出力
    pub(super) fn compile_print_lit_integer(&mut self, value: i32) {
        let value = value as i16;
        let StrLabels { len, pos, .. } = self.get_lit_str_labels(&value.to_string());
        self.comment(format!("Print {}", value));
        self.code(casl2::Command::Out {
            pos: pos.into(),
            len: len.into(),
        });
    }

    // Print <lit_str>ステートメント
    // 文字列リテラルの画面出力
    pub(super) fn compile_print_lit_string(&mut self, value: &str) {
        let StrLabels { len, pos, .. } = self.get_lit_str_labels(value);
        self.comment(format!(r#"Print "{}""#, value.replace('"', r#""""#)));
        self.code(casl2::Command::Out {
            pos: pos.into(),
            len: len.into(),
        });
    }

    // Print <str_var>ステートメント
    // 文字列変数の画面出力
    pub(super) fn compile_print_var_string(&mut self, var_name: &str) {
        let StrLabels { len, pos, .. } = self.get_str_var_labels(var_name);
        self.comment(format!("Print {}", var_name));
        self.code(casl2::Command::Out {
            pos: pos.into(),
            len: len.into(),
        });
    }

    // Print <bool_expr>ステートメント
    // 真理値の演算結果の画面出力
    pub(super) fn compile_print_expr_boolean(&mut self, value: &parser::Expr) {
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

    // Print <str_exprステートメント
    // 文字列の演算結果の画面出力
    pub(super) fn compile_print_expr_string(&mut self, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::String));

        self.comment(format!("Print {}", value));

        let labels = self.compile_str_expr(value);

        let labels = match &labels {
            StrLabels {
                label_type: StrLabelType::Lit(s),
                ..
            } => self.get_lit_str_labels(&s),
            StrLabels {
                pos,
                len,
                label_type: StrLabelType::ArgRef,
            } => {
                let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
                let temp_labels = self.get_temp_str_var_label();
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" LAD   GR1,{tmppos}
                        LAD   GR2,{tmplen}
                        LD    GR3,{pos}
                        LD    GR4,{len}
                        LD    GR4,0,GR4
                        CALL  {copy}
                    "#,
                    tmppos = temp_labels.pos,
                    tmplen = temp_labels.len,
                    pos = pos,
                    len = len,
                    copy = copystr
                ));
                self.code(recovers);
                temp_labels
            }
            _ => labels,
        };

        self.code(format!(
            r#" OUT  {pos},{len}"#,
            pos = labels.pos,
            len = labels.len
        ));

        self.return_temp_str_var_label(labels);
    }

    // Print <int_expr>ステートメント
    // 整数の計算結果の画面出力
    pub(super) fn compile_print_expr_integer(&mut self, value: &parser::Expr) {
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
}
