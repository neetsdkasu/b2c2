use super::*;

impl Compiler {
    // 戻り値が文字列の関数の処理
    pub(super) fn compile_function_string(
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
            String => self.call_function_string(param),

            // 戻り値が文字列ではないもの
            Abs | Array | Asc | CArray | CBool | CInt | Eof | Len | Max | Min | SubArray => {
                unreachable!("BUG")
            }
        }
    }

    // String関数
    // String(<int_arr>)
    // String(<length>,<char>)
    pub(super) fn call_function_string(&mut self, param: &parser::Expr) -> StrLabels {
        if param.return_type().is_int_array() {
            let arr_label = self.compile_ref_arr_expr(param);
            assert!(matches!(
                arr_label.element_type(),
                parser::ExprType::Integer
            ));

            let temp_labels = self.get_temp_str_var_label();

            let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);

            let (saves, recovers) = {
                use casl2::Register::*;
                self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
            };

            self.code(saves);
            self.code(format!(
                r#" LAD   GR1,{temppos}
                    LAD   GR2,{templen}
                    {lad_gr3_srcpos}
                    LAD   GR4,{size}
                    CALL  {copy}
                    "#,
                temppos = temp_labels.pos,
                templen = temp_labels.len,
                lad_gr3_srcpos = arr_label.lad_pos(casl2::Register::Gr3),
                size = arr_label.size(),
                copy = copystr
            ));
            self.code(recovers);

            if let Some(labels) = arr_label.release() {
                self.return_temp_str_var_label(labels);
            }

            temp_labels
        } else if let parser::Expr::ParamList(list) = param {
            let (count, value) = if let [count, value] = list.as_slice() {
                assert!(matches!(count.return_type(), parser::ExprType::Integer));
                assert!(matches!(value.return_type(), parser::ExprType::Integer));
                (count, value)
            } else {
                unreachable!("BUG");
            };
            let count_reg = self.compile_int_expr(count);
            let value_reg = self.compile_int_expr(value);
            self.restore_register(count_reg);

            let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
            let fill = self.load_subroutine(subroutine::Id::UtilFill);

            let temp_labels = self.get_temp_str_var_label();

            let (saves, recovers) = {
                use casl2::Register::*;
                self.get_save_registers_src(&[Gr1, Gr2, Gr3])
            };

            self.code(saves);
            self.code(format!(
                r#" LD    GR3,{value}
                    LD    GR1,{count}
                    LAD   GR2,257
                    CALL  {fit}
                    ST    GR0,{templen}
                    LAD   GR1,{temppos}
                    LD    GR2,GR3
                    LD    GR3,GR0
                    CALL  {fill}
                    "#,
                value = value_reg,
                count = count_reg,
                temppos = temp_labels.pos,
                templen = temp_labels.len,
                fit = safe_index,
                fill = fill
            ));
            self.code(recovers);

            self.set_register_idle(value_reg);
            self.set_register_idle(count_reg);

            temp_labels
        } else {
            unreachable!("BUG");
        }
    }

    // Mid関数
    // Mid(<string>,<integer>)
    // Mid(<string>,<integer>,<integer>)
    pub(super) fn call_function_mid(&mut self, param: &parser::Expr) -> StrLabels {
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
                    {lad_gr3_srcpos}
                    {ld_gr4_srclen}
                    LD    GR2,GR4
                    CALL  {copy}
                    ST    GR0,{dstlen}"#,
                length_line1 = length_line1,
                offset_line = offset_line,
                length_line2 = length_line2,
                dstpos = dst_labels.pos,
                dstlen = dst_labels.len,
                lad_gr3_srcpos = src_labels.lad_pos(casl2::Register::Gr3),
                ld_gr4_srclen = src_labels.ld_len(casl2::Register::Gr4),
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
                    {lad_gr3_srcpos}
                    {ld_gr4_srclen}
                    LD    GR2,GR4
                    LD    GR6,GR4
                    CALL  {copy}
                    ST    GR0,{dstlen}"#,
                offset_line = offset_line,
                dstpos = dst_labels.pos,
                dstlen = dst_labels.len,
                lad_gr3_srcpos = src_labels.lad_pos(casl2::Register::Gr3),
                ld_gr4_srclen = src_labels.ld_len(casl2::Register::Gr4),
                copy = partialcopy
            ));
            self.code(recovers);
        }

        self.set_register_idle(offset_reg);
        self.return_temp_str_var_label(src_labels);
        dst_labels
    }

    // Chr(<integer>)
    pub(super) fn call_function_chr(&mut self, param: &parser::Expr) -> StrLabels {
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
    pub(super) fn call_function_space(&mut self, param: &parser::Expr) -> StrLabels {
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
    pub(super) fn call_function_cstr(&mut self, param: &parser::Expr) -> StrLabels {
        // リテラルのとき
        match param {
            parser::Expr::LitBoolean(value) => {
                return self.get_lit_str_label_if_exists(if *value { "True" } else { "False" })
            }
            parser::Expr::LitInteger(value) => {
                let value = *value as i16;
                return self.get_lit_str_label_if_exists(&value.to_string());
            }
            _ => {}
        }

        let id = match param.return_type() {
            parser::ExprType::Boolean => subroutine::Id::FuncCStrArgBool,
            parser::ExprType::Integer => subroutine::Id::FuncCStrArgInt,
            parser::ExprType::String
            | parser::ExprType::ParamList
            | parser::ExprType::ReferenceOfVar(..) => {
                unreachable!("BUG")
            }
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
}
