// b2c2-compiler crate::int_arr_func
// author: Leonardone @ NEETSDKASU

use super::*;

impl Compiler {
    // (式展開の処理の一部)
    // 整数配列が戻り値の関数
    pub(super) fn compile_function_integer_array(
        &mut self,
        size: usize,
        func: tokenizer::Function,
        param: &parser::Expr,
    ) -> ArrayLabel {
        use tokenizer::Function::*;

        assert!((1..=MAX_ARRAY_SIZE).contains(&size));

        match func {
            Array => self.call_function_array_with_integer_array(size, param),
            CArray => self.call_function_carray_with_integer_array(size, param),
            SubArray => self.call_function_subarray_with_integer_array(size, param),

            // 戻り値が整数配列ではないもの
            Abs | Asc | CBool | Chr | CInt | CStr | Eof | Len | Max | Mid | Min | Space
            | String => {
                unreachable!("BUG")
            }
        }
    }

    // (式展開の処理の一部)
    // CArray (<int_arr>/<string>, <size>) の処理
    pub(super) fn call_function_carray_with_integer_array(
        &mut self,
        size: usize,
        param: &parser::Expr,
    ) -> ArrayLabel {
        let expr = if let parser::Expr::ParamList(list) = param {
            if let [expr, parser::Expr::LitInteger(len)] = list.as_slice() {
                assert_eq!(size as i32, *len);
                expr
            } else {
                unreachable!("BUG");
            }
        } else {
            unreachable!("BUG");
        };

        if expr.return_type().is_int_array() {
            let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);

            let arr_label = self.compile_ref_arr_expr(expr);
            assert!(matches!(
                arr_label.element_type(),
                parser::ExprType::Integer
            ));

            let temp_labels = self.get_temp_str_var_label();

            let (saves, recovers) = {
                use casl2::Register::*;
                self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
            };

            self.code(saves);
            if size > arr_label.size() {
                let fill = self.load_subroutine(subroutine::Id::UtilFill);
                self.code(format!(
                    r#" LAD   GR1,{temppos}
                        XOR   GR2,GR2
                        LAD   GR3,{size}
                        CALL  {fill}
                        LAD   GR2,{templen}
                        {lad_gr3_srcpos}
                        LAD   GR4,{copylen}
                        CALL  {copy}"#,
                    temppos = temp_labels.pos,
                    size = size,
                    fill = fill,
                    templen = temp_labels.len,
                    lad_gr3_srcpos = arr_label.lad_pos(casl2::Register::Gr3),
                    copylen = size.min(arr_label.size()),
                    copy = copystr
                ));
            } else {
                self.code(format!(
                    r#" LAD   GR1,{temppos}
                        LAD   GR2,{templen}
                        {lad_gr3_srcpos}
                        LAD   GR4,{copylen}
                        CALL  {copy}"#,
                    temppos = temp_labels.pos,
                    templen = temp_labels.len,
                    lad_gr3_srcpos = arr_label.lad_pos(casl2::Register::Gr3),
                    copylen = size.min(arr_label.size()),
                    copy = copystr
                ));
            }
            self.code(recovers);

            if let Some(labels) = arr_label.release() {
                self.return_temp_str_var_label(labels);
            }

            ArrayLabel::TempArrayOfInteger(temp_labels, size)
        } else if matches!(expr.return_type(), parser::ExprType::String) {
            let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
            let fill = self.load_subroutine(subroutine::Id::UtilFill);

            let str_labels = self.compile_str_expr(expr);

            let temp_labels = self.get_temp_str_var_label();

            let (saves, recovers) = {
                use casl2::Register::*;
                self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4, Gr5, Gr6])
            };

            self.code(saves);
            self.code(format!(
                r#" LAD   GR1,{temppos}
                    XOR   GR2,GR2
                    LAD   GR3,{size}
                    CALL  {fill}
                    LAD   GR2,{templen}
                    {lad_gr3_srcpos}
                    {ld_gr4_srclen}
                    CALL  {copy}"#,
                temppos = temp_labels.pos,
                templen = temp_labels.len,
                size = size,
                fill = fill,
                lad_gr3_srcpos = str_labels.lad_pos(casl2::Register::Gr3),
                ld_gr4_srclen = str_labels.ld_len(casl2::Register::Gr4),
                copy = copystr
            ));
            self.code(recovers);

            self.return_temp_str_var_label(str_labels);

            ArrayLabel::TempArrayOfInteger(temp_labels, size)
        } else {
            unreachable!("BUG");
        }
    }

    // (式展開の処理の一部)
    // SubArray (<int_arr>, <offset>, <size>) の処理
    pub(super) fn call_function_subarray_with_integer_array(
        &mut self,
        size: usize,
        param: &parser::Expr,
    ) -> ArrayLabel {
        let (arr, offset) = if let parser::Expr::ParamList(list) = param {
            if let [arr, offset, parser::Expr::LitInteger(len)] = list.as_slice() {
                assert!(arr.return_type().is_int_array());
                assert!(matches!(offset.return_type(), parser::ExprType::Integer));
                assert_eq!(size as i32, *len);
                (arr, offset)
            } else {
                unreachable!("BUG");
            }
        } else {
            unreachable!("BUG");
        };

        let copystr = self.load_subroutine(subroutine::Id::UtilCopyFromOffsetStr);
        let fill = self.load_subroutine(subroutine::Id::UtilFill);

        let arr_label = self.compile_ref_arr_expr(arr);
        assert!(matches!(
            arr_label.element_type(),
            parser::ExprType::Integer
        ));

        let offset_reg = self.compile_int_expr(offset);

        let temp_labels = self.get_temp_str_var_label();

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4, Gr5, Gr6])
        };

        let offset_line = if matches!(offset_reg, casl2::Register::Gr4) {
            "".to_string()
        } else {
            format!(" LD GR4,{offset}", offset = offset_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {offset_line}
                LAD   GR1,{temppos}
                XOR   GR2,GR2
                LAD   GR3,{size}
                CALL  {fill}
                LD    GR5,GR1
                LD    GR1,GR4
                LD    GR4,GR3
                LD    GR6,GR3
                LAD   GR2,{srclen}
                {lad_gr3_srcpos}
                CALL  {copy}"#,
            offset_line = offset_line,
            temppos = temp_labels.pos,
            size = size,
            fill = fill,
            srclen = arr_label.size(),
            lad_gr3_srcpos = arr_label.lad_pos(casl2::Register::Gr3),
            copy = copystr
        ));
        self.code(recovers);

        if let Some(labels) = arr_label.release() {
            self.return_temp_str_var_label(labels);
        }
        self.set_register_idle(offset_reg);

        ArrayLabel::TempArrayOfInteger(temp_labels, size)
    }

    // (式展開の処理の一部)
    // Array (<int_expr>, ...) の処理
    pub(super) fn call_function_array_with_integer_array(
        &mut self,
        size: usize,
        param: &parser::Expr,
    ) -> ArrayLabel {
        let labels = self.get_temp_str_var_label();

        if matches!(param.return_type(), parser::ExprType::Integer) {
            assert_eq!(size, 1);
            let reg = self.compile_int_expr(param);
            self.code(format!(r#" ST {reg},{arr}"#, reg = reg, arr = labels.pos));
            self.set_register_idle(reg);
            return ArrayLabel::TempArrayOfInteger(labels, size);
        }

        assert_ne!(size, 1);

        let list = if let parser::Expr::ParamList(list) = param {
            assert_eq!(size, list.len());
            list
        } else {
            unreachable!("BUG");
        };

        let index_reg = self.get_idle_register();
        self.code(format!(
            r#" LAD {index},{arr}"#,
            index = index_reg,
            arr = labels.pos
        ));

        for (i, expr) in list.iter().enumerate() {
            assert!(matches!(expr.return_type(), parser::ExprType::Integer));
            let reg = self.compile_int_expr(expr);
            assert_ne!(reg, index_reg);
            self.restore_register(index_reg);
            self.code(format!(
                r#" ST {reg},0,{index}"#,
                reg = reg,
                index = index_reg
            ));
            self.set_register_idle(reg);
            if i + 1 < list.len() {
                self.code(format!(r#" LAD {index},1,{index}"#, index = index_reg));
            }
        }

        self.set_register_idle(index_reg);

        ArrayLabel::TempArrayOfInteger(labels, size)
    }
}
