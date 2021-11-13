// b2c2-compiler crate::str_bin_op
// author: Leonardone @ NEETSDKASU

use super::*;

impl Compiler {
    // 文字列を返す二項演算子の処理
    pub(super) fn compile_bin_op_string(
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
            | GreaterThan | ShiftLeftArithmetic | ShiftRightArithmetic | ShiftLeftLogical
            | ShiftRightLogical | Add | Sub | Mul | Div | Mod | Not | AddInto | SubInto
            | OpenBracket | CloseBracket | Comma => unreachable!("BUG"),
        }
    }

    // 文字列を返す二項演算子の処理
    // 文字列の結合 ( & )
    pub(super) fn compile_bin_op_string_concat(
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
                    {lad_gr3_srcpos}
                    {ld_gr4_srclen}
                    CALL  {copy}"#,
                tmppos = temp_labels.pos,
                tmplen = temp_labels.len,
                lad_gr3_srcpos = lhs_labels.lad_pos(casl2::Register::Gr3),
                ld_gr4_srclen = lhs_labels.ld_len(casl2::Register::Gr4),
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
                {lad_gr3_rhspos}
                {ld_gr4_rhslen}
                CALL  {concat}"#,
            lhspos = lhs_labels.pos,
            lhslen = lhs_labels.len,
            lad_gr3_rhspos = rhs_labels.lad_pos(casl2::Register::Gr3),
            ld_gr4_rhslen = rhs_labels.ld_len(casl2::Register::Gr4),
            concat = concat
        ));

        self.code(recovers);

        self.return_temp_str_var_label(rhs_labels);
        lhs_labels
    }
}
