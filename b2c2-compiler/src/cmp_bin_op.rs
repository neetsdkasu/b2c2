// b2c2-compiler crate::cmp_bin_op
// author: Leonardone @ NEETSDKASU

use super::*;

impl Compiler {
    // (式展開の処理の一部)
    // 比較演算子( <> )
    pub(super) fn compile_bin_op_boolean_not_equal(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert!(lhs.return_type().match_for_bin_op(&rhs.return_type()));
        assert!(rhs.return_type().match_for_bin_op(&lhs.return_type()));

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
                    r#" {lad_gr1_lhspos}
                        {ld_gr2_lhslen}
                        {lad_gr3_rhspos}
                        {ld_gr4_rhslen}
                        CALL  {cmpstr}"#,
                    lad_gr1_lhspos = lhs_labels.lad_pos(casl2::Register::Gr1),
                    ld_gr2_lhslen = lhs_labels.ld_len(casl2::Register::Gr2),
                    lad_gr3_rhspos = rhs_labels.lad_pos(casl2::Register::Gr3),
                    ld_gr4_rhslen = rhs_labels.ld_len(casl2::Register::Gr4),
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

            parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfBoolean(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfBoolean(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size1)) => {
                match rhs.return_type() {
                    parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfBoolean(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfBoolean(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size2))
                        if size1 == size2 => {}
                    _ => unreachable!("BUG"),
                }
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_label = self.compile_ref_arr_expr(lhs);
                assert_eq!(size1, lhs_label.size());
                let rhs_label = self.compile_ref_arr_expr(rhs);
                assert_eq!(size1, rhs_label.size());
                assert_eq!(lhs_label.element_type(), rhs_label.element_type());
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" {lad_gr1_lhs}
                        LAD   GR2,{len}
                        {lad_gr3_rhs}
                        LD    GR4,GR2
                        CALL  {cmpstr}"#,
                    lad_gr1_lhs = lhs_label.lad_pos(casl2::Register::Gr1),
                    lad_gr3_rhs = rhs_label.lad_pos(casl2::Register::Gr3),
                    len = size1,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SLL   GR0,15
                        SRA   GR0,15
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                if let Some(labels) = lhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                if let Some(labels) = rhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                self.set_register_used(reg);
                reg
            }

            parser::ExprType::ReferenceOfVar(..) | parser::ExprType::ParamList => {
                unreachable!("BUG")
            }
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( <= )
    pub(super) fn compile_bin_op_boolean_less_or_equal(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert!(lhs.return_type().match_for_bin_op(&rhs.return_type()));
        assert!(rhs.return_type().match_for_bin_op(&lhs.return_type()));

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
                    r#" {lad_gr3_lhspos}
                        {ld_gr4_lhslen}
                        {lad_gr1_rhspos}
                        {ld_gr2_rhslen}
                        CALL  {cmpstr}"#,
                    lad_gr3_lhspos = lhs_labels.lad_pos(casl2::Register::Gr3),
                    ld_gr4_lhslen = lhs_labels.ld_len(casl2::Register::Gr4),
                    lad_gr1_rhspos = rhs_labels.lad_pos(casl2::Register::Gr1),
                    ld_gr2_rhslen = rhs_labels.ld_len(casl2::Register::Gr2),
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

            parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size1)) => {
                match rhs.return_type() {
                    parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size2))
                        if size1 == size2 => {}
                    _ => unreachable!("BUG"),
                }
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_label = self.compile_ref_arr_expr(lhs);
                assert_eq!(size1, lhs_label.size());
                let rhs_label = self.compile_ref_arr_expr(rhs);
                assert_eq!(size1, rhs_label.size());
                assert_eq!(lhs_label.element_type(), rhs_label.element_type());
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" {lad_gr3_lhs}
                        LAD   GR2,{len}
                        {lad_gr1_rhs}
                        LD    GR4,GR2
                        CALL  {cmpstr}"#,
                    lad_gr3_lhs = lhs_label.lad_pos(casl2::Register::Gr3),
                    lad_gr1_rhs = rhs_label.lad_pos(casl2::Register::Gr1),
                    len = size1,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SRA   GR0,1
                        XOR   GR0,=#FFFF
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                if let Some(labels) = lhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                if let Some(labels) = rhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                self.set_register_used(reg);
                reg
            }

            parser::ExprType::ReferenceOfVar(..)
            | parser::ExprType::Boolean
            | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( >= )
    pub(super) fn compile_bin_op_boolean_greater_or_equal(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert!(lhs.return_type().match_for_bin_op(&rhs.return_type()));
        assert!(rhs.return_type().match_for_bin_op(&lhs.return_type()));

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
                    r#" {lad_gr1_lhspos}
                        {ld_gr2_lhslen}
                        {lad_gr3_rhspos}
                        {ld_gr4_rhslen}
                        CALL  {cmpstr}"#,
                    lad_gr1_lhspos = lhs_labels.lad_pos(casl2::Register::Gr1),
                    ld_gr2_lhslen = lhs_labels.ld_len(casl2::Register::Gr2),
                    lad_gr3_rhspos = rhs_labels.lad_pos(casl2::Register::Gr3),
                    ld_gr4_rhslen = rhs_labels.ld_len(casl2::Register::Gr4),
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

            parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size1)) => {
                match rhs.return_type() {
                    parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size2))
                        if size1 == size2 => {}
                    _ => unreachable!("BUG"),
                }
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_label = self.compile_ref_arr_expr(lhs);
                assert_eq!(size1, lhs_label.size());
                let rhs_label = self.compile_ref_arr_expr(rhs);
                assert_eq!(size1, rhs_label.size());
                assert_eq!(lhs_label.element_type(), rhs_label.element_type());
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" {lad_gr1_lhs}
                        LAD   GR2,{len}
                        {lad_gr3_rhs}
                        LD    GR4,GR2
                        CALL  {cmpstr}"#,
                    lad_gr1_lhs = lhs_label.lad_pos(casl2::Register::Gr1),
                    lad_gr3_rhs = rhs_label.lad_pos(casl2::Register::Gr3),
                    len = size1,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SRA   GR0,15
                        XOR   GR0,=#FFFF
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                if let Some(labels) = lhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                if let Some(labels) = rhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                self.set_register_used(reg);
                reg
            }

            parser::ExprType::ReferenceOfVar(..)
            | parser::ExprType::Boolean
            | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( > )
    pub(super) fn compile_bin_op_boolean_greater_than(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert!(lhs.return_type().match_for_bin_op(&rhs.return_type()));
        assert!(rhs.return_type().match_for_bin_op(&lhs.return_type()));

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
                    r#" {lad_gr3_lhspos}
                        {ld_gr4_lhslen}
                        {lad_gr1_rhspos}
                        {ld_gr2_rhslen}
                        CALL  {cmpstr}"#,
                    lad_gr3_lhspos = lhs_labels.lad_pos(casl2::Register::Gr3),
                    ld_gr4_lhslen = lhs_labels.ld_len(casl2::Register::Gr4),
                    lad_gr1_rhspos = rhs_labels.lad_pos(casl2::Register::Gr1),
                    ld_gr2_rhslen = rhs_labels.ld_len(casl2::Register::Gr2),
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

            parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size1)) => {
                match rhs.return_type() {
                    parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size2))
                        if size1 == size2 => {}
                    _ => unreachable!("BUG"),
                }
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_label = self.compile_ref_arr_expr(lhs);
                assert_eq!(size1, lhs_label.size());
                let rhs_label = self.compile_ref_arr_expr(rhs);
                assert_eq!(size1, rhs_label.size());
                assert_eq!(lhs_label.element_type(), rhs_label.element_type());
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" {lad_gr3_lhs}
                        LAD   GR2,{len}
                        {lad_gr1_rhs}
                        LD    GR4,GR2
                        CALL  {cmpstr}"#,
                    lad_gr3_lhs = lhs_label.lad_pos(casl2::Register::Gr3),
                    lad_gr1_rhs = rhs_label.lad_pos(casl2::Register::Gr1),
                    len = size1,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SRA   GR0,15
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                if let Some(labels) = lhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                if let Some(labels) = rhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                self.set_register_used(reg);
                reg
            }

            parser::ExprType::ReferenceOfVar(..)
            | parser::ExprType::Boolean
            | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( < )
    pub(super) fn compile_bin_op_boolean_less_than(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert!(lhs.return_type().match_for_bin_op(&rhs.return_type()));
        assert!(rhs.return_type().match_for_bin_op(&lhs.return_type()));

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
                    r#" {lad_gr1_lhspos}
                        {ld_gr2_lhslen}
                        {lad_gr3_rhspos}
                        {ld_gr4_rhslen}
                        CALL  {cmpstr}"#,
                    lad_gr1_lhspos = lhs_labels.lad_pos(casl2::Register::Gr1),
                    ld_gr2_lhslen = lhs_labels.ld_len(casl2::Register::Gr2),
                    lad_gr3_rhspos = rhs_labels.lad_pos(casl2::Register::Gr3),
                    ld_gr4_rhslen = rhs_labels.ld_len(casl2::Register::Gr4),
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

            parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size1)) => {
                match rhs.return_type() {
                    parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size2))
                        if size1 == size2 => {}
                    _ => unreachable!("BUG"),
                }
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_label = self.compile_ref_arr_expr(lhs);
                assert_eq!(size1, lhs_label.size());
                let rhs_label = self.compile_ref_arr_expr(rhs);
                assert_eq!(size1, rhs_label.size());
                assert_eq!(lhs_label.element_type(), rhs_label.element_type());
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" {lad_gr1_lhs}
                        LAD   GR2,{len}
                        {lad_gr3_rhs}
                        LD    GR4,GR2
                        CALL  {cmpstr}"#,
                    lad_gr1_lhs = lhs_label.lad_pos(casl2::Register::Gr1),
                    lad_gr3_rhs = rhs_label.lad_pos(casl2::Register::Gr3),
                    len = size1,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SRA   GR0,15
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                if let Some(labels) = lhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                if let Some(labels) = rhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                self.set_register_used(reg);
                reg
            }

            parser::ExprType::ReferenceOfVar(..)
            | parser::ExprType::Boolean
            | parser::ExprType::ParamList => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 比較演算子( = )
    pub(super) fn compile_bin_op_boolean_equal(
        &mut self,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        assert!(lhs.return_type().match_for_bin_op(&rhs.return_type()));
        assert!(rhs.return_type().match_for_bin_op(&lhs.return_type()));

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
                    r#" {lad_gr1_lhspos}
                        {ld_gr2_lhslen}
                        {lad_gr3_rhspos}
                        {ld_gr4_rhslen}
                        CALL  {cmpstr}"#,
                    lad_gr1_lhspos = lhs_str.lad_pos(casl2::Register::Gr1),
                    ld_gr2_lhslen = lhs_str.ld_len(casl2::Register::Gr2),
                    lad_gr3_rhspos = rhs_str.lad_pos(casl2::Register::Gr3),
                    ld_gr4_rhslen = rhs_str.ld_len(casl2::Register::Gr4),
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

            parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfBoolean(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfBoolean(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size1))
            | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size1)) => {
                match rhs.return_type() {
                    parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfBoolean(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfBoolean(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::ArrayOfInteger(size2))
                    | parser::ExprType::ReferenceOfVar(parser::VarType::RefArrayOfInteger(size2))
                        if size1 == size2 => {}
                    _ => unreachable!("BUG"),
                }
                let reg = self.get_idle_register();
                self.set_register_idle(reg);
                let lhs_label = self.compile_ref_arr_expr(lhs);
                assert_eq!(size1, lhs_label.size());
                let rhs_label = self.compile_ref_arr_expr(rhs);
                assert_eq!(size1, rhs_label.size());
                assert_eq!(lhs_label.element_type(), rhs_label.element_type());
                let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);
                // レジスタ退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                };
                self.code(saves);
                self.code(format!(
                    r#" {lad_gr1_lhs}
                        LAD   GR2,{len}
                        {lad_gr3_rhs}
                        LD    GR4,GR2
                        CALL  {cmpstr}"#,
                    lad_gr1_lhs = lhs_label.lad_pos(casl2::Register::Gr1),
                    lad_gr3_rhs = rhs_label.lad_pos(casl2::Register::Gr3),
                    len = size1,
                    cmpstr = cmpstr
                ));
                self.code(recovers);
                self.code(format!(
                    r#" SLL   GR0,15
                        SRA   GR0,15
                        XOR   GR0,=#FFFF
                        LD    {reg},GR0"#,
                    reg = reg
                ));
                if let Some(labels) = lhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                if let Some(labels) = rhs_label.release() {
                    self.return_temp_str_var_label(labels);
                }
                self.set_register_used(reg);
                reg
            }

            parser::ExprType::ReferenceOfVar(..) | parser::ExprType::ParamList => {
                unreachable!("BUG")
            }
        }
    }
}
