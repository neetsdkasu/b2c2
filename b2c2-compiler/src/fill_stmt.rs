// b2c2-compiler crate::fill_stmt
// author: Leonardone @ NEETSDKASU

use super::*;

impl Compiler {
    // Fill <ref_str_var>, <value>
    pub(super) fn compile_fill_ref_string(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| format!("Fill {var}, {value}", var = var_name, value = value));
        self.comment(format!(
            "Fill {var}, {value}",
            var = var_name,
            value = value
        ));

        let str_var = self.get_ref_str_var_labels(var_name);

        let value_reg = self.compile_int_expr(value);
        let fill = self.load_subroutine(subroutine::Id::UtilFill);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR2,{value}
                {lad_gr1_pos}
                {ld_gr3_len}
                CALL  {fill}"#,
            value = value_reg,
            lad_gr1_pos = str_var.lad_pos(casl2::Register::Gr1),
            ld_gr3_len = str_var.ld_len(casl2::Register::Gr3),
            fill = fill
        ));
        self.code(recovers);

        self.set_register_idle(value_reg);
        self.return_temp_str_var_label(str_var);
    }

    // Fill <str_var>, <value>
    pub(super) fn compile_fill_string(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| format!("Fill {var}, {value}", var = var_name, value = value));
        self.comment(format!(
            "Fill {var}, {value}",
            var = var_name,
            value = value
        ));

        let str_var = self.get_str_var_labels(var_name);

        let value_reg = self.compile_int_expr(value);
        let fill = self.load_subroutine(subroutine::Id::UtilFill);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR2,{value}
                {lad_gr1_pos}
                {ld_gr3_len}
                CALL  {fill}"#,
            value = value_reg,
            lad_gr1_pos = str_var.lad_pos(casl2::Register::Gr1),
            ld_gr3_len = str_var.ld_len(casl2::Register::Gr3),
            fill = fill
        ));
        self.code(recovers);

        self.set_register_idle(value_reg);
        self.return_temp_str_var_label(str_var);
    }

    // Fill <ref_int_arr>, <value>
    pub(super) fn compile_fill_ref_integer_array(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| format!("Fill {var}, {value}", var = var_name, value = value));
        self.comment(format!(
            "Fill {var}, {value}",
            var = var_name,
            value = value
        ));

        let arr_label = self.get_ref_int_arr_label(var_name);
        let value_reg = self.compile_int_expr(value);
        let fill = self.load_subroutine(subroutine::Id::UtilFill);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR2,{value}
                {lad_gr1_arrpos}
                LAD   GR3,{size}
                CALL  {fill}"#,
            value = value_reg,
            lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
            size = arr_label.size(),
            fill = fill
        ));
        self.code(recovers);

        self.set_register_idle(value_reg);
    }

    // Fill <int_arr>, <value>
    pub(super) fn compile_fill_integer_array(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| format!("Fill {var}, {value}", var = var_name, value = value));
        self.comment(format!(
            "Fill {var}, {value}",
            var = var_name,
            value = value
        ));

        let arr_label = self.get_int_arr_label(var_name);
        let value_reg = self.compile_int_expr(value);
        let fill = self.load_subroutine(subroutine::Id::UtilFill);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR2,{value}
                {lad_gr1_arrpos}
                LAD   GR3,{size}
                CALL  {fill}"#,
            value = value_reg,
            lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
            size = arr_label.size(),
            fill = fill
        ));
        self.code(recovers);

        self.set_register_idle(value_reg);
    }

    // Fill <ref_bool_arr>, <value>
    pub(super) fn compile_fill_ref_boolean_array(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.add_debugger_hint(|| format!("Fill {var}, {value}", var = var_name, value = value));
        self.comment(format!(
            "Fill {var}, {value}",
            var = var_name,
            value = value
        ));

        let arr_label = self.get_ref_bool_arr_label(var_name);
        let value_reg = self.compile_int_expr(value);
        let fill = self.load_subroutine(subroutine::Id::UtilFill);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR2,{value}
                {lad_gr1_arrpos}
                LAD   GR3,{size}
                CALL  {fill}"#,
            value = value_reg,
            lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
            size = arr_label.size(),
            fill = fill
        ));
        self.code(recovers);

        self.set_register_idle(value_reg);
    }

    // Fill <bool_arr>, <value>
    pub(super) fn compile_fill_boolean_array(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.add_debugger_hint(|| format!("Fill {var}, {value}", var = var_name, value = value));
        self.comment(format!(
            "Fill {var}, {value}",
            var = var_name,
            value = value
        ));

        let arr_label = self.get_bool_arr_label(var_name);
        let value_reg = self.compile_int_expr(value);
        let fill = self.load_subroutine(subroutine::Id::UtilFill);

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR2,{value}
                {lad_gr1_arrpos}
                LAD   GR3,{size}
                CALL  {fill}"#,
            value = value_reg,
            lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
            size = arr_label.size(),
            fill = fill
        ));
        self.code(recovers);

        self.set_register_idle(value_reg);
    }
}
