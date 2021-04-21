use super::*;

impl Compiler {
    // Assign Integer Array
    // int_arr = some_int_arr
    pub(super) fn compile_assign_integer_array(&mut self, var_name: &str, value: &parser::Expr) {
        let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);

        self.comment(format!("{} = {}", var_name, value));

        let dst_arr_label = self.get_int_arr_label(var_name);

        let src_arr_label = self.compile_ref_arr_expr(value);
        assert!(matches!(
            src_arr_label.element_type(),
            parser::ExprType::Integer
        ));
        assert_eq!(dst_arr_label.size(), src_arr_label.size());

        let temp = self.get_temp_int_var_label();

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };

        self.code(saves);
        self.code(format!(
            r#" {lad_gr1_dstpos}
                LAD   GR2,{temp}
                {lad_gr3_srcpos}
                LAD   GR4,{size}
                CALL  {copy}"#,
            lad_gr1_dstpos = dst_arr_label.lad_pos(casl2::Register::Gr1),
            temp = temp,
            lad_gr3_srcpos = src_arr_label.lad_pos(casl2::Register::Gr3),
            size = src_arr_label.size(),
            copy = copystr
        ));
        self.code(recovers);

        self.return_if_temp_arr_label(src_arr_label);
        self.return_if_temp_arr_label(dst_arr_label);
        self.return_temp_int_var_label(temp);
    }

    // Assign Ref Integer Array
    // ref_int_arr = some_int_arr
    pub(super) fn compile_assign_ref_integer_array(
        &mut self,
        var_name: &str,
        value: &parser::Expr,
    ) {
        let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);

        self.comment(format!("{} = {}", var_name, value));

        let dst_arr_label = self.get_ref_int_arr_label(var_name);

        let src_arr_label = self.compile_ref_arr_expr(value);
        assert!(matches!(
            src_arr_label.element_type(),
            parser::ExprType::Integer
        ));
        assert_eq!(dst_arr_label.size(), src_arr_label.size());

        let temp = self.get_temp_int_var_label();

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };

        self.code(saves);
        self.code(format!(
            r#" {lad_gr1_dstpos}
                LAD   GR2,{temp}
                {lad_gr3_srcpos}
                LAD   GR4,{size}
                CALL  {copy}"#,
            lad_gr1_dstpos = dst_arr_label.lad_pos(casl2::Register::Gr1),
            temp = temp,
            lad_gr3_srcpos = src_arr_label.lad_pos(casl2::Register::Gr3),
            size = src_arr_label.size(),
            copy = copystr
        ));
        self.code(recovers);

        self.return_if_temp_arr_label(src_arr_label);
        self.return_if_temp_arr_label(dst_arr_label);
        self.return_temp_int_var_label(temp);
    }

    // Assign Boolean Array
    // bool_arr = some_bool_arr
    pub(super) fn compile_assign_boolean_array(&mut self, var_name: &str, value: &parser::Expr) {
        let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);

        self.comment(format!("{} = {}", var_name, value));

        let dst_arr_label = self.get_bool_arr_label(var_name);

        let src_arr_label = self.compile_ref_arr_expr(value);
        assert!(matches!(
            src_arr_label.element_type(),
            parser::ExprType::Boolean
        ));
        assert_eq!(dst_arr_label.size(), src_arr_label.size());

        let temp = self.get_temp_int_var_label();

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };

        self.code(saves);
        self.code(format!(
            r#" {lad_gr1_dstpos}
                LAD   GR2,{temp}
                {lad_gr3_srcpos}
                LAD   GR4,{size}
                CALL  {copy}"#,
            lad_gr1_dstpos = dst_arr_label.lad_pos(casl2::Register::Gr1),
            temp = temp,
            lad_gr3_srcpos = src_arr_label.lad_pos(casl2::Register::Gr3),
            size = src_arr_label.size(),
            copy = copystr
        ));
        self.code(recovers);

        self.return_if_temp_arr_label(src_arr_label);
        self.return_if_temp_arr_label(dst_arr_label);
        self.return_temp_int_var_label(temp);
    }

    // Assign Ref Boolean Array
    // ref_bool_arr = some_bool_arr
    pub(super) fn compile_assign_ref_boolean_array(
        &mut self,
        var_name: &str,
        value: &parser::Expr,
    ) {
        let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);

        self.comment(format!("{} = {}", var_name, value));

        let dst_arr_label = self.get_ref_bool_arr_label(var_name);

        let src_arr_label = self.compile_ref_arr_expr(value);
        assert!(matches!(
            src_arr_label.element_type(),
            parser::ExprType::Boolean
        ));
        assert_eq!(dst_arr_label.size(), src_arr_label.size());

        let temp = self.get_temp_int_var_label();

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };

        self.code(saves);
        self.code(format!(
            r#" {lad_gr1_dstpos}
                LAD   GR2,{temp}
                {lad_gr3_srcpos}
                LAD   GR4,{size}
                CALL  {copy}"#,
            lad_gr1_dstpos = dst_arr_label.lad_pos(casl2::Register::Gr1),
            temp = temp,
            lad_gr3_srcpos = src_arr_label.lad_pos(casl2::Register::Gr3),
            size = src_arr_label.size(),
            copy = copystr
        ));
        self.code(recovers);

        self.return_if_temp_arr_label(src_arr_label);
        self.return_if_temp_arr_label(dst_arr_label);
        self.return_temp_int_var_label(temp);
    }

    // Assign Sub Into ステートメント
    // int_var -= int_expr
    pub(super) fn compile_assign_sub_into(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!("{var} -= {value}", var = var_name, value = value));

        let var_label = self.get_int_var_label(var_name);

        let value_reg = self.compile_int_expr(value);

        let reg = self.get_idle_register();

        self.code(format!(
            r#" LD    {reg},{var}
                SUBA  {reg},{value}
                ST    {reg},{var}"#,
            reg = reg,
            var = var_label,
            value = value_reg
        ));

        self.set_register_idle(reg);
        self.set_register_idle(value_reg);
    }

    // Assign Ref Sub Into ステートメント
    // ref_int_var -= int_expr
    pub(super) fn compile_assign_ref_sub_into(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!("{var} -= {value}", var = var_name, value = value));

        let var_label = self.get_ref_int_var_label(var_name);

        let value_reg = self.compile_int_expr(value);

        let pos = self.get_idle_register();
        assert_ne!(value_reg, pos);
        let reg = self.get_idle_register();
        assert_ne!(value_reg, reg);
        assert_ne!(pos, reg);

        self.code(format!(
            r#" LD    {pos},{var}
                LD    {reg},0,{pos}
                SUBA  {reg},{value}
                ST    {reg},0,{pos}"#,
            pos = pos,
            reg = reg,
            var = var_label,
            value = value_reg
        ));

        self.set_register_idle(reg);
        self.set_register_idle(pos);
        self.set_register_idle(value_reg);
    }

    // Assign Add Into ステートメント
    // int_var += int_expr
    pub(super) fn compile_assign_add_into(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!("{var} += {value}", var = var_name, value = value));

        let value_reg = self.compile_int_expr(value);

        let var_label = self.get_int_var_label(var_name);

        self.code(format!(
            r#" ADDA  {reg},{var}
                ST    {reg},{var}"#,
            reg = value_reg,
            var = var_label
        ));

        self.set_register_idle(value_reg);
    }

    // Assign Ref Add Into ステートメント
    // ref_int_var += int_expr
    pub(super) fn compile_assign_ref_add_into(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.comment(format!("{var} += {value}", var = var_name, value = value));

        let value_reg = self.compile_int_expr(value);

        let var_label = self.get_ref_int_var_label(var_name);

        let temp_reg = self.get_idle_register();

        self.code(format!(
            r#" LD    {temp},{var}
                ADDA  {reg},0,{temp}
                ST    {reg},0,{temp}"#,
            temp = temp_reg,
            reg = value_reg,
            var = var_label
        ));

        self.set_register_idle(temp_reg);
        self.set_register_idle(value_reg);
    }

    // Assign Boolean ステートメント
    // bool_var = bool_expr
    pub(super) fn compile_assign_boolean(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.comment(format!("{var} = {value}", var = var_name, value = value));

        let reg = self.compile_int_expr(value);
        let var_label = self.get_bool_var_label(var_name);
        let adr = casl2::Adr::label(&var_label);

        // ST {reg},{var}
        self.code(casl2::Command::A {
            code: casl2::A::St,
            r: reg,
            adr,
            x: None,
        });

        self.set_register_idle(reg);
    }

    // Assign Ref Boolean ステートメント
    // Ref bool_var = bool_expr
    pub(super) fn compile_assign_ref_boolean(&mut self, var_name: &str, value: &parser::Expr) {
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.comment(format!("{var} = {value}", var = var_name, value = value));

        let value_reg = self.compile_int_expr(value);
        let var_label = self.get_ref_bool_var_label(var_name);
        let temp_reg = self.get_idle_register();
        assert_ne!(value_reg, temp_reg);

        self.code(format!(
            r#" LD  {temp},{var}
                ST  {value},0,{temp}"#,
            temp = temp_reg,
            var = var_label,
            value = value_reg
        ));

        self.set_register_idle(temp_reg);
        self.set_register_idle(value_reg);
    }

    // Assign String ステートメント
    // str_var = str_expr
    pub(super) fn compile_assign_string(&mut self, var_name: &str, value: &parser::Expr) {
        self.comment(format!("{var} = {value}", var = var_name, value = value));

        let value_label = self.compile_str_expr(value);
        let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
        let var_label = self.get_str_var_labels(var_name);

        let src = format!(
            r#" LAD   GR1,{dstpos}
                LAD   GR2,{dstlen}
                {lad_srcpos}
                {ld_srclen}
                CALL  {copystr}"#,
            dstpos = var_label.pos,
            dstlen = var_label.len,
            lad_srcpos = value_label.lad_pos(casl2::Register::Gr3),
            ld_srclen = value_label.ld_len(casl2::Register::Gr4),
            copystr = copystr
        );

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };
        self.code(saves);
        self.code(src);
        self.code(recovers);

        self.return_temp_str_var_label(value_label);
    }

    // Assign Ref String ステートメント
    // ref_str_var = str_expr
    pub(super) fn compile_assign_ref_string(&mut self, var_name: &str, value: &parser::Expr) {
        self.comment(format!("{var} = {value}", var = var_name, value = value));

        let value_label = self.compile_str_expr(value);
        let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
        let var_labels = self.get_ref_str_var_labels(var_name);

        let src = format!(
            r#" LD    GR1,{dstpos}
                LD    GR2,{dstlen}
                {lad_srcpos}
                {ld_srclen}
                CALL  {copystr}"#,
            dstpos = var_labels.pos,
            dstlen = var_labels.len,
            lad_srcpos = value_label.lad_pos(casl2::Register::Gr3),
            ld_srclen = value_label.ld_len(casl2::Register::Gr4),
            copystr = copystr
        );

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };
        self.code(saves);
        self.code(src);
        self.code(recovers);

        self.return_temp_str_var_label(value_label);
    }

    // Assign Ref Integer ステートメント
    // ref_int_var = int_expr
    pub(super) fn compile_assign_ref_integer(&mut self, var_name: &str, value: &parser::Expr) {
        self.comment(format!("{var} = {value}", var = var_name, value = value));
        let value_reg = self.compile_int_expr(value);
        let ref_reg = self.get_idle_register();

        let var_label = self.get_ref_int_var_label(var_name);

        let src = format!(
            r#" LD {refvar},{var}
                ST {value},0,{refvar}"#,
            refvar = ref_reg,
            var = var_label,
            value = value_reg
        );
        self.code(src);

        self.set_register_idle(ref_reg);
        self.set_register_idle(value_reg);
    }

    // Assign Integer ステートメント
    // int_var = int_expr
    pub(super) fn compile_assign_integer(&mut self, var_name: &str, value: &parser::Expr) {
        self.comment(format!("{var} = {value}", var = var_name, value = value));
        let reg = self.compile_int_expr(value);

        let var_label = self.get_int_var_label(var_name);

        let adr = casl2::Adr::label(&var_label);

        // ST {reg},{var}
        self.code(casl2::Command::A {
            code: casl2::A::St,
            r: reg,
            adr,
            x: None,
        });

        self.set_register_idle(reg);
    }
}
