use super::*;

impl Compiler {
    // Assign Sub Into Element ステートメント
    // int_arr(index) -= int_expr
    pub(super) fn compile_assign_sub_into_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) -= {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) -= {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let arr_label = self.get_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let temp_reg = self.get_idle_register();
            // おそらくGR5
            let extra_reg = self.get_idle_register();
            assert_ne!(value_reg, temp_reg);
            assert_ne!(value_reg, extra_reg);
            assert_ne!(temp_reg, extra_reg);
            if index == 0 {
                self.code(format!(
                    r#" {ld_temp_first_elem}
                        SUBA  {temp},{value}
                        {st_temp_first_elem}"#,
                    ld_temp_first_elem = arr_label.ld_first_elem(temp_reg),
                    temp = temp_reg,
                    value = value_reg,
                    st_temp_first_elem = arr_label.st_first_elem(temp_reg, extra_reg)
                ));
            } else {
                self.code(format!(
                    r#" {lad_extra_arrpos}
                        LD    {temp},{index},{extra}
                        SUBA  {temp},{value}
                        ST    {temp},{index},{extra}"#,
                    lad_extra_arrpos = arr_label.lad_pos(extra_reg),
                    extra = extra_reg,
                    index = index,
                    temp = temp_reg,
                    value = value_reg
                ));
            }
            self.set_register_idle(extra_reg);
            self.set_register_idle(temp_reg);
            self.set_register_idle(value_reg);
            return;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD   GR1,{index}
                LAD  GR2,{size}
                CALL {fit}"#,
            index = index_reg,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL {index},GR0"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        let reg = self.get_idle_register();

        self.code(format!(
            r#" LD    {reg},0,{index}
                SUBA  {reg},{value}
                ST    {reg},0,{index}"#,
            reg = reg,
            value = value_reg,
            index = index_reg
        ));

        self.set_register_idle(reg);
        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Ref Sub Into Element ステートメント
    // ref_int_arr(index) -= int_expr
    pub(super) fn compile_assign_ref_sub_into_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) -= {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) -= {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let arr_label = self.get_ref_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let temp_reg = self.get_idle_register();
            assert_ne!(value_reg, temp_reg);
            // おそらくGR5
            let arr_reg = self.get_idle_register();
            assert_ne!(value_reg, arr_reg);
            assert_ne!(temp_reg, arr_reg);
            self.code(format!(
                r#" {lad_arr_pos}
                    LD    {reg},{index},{arr}
                    SUBA  {reg},{value}
                    ST    {reg},{index},{arr}"#,
                lad_arr_pos = arr_label.lad_pos(arr_reg),
                arr = arr_reg,
                index = index,
                reg = temp_reg,
                value = value_reg
            ));
            self.set_register_idle(arr_reg);
            self.set_register_idle(temp_reg);
            self.set_register_idle(value_reg);
            return;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD   GR1,{index}
                LAD  GR2,{size}
                CALL {fit}"#,
            index = index_reg,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL {index},GR0"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        let reg = self.get_idle_register();

        self.code(format!(
            r#" LD    {reg},0,{index}
                SUBA  {reg},{value}
                ST    {reg},0,{index}"#,
            reg = reg,
            value = value_reg,
            index = index_reg
        ));

        self.set_register_idle(reg);
        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Add Into Element ステートメント
    // int_arr(index) += int_expr
    pub(super) fn compile_assign_add_into_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) += {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) += {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let arr_label = self.get_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let temp_reg = self.get_idle_register();
            assert_ne!(value_reg, temp_reg);
            self.code(format!(
                r#" {lad_temp_arrpos}
                    ADDA  {value},{index},{temp}
                    ST    {value},{index},{temp}"#,
                lad_temp_arrpos = arr_label.lad_pos(temp_reg),
                value = value_reg,
                index = index,
                temp = temp_reg
            ));
            self.set_register_idle(temp_reg);
            self.set_register_idle(value_reg);
            return;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD   GR1,{index}
                LAD  GR2,{size}
                CALL {fit}"#,
            index = index_reg,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL {index},GR0"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ADDA  {value},0,{index}
                ST    {value},0,{index}"#,
            value = value_reg,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Ref Add Into Element ステートメント
    // ref_int_arr(index) += int_expr
    pub(super) fn compile_assign_ref_add_into_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) += {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) += {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let arr_label = self.get_ref_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let reg = self.get_idle_register();
            assert_ne!(value_reg, reg);
            self.code(format!(
                r#" {lad_reg_arrpos}
                    ADDA  {value},{index},{reg}
                    ST    {value},{index},{reg}"#,
                lad_reg_arrpos = arr_label.lad_pos(reg),
                value = value_reg,
                index = index,
                reg = reg
            ));
            self.set_register_idle(reg);
            self.set_register_idle(value_reg);
            return;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD   GR1,{index}
                LAD  GR2,{size}
                CALL {fit}"#,
            index = index_reg,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL {index},GR0"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ADDA  {value},0,{index}
                ST    {value},0,{index}"#,
            value = value_reg,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Boolean Element ステートメント
    // bool_arr(index) = bool_expr
    pub(super) fn compile_assign_boolean_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) = {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let arr_label = self.get_bool_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // GR6
            let temp_reg = self.get_idle_register();
            assert_ne!(value_reg, temp_reg);
            self.code(format!(
                r#" {lad_temp_arrpos}
                    ST    {value},{index},{temp}"#,
                lad_temp_arrpos = arr_label.lad_pos(temp_reg),
                value = value_reg,
                index = index,
                temp = temp_reg
            ));
            self.set_register_idle(temp_reg);
            self.set_register_idle(value_reg);
            return;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL {index},GR0"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ST {value},0,{index}"#,
            value = value_reg,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Ref Boolean Element ステートメント
    // ref_bool_arr(index) = bool_expr
    pub(super) fn compile_assign_ref_boolean_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Boolean));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) = {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let arr_label = self.get_ref_bool_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let temp_reg = self.get_idle_register();
            assert_ne!(value_reg, temp_reg);
            self.code(format!(
                r#" {lad_temp_arrpos}
                    ST  {value},{index},{temp}"#,
                lad_temp_arrpos = arr_label.lad_pos(temp_reg),
                value = value_reg,
                index = index,
                temp = temp_reg
            ));
            self.set_register_idle(temp_reg);
            self.set_register_idle(value_reg);
            return;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL {index},GR0"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ST   {value},0,{index}"#,
            value = value_reg,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Integer Element ステートメント
    // int_arr(index) = int_expr
    pub(super) fn compile_assign_integer_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) = {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let arr_label = self.get_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let temp_reg = self.get_idle_register();
            assert_ne!(value_reg, temp_reg);
            self.code(format!(
                r#" {lad_temp_arrpos}
                    ST    {value},{index},{temp}"#,
                lad_temp_arrpos = arr_label.lad_pos(temp_reg),
                value = value_reg,
                index = index,
                temp = temp_reg
            ));
            self.set_register_idle(temp_reg);
            self.set_register_idle(value_reg);
            return;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(
            r#"  {lad_index_arrpos}
                 ADDL {index},GR0"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ST {value},0,{index}"#,
            value = value_reg,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Ref Integer Element ステートメント
    // ref_int_arr(index) = int_expr
    pub(super) fn compile_assign_ref_integer_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) = {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let arr_label = self.get_ref_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let temp_reg = self.get_idle_register();
            assert_ne!(value_reg, temp_reg);
            self.code(format!(
                r#" {lad_temp_arrpos}
                    ST  {value},{index},{temp}"#,
                lad_temp_arrpos = arr_label.lad_pos(temp_reg),
                value = value_reg,
                index = index,
                temp = temp_reg
            ));
            self.set_register_idle(temp_reg);
            self.set_register_idle(value_reg);
            return;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL {index},GR0"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ST    {value},0,{index}"#,
            value = value_reg,
            index = index_reg
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Character Element ステートメント
    // str_var(index) = int_expr
    pub(super) fn compile_assign_character_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) = {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let str_labels = self.get_str_var_labels(var_name);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                {ld_gr2_len}
                CALL  {fit}"#,
            index = index_reg,
            ld_gr2_len = str_labels.ld_len(casl2::Register::Gr2),
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(
            r#" {lad_index_pos}
                ADDL {index},GR0"#,
            lad_index_pos = str_labels.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        // 仮に文字列長が0の文字列変数であったとしても
        // index=0は文字列変数のバッファ予約領域なので書き込んでも無問題

        self.code(format!(
            r#" ST {value},0,{index}"#,
            value = value_reg,
            index = casl2::IndexRegister::try_from(index_reg).unwrap()
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }

    // Assign Ref Character Element ステートメント
    // ref_str_var(index) = int_expr
    pub(super) fn compile_assign_ref_character_element(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
        value: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| {
            format!(
                "{var}( {index} ) = {value}",
                var = var_name,
                index = index,
                value = value
            )
        });
        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let str_labels = self.get_ref_str_var_labels(var_name);

        let index_reg = self.compile_int_expr(index);

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        self.code(format!(
            r#" LD    GR1,{index}
                {ld_gr2_len}
                CALL  {fit}"#,
            index = index_reg,
            ld_gr2_len = str_labels.ld_len(casl2::Register::Gr2),
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(
            r#" {lad_index_pos}
                ADDL {index},GR0"#,
            lad_index_pos = str_labels.lad_pos(index_reg),
            index = index_reg
        ));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        // 仮に文字列長が0の文字列変数であったとしても
        // index=0は文字列変数のバッファ予約領域なので書き込んでも無問題

        self.code(format!(
            r#" ST  {value},0,{index}"#,
            value = value_reg,
            index = casl2::IndexRegister::try_from(index_reg).unwrap()
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }
}
