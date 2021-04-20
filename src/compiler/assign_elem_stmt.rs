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

        self.comment(format!(
            "{var}( {index} ) -= {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.get_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_size - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let temp_reg = self.get_idle_register();
            assert_ne!(value_reg, temp_reg);
            if index == 0 {
                self.code(format!(
                    r#" LD    {reg},{arr}
                        SUBA  {reg},{value}
                        ST    {reg},{arr}"#,
                    reg = temp_reg,
                    arr = arr_label,
                    value = value_reg
                ));
            } else {
                // おそらくGR5
                let index_reg = self.get_idle_register();
                assert_ne!(value_reg, index_reg);
                assert_ne!(temp_reg, index_reg);
                self.code(format!(
                    r#" LAD   {index_reg},{index}
                        LD    {reg},{arr},{index_reg}
                        SUBA  {reg},{value}
                        ST    {reg},{arr},{index_reg}"#,
                    index_reg = index_reg,
                    index = index,
                    reg = temp_reg,
                    arr = arr_label,
                    value = value_reg
                ));
                self.set_register_idle(index_reg);
            }
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
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        let reg = self.get_idle_register();

        self.code(format!(
            r#" LD    {reg},{arr},{index}
                SUBA  {reg},{value}
                ST    {reg},{arr},{index}"#,
            reg = reg,
            value = value_reg,
            arr = arr_label,
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

        self.comment(format!(
            "{var}( {index} ) -= {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.get_ref_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_size - 1);
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
                r#" LD    {arr_reg},{arr}
                    LD    {reg},{index},{arr_reg}
                    SUBA  {reg},{value}
                    ST    {reg},{index},{arr_reg}"#,
                arr_reg = arr_reg,
                index = index,
                reg = temp_reg,
                arr = arr_label,
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
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        let reg = self.get_idle_register();

        self.code(format!(
            r#" ADDL  {index},{arr}
                LD    {reg},0,{index}
                SUBA  {reg},{value}
                ST    {reg},0,{index}"#,
            reg = reg,
            value = value_reg,
            arr = arr_label,
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

        self.comment(format!(
            "{var}( {index} ) += {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.get_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_size - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            if index == 0 {
                self.code(format!(
                    r#" ADDA  {value},{arr}
                        ST    {value},{arr}"#,
                    arr = arr_label,
                    value = value_reg
                ));
            } else {
                // おそらくGR6
                let index_reg = self.get_idle_register();
                assert_ne!(value_reg, index_reg);
                self.code(format!(
                    r#" LAD   {index_reg},{index}
                        ADDA  {value},{arr},{index_reg}
                        ST    {value},{arr},{index_reg}"#,
                    index_reg = index_reg,
                    index = index,
                    arr = arr_label,
                    value = value_reg
                ));
                self.set_register_idle(index_reg);
            }
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
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ADDA  {value},{arr},{index}
                ST    {value},{arr},{index}"#,
            value = value_reg,
            arr = arr_label,
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

        self.comment(format!(
            "{var}( {index} ) += {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.get_ref_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_size - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let reg = self.get_idle_register();
            assert_ne!(value_reg, reg);
            self.code(format!(
                r#" LD    {reg},{arr}
                    ADDA  {value},{index},{reg}
                    ST    {value},{index},{reg}"#,
                reg = reg,
                index = index,
                arr = arr_label,
                value = value_reg
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
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ADDL  {index},{arr}
                ADDA  {value},0,{index}
                ST    {value},0,{index}"#,
            value = value_reg,
            arr = arr_label,
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

        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.get_bool_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_size - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            if index == 0 {
                self.code(format!(
                    r#" ST    {value},{arr}"#,
                    arr = arr_label,
                    value = value_reg
                ));
            } else {
                // おそらくGR6
                let index_reg = self.get_idle_register();
                assert_ne!(value_reg, index_reg);
                self.code(format!(
                    r#" LAD   {index_reg},{index}
                        ST    {value},{arr},{index_reg}"#,
                    index_reg = index_reg,
                    index = index,
                    arr = arr_label,
                    value = value_reg
                ));
                self.set_register_idle(index_reg);
            }
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
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ST {value},{arr},{index}"#,
            value = value_reg,
            arr = arr_label,
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

        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.get_ref_bool_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_size - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let reg = self.get_idle_register();
            assert_ne!(value_reg, reg);
            self.code(format!(
                r#" LD  {reg},{arr}
                    ST  {value},{index},{reg}"#,
                reg = reg,
                index = index,
                arr = arr_label,
                value = value_reg
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
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ADDL {index},{arr}
                ST   {value},0,{index}"#,
            value = value_reg,
            arr = arr_label,
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

        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.get_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_size - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            if index == 0 {
                self.code(format!(
                    r#" ST    {value},{arr}"#,
                    arr = arr_label,
                    value = value_reg
                ));
            } else {
                // おそらくGR6
                let index_reg = self.get_idle_register();
                assert_ne!(value_reg, index_reg);
                self.code(format!(
                    r#" LAD   {index_reg},{index}
                        ST    {value},{arr},{index_reg}"#,
                    index_reg = index_reg,
                    index = index,
                    arr = arr_label,
                    value = value_reg
                ));
                self.set_register_idle(index_reg);
            }
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
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ST {value},{arr},{index}"#,
            value = value_reg,
            arr = arr_label,
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

        self.comment(format!(
            "{var}( {index} ) = {value}",
            var = var_name,
            index = index,
            value = value
        ));

        let (arr_label, arr_size) = self.get_ref_int_arr_label(var_name);
        assert!(arr_size > 0);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_size - 1);
            // おそらくGR7
            let value_reg = self.compile_int_expr(value);
            // おそらくGR6
            let reg = self.get_idle_register();
            assert_ne!(value_reg, reg);
            self.code(format!(
                r#" LD    {reg},{arr}
                    ST    {value},{index},{reg}"#,
                reg = reg,
                index = index,
                arr = arr_label,
                value = value_reg
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
            r#" LD    GR1,{index}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = arr_size,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        self.code(format!(
            r#" ADDL  {index},{arr}
                ST    {value},0,{index}"#,
            value = value_reg,
            arr = arr_label,
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
                LD    GR2,{size}
                CALL  {fit}"#,
            index = index_reg,
            size = str_labels.len,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        // 仮に文字列長が0の文字列変数であったとしても
        // index=0は文字列変数のバッファ予約領域なので書き込んでも無問題

        self.code(format!(
            r#" ST {value},{arr},{index}"#,
            value = value_reg,
            arr = str_labels.pos,
            index = casl2::IndexRegister::try_from(index_reg).expect("BUG")
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
                LD    GR2,{len}
                LD    GR2,0,GR2
                CALL  {fit}"#,
            index = index_reg,
            len = str_labels.len,
            fit = safe_index
        ));
        self.code(recovers);

        self.code(format!(" LD {index},GR0", index = index_reg));

        // 想定では GR6
        let value_reg = self.compile_int_expr(value);

        self.restore_register(index_reg);

        // 仮に文字列長が0の文字列変数であったとしても
        // index=0は文字列変数のバッファ予約領域なので書き込んでも無問題

        self.code(format!(
            r#" ADDL  {index},{arr}
                ST    {value},0,{index}"#,
            value = value_reg,
            arr = str_labels.pos,
            index = casl2::IndexRegister::try_from(index_reg).expect("BUG")
        ));

        self.set_register_idle(value_reg);
        self.set_register_idle(index_reg);
    }
}
