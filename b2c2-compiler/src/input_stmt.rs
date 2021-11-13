// b2c2-compiler crate::input_stmt
// author: Leonardone @ NEETSDKASU

use super::*;

impl Compiler {
    // Input <int_arr>( <index> ) ステートメント
    // 整数配列の要素へのコンソール入力
    pub(super) fn compile_input_element_integer(&mut self, var_name: &str, index: &parser::Expr) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| format!("Input {arr}( {index} )", arr = var_name, index = index));
        self.comment(format!(
            "Input {arr}( {index} )",
            arr = var_name,
            index = index
        ));

        self.has_eof = true;
        let cint_label = self.load_subroutine(subroutine::Id::FuncCInt);

        let arr_label = self.get_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            let s_labels = self.get_temp_str_var_label();
            let ok_label = self.get_new_jump_label();
            // レジスタ退避
            let (saves, recovers) = {
                use casl2::Register::*;
                self.get_save_registers_src(&[Gr1, Gr2])
            };
            self.code(saves);
            if self.option_external_eof {
                self.code(format!(
                    r#" IN    {pos},{len}
                        XOR   GR0,GR0
                        CALL  EOF
                        LAD   GR1,{pos}
                        LD    GR2,{len}
                        JPL   {ok}
                        JZE   {ok}
                        LD    GR0,GR2
                        CALL  EOF
                        XOR   GR2,GR2
{ok}                    CALL  {cint}"#,
                    pos = s_labels.pos,
                    len = s_labels.len,
                    cint = cint_label,
                    ok = ok_label
                ));
            } else {
                self.code(format!(
                    r#" IN    {pos},{len}
                        XOR   GR0,GR0
                        ST    GR0,EOF
                        LAD   GR1,{pos}
                        LD    GR2,{len}
                        JPL   {ok}
                        JZE   {ok}
                        ST    GR2,EOF
                        XOR   GR2,GR2
{ok}                    CALL  {cint}"#,
                    pos = s_labels.pos,
                    len = s_labels.len,
                    cint = cint_label,
                    ok = ok_label
                ));
            }
            self.code(format!(
                r#" {lad_gr1_arrpos}
                    ST GR0,{index},GR1"#,
                lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
                index = index
            ));
            self.code(recovers);
            self.return_temp_str_var_label(s_labels);
            return;
        }

        // 想定では GR7
        let index_reg = self.compile_int_expr(index);

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
        let s_labels = self.get_temp_str_var_label();
        let ok_label = self.get_new_jump_label();

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        // index_regがGR1またはGR2の場合にGR3にfit後のindexを保持は必要
        // index_regがGR1でもGR2でも無い場合でもindexにGR3を利用しててもデメリットは無いと思われる

        self.code(saves);
        if self.option_external_eof {
            self.code(format!(
                r#" LD    GR1,{index}
                    LAD   GR2,{size}
                    CALL  {fit}
                    {lad_gr3_arrpos}
                    ADDL  GR3,GR0
                    IN    {pos},{len}
                    XOR   GR0,GR0
                    CALL  EOF
                    LAD   GR1,{pos}
                    LD    GR2,{len}
                    JPL   {ok}
                    JZE   {ok}
                    LD    GR0,GR2
                    CALL  EOF
                    XOR   GR2,GR2
{ok}                CALL  {cint}
                    ST    GR0,0,GR3"#,
                index = index_reg,
                size = arr_label.size(),
                fit = safe_index,
                lad_gr3_arrpos = arr_label.lad_pos(casl2::Register::Gr3),
                pos = s_labels.pos,
                len = s_labels.len,
                ok = ok_label,
                cint = cint_label
            ));
        } else {
            self.code(format!(
                r#" LD    GR1,{index}
                    LAD   GR2,{size}
                    CALL  {fit}
                    {lad_gr3_arrpos}
                    ADDL  GR3,GR0
                    IN    {pos},{len}
                    XOR   GR0,GR0
                    ST    GR0,EOF
                    LAD   GR1,{pos}
                    LD    GR2,{len}
                    JPL   {ok}
                    JZE   {ok}
                    ST    GR2,EOF
                    XOR   GR2,GR2
{ok}                CALL  {cint}
                    ST    GR0,0,GR3"#,
                index = index_reg,
                size = arr_label.size(),
                fit = safe_index,
                lad_gr3_arrpos = arr_label.lad_pos(casl2::Register::Gr3),
                pos = s_labels.pos,
                len = s_labels.len,
                ok = ok_label,
                cint = cint_label
            ));
        }
        self.code(recovers);

        self.set_register_idle(index_reg); // GR7 解放のはず
        self.return_temp_str_var_label(s_labels);
    }

    // Input <int_var> ステートメント
    // 整数変数へのコンソール入力
    pub(super) fn compile_input_integer(&mut self, var_name: &str) {
        let cint_label = self.load_subroutine(subroutine::Id::FuncCInt);
        let s_labels = self.get_temp_str_var_label();
        let var_label = self.get_int_var_label(var_name);
        let label = self.get_new_jump_label();

        self.has_eof = true;

        self.add_debugger_hint(|| format!("Input {}", var_name));
        self.comment(format!("Input {}", var_name));

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        if self.option_external_eof {
            self.code(format!(
                r#" IN    {pos},{len}
                    XOR   GR0,GR0
                    CALL  EOF
                    LAD   GR1,{pos}
                    LD    GR2,{len}
                    JPL   {ok}
                    JZE   {ok}
                    LD    GR0,GR2
                    CALL  EOF
                    XOR   GR2,GR2
{ok}                CALL  {cint}
                    {st_gr0_var}"#,
                pos = s_labels.pos,
                len = s_labels.len,
                ok = label,
                cint = cint_label,
                st_gr0_var = var_label.st_value(casl2::Register::Gr0, casl2::Register::Gr1)
            ));
        } else {
            self.code(format!(
                r#" IN    {pos},{len}
                    XOR   GR0,GR0
                    ST    GR0,EOF
                    LAD   GR1,{pos}
                    LD    GR2,{len}
                    JPL   {ok}
                    JZE   {ok}
                    ST    GR2,EOF
                    XOR   GR2,GR2
{ok}                CALL  {cint}
                    {st_gr0_var}"#,
                pos = s_labels.pos,
                len = s_labels.len,
                ok = label,
                cint = cint_label,
                st_gr0_var = var_label.st_value(casl2::Register::Gr0, casl2::Register::Gr1)
            ));
        }
        self.code(recovers);

        self.return_temp_str_var_label(s_labels);
    }

    // Input <str_var> ステートメント
    // 文字列変数へのコンソール入力
    pub(super) fn compile_input_string(&mut self, var_name: &str) {
        let str_labels = self.get_str_var_labels(var_name);
        let label = self.get_new_jump_label();
        self.has_eof = true;
        self.add_debugger_hint(|| format!("Input {}", var_name));
        self.comment(format!("Input {}", var_name));

        if self.option_use_allocator {
            let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
            let temp_labels = self.get_temp_str_var_label();

            let (saves, recovers) = {
                use casl2::Register::*;
                self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
            };

            self.code(saves);
            if self.option_external_eof {
                self.code(format!(
                    r#" IN    {pos},{len}
                        XOR   GR0,GR0
                        CALL  EOF
                        LD    GR4,{len}
                        JPL   {ok}
                        JZE   {ok}
                        LD    GR0,GR4
                        CALL  EOF
                        XOR   GR4,GR4
{ok}                    {lad_gr1_strpos}
                        {lad_gr2_strlen}
                        LAD   GR3,{pos}
                        CALL  {copy}
    "#,
                    pos = temp_labels.pos,
                    len = temp_labels.len,
                    ok = label,
                    lad_gr1_strpos = str_labels.lad_pos(casl2::Register::Gr1),
                    lad_gr2_strlen = str_labels.lad_len(casl2::Register::Gr2),
                    copy = copystr
                ));
            } else {
                self.code(format!(
                    r#" IN    {pos},{len}
                        XOR   GR0,GR0
                        ST    GR0,EOF
                        LD    GR4,{len}
                        JPL   {ok}
                        JZE   {ok}
                        ST    GR4,EOF
                        XOR   GR4,GR4
{ok}                    {lad_gr1_strpos}
                        {lad_gr2_strlen}
                        LAD   GR3,{pos}
                        CALL  {copy}
"#,
                    pos = temp_labels.pos,
                    len = temp_labels.len,
                    ok = label,
                    lad_gr1_strpos = str_labels.lad_pos(casl2::Register::Gr1),
                    lad_gr2_strlen = str_labels.lad_len(casl2::Register::Gr2),
                    copy = copystr
                ));
            }
            self.code(recovers);
            self.return_temp_str_var_label(temp_labels);
        } else {
            // 通常処理

            if self.option_external_eof {
                self.code(format!(
                    r#" IN    {pos},{len}
                        XOR   GR0,GR0
                        CALL  EOF
                        LD    GR0,{len}
                        JPL   {ok}
                        JZE   {ok}
                        CALL  EOF
                        XOR   GR0,GR0
                        ST    GR0,{len}
{ok}                    NOP"#,
                    pos = str_labels.pos,
                    len = str_labels.len,
                    ok = label
                ));
            } else {
                // IN {var_pos},{var_len}
                self.code(format!(
                    r#" IN   {pos},{len}
                        XOR  GR0,GR0
                        ST   GR0,EOF
                        LD   GR0,{len}
                        JPL  {ok}
                        JZE  {ok}
                        ST   GR0,EOF
                        XOR  GR0,GR0
                        ST   GR0,{len}
{ok}                    NOP"#,
                    pos = str_labels.pos,
                    len = str_labels.len,
                    ok = label
                ));
            }
        }
    }

    // Input <ref_int_arr>( <index> ) ステートメント
    // 整数配列(参照型)の要素へのコンソール入力
    pub(super) fn compile_input_ref_element_integer(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
    ) {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        self.add_debugger_hint(|| format!("Input {arr}( {index} )", arr = var_name, index = index));
        self.comment(format!(
            "Input {arr}( {index} )",
            arr = var_name,
            index = index
        ));

        self.has_eof = true;
        let cint_label = self.load_subroutine(subroutine::Id::FuncCInt);

        let arr_label = self.get_ref_int_arr_label(var_name);

        // indexがリテラルの場合
        if let parser::Expr::LitInteger(index) = index {
            let index = *index as i16;
            let index = (index.max(0) as usize).min(arr_label.size() - 1);
            let s_labels = self.get_temp_str_var_label();
            let ok_label = self.get_new_jump_label();
            // レジスタ退避
            let (saves, recovers) = {
                use casl2::Register::*;
                self.get_save_registers_src(&[Gr1, Gr2])
            };
            self.code(saves);
            if self.option_external_eof {
                self.code(format!(
                    r#" IN    {pos},{len}
                        XOR   GR0,GR0
                        CALL  EOF
                        LAD   GR1,{pos}
                        LD    GR2,{len}
                        JPL   {ok}
                        JZE   {ok}
                        LD    GR0,GR2
                        CALL  EOF
                        XOR   GR2,GR2
{ok}                    CALL  {cint}"#,
                    pos = s_labels.pos,
                    len = s_labels.len,
                    cint = cint_label,
                    ok = ok_label
                ));
            } else {
                self.code(format!(
                    r#" IN    {pos},{len}
                        XOR   GR0,GR0
                        ST    GR0,EOF
                        LAD   GR1,{pos}
                        LD    GR2,{len}
                        JPL   {ok}
                        JZE   {ok}
                        ST    GR2,EOF
                        XOR   GR2,GR2
{ok}                    CALL  {cint}"#,
                    pos = s_labels.pos,
                    len = s_labels.len,
                    cint = cint_label,
                    ok = ok_label
                ));
            }
            self.code(format!(
                r#" {lad_gr1_arrpos}
                    ST   GR0,{index},GR1"#,
                lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
                index = index
            ));
            self.code(recovers);
            self.return_temp_str_var_label(s_labels);
            return;
        }

        // 想定では GR7
        let index_reg = self.compile_int_expr(index);

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
        let s_labels = self.get_temp_str_var_label();
        let ok_label = self.get_new_jump_label();

        // 想定では、
        //  index_reg = GR7
        //  他のレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3])
        };

        // index_regがGR1またはGR2の場合にGR3にfit後のindexを保持は必要
        // index_regがGR1でもGR2でも無い場合でもindexにGR3を利用しててもデメリットは無いと思われる

        self.code(saves);
        if self.option_external_eof {
            self.code(format!(
                r#" LD    GR1,{index}
                    LAD   GR2,{size}
                    CALL  {fit}
                    {lad_gr3_arrpos}
                    ADDL  GR3,GR0
                    IN    {pos},{len}
                    XOR   GR0,GR0
                    CALL  EOF
                    LAD   GR1,{pos}
                    LD    GR2,{len}
                    JPL   {ok}
                    JZE   {ok}
                    LD    GR0,GR2
                    CALL  EOF
                    XOR   GR2,GR2
{ok}                CALL  {cint}
                    ST    GR0,0,GR3"#,
                index = index_reg,
                size = arr_label.size(),
                fit = safe_index,
                lad_gr3_arrpos = arr_label.lad_pos(casl2::Register::Gr3),
                pos = s_labels.pos,
                len = s_labels.len,
                ok = ok_label,
                cint = cint_label
            ));
        } else {
            self.code(format!(
                r#" LD    GR1,{index}
                    LAD   GR2,{size}
                    CALL  {fit}
                    {lad_gr3_arrpos}
                    ADDL  GR3,GR0
                    IN    {pos},{len}
                    XOR   GR0,GR0
                    ST    GR0,EOF
                    LAD   GR1,{pos}
                    LD    GR2,{len}
                    JPL   {ok}
                    JZE   {ok}
                    ST    GR2,EOF
                    XOR   GR2,GR2
{ok}                CALL  {cint}
                    ST    GR0,0,GR3"#,
                index = index_reg,
                size = arr_label.size(),
                fit = safe_index,
                lad_gr3_arrpos = arr_label.lad_pos(casl2::Register::Gr3),
                pos = s_labels.pos,
                len = s_labels.len,
                ok = ok_label,
                cint = cint_label
            ));
        }
        self.code(recovers);

        self.set_register_idle(index_reg); // GR7 解放のはず
        self.return_temp_str_var_label(s_labels);
    }

    // Input <ref_int_var> ステートメント
    // 整数変数(参照型)へのコンソール入力
    pub(super) fn compile_input_ref_integer(&mut self, var_name: &str) {
        let cint_label = self.load_subroutine(subroutine::Id::FuncCInt);
        let s_labels = self.get_temp_str_var_label();
        let var_label = self.get_ref_int_var_label(var_name);
        let label = self.get_new_jump_label();

        self.has_eof = true;

        self.add_debugger_hint(|| format!("Input {}", var_name));
        self.comment(format!("Input {}", var_name));

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2])
        };

        self.code(saves);
        if self.option_external_eof {
            self.code(format!(
                r#" IN    {pos},{len}
                    XOR   GR0,GR0
                    CALL  EOF
                    LAD   GR1,{pos}
                    LD    GR2,{len}
                    JPL   {ok}
                    JZE   {ok}
                    LD    GR0,GR2
                    CALL  EOF
                    XOR   GR2,GR2
{ok}                CALL  {cint}
                    {st_gr0_var}"#,
                pos = s_labels.pos,
                len = s_labels.len,
                ok = label,
                cint = cint_label,
                st_gr0_var = var_label.st_value(casl2::Register::Gr0, casl2::Register::Gr1)
            ));
        } else {
            self.code(format!(
                r#" IN    {pos},{len}
                    XOR   GR0,GR0
                    ST    GR0,EOF
                    LAD   GR1,{pos}
                    LD    GR2,{len}
                    JPL   {ok}
                    JZE   {ok}
                    ST    GR2,EOF
                    XOR   GR2,GR2
{ok}                CALL  {cint}
                    {st_gr0_var}"#,
                pos = s_labels.pos,
                len = s_labels.len,
                ok = label,
                cint = cint_label,
                st_gr0_var = var_label.st_value(casl2::Register::Gr0, casl2::Register::Gr1)
            ));
        }
        self.code(recovers);

        self.return_temp_str_var_label(s_labels);
    }

    // Input <ref_str_var> ステートメント
    // 文字列変数(参照型)へのコンソール入力
    pub(super) fn compile_input_ref_string(&mut self, var_name: &str) {
        let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);

        let str_labels = self.get_ref_str_var_labels(var_name);

        let temp_labels = self.get_temp_str_var_label();

        let label = self.get_new_jump_label();

        self.has_eof = true;

        self.add_debugger_hint(|| format!("Input {}", var_name));
        self.comment(format!("Input {}", var_name));

        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };

        self.code(saves);
        if self.option_external_eof {
            self.code(format!(
                r#" IN    {pos},{len}
                    XOR   GR0,GR0
                    CALL  EOF
                    LD    GR4,{len}
                    JPL   {ok}
                    JZE   {ok}
                    LD    GR0,GR4
                    CALL  EOF
                    XOR   GR4,GR4
{ok}                {lad_gr1_strpos}
                    {lad_gr2_strlen}
                    LAD   GR3,{pos}
                    CALL  {copy}
"#,
                pos = temp_labels.pos,
                len = temp_labels.len,
                ok = label,
                lad_gr1_strpos = str_labels.lad_pos(casl2::Register::Gr1),
                lad_gr2_strlen = str_labels.lad_len(casl2::Register::Gr2),
                copy = copystr
            ));
        } else {
            self.code(format!(
                r#" IN    {pos},{len}
                    XOR   GR0,GR0
                    ST    GR0,EOF
                    LD    GR4,{len}
                    JPL   {ok}
                    JZE   {ok}
                    ST    GR4,EOF
                    XOR   GR4,GR4
{ok}                {lad_gr1_strpos}
                    {lad_gr2_strlen}
                    LAD   GR3,{pos}
                    CALL  {copy}
"#,
                pos = temp_labels.pos,
                len = temp_labels.len,
                ok = label,
                lad_gr1_strpos = str_labels.lad_pos(casl2::Register::Gr1),
                lad_gr2_strlen = str_labels.lad_len(casl2::Register::Gr2),
                copy = copystr
            ));
        }
        self.code(recovers);

        self.return_temp_str_var_label(temp_labels);
    }
}
