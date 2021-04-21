use super::*;

// 式展開等
impl Compiler {
    // 式の展開 (戻り値が文字列)
    pub(super) fn compile_str_expr(&mut self, expr: &parser::Expr) -> StrLabels {
        use parser::Expr::*;
        match expr {
            BinaryOperatorString(op, lhs, rhs) => self.compile_bin_op_string(*op, lhs, rhs),
            FunctionString(func, param) => self.compile_function_string(*func, param),
            LitString(lit_str) => self.get_lit_str_label_if_exists(lit_str),
            VarString(var_name) => self.get_str_var_labels(var_name),
            VarRefString(var_name) => self.get_ref_str_var_labels(var_name),

            // 戻り値が文字列ではないもの
            BinaryOperatorBoolean(..)
            | BinaryOperatorInteger(..)
            | CharOfLitString(..)
            | CharOfVarString(..)
            | CharOfVarRefString(..)
            | FunctionBoolean(..)
            | FunctionInteger(..)
            | FunctionBooleanArray(..)
            | FunctionIntegerArray(..)
            | LitBoolean(..)
            | LitInteger(..)
            | LitCharacter(..)
            | UnaryOperatorInteger(..)
            | UnaryOperatorBoolean(..)
            | VarBoolean(..)
            | VarInteger(..)
            | VarRefBoolean(..)
            | VarRefInteger(..)
            | VarArrayOfBoolean(..)
            | VarArrayOfInteger(..)
            | VarRefArrayOfBoolean(..)
            | VarRefArrayOfInteger(..)
            | ReferenceOfVar(..)
            | ParamList(..) => unreachable!("BUG"),
        }
    }

    // 式の展開 (戻り値が整数or真理値)
    pub(super) fn compile_int_expr(&mut self, expr: &parser::Expr) -> casl2::Register {
        use parser::Expr::*;
        match expr {
            BinaryOperatorBoolean(op, lhs, rhs) => self.compile_bin_op_boolean(*op, lhs, rhs),
            BinaryOperatorInteger(op, lhs, rhs) => self.compile_bin_op_integer(*op, lhs, rhs),
            CharOfLitString(lit_str, index) => self.compile_character_of_literal(lit_str, index),
            CharOfVarString(var_name, index) => self.compile_character_of_variable(var_name, index),
            CharOfVarRefString(var_name, index) => {
                self.compile_character_of_ref_variable(var_name, index)
            }
            FunctionBoolean(func, param) => self.compile_function_boolean(*func, param),
            FunctionInteger(func, param) => self.compile_function_integer(*func, param),
            LitBoolean(lit_bool) => self.compile_literal_boolean(*lit_bool),
            LitInteger(lit_int) => self.compile_literal_integer(*lit_int),
            LitCharacter(lit_char) => self.compile_literal_character(*lit_char),
            UnaryOperatorInteger(op, value) => self.compile_unary_op_integer(*op, value),
            UnaryOperatorBoolean(op, value) => self.compile_unary_op_boolean(*op, value),
            VarBoolean(var_name) => self.compile_variable_boolean(var_name),
            VarRefBoolean(var_name) => self.compile_variable_ref_boolean(var_name),
            VarInteger(var_name) => self.compile_variable_integer(var_name),
            VarRefInteger(var_name) => self.compile_variable_ref_integer(var_name),
            VarArrayOfBoolean(arr_name, index) => {
                self.compile_variable_array_of_boolean(arr_name, index)
            }
            VarRefArrayOfBoolean(arr_name, index) => {
                self.compile_variable_ref_array_of_boolean(arr_name, index)
            }
            VarArrayOfInteger(arr_name, index) => {
                self.compile_variable_array_of_integer(arr_name, index)
            }
            VarRefArrayOfInteger(arr_name, index) => {
                self.compile_variable_ref_array_of_integer(arr_name, index)
            }

            // 戻り値が整数でも真理値でもないもの
            BinaryOperatorString(..)
            | FunctionString(..)
            | FunctionBooleanArray(..)
            | FunctionIntegerArray(..)
            | LitString(..)
            | VarString(..)
            | VarRefString(..)
            | ReferenceOfVar(..)
            | ParamList(_) => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 整数を返す単項演算子の処理
    pub(super) fn compile_unary_op_integer(
        &mut self,
        op: tokenizer::Operator,
        value: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        match op {
            Not => {
                assert!(matches!(value.return_type(), parser::ExprType::Integer));
                let reg = self.compile_int_expr(value);
                self.code(format!(" XOR {reg},=#FFFF", reg = reg));
                reg
            }

            Sub => {
                assert!(matches!(value.return_type(), parser::ExprType::Integer));
                let reg = self.compile_int_expr(value);
                self.code(format!(
                    r#" XOR {reg},=#FFFF
                        LAD {reg},1,{reg}"#,
                    reg = reg
                ));
                reg
            }

            // 真理値を返さないもの、または単項演算子ではないもの
            And | Xor | Or | NotEqual | LessOrEequal | GreaterOrEqual | Equal | LessThan
            | GreaterThan | ShiftLeft | ShiftRight | Add | Mul | Div | Mod | AddInto | SubInto
            | Concat | OpenBracket | CloseBracket | Comma => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 真理値を返す単項演算子の処理
    pub(super) fn compile_unary_op_boolean(
        &mut self,
        op: tokenizer::Operator,
        value: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        match op {
            Not => {
                assert!(matches!(value.return_type(), parser::ExprType::Boolean));
                let reg = self.compile_int_expr(value);
                self.code(format!(" XOR {reg},=#FFFF", reg = reg));
                reg
            }

            // 真理値を返さないもの、または単項演算子ではないもの
            And | Xor | Or | NotEqual | LessOrEequal | GreaterOrEqual | Equal | LessThan
            | GreaterThan | ShiftLeft | ShiftRight | Add | Sub | Mul | Div | Mod | AddInto
            | SubInto | Concat | OpenBracket | CloseBracket | Comma => unreachable!("BUG"),
        }
    }

    // (式展開の処理の一部)
    // 文字リテラルを返す
    pub(super) fn compile_literal_character(&mut self, lit_char: char) -> casl2::Register {
        let reg = self.get_idle_register();

        if lit_char == '\'' {
            self.code(format!(r#" LD {reg},=''''"#, reg = reg));
        } else {
            self.code(format!(r#" LD {reg},='{ch}'"#, reg = reg, ch = lit_char));
        }

        reg
    }

    // (式展開の処理の一部)
    // 文字列リテラルの文字を取り出す
    pub(super) fn compile_character_of_literal(
        &mut self,
        lit_str: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        // サイズ0の文字列…(parserでコンパイルエラーにすべきな気が…)
        if lit_str.is_empty() {
            let reg = self.get_idle_register();
            self.code(format!(" XOR {reg},{reg}", reg = reg));
            return reg;
        }

        let str_labels = self.get_lit_str_label_if_exists(lit_str);

        // リテラル文字列の一部をリテラル整数で指定する、だと…？
        if let parser::Expr::LitInteger(index) = index {
            let index = ((*index).max(0) as usize).min(lit_str.chars().count() - 1);
            let ch = lit_str.chars().nth(index).unwrap();
            return self.compile_literal_character(ch);
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                {ld_size}
                CALL  {fit}"#,
            index_line = index_line,
            ld_size = str_labels.ld_len(casl2::Register::Gr2),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" LD    {index},GR0
                LD    {index},{lit},{index}"#,
            index = index_reg,
            lit = str_labels.pos
        ));

        index_reg
    }

    // (式展開の処理の一部)
    // 文字列変数の文字を取り出す
    pub(super) fn compile_character_of_variable(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let load_elem = self.load_subroutine(subroutine::Id::UtilLoadElement);

        let index_reg = self.compile_int_expr(index);

        let str_labels = self.get_str_var_labels(var_name);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2, Gr3])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2, Gr3])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        /*
           考察

           サブルーチンを一切使用しない場合
           新規レジスタは無いが、11行でラベルを4つ消費…(ラベルコストが高すぎる、10行あたり2個が理想)
                 AND   {index},{index}
                 JPL   {ok1}
                 XOR   {index},{index}
          {ok1}  CPL   {index},{len}
                 JMI   {ok3}
                 LD    {index},{len}
                 JNZ   {ok2}
                 XOR   {index},{index}
                 JUMP  {ok4}
         {ok2}   LAD   {index},-1,{index}
         {ok3}   LD    {index},{var},{index}
         {ok4}   NOP

                 7～12行でラベルを1つ消費(ただしサブルーチン行コスト13がある)
                 (大半のケースは8行と思われ)(複雑な深いネストの計算式でもないとGR2あたりまで使用しない)
                 (8行呼び出し5回以上でサブルーチンコストはチャラになる)
                 (5回以上呼び出すケースはレアそうだが,INTSORTなど)
                 (ラベルコストは4回以上でチャラ)
                 ( 11 * 1 = 11　[ 4],  8 * 1 + 13 = 21 [1+4 = 5])
                 ( 11 * 2 = 22 [ 8],  8 * 2 + 13 = 29 [2+4 = 6])
                 ( 11 * 3 = 33 [12],  8 * 3 + 13 = 37 [3+4 = 7])
                 ( 11 * 4 = 44 [16],  8 * 4 + 13 = 45 [4+4 = 8])
                 ( 11 * 5 = 55 [20],  8 * 5 + 13 = 53 [5+4 = 9])
                 ( 11 * 6 = 66 [24],  8 * 6 + 13 = 61 [6+4 =10])
                 ( 11 * 7 = 77 [28],  8 * 7 + 13 = 69 [7+4 =11])

                 [ PUSH  0,GR1 ]
                 [ PUSH  0,GR2 ]
                 [ LD    GR1,{index} ]
                   LD    GR2,{len}
                   CALL  {fit}
                 [ POP   GR2 ]
                 [ POP   GR1 ]
                   LD    {index},GR0
                   LD    {index},{var},{index}
                   LD    GR0,{len}
                   JNZ   {ok}
                   XOR   {index},{index}
         {ok}      NOP

                 SafeIndexサブルーチン(13行)
         {fit}     AND    GR2,GR2
                   JNZ    {lbound}
                   XOR    GR0,GR0
                   RET
         {lbound}  LD     GR0,GR1
                   JPL    {ubound}
                   XOR    GR0,GR0
                   RET
         {ubound}  CPL    GR0,GR2
                   JMI    {ret}
                   LAD    GR0,-1
                   ADDL   GR0,GR2
         {ret}     RET



                 いっそ要素参照までをサブルーチン化は？
                 4～11行のコスト、ラベルコストはない
                 (大半は5行となると思う)(複雑な深いネストの計算式でもないとGR3あたりまで使用しない)
                 サブルーチンコストは
                    行コスト 13+10=23
                    ラベルコスト 4+2=6
                 4回以上呼び出しでチャラだが…
                 (長さ0の文字列変数があるればの恩恵で、長さ1以上が確定の固定長整数配列などにはあまり意味が無いSafeIndexだけでよいが)
                 ( 11 * 1 = 11　[ 4],  5 * 1 + 23 = 28 [6])
                 ( 11 * 2 = 22 [ 8],  5 * 2 + 23 = 33 [6])
                 ( 11 * 3 = 33 [12],  5 * 3 + 23 = 38 [6])
                 ( 11 * 4 = 44 [16],  5 * 4 + 23 = 43 [6])
                 ( 11 * 5 = 55 [20],  5 * 5 + 23 = 48 [6])
                 ( 11 * 6 = 66 [24],  5 * 6 + 23 = 53 [6])
                 ( 11 * 7 = 77 [28],  5 * 7 + 23 = 58 [6])

                 [ PUSH 0,GR1 ]
                 [ PUSH 0,GR2 ]
                 [ PUSH 0,GR3 ]
                 [ LD   GR1,{index} ]
                   LD   GR2,{len}
                   LAD  GR3,{var}
                   CALL {load}
                 [ POP  GR3 ]
                 [ POP  GR2 ]
                 [ POP  GR1 ]
                   LD   {index},GR0

               LoadElementサブルーチン(10行)
        {load}     AND  GR2,GR2
                   JNZ  {ok}
                   XOR  GR0,GR0
                   RET
        {ok}       CALL {fit}
                   PUSH 0,GR3
                   ADDL GR3,GR0
                   LD   GR0,0,GR3
                   POP  GR3
                   RET

         文字列変数は長さ0がありうるからそのチェックが必要だが
         真理値・整数配列なら固定長配列で長さ1以上が保証されてるから…
         素で書くと8行コストのラベル2個か…
                 AND   {index},{index}
                 JPL   {ok1}
                 XOR   {index},{index}
          {ok1}  CPL   {index},{len}
                 JMI   {ok2}
                 LAD   {index},-1
                 ADDL  {index},{len}
          {ok2}  LD    {index},{arr},{index}

          SafeIndexサブルーチンを使用すると4～9行(大半は5行と思われ)、ラベルコストなし
          (LoadElementサブルーチンを使う利点は皆無…最大行が増えるリスクが発生してしまう)
                 [ PUSH  0,GR1 ]
                 [ PUSH  0,GR2 ]
                 [ LD    GR1,{index} ]
                   LD    GR2,{len}
                   CALL  {fit}
                 [ POP   GR2 ]
                 [ POP   GR1 ]
                   LD    {index},GR0
                   LD    {index},{arr},{index}

         */

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                {ld_gr2_len}
                {lad_gr3_pos}
                CALL  {load}"#,
            index_line = index_line,
            ld_gr2_len = str_labels.ld_len(casl2::Register::Gr2),
            lad_gr3_pos = str_labels.lad_pos(casl2::Register::Gr3),
            load = load_elem
        ));
        self.code(recovers);
        self.code(format!(r#" LD {index},GR0"#, index = index_reg));

        index_reg
    }

    // (式展開の処理の一部)
    // 文字列変数(参照型)の文字を取り出す
    pub(super) fn compile_character_of_ref_variable(
        &mut self,
        var_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let load_elem = self.load_subroutine(subroutine::Id::UtilLoadElement);

        let index_reg = self.compile_int_expr(index);

        let str_labels = self.get_ref_str_var_labels(var_name);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2, Gr3])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2, Gr3])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                {ld_gr2_len}
                {lad_gr3_pos}
                CALL  {load}"#,
            index_line = index_line,
            ld_gr2_len = str_labels.ld_len(casl2::Register::Gr2),
            lad_gr3_pos = str_labels.lad_pos(casl2::Register::Gr3),
            load = load_elem
        ));
        self.code(recovers);
        self.code(format!(r#" LD {index},GR0"#, index = index_reg));

        index_reg
    }

    // (式展開の処理の一部)
    // 真理値配列の要素を取り出す
    pub(super) fn compile_variable_array_of_boolean(
        &mut self,
        arr_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let arr_label = self.get_bool_arr_label(arr_name);

        // インデックスがリテラル整数で指定…
        if let parser::Expr::LitInteger(index) = index {
            let index = ((*index).max(0) as usize).min(arr_label.size() - 1);
            let reg = self.get_idle_register();
            self.code(if index == 0 {
                arr_label.ld_first_elem(reg)
            } else {
                format!(
                    r#" {lad_reg_arrpos}
                        LD  {reg},{index},{reg}"#,
                    lad_reg_arrpos = arr_label.lad_pos(reg),
                    reg = reg,
                    index = index
                )
            });
            return reg;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index_line = index_line,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL  {index},GR0
                LD    {index},0,{index}"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        index_reg
    }

    // (式展開の処理の一部)
    // 真理値配列(参照型)の要素を取り出す
    pub(super) fn compile_variable_ref_array_of_boolean(
        &mut self,
        arr_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let arr_label = self.get_ref_bool_arr_label(arr_name);

        // インデックスがリテラル整数で指定…
        if let parser::Expr::LitInteger(index) = index {
            let index = ((*index).max(0) as usize).min(arr_label.size() - 1);
            let reg = self.get_idle_register();
            self.code(format!(
                r#" {lad_reg_arrpos}
                    LD  {reg},{index},{reg}"#,
                lad_reg_arrpos = arr_label.lad_pos(reg),
                reg = reg,
                index = index
            ));
            return reg;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index_line = index_line,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL  {index},GR0
                LD    {index},0,{index}"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        index_reg
    }

    // (式展開の処理の一部)
    // 整数配列の要素を取り出す
    pub(super) fn compile_variable_array_of_integer(
        &mut self,
        arr_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let arr_label = self.get_int_arr_label(arr_name);

        // インデックスがリテラル整数で指定…
        if let parser::Expr::LitInteger(index) = index {
            let index = ((*index).max(0) as usize).min(arr_label.size() - 1);
            let reg = self.get_idle_register();
            self.code(if index == 0 {
                arr_label.ld_first_elem(reg)
            } else {
                format!(
                    r#" {lad_reg_arrpos}
                        LD  {reg},{index},{reg}"#,
                    lad_reg_arrpos = arr_label.lad_pos(reg),
                    reg = reg,
                    index = index
                )
            });
            return reg;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index_line = index_line,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL  {index},GR0
                LD    {index},0,{index}"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        index_reg
    }

    // (式展開の処理の一部)
    // 整数配列(参照型)の要素を取り出す
    pub(super) fn compile_variable_ref_array_of_integer(
        &mut self,
        arr_name: &str,
        index: &parser::Expr,
    ) -> casl2::Register {
        assert!(matches!(index.return_type(), parser::ExprType::Integer));

        let arr_label = self.get_ref_int_arr_label(arr_name);

        // インデックスがリテラル整数で指定…
        if let parser::Expr::LitInteger(index) = index {
            let index = ((*index).max(0) as usize).min(arr_label.size() - 1);
            let reg = self.get_idle_register();
            self.code(format!(
                r#" {lad_reg_arrpos}
                    LD  {reg},{index},{reg}"#,
                lad_reg_arrpos = arr_label.lad_pos(reg),
                reg = reg,
                index = index
            ));
            return reg;
        }

        let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);

        let index_reg = self.compile_int_expr(index);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            if matches!(index_reg, Gr1) {
                self.get_save_registers_src(&[Gr2])
            } else {
                self.get_save_registers_src(&[Gr1, Gr2])
            }
        };

        let index_line = if matches!(index_reg, casl2::Register::Gr1) {
            "".to_string()
        } else {
            format!(" LD GR1,{index}", index = index_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {index_line}
                LAD   GR2,{size}
                CALL  {fit}"#,
            index_line = index_line,
            size = arr_label.size(),
            fit = safe_index
        ));
        self.code(recovers);
        self.code(format!(
            r#" {lad_index_arrpos}
                ADDL  {index},GR0
                LD    {index},0,{index}"#,
            lad_index_arrpos = arr_label.lad_pos(index_reg),
            index = index_reg
        ));

        index_reg
    }

    // (式展開の処理の一部)
    // 真理値を返す二項演算子の処理
    pub(super) fn compile_bin_op_boolean(
        &mut self,
        op: tokenizer::Operator,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        let cmd = match op {
            // lhs,rhsは真理値のみ
            And => "AND",
            Xor => "XOR",
            Or => "OR",

            // lhs,rhsは真理値、整数、文字列のいずれか
            NotEqual => return self.compile_bin_op_boolean_not_equal(lhs, rhs),
            LessOrEequal => return self.compile_bin_op_boolean_less_or_equal(lhs, rhs),
            GreaterOrEqual => return self.compile_bin_op_boolean_greater_or_equal(lhs, rhs),
            Equal => return self.compile_bin_op_boolean_equal(lhs, rhs),
            LessThan => return self.compile_bin_op_boolean_less_than(lhs, rhs),
            GreaterThan => return self.compile_bin_op_boolean_greater_than(lhs, rhs),

            // 二項演算子ではないものや、真理値を返さないもの
            ShiftLeft | ShiftRight | Add | Sub | Mul | Div | Mod | Not | AddInto | SubInto
            | Concat | OpenBracket | CloseBracket | Comma => {
                unreachable!("BUG")
            }
        };

        assert!(matches!(lhs.return_type(), parser::ExprType::Boolean));
        assert!(matches!(rhs.return_type(), parser::ExprType::Boolean));

        let lhs_reg = self.compile_int_expr(lhs);
        let rhs_reg = self.compile_int_expr(rhs);

        self.code(format!(
            r#" {cmd} {lhs},{rhs}"#,
            cmd = cmd,
            lhs = lhs_reg,
            rhs = rhs_reg
        ));

        self.set_register_idle(rhs_reg);
        lhs_reg
    }

    // (式展開の処理の一部)
    // 整数変数の読み込み
    pub(super) fn compile_variable_integer(&mut self, var_name: &str) -> casl2::Register {
        let reg = self.get_idle_register();

        let var_label = self.get_int_var_label(var_name);

        let adr = casl2::Adr::label(&var_label);

        // LD REG,VAR
        self.code(casl2::Command::A {
            code: casl2::A::Ld,
            r: reg,
            adr,
            x: None,
        });

        reg
    }

    // (式展開の処理の一部)
    // 整数変数(参照型)の読み込み
    pub(super) fn compile_variable_ref_integer(&mut self, var_name: &str) -> casl2::Register {
        let reg = self.get_idle_register();

        let var_label = self.get_ref_int_var_label(var_name);

        // LD REG,VAR
        // LD REG,0,REG
        let src = format!(
            r#" LD {reg},{var}
                LD {reg},0,{reg}"#,
            reg = reg,
            var = var_label
        );
        self.code(src);

        reg
    }

    // (式展開の処理の一部)
    // 整数リテラルの読み込み
    pub(super) fn compile_literal_integer(&mut self, value: i32) -> casl2::Register {
        let reg = self.get_idle_register();

        if value == 0 {
            // XOR REG,REG
            self.code(casl2::Command::R {
                code: casl2::R::Xor,
                r1: reg,
                r2: reg,
            });
        } else {
            // LAD REG,VALUE
            self.code(casl2::Command::A {
                code: casl2::A::Lad,
                r: reg,
                adr: casl2::Adr::Dec(value as i16),
                x: None,
            });
        }

        reg
    }

    // (式展開の処理の一部)
    // 真理値変数の読み込み
    pub(super) fn compile_variable_boolean(&mut self, var_name: &str) -> casl2::Register {
        let reg = self.get_idle_register();
        let var_label = self.get_bool_var_label(var_name);
        let adr = casl2::Adr::label(&var_label);

        // LD REG,VAR
        self.code(casl2::Command::A {
            code: casl2::A::Ld,
            r: reg,
            adr,
            x: None,
        });

        reg
    }

    // (式展開の処理の一部)
    // 真理値変数(参照型)の読み込み
    pub(super) fn compile_variable_ref_boolean(&mut self, var_name: &str) -> casl2::Register {
        let reg = self.get_idle_register();
        let var_label = self.get_ref_bool_var_label(var_name);

        self.code(format!(
            r#" LD  {reg},{var}
                LD  {reg},0,{reg}"#,
            reg = reg,
            var = var_label
        ));

        reg
    }

    // (式展開の処理の一部)
    // 真理値リテラルの読み込み
    pub(super) fn compile_literal_boolean(&mut self, value: bool) -> casl2::Register {
        let reg = self.get_idle_register();

        if value {
            // LAD REG,VALUE
            self.code(casl2::Command::A {
                code: casl2::A::Lad,
                r: reg,
                adr: casl2::Adr::Hex(0xFFFF),
                x: None,
            });
        } else {
            // XOR REG,REG
            self.code(casl2::Command::R {
                code: casl2::R::Xor,
                r1: reg,
                r2: reg,
            });
        }

        reg
    }

    // 式の展開 (戻り値が配列参照)
    pub(super) fn compile_ref_arr_expr(&mut self, expr: &parser::Expr) -> ArrayLabel {
        use parser::Expr::*;
        match expr {
            ReferenceOfVar(var_name, parser::VarType::ArrayOfBoolean(size)) => {
                let arr_label = self.get_bool_arr_label(var_name);
                assert_eq!(arr_label.size(), *size);
                arr_label
            }
            ReferenceOfVar(var_name, parser::VarType::ArrayOfInteger(size)) => {
                let arr_label = self.get_int_arr_label(var_name);
                assert_eq!(arr_label.size(), *size);
                arr_label
            }
            ReferenceOfVar(var_name, parser::VarType::RefArrayOfBoolean(size)) => {
                let arr_label = self.get_ref_bool_arr_label(var_name);
                assert_eq!(arr_label.size(), *size);
                arr_label
            }
            ReferenceOfVar(var_name, parser::VarType::RefArrayOfInteger(size)) => {
                let arr_label = self.get_ref_int_arr_label(var_name);
                assert_eq!(arr_label.size(), *size);
                arr_label
            }
            FunctionBooleanArray(size, func, param) => {
                self.compile_function_boolean_array(*size, *func, param)
            }
            FunctionIntegerArray(size, func, param) => {
                self.compile_function_integer_array(*size, *func, param)
            }

            // 戻り値が配列参照ではないもの
            ReferenceOfVar(..)
            | BinaryOperatorBoolean(..)
            | BinaryOperatorInteger(..)
            | CharOfLitString(..)
            | CharOfVarString(..)
            | CharOfVarRefString(..)
            | FunctionBoolean(..)
            | FunctionInteger(..)
            | LitBoolean(..)
            | LitInteger(..)
            | LitCharacter(..)
            | UnaryOperatorInteger(..)
            | UnaryOperatorBoolean(..)
            | VarBoolean(..)
            | VarRefBoolean(..)
            | VarInteger(..)
            | VarRefInteger(..)
            | VarArrayOfBoolean(..)
            | VarRefArrayOfBoolean(..)
            | VarArrayOfInteger(..)
            | VarRefArrayOfInteger(..)
            | BinaryOperatorString(..)
            | FunctionString(..)
            | LitString(..)
            | VarString(..)
            | VarRefString(..)
            | ParamList(_) => unreachable!("BUG"),
        }
    }
}
