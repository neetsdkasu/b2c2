use super::*;

impl Compiler {
    // (式展開の処理の一部)
    // 整数を返す二項演算子の処理
    pub(super) fn compile_bin_op_integer(
        &mut self,
        op: tokenizer::Operator,
        lhs: &parser::Expr,
        rhs: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Operator::*;

        let lhs_reg = self.compile_int_expr(lhs);

        // rhsを直接埋め込める場合
        let with_lit_src = match rhs {
            parser::Expr::LitInteger(value) => match op {
                And => Some(format!(" AND {},={}", lhs_reg, value)),
                Xor => Some(format!(" XOR {},={}", lhs_reg, value)),
                Or => Some(format!(" OR {},={}", lhs_reg, value)),
                ShiftLeftArithmetic => Some(format!(" SLA {},{}", lhs_reg, value)),
                ShiftRightArithmetic => Some(format!(" SRA {},{}", lhs_reg, value)),
                ShiftLeftLogical => Some(format!(" SLL {},{}", lhs_reg, value)),
                ShiftRightLogical => Some(format!(" SRL {},{}", lhs_reg, value)),
                Add => Some(format!(" LAD {0},{1},{0}", lhs_reg, value)),
                Sub => Some(format!(" LAD {0},{1},{0}", lhs_reg, (-value) as i16)),
                _ => None,
            },
            parser::Expr::LitCharacter(value) => match op {
                And => Some(format!(" AND {},='{}'", lhs_reg, value)),
                Xor => Some(format!(" XOR {},='{}'", lhs_reg, value)),
                Or => Some(format!(" OR {},='{}'", lhs_reg, value)),
                Add => Some(format!(" ADDA {},='{}'", lhs_reg, value)),
                Sub => Some(format!(" SUBA {},='{}'", lhs_reg, value)),
                _ => None,
            },
            _ => None,
        };
        if let Some(src) = with_lit_src {
            self.code(src);
            return lhs_reg;
        }

        // rhsを計算する場合

        let rhs_reg = self.compile_int_expr(rhs);

        assert_ne!(lhs_reg, rhs_reg); // たぶん、大丈夫…

        self.restore_register(lhs_reg);

        let src = match op {
            And => format!(" AND {},{}", lhs_reg, rhs_reg),
            Xor => format!(" XOR {},{}", lhs_reg, rhs_reg),
            Or => format!(" OR {},{}", lhs_reg, rhs_reg),
            ShiftLeftArithmetic => format!(" SLA {},0,{}", lhs_reg, rhs_reg),
            ShiftRightArithmetic => format!(" SRA {},0,{}", lhs_reg, rhs_reg),
            ShiftLeftLogical => format!(" SLL {},0,{}", lhs_reg, rhs_reg),
            ShiftRightLogical => format!(" SRL {},0,{}", lhs_reg, rhs_reg),
            Add => format!(" ADDA {},{}", lhs_reg, rhs_reg),
            Sub => format!(" SUBA {},{}", lhs_reg, rhs_reg),

            // 組み込みサブルーチンで処理するもの
            Mul => return self.compile_bin_op_integer_mul(lhs_reg, rhs_reg),
            Div => return self.compile_bin_op_integer_div(lhs_reg, rhs_reg),
            Mod => return self.compile_bin_op_integer_mod(lhs_reg, rhs_reg),

            // 二項演算子ではないものや、整数を返さないもの
            Not | NotEqual | LessOrEequal | GreaterOrEqual | AddInto | SubInto | Equal
            | LessThan | GreaterThan | Concat | OpenBracket | CloseBracket | Comma => {
                unreachable!("BUG")
            }
        };

        self.code(src);

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // 整数を返す二項演算子( Mul (*) )の処理
    pub(super) fn compile_bin_op_integer_mul(
        &mut self,
        lhs_reg: casl2::Register,
        rhs_reg: casl2::Register,
    ) -> casl2::Register {
        let mul_label = self.load_subroutine(subroutine::Id::UtilMul);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            let mut regs = vec![Gr1];
            if !matches!(lhs_reg, Gr2) {
                regs.push(Gr2);
            }
            if !matches!(rhs_reg, Gr3) {
                regs.push(Gr3);
            }
            self.get_save_registers_src(&regs)
        };

        // lhs=GR2 rhs=GR3
        //    no code
        //    no code
        //    no code
        // lhs=GR2 rhs=GR*
        //    no code
        //    LD GR3,{rhs}
        //    no code
        // lhs=GR* rhs=GR3
        //    no code
        //    no code
        //    LD GR2,{lhs}
        // lhs=GR3 rhs=GR2
        //    LD GR0,{lhs}
        //    LD GR3,{rhs}
        //    LD GR2,GR0
        // lhs=GR3 rhs=GR*
        //    LD GR2,{lhs}
        //    LD GR3,{rhs}
        //    no code
        // lhs=GR* rhs=GR2
        //    no code
        //    LD GR3,{rhs}
        //    LD GR2,{lhs}
        // lhs=GR* rhs=GR*
        //    no code
        //    LD GR3,{rhs}
        //    LD GR2,{lhs}

        let (lhs_line1, lhs_line2) = if matches!(lhs_reg, casl2::Register::Gr3) {
            if matches!(rhs_reg, casl2::Register::Gr2) {
                (" LD GR0,GR3".to_string(), " LD GR2,GR0".to_string())
            } else {
                (" LD GR2,GR3".to_string(), "".to_string())
            }
        } else {
            (
                "".to_string(),
                if matches!(lhs_reg, casl2::Register::Gr2) {
                    "".to_string()
                } else {
                    format!(" LD GR2,{lhs}", lhs = lhs_reg)
                },
            )
        };

        let rhs_line = if matches!(rhs_reg, casl2::Register::Gr3) {
            "".to_string()
        } else {
            format!(" LD GR3,{rhs}", rhs = rhs_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {lhs_line1}
                {rhs_line}
                {lhs_line2}
                CALL  {mul}"#,
            rhs_line = rhs_line,
            lhs_line1 = lhs_line1,
            lhs_line2 = lhs_line2,
            mul = mul_label
        ));

        self.code(recovers);

        self.code(format!(" LD {lhs},GR0", lhs = lhs_reg));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // 整数を返す二項演算子( Div (\) )の処理
    pub(super) fn compile_bin_op_integer_div(
        &mut self,
        lhs_reg: casl2::Register,
        rhs_reg: casl2::Register,
    ) -> casl2::Register {
        let divmod_label = self.load_subroutine(subroutine::Id::UtilDivMod);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            let mut regs = vec![Gr1];
            if !matches!(lhs_reg, Gr2) {
                regs.push(Gr2);
            }
            if !matches!(rhs_reg, Gr3) {
                regs.push(Gr3);
            }
            self.get_save_registers_src(&regs)
        };

        let (lhs_line1, lhs_line2) = if matches!(lhs_reg, casl2::Register::Gr3) {
            if matches!(rhs_reg, casl2::Register::Gr2) {
                (" LD GR0,GR3".to_string(), " LD GR2,GR0".to_string())
            } else {
                (" LD GR2,GR3".to_string(), "".to_string())
            }
        } else {
            (
                "".to_string(),
                if matches!(lhs_reg, casl2::Register::Gr2) {
                    "".to_string()
                } else {
                    format!(" LD GR2,{lhs}", lhs = lhs_reg)
                },
            )
        };

        let rhs_line = if matches!(rhs_reg, casl2::Register::Gr3) {
            "".to_string()
        } else {
            format!(" LD GR3,{rhs}", rhs = rhs_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {lhs_line1}
                {rhs_line}
                {lhs_line2}
                CALL  {divmod}"#,
            rhs_line = rhs_line,
            lhs_line1 = lhs_line1,
            lhs_line2 = lhs_line2,
            divmod = divmod_label
        ));
        self.code(recovers);
        self.code(format!(" LD {lhs},GR0", lhs = lhs_reg));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // 整数を返す二項演算子( Mod )の処理
    pub(super) fn compile_bin_op_integer_mod(
        &mut self,
        lhs_reg: casl2::Register,
        rhs_reg: casl2::Register,
    ) -> casl2::Register {
        let divmod_label = self.load_subroutine(subroutine::Id::UtilDivMod);

        // レジスタ退避
        let (saves, recovers) = {
            use casl2::Register::*;
            let mut regs = vec![Gr1];
            if !matches!(lhs_reg, Gr2) {
                regs.push(Gr2);
            }
            if !matches!(rhs_reg, Gr3) {
                regs.push(Gr3);
            }
            self.get_save_registers_src(&regs)
        };

        let (lhs_line1, lhs_line2) = if matches!(lhs_reg, casl2::Register::Gr3) {
            if matches!(rhs_reg, casl2::Register::Gr2) {
                (" LD GR0,GR3".to_string(), " LD GR2,GR0".to_string())
            } else {
                (" LD GR2,GR3".to_string(), "".to_string())
            }
        } else {
            (
                "".to_string(),
                if matches!(lhs_reg, casl2::Register::Gr2) {
                    "".to_string()
                } else {
                    format!(" LD GR2,{lhs}", lhs = lhs_reg)
                },
            )
        };

        let rhs_line = if matches!(rhs_reg, casl2::Register::Gr3) {
            "".to_string()
        } else {
            format!(" LD GR3,{rhs}", rhs = rhs_reg)
        };

        self.code(saves);
        self.code(format!(
            r#" {lhs_line1}
                {rhs_line}
                {lhs_line2}
                CALL  {divmod}
                LD    GR0,GR1"#,
            rhs_line = rhs_line,
            lhs_line1 = lhs_line1,
            lhs_line2 = lhs_line2,
            divmod = divmod_label
        ));
        self.code(recovers);
        self.code(format!(" LD {lhs},GR0", lhs = lhs_reg));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }
}
