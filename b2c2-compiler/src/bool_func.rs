// b2c2-compiler crate::bool_func
// author: Leonardone @ NEETSDKASU

use super::*;

impl Compiler {
    // (式展開の処理の一部)
    // 真理値を返す関数の処理
    pub(super) fn compile_function_boolean(
        &mut self,
        func: tokenizer::Function,
        param: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Function::*;
        match func {
            CBool => self.call_function_cbool(param),
            Eof => self.call_function_eof(param),

            // 戻り値が真理値ではないもの
            Abs | Array | Asc | CArray | Chr | CInt | CStr | Len | Max | Mid | Min | Space
            | String | SubArray => {
                unreachable!("BUG")
            }
        }
    }

    // EOF()
    pub(super) fn call_function_eof(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param, parser::Expr::LitInteger(0)));
        self.has_eof = true;
        let reg = self.get_idle_register();
        if self.option_external_eof {
            self.code(format!(
                r#" LAD   GR0,1
                    CALL  EOF
                    LD    {reg},GR0"#,
                reg = reg
            ));
        } else {
            self.code(format!(" LD {reg},EOF", reg = reg));
        }
        reg
    }

    // CBool(<integer>)
    pub(super) fn call_function_cbool(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param.return_type(), parser::ExprType::Integer));

        // リテラルのとき(は？)
        if let parser::Expr::LitInteger(value) = param {
            let reg = self.get_idle_register();
            if *value == 0 {
                self.code(format!(" XOR {reg},{reg}", reg = reg));
            } else {
                self.code(format!(" LAD {reg},#FFFF", reg = reg));
            }
            return reg;
        }

        let reg = self.compile_int_expr(param);
        let label = self.get_new_jump_label();

        self.code(format!(
            r#" AND  {reg},{reg}
                JZE  {ok}
                LAD  {reg},#FFFF
{ok}            NOP"#,
            reg = reg,
            ok = label
        ));

        reg
    }
}
