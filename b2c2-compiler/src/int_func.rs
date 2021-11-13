// b2c2-compiler crate::int_func
// author: Leonardone @ NEETSDKASU

use super::*;

impl Compiler {
    // (式展開の処理の一部)
    // Function integer
    // 戻り値が整数の関数 (※引数の型とかは関数による…オーバーロードもあるか？)
    pub(super) fn compile_function_integer(
        &mut self,
        func: tokenizer::Function,
        param: &parser::Expr,
    ) -> casl2::Register {
        use tokenizer::Function::*;
        match func {
            Abs => self.call_function_abs(param),
            Asc => self.call_function_asc(param),
            CInt => self.call_function_cint(param),
            Len => self.call_function_len(param),
            Max => self.call_function_max(param),
            Min => self.call_function_min(param),

            // 戻り値が整数ではないもの
            Array | CArray | CBool | Chr | CStr | Eof | Mid | Space | String | SubArray => {
                unreachable!("BUG")
            }
        }
    }

    // (式展開の処理の一部)
    // Min(<integer>,<integer>)の処理
    pub(super) fn call_function_min(&mut self, param: &parser::Expr) -> casl2::Register {
        let list = if let parser::Expr::ParamList(list) = param {
            list
        } else {
            unreachable!("BUG");
        };

        let (lhs, rhs) = match list.as_slice() {
            [lhs, rhs]
                if matches!(lhs.return_type(), parser::ExprType::Integer)
                    && matches!(rhs.return_type(), parser::ExprType::Integer) =>
            {
                (lhs, rhs)
            }
            _ => unreachable!("BUG"),
        };

        let lhs_reg = self.compile_int_expr(lhs);

        let rhs_reg = self.compile_int_expr(rhs);

        self.restore_register(lhs_reg);

        let ok_label = self.get_new_jump_label();

        self.code(format!(
            r#" CPA  {lhs},{rhs}
                JMI  {ok}
                LD   {lhs},{rhs}
{ok}            NOP
"#,
            lhs = lhs_reg,
            rhs = rhs_reg,
            ok = ok_label
        ));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // Max(<integer>,<integer>)の処理
    pub(super) fn call_function_max(&mut self, param: &parser::Expr) -> casl2::Register {
        let list = if let parser::Expr::ParamList(list) = param {
            list
        } else {
            unreachable!("BUG");
        };

        let (lhs, rhs) = match list.as_slice() {
            [lhs, rhs]
                if matches!(lhs.return_type(), parser::ExprType::Integer)
                    && matches!(rhs.return_type(), parser::ExprType::Integer) =>
            {
                (lhs, rhs)
            }
            _ => unreachable!("BUG"),
        };

        let lhs_reg = self.compile_int_expr(lhs);

        let rhs_reg = self.compile_int_expr(rhs);

        self.restore_register(lhs_reg);

        let ok_label = self.get_new_jump_label();

        self.code(format!(
            r#" CPA  {lhs},{rhs}
                JPL  {ok}
                LD   {lhs},{rhs}
{ok}            NOP
"#,
            lhs = lhs_reg,
            rhs = rhs_reg,
            ok = ok_label
        ));

        self.set_register_idle(rhs_reg);

        lhs_reg
    }

    // (式展開の処理の一部)
    // Asc<string>) の処理
    pub(super) fn call_function_asc(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param.return_type(), parser::ExprType::String));

        let reg = self.get_idle_register();
        self.set_register_idle(reg);

        let labels = self.compile_str_expr(param);

        self.set_register_used(reg);

        match &labels.label_type {
            StrLabelType::Lit(s) | StrLabelType::Const(s) if s.is_empty() => {
                self.code(casl2::Command::R {
                    code: casl2::R::Xor,
                    r1: reg,
                    r2: reg,
                })
            }
            StrLabelType::Lit(_) | StrLabelType::Const(_) => {
                self.code(format!(r#" LD {reg},{pos}"#, reg = reg, pos = labels.pos))
            }
            StrLabelType::Var | StrLabelType::Temp | StrLabelType::ArgVal => {
                let ok = self.get_new_jump_label();
                self.code(format!(
                    r#" LD   {reg},{len}
                        JZE  {ok}
                        LD   {reg},{pos}
{ok}                    NOP"#,
                    reg = reg,
                    len = labels.len,
                    pos = labels.pos,
                    ok = ok
                ));
            }
            StrLabelType::ArgRef => {
                let ok = self.get_new_jump_label();
                self.code(format!(
                    r#" LD   {reg},{len}
                        LD   {reg},0,{reg}
                        JZE  {ok}
                        LD   {reg},{pos}
                        LD   {reg},0,{reg}
{ok}                    NOP"#,
                    reg = reg,
                    len = labels.len,
                    pos = labels.pos,
                    ok = ok
                ));
            }
            StrLabelType::MemVal(offset) => {
                let ok = self.get_new_jump_label();
                self.code(format!(
                    r#" LD   {reg},MEM
                        LD   {reg},{len_offset},{reg}
                        JZE  {ok}
                        LD   {reg},MEM
                        LD   {reg},{pos_offset},{reg}
{ok}                    NOP"#,
                    reg = reg,
                    len_offset = *offset,
                    ok = ok,
                    pos_offset = *offset + 1
                ));
            }
            StrLabelType::MemRef(offset) => {
                let ok = self.get_new_jump_label();
                self.code(format!(
                    r#" LD   {reg},MEM
                        LD   {reg},{len_offset},{reg}
                        LD   {reg},0,{reg}
                        JZE  {ok}
                        LD   {reg},MEM
                        LD   {reg},{pos_offset},{reg}
                        LD   {reg},0,{reg}
{ok}                    NOP"#,
                    reg = reg,
                    len_offset = *offset,
                    ok = ok,
                    pos_offset = *offset + 1
                ));
            }
        }

        self.return_temp_str_var_label(labels);
        reg
    }

    // (式展開の処理の一部)
    // Abs<integer>) の処理
    pub(super) fn call_function_abs(&mut self, param: &parser::Expr) -> casl2::Register {
        assert!(matches!(param.return_type(), parser::ExprType::Integer));

        /*
            考察

            素で書くと4行1ラベル
                  LD    GR0,{reg}
                  JPL   {ok}
                  XOR   {reg},{reg}
                  SUBA  {reg},GR0
            {ok}  NOP

            サブルーチン利用だと2～5行0ラベル(大半3行)
                [ PUSH  0,GR1     ]
                [ LD    GR1,{reg} ]
                  CALL  {abs}
                [ POP   GR1       ]
                  LD    {reg},GR0
            Absサブルーチン6行2ラベル
            {abs} LD    GR0,GR1
                  JMI   {mi}
                  RET
            {mi}  XOR   GR0,GR0
                  SUBA  GR0,GR1
                  RET

            6~7回以上でチャラだが、普通そんなにAbsを多用せんだろJK...
            (4 * 1 =  4 [1*1=1], 3 * 1 + 6 =  9 [2])
            (4 * 2 =  8 [1*2=2], 3 * 2 + 6 = 12 [2])
            (4 * 3 = 12 [1*3=3], 3 * 3 + 6 = 15 [2])
            (4 * 4 = 16 [1*4=4], 3 * 4 + 6 = 18 [2])
            (4 * 5 = 20 [1*5=5], 3 * 5 + 6 = 21 [2])
            (4 * 6 = 24 [1*6=6], 3 * 6 + 6 = 24 [2])
            (4 * 7 = 28 [1*7=7], 3 * 7 + 6 = 27 [2])
        */

        let reg = self.compile_int_expr(param);

        let ok_label = self.get_new_jump_label();

        self.code(format!(
            r#" LD    GR0,{reg}
                JPL   {ok}
                XOR   {reg},{reg}
                SUBA  {reg},GR0
{ok}            NOP
"#,
            reg = reg,
            ok = ok_label
        ));

        reg
    }

    // (式展開の処理の一部)
    // Len(<string>) の処理
    pub(super) fn call_function_len(&mut self, param: &parser::Expr) -> casl2::Register {
        use parser::{ExprType, VarType};

        let reg = self.get_idle_register();

        match param.return_type() {
            ExprType::ReferenceOfVar(VarType::ArrayOfBoolean(size))
            | ExprType::ReferenceOfVar(VarType::RefArrayOfBoolean(size))
            | ExprType::ReferenceOfVar(VarType::ArrayOfInteger(size))
            | ExprType::ReferenceOfVar(VarType::RefArrayOfInteger(size)) => {
                self.code(format!(r#" LAD {reg},{size}"#, reg = reg, size = size));
            }
            ExprType::String => {
                self.set_register_idle(reg);
                let str_labels = self.compile_str_expr(param);
                self.set_register_used(reg);
                self.code(str_labels.ld_len(reg));
                self.return_temp_str_var_label(str_labels);
            }
            _ => unreachable!("BUG"),
        }

        reg
    }

    // (式展開の処理の一部)
    // CInt(<boolean>/<string>) の処理
    pub(super) fn call_function_cint(&mut self, param: &parser::Expr) -> casl2::Register {
        match param.return_type() {
            parser::ExprType::Boolean => self.compile_int_expr(param),
            parser::ExprType::String => {
                let ret_reg = self.get_idle_register();
                self.set_register_idle(ret_reg);
                let arg_str = self.compile_str_expr(param);
                let cint = self.load_subroutine(subroutine::Id::FuncCInt);
                // レジスタの退避
                let (saves, recovers) = {
                    use casl2::Register::*;
                    let mut regs = vec![Gr1, Gr2];
                    regs.retain(|r| *r != ret_reg);
                    self.get_save_registers_src(&regs)
                };
                self.code(saves);
                self.code(format!(
                    r#" {lad_gr1_strpos}
                        {ld_gr2_strlen}
                        CALL  {cint}"#,
                    lad_gr1_strpos = arg_str.lad_pos(casl2::Register::Gr1),
                    ld_gr2_strlen = arg_str.ld_len(casl2::Register::Gr2),
                    cint = cint
                ));
                self.code(recovers);
                self.code(format!(" LD {reg},GR0", reg = ret_reg));
                self.return_temp_str_var_label(arg_str);
                self.set_register_used(ret_reg);
                ret_reg
            }
            parser::ExprType::Integer
            | parser::ExprType::ParamList
            | parser::ExprType::ReferenceOfVar(..) => unreachable!("BUG"),
        }
    }
}
