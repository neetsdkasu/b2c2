use super::*;

impl Compiler {
    // Do While <condition>
    // Loop
    // ステートメント
    pub(super) fn compile_do_while_loop(
        &mut self,
        exit_id: usize,
        condition: &parser::Expr,
        block: &[parser::Statement],
    ) {
        assert!(matches!(condition.return_type(), parser::ExprType::Boolean));

        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.comment(format!("Do While {cond}", cond = condition));
        self.labeled(&loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.code(format!(
            r#" AND {reg},{reg}
                JZE {exit}"#,
            reg = cond_reg,
            exit = exit_label
        ));

        self.set_register_idle(cond_reg);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment("Loop");
        self.code(format!(" JUMP {next}", next = loop_label));
        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Do Until <condition>
    // Loop
    // ステートメント
    pub(super) fn compile_do_until_loop(
        &mut self,
        exit_id: usize,
        condition: &parser::Expr,
        block: &[parser::Statement],
    ) {
        assert!(matches!(condition.return_type(), parser::ExprType::Boolean));

        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.comment(format!("Do Until {cond}", cond = condition));
        self.labeled(&loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.code(format!(
            r#" AND {reg},{reg}
                JNZ {exit}"#,
            reg = cond_reg,
            exit = exit_label
        ));

        self.set_register_idle(cond_reg);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment("Loop");
        self.code(format!(" JUMP {next}", next = loop_label));
        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Do
    // Loop While <condition>
    // ステートメント
    pub(super) fn compile_do_loop_while(
        &mut self,
        exit_id: usize,
        condition: &parser::Expr,
        block: &[parser::Statement],
    ) {
        assert!(matches!(condition.return_type(), parser::ExprType::Boolean));

        self.comment("Do");

        let top_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.labeled(&top_label, casl2::Command::Nop);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment(format!("Loop While {cond}", cond = condition));
        self.labeled(loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.code(format!(
            r#" AND {reg},{reg}
                JNZ {next}"#,
            reg = cond_reg,
            next = top_label
        ));

        self.set_register_idle(cond_reg);

        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Do
    // Loop Until <condition>
    // ステートメント
    pub(super) fn compile_do_loop_until(
        &mut self,
        exit_id: usize,
        condition: &parser::Expr,
        block: &[parser::Statement],
    ) {
        assert!(matches!(condition.return_type(), parser::ExprType::Boolean));

        self.comment("Do");

        let top_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.labeled(&top_label, casl2::Command::Nop);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment(format!("Loop Until {cond}", cond = condition));
        self.labeled(loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.code(format!(
            r#" AND {reg},{reg}
                JZE {next}"#,
            reg = cond_reg,
            next = top_label
        ));

        self.set_register_idle(cond_reg);

        self.labeled(exit_label, casl2::Command::Nop);
    }

    // Do ~ Loop ステートメント
    pub(super) fn compile_do_loop(&mut self, exit_id: usize, block: &[parser::Statement]) {
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.comment("Do");
        self.labeled(&loop_label, casl2::Command::Nop);

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.comment("Loop");
        self.code(casl2::Command::P {
            code: casl2::P::Jump,
            adr: casl2::Adr::label(&loop_label),
            x: None,
        });

        self.labeled(exit_label, casl2::Command::Nop);
    }
}
