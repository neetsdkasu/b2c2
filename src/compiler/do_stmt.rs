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

        let comment = format!("Do While {cond}", cond = condition);

        self.add_debugger_hint(|| comment.clone());
        self.nest_depth += 1;
        self.comment(comment.as_str());
        self.labeled(&loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.set_debugger_hint_extra_info(|| ExtraInfo::Condition(cond_reg));
        self.add_debugger_hint(|| "(begin do)".to_string());
        self.set_debugger_hint_extra_info(|| ExtraInfo::RelatedCode(comment.clone()));

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

        self.nest_depth -= 1;
        self.add_debugger_hint(|| "Loop".to_string());
        self.comment("Loop");
        self.add_debugger_hint(|| "(end do)".to_string());
        self.set_debugger_hint_extra_info(|| ExtraInfo::RelatedCode(comment));
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

        let comment = format!("Do Until {cond}", cond = condition);

        self.add_debugger_hint(|| comment.clone());
        self.nest_depth += 1;
        self.comment(comment.as_str());
        self.labeled(&loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.set_debugger_hint_extra_info(|| ExtraInfo::Condition(cond_reg));
        self.add_debugger_hint(|| "(begin do)".to_string());
        self.set_debugger_hint_extra_info(|| ExtraInfo::RelatedCode(comment.clone()));

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

        self.nest_depth -= 1;
        self.add_debugger_hint(|| "Loop".to_string());
        self.comment("Loop");
        self.add_debugger_hint(|| "(end do)".to_string());
        self.set_debugger_hint_extra_info(|| ExtraInfo::RelatedCode(comment));
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

        self.add_debugger_hint(|| "Do".to_string());
        self.nest_depth += 1;
        self.comment("Do");

        let comment = format!("Loop While {cond}", cond = condition);

        let top_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.labeled(&top_label, casl2::Command::Nop);

        self.add_debugger_hint(|| "(begin do)".to_string());
        self.set_debugger_hint_extra_info(|| ExtraInfo::RelatedCode(comment.clone()));

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.nest_depth -= 1;
        self.add_debugger_hint(|| comment.clone());
        self.comment(comment.as_str());
        self.labeled(loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.set_debugger_hint_extra_info(|| ExtraInfo::Condition(cond_reg));
        self.add_debugger_hint(|| "(end do)".to_string());
        self.set_debugger_hint_extra_info(|| ExtraInfo::RelatedCode(comment));

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

        self.add_debugger_hint(|| "Do".to_string());
        self.nest_depth += 1;
        self.comment("Do");

        let comment = format!("Loop Until {cond}", cond = condition);

        let top_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        self.labeled(&top_label, casl2::Command::Nop);

        self.add_debugger_hint(|| "(begin do)".to_string());
        self.set_debugger_hint_extra_info(|| ExtraInfo::RelatedCode(comment.clone()));

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.nest_depth -= 1;
        self.add_debugger_hint(|| comment.clone());
        self.comment(comment.as_str());
        self.labeled(loop_label, casl2::Command::Nop);

        let cond_reg = self.compile_int_expr(condition);

        self.set_debugger_hint_extra_info(|| ExtraInfo::Condition(cond_reg));
        self.add_debugger_hint(|| "(end do)".to_string());
        self.set_debugger_hint_extra_info(|| ExtraInfo::RelatedCode(comment));

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

        self.add_debugger_hint(|| "Do".to_string());
        self.nest_depth += 1;
        self.comment("Do");
        self.labeled(&loop_label, casl2::Command::Nop);
        self.add_debugger_hint(|| "(begin do)".to_string());

        for stmt in block.iter() {
            self.compile(stmt);
        }

        self.nest_depth -= 1;
        self.add_debugger_hint(|| "Loop".to_string());
        self.comment("Loop");
        self.add_debugger_hint(|| "(end do)".to_string());
        self.code(casl2::Command::P {
            code: casl2::P::Jump,
            adr: casl2::Adr::label(&loop_label),
            x: None,
        });

        self.labeled(exit_label, casl2::Command::Nop);
    }
}
