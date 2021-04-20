use super::*;

impl Compiler {
    // For ステートメント (Stepが定数)
    pub(super) fn compile_for_with_literal_step(
        &mut self,
        for_stmt: &parser::Statement,
        step: i32,
    ) {
        let (exit_id, counter, is_ref, init, end, block) = if let parser::Statement::For {
            exit_id,
            counter,
            counter_is_ref: is_ref,
            init,
            end,
            step: _,
            block,
        } = for_stmt
        {
            (*exit_id, counter, *is_ref, init, end, block)
        } else {
            unreachable!("BUG");
        };

        self.comment(format!(
            "For {counter} = {init} To {end} Step {step}",
            counter = counter,
            init = init,
            end = end,
            step = step
        ));

        // calc {end}
        let end_var = if let parser::Expr::LitInteger(_) = end {
            None
        } else {
            let end_var = self.get_temp_int_var_label();
            // 想定では GR7
            let end_reg = self.compile_int_expr(end);
            self.code(casl2::Command::A {
                code: casl2::A::St,
                r: end_reg,
                adr: casl2::Adr::label(&end_var),
                x: None,
            });
            self.set_register_idle(end_reg); // GR7 解放のはず
            Some(end_var)
        };

        // カウンタの準備
        let counter_var = if is_ref {
            self.get_ref_int_var_label(counter)
        } else {
            self.get_int_var_label(counter)
        };

        // calc {init} and assign to {counter}
        // 想定では GR7
        let init_reg = self.compile_int_expr(init);
        if is_ref {
            let reg = self.get_idle_register();
            self.code(format!(
                r#" LD  {reg},{var}
                    ST  {init},0,{reg}"#,
                reg = reg,
                var = counter_var,
                init = init_reg
            ));
            self.set_register_idle(reg);
        } else {
            self.code(format!(
                r#" ST {init},{var}"#,
                init = init_reg,
                var = counter_var
            ));
        }
        self.set_register_idle(init_reg); // GR7 解放のはず

        // ラベルの準備
        let condition_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        // ループ継続の判定部分

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::Gr1));

        self.code(format!("{cond} NOP", cond = condition_label));
        self.code(saves);
        if let Some(end_var) = end_var.as_ref() {
            self.code(format!(
                r#" LD    GR1,{counter}
                    {ld_counter_if_ref}
                    CPA   GR1,{end}"#,
                counter = counter_var,
                end = end_var,
                ld_counter_if_ref = if is_ref { "LD GR1,0,GR1" } else { "" }
            ));
        } else if let parser::Expr::LitInteger(end) = end {
            self.code(format!(
                r#" LD    GR1,{counter}
                    {ld_counter_if_ref}
                    CPA   GR1,={end}"#,
                counter = counter_var,
                end = *end as i16,
                ld_counter_if_ref = if is_ref { "LD GR1,0,GR1" } else { "" }
            ));
        } else {
            unreachable!("BUG");
        }
        self.code(recovers);

        if step < 0 {
            self.code(format!(" JMI {exit}", exit = exit_label));
        } else {
            self.code(format!(" JPL {exit}", exit = exit_label));
        }

        // ループ内のコードを実行
        for stmt in block.iter() {
            self.compile(stmt);
        }

        // ループ末尾 (カウンタの更新など)

        self.comment(format!("Next {counter}", counter = counter));

        self.code(format!("{next} NOP", next = loop_label));
        if is_ref {
            let (saves, recovers) = {
                use casl2::Register::*;
                self.get_save_registers_src(&[Gr1, Gr2])
            };
            self.code(saves);
            self.code(format!(
                r#" LD    GR1,{counter}
                    LD    GR2,0,GR1
                    LAD   GR2,{step},GR2
                    ST    GR2,0,GR1"#,
                counter = counter_var,
                step = step
            ));
            self.code(recovers);
        } else {
            let (saves, recovers) =
                self.get_save_registers_src(std::slice::from_ref(&casl2::Register::Gr1));
            self.code(saves);
            self.code(format!(
                r#" LD    GR1,{counter}
                    LAD   GR1,{step},GR1
                    ST    GR1,{counter}"#,
                counter = counter_var,
                step = step
            ));
            self.code(recovers);
        }
        self.code(format!(" JUMP {cond}", cond = condition_label));
        self.code(format!("{exit} NOP", exit = exit_label));

        if let Some(end_var) = end_var {
            self.return_temp_int_var_label(end_var);
        }
    }

    // For ステートメント
    pub(super) fn compile_for(&mut self, for_stmt: &parser::Statement) {
        let (exit_id, counter, is_ref, init, end, step, block) = if let parser::Statement::For {
            exit_id,
            counter,
            counter_is_ref: is_ref,
            init,
            end,
            step: Some(step),
            block,
        } = for_stmt
        {
            (*exit_id, counter, *is_ref, init, end, step, block)
        } else {
            unreachable!("BUG");
        };

        self.comment(format!(
            "For {counter} = {init} To {end} Step {step}",
            counter = counter,
            init = init,
            end = end,
            step = step
        ));

        // calc {step}
        let step_var = self.get_temp_int_var_label();
        // 想定では GR7
        let step_reg = self.compile_int_expr(step);
        self.code(casl2::Command::A {
            code: casl2::A::St,
            r: step_reg,
            adr: casl2::Adr::label(&step_var),
            x: None,
        });
        self.set_register_idle(step_reg); // GR7 解放のはず

        // calc {end}
        let end_var = if let parser::Expr::LitInteger(_) = end {
            None
        } else {
            let end_var = self.get_temp_int_var_label();
            // 想定では GR7
            let end_reg = self.compile_int_expr(end);
            self.code(casl2::Command::A {
                code: casl2::A::St,
                r: end_reg,
                adr: casl2::Adr::label(&end_var),
                x: None,
            });
            self.set_register_idle(end_reg); // GR7 解放のはず
            Some(end_var)
        };

        // カウンタの準備
        let counter_var = if is_ref {
            self.get_ref_int_var_label(counter)
        } else {
            self.get_int_var_label(counter)
        };

        // calc {init} and assign to {counter}
        // 想定では GR7
        let init_reg = self.compile_int_expr(init);
        if is_ref {
            let reg = self.get_idle_register();
            self.code(format!(
                r#" LD  {reg},{var}
                    ST  {init},0,{reg}"#,
                reg = reg,
                var = counter_var,
                init = init_reg
            ));
            self.set_register_idle(reg);
        } else {
            self.code(format!(
                r#" ST {init},{var}"#,
                init = init_reg,
                var = counter_var
            ));
        }
        self.set_register_idle(init_reg); // GR7 解放のはず

        // ラベルの準備
        let condition_label = self.get_new_jump_label();
        let negastep_label = self.get_new_jump_label();
        let blockhead_label = self.get_new_jump_label();
        let loop_label = self.get_loop_label(exit_id);
        let exit_label = self.get_exit_label(exit_id);

        // ループ継続の判定部分

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::Gr1));

        self.code(format!("{cond} NOP", cond = condition_label));
        self.code(saves);
        if let Some(end_var) = end_var.as_ref() {
            if is_ref {
                self.code(format!(
                    r#" LD    GR1,{step}
                        JMI   {nega}
                        LD    GR1,{counter}
                        LD    GR1,0,GR1
                        CPA   GR1,{end}
                        JUMP  {block}
{nega}                  LD    GR0,{end}
                        LD    GR1,{counter}
                        CPA   GR0,0,GR1
{block}                 NOP"#,
                    counter = counter_var,
                    step = step_var,
                    nega = negastep_label,
                    end = end_var,
                    block = blockhead_label
                ));
            } else {
                self.code(format!(
                    r#" LD    GR1,{step}
                        JMI   {nega}
                        LD    GR1,{counter}
                        CPA   GR1,{end}
                        JUMP  {block}
{nega}                  LD    GR1,{end}
                        CPA   GR1,{counter}
{block}                 NOP"#,
                    counter = counter_var,
                    step = step_var,
                    nega = negastep_label,
                    end = end_var,
                    block = blockhead_label
                ));
            }
        } else if let parser::Expr::LitInteger(end) = end {
            if is_ref {
                self.code(format!(
                    r#" LD    GR1,{step}
                        JMI   {nega}
                        LD    GR1,{counter}
                        LD    GR1,0,GR1
                        CPA   GR1,={end}
                        JUMP  {block}
{nega}                  LAD   GR0,{end}
                        LD    GR1,{counter}
                        CPA   GR0,0,GR1
{block}                 NOP"#,
                    counter = counter_var,
                    step = step_var,
                    nega = negastep_label,
                    end = *end as i16,
                    block = blockhead_label
                ));
            } else {
                self.code(format!(
                    r#" LD    GR1,{step}
                        JMI   {nega}
                        LD    GR1,{counter}
                        CPA   GR1,={end}
                        JUMP  {block}
{nega}                  LAD   GR1,{end}
                        CPA   GR1,{counter}
{block}                 NOP"#,
                    counter = counter_var,
                    step = step_var,
                    nega = negastep_label,
                    end = *end as i16,
                    block = blockhead_label
                ));
            }
        } else {
            unreachable!("BUG");
        }
        self.code(recovers);
        self.code(format!(" JPL {exit}", exit = exit_label));

        // ループ内のコードを実行
        for stmt in block.iter() {
            self.compile(stmt);
        }

        // ループ末尾 (カウンタの更新など)

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) =
            self.get_save_registers_src(std::slice::from_ref(&casl2::Register::Gr1));

        self.comment(format!("Next {counter}", counter = counter));
        self.code(format!("{next} NOP", next = loop_label));
        self.code(saves);
        if is_ref {
            self.code(format!(
                r#" LD    GR1,{counter}
                    LD    GR0,0,GR1
                    ADDA  GR0,{step}
                    ST    GR0,0,GR1"#,
                counter = counter_var,
                step = step_var
            ));
        } else {
            self.code(format!(
                r#" LD    GR1,{counter}
                    ADDA  GR1,{step}
                    ST    GR1,{counter}"#,
                counter = counter_var,
                step = step_var
            ));
        }
        self.code(recovers);
        self.code(format!(" JUMP {cond}", cond = condition_label));
        self.code(format!("{exit} NOP", exit = exit_label));

        if let Some(end_var) = end_var {
            self.return_temp_int_var_label(end_var);
        }
        self.return_temp_int_var_label(step_var);
    }
}
