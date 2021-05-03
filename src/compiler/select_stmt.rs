use super::*;

impl Compiler {
    // Select Case <string> ステートメント
    pub(super) fn compile_select_string(
        &mut self,
        exit_id: usize,
        value: &parser::Expr,
        case_blocks: &[parser::Statement],
    ) {
        assert!(matches!(value.return_type(), parser::ExprType::String));

        self.add_debugger_hint(|| format!("Select Case {value}", value = value));
        self.comment(format!("Select Case {value}", value = value));

        let head_debugger_hint = self.get_current_debugger_hint();

        let exit_label = self.get_exit_label(exit_id);

        let label_and_blocks: Vec<_> = case_blocks
            .iter()
            .map(|b| (self.get_new_jump_label(), b))
            .collect();

        let value_labels = self.compile_str_expr(value);

        // 想定では、
        //  全てのレジスタ未使用
        //  になっているはず…
        let (saves, recovers) = {
            use casl2::Register::*;
            self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
        };

        self.code(saves);
        self.code(value_labels.lad_pos(casl2::Register::Gr1));
        self.code(value_labels.ld_len(casl2::Register::Gr2));

        self.return_temp_str_var_label(value_labels);

        let cmpstr = self.load_subroutine(subroutine::Id::UtilCompareStr);

        for (label, case_stmt) in label_and_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseString { values, .. } => {
                    for lit in values.iter() {
                        let lit_labels = self.get_lit_str_label_if_exists(lit);
                        self.code(format!(
                            r#" LAD   GR3,{pos}
                                LAD   GR4,{len}
                                CALL  {cmp}
                                AND   GR0,GR0
                                JZE   {label}"#,
                            pos = lit_labels.pos,
                            len = lit.chars().count(),
                            cmp = cmpstr,
                            label = label
                        ));
                    }
                }
                parser::Statement::CaseElse { .. } => {
                    self.code(&recovers);
                    self.code(format!(" JUMP {label}", label = label));
                }
                _ => unreachable!("BUG"),
            }
        }

        if !matches!(case_blocks.last(), Some(parser::Statement::CaseElse { .. })) {
            self.code(&recovers);
            self.code(format!(" JUMP {exit}", exit = exit_label));
        }

        for (label, case_stmt) in label_and_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseString { values, block } => {
                    self.comment(format!(
                        "Case {}",
                        values
                            .iter()
                            .map(|s| format!("'{}'", s.replace('\'', "''")))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                    self.labeled(label, casl2::Command::Nop);
                    self.code(&recovers);
                    if let Some(cmd) = head_debugger_hint.clone() {
                        self.add_debugger_hint_command(
                            cmd,
                            format!(
                                "Case {}",
                                values
                                    .iter()
                                    .map(|s| format!("'{}'", s.replace('\'', "''")))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        );
                    }
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                    self.show_debugger_hint();
                }
                parser::Statement::CaseElse { block } => {
                    self.comment("Case Else");
                    self.labeled(label, casl2::Command::Nop);
                    if let Some(cmd) = head_debugger_hint.clone() {
                        self.add_debugger_hint_command(cmd, "Case Else".to_string());
                    }
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                    self.show_debugger_hint();
                }
                _ => unreachable!("BUG"),
            }
            self.code(format!(" JUMP {exit}", exit = exit_label));
        }

        assert_eq!(
            self.statements.pop(),
            Some(casl2::Statement::code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            }))
        );

        self.comment("End Select");
        self.labeled(exit_label, casl2::Command::Nop);
        self.add_debugger_hint_message(|| "End Select".to_string());
    }

    // Select Case <integer> ステートメント
    pub(super) fn compile_select_integer(
        &mut self,
        exit_id: usize,
        value: &parser::Expr,
        case_blocks: &[parser::Statement],
    ) {
        assert!(
            {
                let mut iter = case_blocks.iter();
                iter.next_back();
                iter.all(|stmt| matches!(stmt, parser::Statement::CaseInteger { .. }))
            },
            "BUG"
        );

        let has_else = matches!(case_blocks.last(), Some(parser::Statement::CaseElse { .. }));

        let case_blocks: Vec<_> = case_blocks
            .iter()
            .map(|case_stmt| (case_stmt, self.get_new_jump_label()))
            .collect();

        self.add_debugger_hint(|| format!("Select Case {}", value));
        self.comment(format!("Select Case {}", value));

        let head_debugger_hint = self.get_current_debugger_hint();

        // 想定では GR7
        let value_reg = self.compile_int_expr(value);

        for (case_stmt, label) in case_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseInteger { values, .. } => {
                    for case_value in values.iter() {
                        use parser::CaseIntegerItem::*;
                        let adr = match case_value {
                            Integer(value) => casl2::Adr::LiteralDec(*value as i16),
                            Character(ch) => casl2::Adr::LiteralStr(ch.to_string()),
                        };
                        self.code(casl2::Command::A {
                            code: casl2::A::Cpa,
                            r: value_reg,
                            adr,
                            x: None,
                        });
                        self.code(casl2::Command::P {
                            code: casl2::P::Jze,
                            adr: casl2::Adr::label(label),
                            x: None,
                        });
                    }
                }
                parser::Statement::CaseElse { .. } => {
                    self.code(casl2::Command::P {
                        code: casl2::P::Jump,
                        adr: casl2::Adr::label(label),
                        x: None,
                    });
                }
                _ => unreachable!("BUG"),
            }
        }

        self.set_register_idle(value_reg); // GR7 解放のはず

        let exit_label = self.get_exit_label(exit_id);

        if !has_else {
            self.code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            });
        }

        for (case_stmt, label) in case_blocks.iter() {
            match case_stmt {
                parser::Statement::CaseInteger { values, block } => {
                    self.comment(format!(
                        "Case {}",
                        values
                            .iter()
                            .map(|v| v.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                    self.labeled(label, casl2::Command::Nop);
                    if let Some(cmd) = head_debugger_hint.clone() {
                        self.add_debugger_hint_command(
                            cmd,
                            format!(
                                "Case {}",
                                values
                                    .iter()
                                    .map(|v| v.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        );
                    }
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                    self.show_debugger_hint();
                }
                parser::Statement::CaseElse { block } => {
                    self.comment("Case Else");
                    self.labeled(label, casl2::Command::Nop);
                    if let Some(cmd) = head_debugger_hint.clone() {
                        self.add_debugger_hint_command(cmd, "Case Else".to_string());
                    }
                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                    self.show_debugger_hint();
                }
                _ => unreachable!("BUG"),
            }
            self.code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            });
        }

        assert_eq!(
            self.statements.pop(),
            Some(casl2::Statement::code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&exit_label),
                x: None,
            }))
        );
        self.comment("End Select");
        self.labeled(exit_label, casl2::Command::Nop);
        self.add_debugger_hint_message(|| "End Select".to_string());
    }
}
