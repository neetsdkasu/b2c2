use super::*;

// ステートメントの一部
impl Compiler {
    // Option
    pub(super) fn set_option(&mut self, option: &parser::CompileOption) {
        match option {
            parser::CompileOption::Allocator {
                enabled,
                common,
                size,
            } => {
                if *enabled {
                    self.option_use_allocator = true;
                    if !*common {
                        self.option_local_allocation_size = Some(*size);
                    }
                }
            }
            parser::CompileOption::ArraySize { .. } => {}
            parser::CompileOption::Eof { common } => {
                self.option_external_eof = *common;
            }
            parser::CompileOption::Register { restore } => {
                self.option_restore_registers = *restore;
            }
            parser::CompileOption::Variable { initialize } => {
                self.option_initialize_variables = *initialize;
            }
        }
    }

    // Call ステートメント
    pub(super) fn compile_call_exterun_sub(
        &mut self,
        name: &str,
        arguments: &[(String, parser::Expr)],
    ) {
        use casl2::IndexRegister;
        use parser::{Expr, ExprType, VarType};

        if self.option_use_allocator {
            self.save_temp_int_var_labels
                .extend(self.engaged_temp_int_var_labels.iter().cloned());
        }

        let name = match self.original_program_name.as_ref() {
            Some(origin) if origin == name => self.program_name.clone().expect("BUG"),
            _ => name.to_string(),
        };

        let argument_def = self.callables.get(&name).cloned().expect("BUG");
        assert_eq!(argument_def.len(), arguments.len());

        let mut set_argument_codes = Vec::<(IndexRegister, String)>::new();
        let mut temp_int_labels = Vec::<String>::new();
        let mut str_labels = Vec::<StrLabels>::new();

        let mut alloc_pos = self.allocate_memory_relative_position;

        self.add_debugger_hint(|| {
            format!(
                "Call {}({})",
                name,
                arguments
                    .iter()
                    .map(|(arg_name, value)| format!("{} = {}", arg_name, value))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        });

        self.comment(format!("Call {}", name));

        for (arg_name, value) in arguments.iter() {
            let arg = argument_def
                .iter()
                .find(|arg| &arg.var_name == arg_name)
                .expect("BUG");

            self.comment(format!("  {}", arg));
            self.comment(format!("  {} = {}", arg_name, value));

            match value.return_type() {
                ExprType::Boolean => {
                    let is_byref = match arg.var_type {
                        VarType::Boolean => false,
                        VarType::RefBoolean => true,
                        _ => unreachable!("BUG"),
                    };
                    match value {
                        Expr::VarBoolean(var_name) => {
                            let label = self.get_bool_var_label(var_name);
                            set_argument_codes.push((
                                arg.register1,
                                if is_byref {
                                    label.lad_pos(arg.register1.into())
                                } else {
                                    label.ld_value(arg.register1.into())
                                },
                            ));
                        }
                        Expr::VarRefBoolean(var_name) => {
                            let label = self.get_ref_bool_var_label(var_name);
                            set_argument_codes.push((
                                arg.register1,
                                if is_byref {
                                    label.lad_pos(arg.register1.into())
                                } else {
                                    label.ld_value(arg.register1.into())
                                },
                            ));
                        }
                        Expr::VarArrayOfBoolean(var_name, index) => {
                            let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
                            let index_reg = self.compile_int_expr(index); // リテラルindexの配慮は面倒なのでしてない
                            let arr_label = self.get_bool_arr_label(var_name);
                            let temp_label = self.get_temp_int_var_label();
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2])
                            };
                            self.code(saves);
                            self.code(format!(
                                r#" LD    GR1,{index}
                                    LAD   GR2,{size}
                                    CALL  {fit}
                                    {lad_gr1_arrpos}
                                    ADDL  GR1,GR0
                                    ST    GR1,{temp}"#,
                                index = index_reg,
                                size = arr_label.size(),
                                fit = safe_index,
                                lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
                                temp = temp_label
                            ));
                            self.code(recovers);
                            self.set_register_idle(index_reg);
                            temp_int_labels.push(temp_label.clone());
                            set_argument_codes.push((
                                arg.register1,
                                format!(" LD {reg},{temp}", reg = arg.register1, temp = temp_label),
                            ));
                        }
                        Expr::VarRefArrayOfBoolean(var_name, index) => {
                            let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
                            let index_reg = self.compile_int_expr(index); // リテラルindexの配慮は面倒なのでしてない
                            let arr_label = self.get_ref_bool_arr_label(var_name);
                            let temp_label = self.get_temp_int_var_label();
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2])
                            };
                            self.code(saves);
                            self.code(format!(
                                r#" LD    GR1,{index}
                                    LAD   GR2,{size}
                                    CALL  {fit}
                                    {lad_gr1_arrpos}
                                    ADDL  GR0,GR1
                                    ST    GR0,{temp}"#,
                                index = index_reg,
                                size = arr_label.size(),
                                fit = safe_index,
                                lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
                                temp = temp_label
                            ));
                            self.code(recovers);
                            self.set_register_idle(index_reg);
                            temp_int_labels.push(temp_label.clone());
                            set_argument_codes.push((
                                arg.register1,
                                format!(" LD {reg},{temp}", reg = arg.register1, temp = temp_label),
                            ));
                        }
                        _ => {
                            let value_reg = self.compile_int_expr(value);
                            if is_byref && self.option_use_allocator {
                                let offset = alloc_pos;
                                alloc_pos += 1;
                                let temp_reg = self.get_idle_register();
                                self.code(format!(
                                    r#" LD  {temp},MEM
                                        ST  {value},{offset},{temp}"#,
                                    temp = temp_reg,
                                    value = value_reg,
                                    offset = offset
                                ));
                                self.set_register_idle(temp_reg);
                                set_argument_codes.push((
                                    arg.register1,
                                    format!(
                                        " LD  {reg},MEM
                                          LAD {reg},{offset},{reg}",
                                        reg = arg.register1,
                                        offset = offset
                                    ),
                                ));
                            } else {
                                let temp_label = self.get_temp_int_var_label();
                                self.code(format!(
                                    r#" ST {reg},{temp}"#,
                                    reg = value_reg,
                                    temp = temp_label
                                ));
                                temp_int_labels.push(temp_label.clone());
                                set_argument_codes.push((
                                    arg.register1,
                                    format!(
                                        " {cmd} {reg},{temp}",
                                        cmd = if is_byref { "LAD" } else { "LD" },
                                        reg = arg.register1,
                                        temp = temp_label
                                    ),
                                ));
                            }
                            self.set_register_idle(value_reg);
                        }
                    }
                }

                ExprType::Integer => {
                    let is_byref = match arg.var_type {
                        VarType::Integer => false,
                        VarType::RefInteger => true,
                        _ => unreachable!("BUG"),
                    };
                    match value {
                        Expr::VarInteger(var_name) => {
                            let label = self.get_int_var_label(var_name);
                            set_argument_codes.push((
                                arg.register1,
                                if is_byref {
                                    label.lad_pos(arg.register1.into())
                                } else {
                                    label.ld_value(arg.register1.into())
                                },
                            ));
                        }
                        Expr::VarRefInteger(var_name) => {
                            let label = self.get_ref_int_var_label(var_name);
                            set_argument_codes.push((
                                arg.register1,
                                if is_byref {
                                    label.lad_pos(arg.register1.into())
                                } else {
                                    label.ld_value(arg.register1.into())
                                },
                            ));
                        }
                        Expr::VarArrayOfInteger(var_name, index) => {
                            let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
                            let index_reg = self.compile_int_expr(index); // リテラルindexの配慮は面倒なのでしてない
                            let arr_label = self.get_int_arr_label(var_name);
                            let temp_label = self.get_temp_int_var_label();
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2])
                            };
                            self.code(saves);
                            self.code(format!(
                                r#" LD    GR1,{index}
                                    LAD   GR2,{size}
                                    CALL  {fit}
                                    {lad_gr1_arrpos}
                                    ADDL  GR1,GR0
                                    ST    GR1,{temp}"#,
                                index = index_reg,
                                size = arr_label.size(),
                                fit = safe_index,
                                lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
                                temp = temp_label
                            ));
                            self.code(recovers);
                            self.set_register_idle(index_reg);
                            temp_int_labels.push(temp_label.clone());
                            set_argument_codes.push((
                                arg.register1,
                                format!(" LD {reg},{temp}", reg = arg.register1, temp = temp_label),
                            ));
                        }
                        Expr::VarRefArrayOfInteger(var_name, index) => {
                            let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
                            let index_reg = self.compile_int_expr(index); // リテラルindexの配慮は面倒なのでしてない
                            let arr_label = self.get_ref_int_arr_label(var_name);
                            let temp_label = self.get_temp_int_var_label();
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2])
                            };
                            self.code(saves);
                            self.code(format!(
                                r#" LD    GR1,{index}
                                    LAD   GR2,{size}
                                    CALL  {fit}
                                    {lad_gr1_arrpos}
                                    ADDL  GR0,GR1
                                    ST    GR0,{temp}"#,
                                index = index_reg,
                                size = arr_label.size(),
                                fit = safe_index,
                                lad_gr1_arrpos = arr_label.lad_pos(casl2::Register::Gr1),
                                temp = temp_label
                            ));
                            self.code(recovers);
                            self.set_register_idle(index_reg);
                            temp_int_labels.push(temp_label.clone());
                            set_argument_codes.push((
                                arg.register1,
                                format!(" LD {reg},{temp}", reg = arg.register1, temp = temp_label),
                            ));
                        }
                        Expr::CharOfVarString(var_name, index) => {
                            let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
                            let index_reg = self.compile_int_expr(index); // リテラルindexの配慮は面倒なのでしてない
                            let labels = self.get_str_var_labels(var_name);
                            let temp_label = self.get_temp_int_var_label();
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2])
                            };
                            self.code(saves);
                            self.code(format!(
                                r#" LD    GR1,{index}
                                    {ld_gr2_strlen}
                                    CALL  {fit}
                                    {lad_gr1_strpos}
                                    ADDL  GR1,GR0
                                    ST    GR1,{temp}"#,
                                index = index_reg,
                                ld_gr2_strlen = labels.ld_len(casl2::Register::Gr2),
                                lad_gr1_strpos = labels.lad_pos(casl2::Register::Gr1),
                                fit = safe_index,
                                temp = temp_label
                            ));
                            self.code(recovers);
                            self.set_register_idle(index_reg);
                            temp_int_labels.push(temp_label.clone());
                            set_argument_codes.push((
                                arg.register1,
                                format!(" LD {reg},{temp}", reg = arg.register1, temp = temp_label),
                            ));
                        }
                        Expr::CharOfVarRefString(var_name, index) => {
                            let safe_index = self.load_subroutine(subroutine::Id::UtilSafeIndex);
                            let index_reg = self.compile_int_expr(index); // リテラルindexの配慮は面倒なのでしてない
                            let labels = self.get_ref_str_var_labels(var_name);
                            let temp_label = self.get_temp_int_var_label();
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2])
                            };
                            self.code(saves);
                            self.code(format!(
                                r#" LD    GR1,{index}
                                    {ld_gr2_strlen}
                                    CALL  {fit}
                                    {lad_gr1_strpos}
                                    ADDL  GR0,GR1
                                    ST    GR0,{temp}"#,
                                index = index_reg,
                                ld_gr2_strlen = labels.ld_len(casl2::Register::Gr2),
                                lad_gr1_strpos = labels.lad_pos(casl2::Register::Gr1),
                                fit = safe_index,
                                temp = temp_label
                            ));
                            self.code(recovers);
                            self.set_register_idle(index_reg);
                            temp_int_labels.push(temp_label.clone());
                            set_argument_codes.push((
                                arg.register1,
                                format!(" LD {reg},{temp}", reg = arg.register1, temp = temp_label),
                            ));
                        }
                        _ => {
                            let value_reg = self.compile_int_expr(value);
                            if is_byref && self.option_use_allocator {
                                let offset = alloc_pos;
                                alloc_pos += 1;
                                let temp_reg = self.get_idle_register();
                                self.code(format!(
                                    r#" LD  {temp},MEM
                                        ST  {value},{offset},{temp}"#,
                                    temp = temp_reg,
                                    value = value_reg,
                                    offset = offset
                                ));
                                self.set_register_idle(temp_reg);
                                set_argument_codes.push((
                                    arg.register1,
                                    format!(
                                        " LD  {reg},MEM
                                          LAD {reg},{offset},{reg}",
                                        reg = arg.register1,
                                        offset = offset
                                    ),
                                ));
                            } else {
                                let temp_label = self.get_temp_int_var_label();
                                self.code(format!(
                                    r#" ST {reg},{temp}"#,
                                    reg = value_reg,
                                    temp = temp_label
                                ));
                                temp_int_labels.push(temp_label.clone());
                                set_argument_codes.push((
                                    arg.register1,
                                    format!(
                                        " {cmd} {reg},{temp}",
                                        cmd = if is_byref { "LAD" } else { "LD" },
                                        reg = arg.register1,
                                        temp = temp_label
                                    ),
                                ));
                            }
                            self.set_register_idle(value_reg);
                        }
                    }
                }

                ExprType::String => {
                    let is_byref = match arg.var_type {
                        VarType::String => false,
                        VarType::RefString => true,
                        _ => unreachable!("BUG"),
                    };
                    let reg1 = arg.register1;
                    let reg2 = arg.register2.expect("BUG");
                    let labels = self.compile_str_expr(value);
                    match labels.label_type {
                        StrLabelType::Const(_) | StrLabelType::Lit(_) | StrLabelType::Temp
                            if is_byref && self.option_use_allocator =>
                        {
                            let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
                            let temp_labels = StrLabels {
                                len: "".to_string(),
                                pos: "".to_string(),
                                label_type: StrLabelType::MemVal(alloc_pos),
                            };
                            alloc_pos += 257;
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                            };
                            self.code(saves);
                            self.code(temp_labels.lad_pos(casl2::Register::Gr1));
                            self.code(temp_labels.lad_len(casl2::Register::Gr2));
                            self.code(labels.lad_pos(casl2::Register::Gr3));
                            self.code(labels.ld_len(casl2::Register::Gr4));
                            self.code(format!(" CALL {copy}", copy = copystr));
                            self.code(recovers);
                            set_argument_codes.push((reg1, temp_labels.lad_len(reg1.into())));
                            set_argument_codes.push((reg2, temp_labels.lad_pos(reg2.into())));
                            self.return_temp_str_var_label(labels);
                        }
                        StrLabelType::Const(_) | StrLabelType::Lit(_) if is_byref => {
                            let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
                            let temp_labels = self.get_temp_str_var_label();
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                            };
                            self.code(saves);
                            self.code(temp_labels.lad_pos(casl2::Register::Gr1));
                            self.code(temp_labels.lad_len(casl2::Register::Gr2));
                            self.code(labels.lad_pos(casl2::Register::Gr3));
                            self.code(labels.ld_len(casl2::Register::Gr4));
                            self.code(format!(" CALL {copy}", copy = copystr));
                            self.code(recovers);
                            set_argument_codes.push((reg1, temp_labels.lad_len(reg1.into())));
                            set_argument_codes.push((reg2, temp_labels.lad_pos(reg2.into())));
                            str_labels.push(temp_labels);
                        }
                        _ => {
                            set_argument_codes.push((
                                reg1,
                                if is_byref {
                                    labels.lad_len(reg1.into())
                                } else {
                                    labels.ld_len(reg1.into())
                                },
                            ));
                            set_argument_codes.push((reg2, labels.lad_pos(reg2.into())));
                            str_labels.push(labels);
                        }
                    }
                }

                ExprType::ReferenceOfVar(VarType::ArrayOfBoolean(size1))
                | ExprType::ReferenceOfVar(VarType::RefArrayOfBoolean(size1)) => {
                    let is_byref = match arg.var_type {
                        VarType::ArrayOfBoolean(size2) if size1 == size2 => false,
                        VarType::RefArrayOfBoolean(size2) if size1 == size2 => true,
                        _ => unreachable!("BUG"),
                    };
                    let label = self.compile_ref_arr_expr(value);
                    match &label {
                        ArrayLabel::TempArrayOfBoolean(labels, size3)
                            if is_byref && self.option_use_allocator =>
                        {
                            assert_eq!(size1, *size3);
                            let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
                            let temp_label = ArrayLabel::MemArrayOfBoolean {
                                offset: alloc_pos,
                                size: size1,
                            };
                            alloc_pos += size1;
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                            };
                            self.code(saves);
                            self.code(temp_label.lad_pos(casl2::Register::Gr1));
                            self.code(labels.lad_len(casl2::Register::Gr2));
                            self.code(labels.lad_pos(casl2::Register::Gr3));
                            self.code(format!(
                                r#" LAD   GR4,{size}
                                    CALL  {copy}"#,
                                size = size1,
                                copy = copystr
                            ));
                            self.code(recovers);
                            self.return_temp_str_var_label(labels.clone());
                            set_argument_codes
                                .push((arg.register1, temp_label.lad_pos(arg.register1.into())));
                        }
                        ArrayLabel::TempArrayOfBoolean(labels, size3) => {
                            assert_eq!(size1, *size3);
                            set_argument_codes
                                .push((arg.register1, labels.lad_pos(arg.register1.into())));
                            str_labels.push(labels.clone());
                        }
                        ArrayLabel::VarArrayOfBoolean(_, size3)
                        | ArrayLabel::VarRefArrayOfBoolean(_, size3)
                        | ArrayLabel::MemArrayOfBoolean { size: size3, .. }
                        | ArrayLabel::MemRefArrayOfBoolean { size: size3, .. } => {
                            assert_eq!(size1, *size3);
                            set_argument_codes
                                .push((arg.register1, label.lad_pos(arg.register1.into())));
                        }
                        _ => unreachable!("BUG"),
                    }
                }

                ExprType::ReferenceOfVar(VarType::ArrayOfInteger(size1))
                | ExprType::ReferenceOfVar(VarType::RefArrayOfInteger(size1)) => {
                    let is_byref = match arg.var_type {
                        VarType::ArrayOfInteger(size2) if size1 == size2 => false,
                        VarType::RefArrayOfInteger(size2) if size1 == size2 => true,
                        _ => unreachable!("BUG"),
                    };
                    let label = self.compile_ref_arr_expr(value);
                    match &label {
                        ArrayLabel::TempArrayOfInteger(labels, size3)
                            if is_byref && self.option_use_allocator =>
                        {
                            assert_eq!(size1, *size3);
                            let copystr = self.load_subroutine(subroutine::Id::UtilCopyStr);
                            let temp_label = ArrayLabel::MemArrayOfInteger {
                                offset: alloc_pos,
                                size: size1,
                            };
                            alloc_pos += size1;
                            let (saves, recovers) = {
                                use casl2::Register::*;
                                self.get_save_registers_src(&[Gr1, Gr2, Gr3, Gr4])
                            };
                            self.code(saves);
                            self.code(temp_label.lad_pos(casl2::Register::Gr1));
                            self.code(labels.lad_len(casl2::Register::Gr2));
                            self.code(labels.lad_pos(casl2::Register::Gr3));
                            self.code(format!(
                                r#" LAD   GR4,{size}
                                    CALL  {copy}"#,
                                size = size1,
                                copy = copystr
                            ));
                            self.code(recovers);
                            self.return_temp_str_var_label(labels.clone());
                            set_argument_codes
                                .push((arg.register1, temp_label.lad_pos(arg.register1.into())));
                        }
                        ArrayLabel::TempArrayOfInteger(labels, size3) => {
                            assert_eq!(size1, *size3);
                            set_argument_codes
                                .push((arg.register1, labels.lad_pos(arg.register1.into())));
                            str_labels.push(labels.clone());
                        }
                        ArrayLabel::VarArrayOfInteger(_, size3)
                        | ArrayLabel::VarRefArrayOfInteger(_, size3)
                        | ArrayLabel::MemArrayOfInteger { size: size3, .. }
                        | ArrayLabel::MemRefArrayOfInteger { size: size3, .. } => {
                            assert_eq!(size1, *size3);
                            set_argument_codes
                                .push((arg.register1, label.lad_pos(arg.register1.into())));
                        }
                        _ => unreachable!("BUG"),
                    }
                }

                ExprType::ReferenceOfVar(..) | ExprType::ParamList => unreachable!("BUG"),
            }
        }

        let temp_allocate_size = alloc_pos - self.allocate_memory_relative_position;
        if temp_allocate_size > 0 {
            self.maximum_allocate_temporary_area_size = self
                .maximum_allocate_temporary_area_size
                .max(temp_allocate_size);
        }

        if !arguments.is_empty() {
            self.comment(format!("  Set Arguments And Call {}", name));
        }

        let regs: Vec<casl2::Register> = set_argument_codes
            .iter()
            .map(|(reg, _)| (*reg).into())
            .collect();

        let (saves, recovers) = self.get_save_registers_src(&regs);

        self.code(saves);
        for (_, src) in set_argument_codes {
            self.code(src);
        }
        self.code(format!(r#" CALL {prog}"#, prog = name));
        self.code(recovers);

        for label in temp_int_labels.into_iter() {
            self.return_temp_int_var_label(label);
        }
        for labels in str_labels.into_iter() {
            self.return_temp_str_var_label(labels);
        }
    }

    // Argument ステートメント
    pub(super) fn compile_argument(&mut self, arguments: &[parser::ArgumentInfo]) {
        let load_copystr = arguments.iter().any(|arg| {
            matches!(
                arg.var_type,
                parser::VarType::ArrayOfBoolean(_)
                    | parser::VarType::ArrayOfInteger(_)
                    | parser::VarType::String
            )
        });
        if load_copystr {
            self.load_subroutine(subroutine::Id::UtilCopyStr);
        }

        for arg in arguments.iter() {
            match arg.var_type {
                parser::VarType::Boolean => {
                    let label = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += 1;
                        ValueLabel::MemBoolean(offset)
                    } else {
                        ValueLabel::VarBoolean(format!("ARG{}", arg.register1 as isize))
                    };
                    self.argument_labels
                        .insert(arg.var_name.clone(), (label, arg.clone()));
                }
                parser::VarType::RefBoolean => {
                    let label = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += 1;
                        ValueLabel::MemRefBoolean(offset)
                    } else {
                        ValueLabel::VarRefBoolean(format!("ARG{}", arg.register1 as isize))
                    };
                    self.argument_labels
                        .insert(arg.var_name.clone(), (label, arg.clone()));
                }
                parser::VarType::Integer => {
                    let label = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += 1;
                        ValueLabel::MemInteger(offset)
                    } else {
                        ValueLabel::VarInteger(format!("ARG{}", arg.register1 as isize))
                    };
                    self.argument_labels
                        .insert(arg.var_name.clone(), (label, arg.clone()));
                }
                parser::VarType::RefInteger => {
                    let label = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += 1;
                        ValueLabel::MemRefInteger(offset)
                    } else {
                        ValueLabel::VarRefInteger(format!("ARG{}", arg.register1 as isize))
                    };
                    self.argument_labels
                        .insert(arg.var_name.clone(), (label, arg.clone()));
                }
                parser::VarType::ArrayOfBoolean(size) => {
                    let label = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += size;
                        ArrayLabel::MemArrayOfBoolean { offset, size }
                    } else {
                        ArrayLabel::VarArrayOfBoolean(
                            format!("ARG{}", arg.register1 as isize),
                            size,
                        )
                    };
                    self.arr_argument_labels
                        .insert(arg.var_name.clone(), (label, arg.clone()));
                }
                parser::VarType::RefArrayOfBoolean(size) => {
                    let label = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += 1;
                        ArrayLabel::MemRefArrayOfBoolean { offset, size }
                    } else {
                        ArrayLabel::VarRefArrayOfBoolean(
                            format!("ARG{}", arg.register1 as isize),
                            size,
                        )
                    };
                    self.arr_argument_labels
                        .insert(arg.var_name.clone(), (label, arg.clone()));
                }
                parser::VarType::ArrayOfInteger(size) => {
                    let label = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += size;
                        ArrayLabel::MemArrayOfInteger { offset, size }
                    } else {
                        ArrayLabel::VarArrayOfInteger(
                            format!("ARG{}", arg.register1 as isize),
                            size,
                        )
                    };
                    self.arr_argument_labels
                        .insert(arg.var_name.clone(), (label, arg.clone()));
                }
                parser::VarType::RefArrayOfInteger(size) => {
                    let label = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += 1;
                        ArrayLabel::MemRefArrayOfInteger { offset, size }
                    } else {
                        ArrayLabel::VarRefArrayOfInteger(
                            format!("ARG{}", arg.register1 as isize),
                            size,
                        )
                    };
                    self.arr_argument_labels
                        .insert(arg.var_name.clone(), (label, arg.clone()));
                }
                parser::VarType::String => {
                    let labels = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += 257;
                        StrLabels {
                            len: "*Str:MemVal:len*".to_string(),
                            pos: "*Str:MemVal:pos*".to_string(),
                            label_type: StrLabelType::MemVal(offset),
                        }
                    } else {
                        StrLabels {
                            len: format!("ARG{}", arg.register1 as isize),
                            pos: format!("ARG{}", arg.register2.expect("BUG") as isize),
                            label_type: StrLabelType::ArgVal,
                        }
                    };
                    self.str_argument_labels
                        .insert(arg.var_name.clone(), (labels, arg.clone()));
                }
                parser::VarType::RefString => {
                    let labels = if self.option_use_allocator {
                        let offset = self.allocate_memory_relative_position;
                        self.allocate_memory_relative_position += 2;
                        StrLabels {
                            len: "*Str:MemRef:len*".to_string(),
                            pos: "*Str:MemRef:pos*".to_string(),
                            label_type: StrLabelType::MemRef(offset),
                        }
                    } else {
                        StrLabels {
                            len: format!("ARG{}", arg.register1 as isize),
                            pos: format!("ARG{}", arg.register2.expect("BUG") as isize),
                            label_type: StrLabelType::ArgRef,
                        }
                    };
                    self.str_argument_labels
                        .insert(arg.var_name.clone(), (labels, arg.clone()));
                }
            }
        }

        self.allocate_arguments_size = self.allocate_memory_relative_position;

        if let Some(name) = self.program_name.clone() {
            self.callables.insert(name, arguments.into());
        }

        self.arguments = arguments.into();
    }

    // Program ステートメント
    pub(super) fn compile_program_name(&mut self, name: &str) {
        if self.program_name.is_none() {
            self.program_name = Some(name.into());
        }
        self.original_program_name = Some(name.into());
        let program_name = self.program_name.clone().unwrap();
        self.callables.insert(program_name, Vec::new());
    }

    // Extern Sub ステートメント
    pub(super) fn compile_extern_sub(&mut self, name: &str, arguments: &[parser::ArgumentInfo]) {
        assert!(!self.callables.contains_key(name));
        self.callables.insert(name.into(), arguments.into());
    }

    // Exit Sub ステートメント
    pub(super) fn compile_exit_program(&mut self) {
        self.add_debugger_hint(|| "Exit Sub".to_string());
        self.show_debugger_hint();
        self.comment("Exit Sub");
        self.code(casl2::Command::P {
            code: casl2::P::Jump,
            adr: casl2::Adr::label("EXIT"),
            x: None,
        });
    }

    // Midステートメント
    // Mid(<var_str>,<offset>) = <str_expr>
    // Mid(<var_str>,<offset>,<length>) = <str_expr>
    pub(super) fn compile_mid(
        &mut self,
        var_name: &str,
        var_is_ref: bool,
        offset: &parser::Expr,
        length: &Option<parser::Expr>,
        value: &parser::Expr,
    ) {
        assert!(matches!(offset.return_type(), parser::ExprType::Integer));
        assert!(matches!(value.return_type(), parser::ExprType::String));

        let partialcopy = self.load_subroutine(subroutine::Id::UtilCopyToOffsetStr);

        let var_labels = if var_is_ref {
            self.get_ref_str_var_labels(var_name)
        } else {
            self.get_str_var_labels(var_name)
        };

        if let Some(length) = length {
            assert!(matches!(length.return_type(), parser::ExprType::Integer));
            self.add_debugger_hint(|| {
                format!(
                    "Mid( {name}, {offset}, {length} ) = {value}",
                    name = var_name,
                    offset = offset,
                    length = length,
                    value = value
                )
            });
            self.comment(format!(
                "Mid( {name}, {offset}, {length} ) = {value}",
                name = var_name,
                offset = offset,
                length = length,
                value = value
            ));
            let value_labels = self.compile_str_expr(value);
            let offset_reg = self.compile_int_expr(offset);
            let length_reg = self.compile_int_expr(length);
            self.restore_register(offset_reg);

            let (saves, recovers) = {
                use casl2::Register::*;
                let mut regs = vec![Gr1, Gr2, Gr3, Gr4, Gr5];
                if matches!(offset_reg, Gr1) {
                    regs.retain(|r| !matches!(r, Gr1));
                }
                if !matches!(length_reg, Gr6) {
                    regs.push(Gr6);
                }
                self.get_save_registers_src(&regs)
            };

            let (length_line1, length_line2) = if matches!(length_reg, casl2::Register::Gr1) {
                if matches!(offset_reg, casl2::Register::Gr6) {
                    (" LD GR0,GR1".to_string(), " LD GR6,GR0".to_string())
                } else {
                    (" LD GR6,GR1".to_string(), "".to_string())
                }
            } else {
                (
                    "".to_string(),
                    if matches!(length_reg, casl2::Register::Gr6) {
                        "".to_string()
                    } else {
                        format!(" LD GR6,{length}", length = length_reg)
                    },
                )
            };

            let offset_line = if matches!(offset_reg, casl2::Register::Gr1) {
                "".to_string()
            } else {
                format!(" LD GR1,{offset}", offset = offset_reg)
            };

            self.code(saves);
            self.code(length_line1);
            self.code(offset_line);
            self.code(length_line2);
            self.code(var_labels.lad_pos(casl2::Register::Gr5));
            self.code(var_labels.ld_len(casl2::Register::Gr2));
            self.code(value_labels.lad_pos(casl2::Register::Gr3));
            self.code(value_labels.ld_len(casl2::Register::Gr4));
            self.code(format!(r#" CALL  {copy}"#, copy = partialcopy));
            self.code(recovers);

            self.set_register_idle(length_reg);
            self.set_register_idle(offset_reg);
            self.return_temp_str_var_label(value_labels);
        } else {
            self.add_debugger_hint(|| {
                format!(
                    "Mid( {name}, {offset} ) = {value}",
                    name = var_name,
                    offset = offset,
                    value = value
                )
            });
            self.comment(format!(
                "Mid( {name}, {offset} ) = {value}",
                name = var_name,
                offset = offset,
                value = value
            ));
            let value_labels = self.compile_str_expr(value);
            let offset_reg = self.compile_int_expr(offset);

            let (saves, recovers) = {
                use casl2::Register::*;
                let mut regs = vec![Gr1, Gr2, Gr3, Gr4, Gr5, Gr6];
                if matches!(offset_reg, Gr1) {
                    regs.retain(|r| !matches!(r, Gr1));
                }
                self.get_save_registers_src(&regs)
            };

            let offset_line = if matches!(offset_reg, casl2::Register::Gr1) {
                "".to_string()
            } else {
                format!(" LD GR1,{offset}", offset = offset_reg)
            };

            self.code(saves);
            self.code(offset_line);
            self.code(var_labels.lad_pos(casl2::Register::Gr5));
            self.code(var_labels.ld_len(casl2::Register::Gr2));
            self.code(value_labels.lad_pos(casl2::Register::Gr3));
            self.code(value_labels.ld_len(casl2::Register::Gr4));
            self.code(format!(
                r#" LD    GR6,GR2
                    CALL  {copy}"#,
                copy = partialcopy
            ));
            self.code(recovers);

            self.set_register_idle(offset_reg);
            self.return_temp_str_var_label(value_labels);
        }
    }

    // If ステートメント
    pub(super) fn compile_if(
        &mut self,
        condition: &parser::Expr,
        block: &[parser::Statement],
        else_blocks: &[parser::Statement],
    ) {
        self.add_debugger_hint(|| format!("If {} Then", condition));
        self.nest_depth += 1;
        self.comment(format!("If {} Then", condition));

        let end_label = self.get_new_jump_label();

        let labels: Vec<_> = (0..else_blocks.len())
            .map(|_| self.get_new_jump_label())
            .chain(vec![end_label.clone()])
            .collect();

        let condition_reg = self.compile_int_expr(condition);

        self.set_debugger_hint_extra_info(|| ExtraInfo::Condition(condition_reg));

        self.add_debugger_hint(|| "(then)".to_string());
        self.set_debugger_hint_extra_info(|| {
            ExtraInfo::RelatedCode(format!("If {} Then", condition))
        });

        self.set_register_idle(condition_reg);

        self.code(format!(
            r#" AND {reg},{reg}
                JZE {next}"#,
            reg = condition_reg,
            next = labels.first().expect("BUG")
        ));

        for stmt in block.iter() {
            self.compile(stmt);
        }
        self.show_debugger_hint();

        let label_iter = labels.iter().zip(labels.iter().skip(1));

        for (else_stmt, (head, next)) in else_blocks.iter().zip(label_iter) {
            self.code(casl2::Command::P {
                code: casl2::P::Jump,
                adr: casl2::Adr::label(&end_label),
                x: None,
            });

            match else_stmt {
                parser::Statement::ElseIf { condition, block } => {
                    self.nest_depth -= 1;
                    self.add_debugger_hint_message(|| format!("ElseIf {} Then", condition));
                    self.nest_depth += 1;
                    self.comment(format!("ElseIf {} Then", condition));
                    self.labeled(head, casl2::Command::Nop);

                    let condition_reg = self.compile_int_expr(condition);

                    self.set_debugger_hint_extra_info(|| ExtraInfo::Condition(condition_reg));
                    self.add_debugger_hint(|| "(then)".to_string());
                    self.set_debugger_hint_extra_info(|| {
                        ExtraInfo::RelatedCode(format!("ElseIf {} Then", condition))
                    });

                    self.set_register_idle(condition_reg);

                    self.code(format!(
                        r#" AND {reg},{reg}
                            JZE {next}"#,
                        reg = condition_reg,
                        next = next
                    ));

                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                    self.show_debugger_hint();
                }
                parser::Statement::Else { block } => {
                    self.nest_depth -= 1;
                    self.add_debugger_hint_message(|| "Else".to_string());
                    self.nest_depth += 1;
                    self.comment("Else");
                    self.labeled(head, casl2::Command::Nop);

                    for stmt in block.iter() {
                        self.compile(stmt);
                    }
                    self.show_debugger_hint();
                }
                _ => unreachable!("BUG"),
            }
        }

        self.comment("End If");
        self.labeled(end_label, casl2::Command::Nop);
        self.nest_depth -= 1;
        self.add_debugger_hint_message(|| "End If".to_string());
    }

    // Continue {Do/For}
    pub(super) fn compile_continue_loop(&mut self, exit_id: usize, keyword: &str) {
        assert!(matches!(keyword, "Do" | "For"));
        let loop_label = self.get_loop_label(exit_id);
        self.add_debugger_hint(|| format!("Continue {}", keyword));
        self.show_debugger_hint();
        self.comment(format!("Continue {}", keyword));
        // JUMP {loop}
        self.code(casl2::Command::P {
            code: casl2::P::Jump,
            adr: casl2::Adr::label(&loop_label),
            x: None,
        });
    }

    // Exit {Do/For/Select}
    pub(super) fn compile_exit_block(&mut self, exit_id: usize, keyword: &str) {
        assert!(matches!(keyword, "Do" | "For" | "Select"));
        let exit_label = self.get_exit_label(exit_id);
        self.add_debugger_hint(|| format!("Exit {}", keyword));
        self.show_debugger_hint();
        self.comment(format!("Exit {}", keyword));
        // JUMP {exit}
        self.code(casl2::Command::P {
            code: casl2::P::Jump,
            adr: casl2::Adr::label(&exit_label),
            x: None,
        });
    }

    // Dim ステートメント
    pub(super) fn compile_dim(&mut self, var_name: &str, var_type: &parser::VarType) {
        use parser::VarType;
        self.var_id += 1;
        match var_type {
            VarType::Boolean => {
                let label = if self.option_use_allocator {
                    let offset = self.allocate_memory_relative_position;
                    self.allocate_memory_relative_position += 1;
                    ValueLabel::MemBoolean(offset)
                } else {
                    ValueLabel::VarBoolean(format!("B{:03}", self.var_id))
                };
                self.bool_var_labels.insert(var_name.into(), label);
                self.var_total_size += 1;
            }
            VarType::Integer => {
                let label = if self.option_use_allocator {
                    let offset = self.allocate_memory_relative_position;
                    self.allocate_memory_relative_position += 1;
                    ValueLabel::MemInteger(offset)
                } else {
                    ValueLabel::VarInteger(format!("I{:03}", self.var_id))
                };
                self.int_var_labels.insert(var_name.into(), label);
                self.var_total_size += 1;
            }
            VarType::String => {
                let labels = if self.option_use_allocator {
                    let offset = self.allocate_memory_relative_position;
                    self.allocate_memory_relative_position += 257;
                    StrLabels {
                        len: "*Str:MemVal:len*".to_string(),
                        pos: "*Str:MemVal:pos*".to_string(),
                        label_type: StrLabelType::MemVal(offset),
                    }
                } else {
                    let len_label = format!("SL{:03}", self.var_id);
                    let pos_label = format!("SB{:03}", self.var_id);
                    StrLabels {
                        len: len_label,
                        pos: pos_label,
                        label_type: StrLabelType::Var,
                    }
                };
                self.str_var_labels.insert(var_name.into(), labels);
                self.var_total_size += 257;
            }
            VarType::ArrayOfBoolean(size) => {
                let label = if self.option_use_allocator {
                    let offset = self.allocate_memory_relative_position;
                    self.allocate_memory_relative_position += *size;
                    ArrayLabel::MemArrayOfBoolean {
                        offset,
                        size: *size,
                    }
                } else {
                    ArrayLabel::VarArrayOfBoolean(format!("BA{:03}", self.var_id), *size)
                };
                self.bool_arr_labels.insert(var_name.into(), label);
                self.var_total_size += size;
            }
            VarType::ArrayOfInteger(size) => {
                let label = if self.option_use_allocator {
                    let offset = self.allocate_memory_relative_position;
                    self.allocate_memory_relative_position += *size;
                    ArrayLabel::MemArrayOfInteger {
                        offset,
                        size: *size,
                    }
                } else {
                    ArrayLabel::VarArrayOfInteger(format!("IA{:03}", self.var_id), *size)
                };
                self.int_arr_labels.insert(var_name.into(), label);
                self.var_total_size += size;
            }
            VarType::RefBoolean
            | VarType::RefInteger
            | VarType::RefString
            | VarType::RefArrayOfBoolean(_)
            | VarType::RefArrayOfInteger(_) => unreachable!("BUG"),
        }
    }
}
