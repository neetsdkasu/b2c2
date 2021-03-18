use crate::casl2;
use crate::parser;
use std::collections::{BTreeSet, HashMap};

type CompileError = String;

// 正しい src が来ることを前提とする (不正な src の判定は面倒くさい)
pub fn compile(
    program_name: &str,
    src: Vec<parser::Statement>,
) -> Result<Vec<casl2::Statement>, CompileError> {
    let mut compiler = Compiler::new(program_name)?;

    for stmt in src.iter() {
        compiler.compile(stmt);
    }

    Ok(compiler.finish())
}

struct Compiler {
    var_id: usize,
    jump_id: usize,
    bool_var_labels: HashMap<String, String>,
    int_var_labels: HashMap<String, String>,
    str_var_labels: HashMap<String, (String, String)>,
    bool_arr_labels: HashMap<String, (String, usize)>,
    int_arr_labels: HashMap<String, (String, usize)>,
    statements: Vec<casl2::Statement>,
}

impl Compiler {
    fn is_valid_program_name(program_name: &str) -> bool {
        casl2::Label::new(program_name).is_valid()

        // 自動生成のラベルとの重複を避けるチェックが必要
        // B123,I123,SL123,SB123, ..
    }

    fn new(program_name: &str) -> Result<Self, CompileError> {
        if !Self::is_valid_program_name(program_name) {
            return Err(format!("invalid Program Name: {}", program_name));
        }

        Ok(Self {
            var_id: 0,
            jump_id: 0,
            bool_var_labels: HashMap::new(),
            int_var_labels: HashMap::new(),
            str_var_labels: HashMap::new(),
            bool_arr_labels: HashMap::new(),
            int_arr_labels: HashMap::new(),
            statements: vec![casl2::Statement::labeled(
                program_name,
                casl2::Command::Start { entry_point: None },
            )],
        })
    }

    fn finish(self) -> Vec<casl2::Statement> {
        let Self {
            bool_var_labels,
            int_var_labels,
            str_var_labels,
            bool_arr_labels,
            int_arr_labels,
            mut statements,
            ..
        } = self;

        statements.push(casl2::Statement::code(casl2::Command::Ret));

        // here: insert operator & function codes

        for label in bool_var_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: 1 },
            ));
        }

        for label in int_var_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: 1 },
            ));
        }

        for (len_label, data_label) in str_var_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &len_label,
                casl2::Command::Ds { size: 1 },
            ));
            statements.push(casl2::Statement::labeled(
                &data_label,
                casl2::Command::Ds { size: 256 },
            ));
        }

        for (label, size) in bool_arr_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: size as u16 },
            ));
        }

        for (label, size) in int_arr_labels
            .into_iter()
            .map(|(_, v)| v)
            .collect::<BTreeSet<_>>()
        {
            statements.push(casl2::Statement::labeled(
                &label,
                casl2::Command::Ds { size: size as u16 },
            ));
        }

        statements.push(casl2::Statement::code(casl2::Command::End));
        statements
    }

    fn compile(&mut self, stmt: &parser::Statement) {
        use parser::Statement::*;
        match stmt {
            AssignAddInto {
                var_name: _,
                value: _,
            } => todo!(),
            AssignAddIntoElement {
                var_name: _,
                index: _,
                value: _,
            } => todo!(),
            AssignBoolean {
                var_name: _,
                value: _,
            } => todo!(),
            AssignElement {
                var_name: _,
                index: _,
                value: _,
            } => todo!(),
            AssignInteger {
                var_name: _,
                value: _,
            } => todo!(),
            AssignString {
                var_name: _,
                value: _,
            } => todo!(),
            AssignSubInto {
                var_name: _,
                value: _,
            } => todo!(),
            AssignSubIntoElement {
                var_name: _,
                index: _,
                value: _,
            } => todo!(),
            ContinueDo { exit_id: _ } => todo!(),
            ContinueFor { exit_id: _ } => todo!(),
            Dim { var_name, var_type } => self.compile_dim(var_name, var_type),
            DoLoop {
                exit_id: _,
                block: _,
            } => todo!(),
            DoLoopUntil {
                exit_id: _,
                condition: _,
                block: _,
            } => todo!(),
            DoLoopWhile {
                exit_id: _,
                condition: _,
                block: _,
            } => todo!(),
            DoUntilLoop {
                exit_id: _,
                condition: _,
                block: _,
            } => todo!(),
            DoWhileLoop {
                exit_id: _,
                condition: _,
                block: _,
            } => todo!(),
            ProvisionalDo {
                exit_id: _,
                until_condition: _,
                while_condition: _,
            } => todo!(),
            ExitDo { exit_id: _ } => todo!(),
            ExitFor { exit_id: _ } => todo!(),
            ExitSelect { exit_id: _ } => todo!(),
            For {
                exit_id: _,
                counter: _,
                init: _,
                end: _,
                step: _,
                block: _,
            } => todo!(),
            ProvisionalFor {
                exit_id: _,
                counter: _,
                init: _,
                end: _,
                step: _,
            } => todo!(),
            InputElementInteger {
                var_name: _,
                index: _,
            } => todo!(),
            If {
                condition: _,
                block: _,
                else_blocks: _,
            } => todo!(),
            ProvitionalIf { condition: _ } => todo!(),
            ElseIf {
                condition: _,
                block: _,
            } => todo!(),
            ProvisionalElseIf { condition: _ } => todo!(),
            Else { block: _ } => todo!(),
            ProvisionalElse => todo!(),
            SelectInteger {
                exit_id: _,
                value: _,
                case_blocks: _,
            } => todo!(),
            ProvisionalSelectInteger {
                exit_id: _,
                value: _,
            } => todo!(),
            CaseInteger {
                values: _,
                block: _,
            } => todo!(),
            ProvisionalCaseInteger { values: _ } => todo!(),
            SelectString {
                exit_id: _,
                value: _,
                case_blocks: _,
            } => todo!(),
            ProvisionalSelectString {
                exit_id: _,
                value: _,
            } => todo!(),
            CaseString {
                values: _,
                block: _,
            } => todo!(),
            ProvisionalCaseString { values: _ } => todo!(),
            CaseElse { block: _ } => todo!(),
            ProvisionalCaseElse => todo!(),
            InputInteger { var_name: _ } => todo!(),
            InputString { var_name: _ } => todo!(),
            PrintLitBoolean { value: _ } => todo!(),
            PrintLitInteger { value: _ } => todo!(),
            PrintLitString { value: _ } => todo!(),
            PrintVarBoolean { var_name: _ } => todo!(),
            PrintVarInteger { var_name: _ } => todo!(),
            PrintVarString { var_name: _ } => todo!(),
            PrintExprBoolan { value: _ } => todo!(),
            PrintExprInteger { value: _ } => todo!(),
            PrintExprString { value: _ } => todo!(),
        }
    }

    fn compile_dim(&mut self, var_name: &str, var_type: &parser::VarType) {
        use parser::VarType;
        self.var_id += 1;
        match var_type {
            VarType::Boolean => {
                let label = format!("B{}", self.var_id);
                self.bool_var_labels.insert(var_name.into(), label);
            }
            VarType::Integer => {
                let label = format!("I{}", self.var_id);
                self.int_var_labels.insert(var_name.into(), label);
            }
            VarType::String => {
                let len_label = format!("SL{}", self.var_id);
                let buf_label = format!("SB{}", self.var_id);
                let labels = (len_label, buf_label);
                self.str_var_labels.insert(var_name.into(), labels);
            }
            VarType::ArrayOfBoolean(size) => {
                let label = format!("BA{}", self.var_id);
                self.bool_arr_labels.insert(var_name.into(), (label, *size));
            }
            VarType::ArrayOfInteger(size) => {
                let label = format!("IA{}", self.var_id);
                self.int_arr_labels.insert(var_name.into(), (label, *size));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_works() {
        assert!(true);
    }

    #[test]
    fn compiler_compile_dim_works() {
        let mut compiler = Compiler::new("TEST").unwrap();

        compiler.compile_dim("strVar1", &parser::VarType::String);
        compiler.compile_dim("boolVar1", &parser::VarType::Boolean);
        compiler.compile_dim("intVar2", &parser::VarType::Integer);
        compiler.compile_dim("boolArr1", &parser::VarType::ArrayOfBoolean(32));
        compiler.compile_dim("boolVar2", &parser::VarType::Boolean);
        compiler.compile_dim("strVar2", &parser::VarType::String);
        compiler.compile_dim("intArr1", &parser::VarType::ArrayOfInteger(155));
        compiler.compile_dim("intVar1", &parser::VarType::Integer);

        assert_eq!(
            compiler.finish(),
            vec![
                casl2::Statement::labeled("TEST", casl2::Command::Start { entry_point: None },),
                casl2::Statement::code(casl2::Command::Ret),
                casl2::Statement::labeled("B2", casl2::Command::Ds { size: 1 },),
                casl2::Statement::labeled("B5", casl2::Command::Ds { size: 1 },),
                casl2::Statement::labeled("I3", casl2::Command::Ds { size: 1 },),
                casl2::Statement::labeled("I8", casl2::Command::Ds { size: 1 },),
                casl2::Statement::labeled("SL1", casl2::Command::Ds { size: 1 },),
                casl2::Statement::labeled("SB1", casl2::Command::Ds { size: 256 },),
                casl2::Statement::labeled("SL6", casl2::Command::Ds { size: 1 },),
                casl2::Statement::labeled("SB6", casl2::Command::Ds { size: 256 },),
                casl2::Statement::labeled("BA4", casl2::Command::Ds { size: 32 },),
                casl2::Statement::labeled("IA7", casl2::Command::Ds { size: 155 },),
                casl2::Statement::code(casl2::Command::End),
            ]
        );
    }
}
