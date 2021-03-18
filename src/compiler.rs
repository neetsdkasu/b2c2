use crate::casl2;
use crate::parser;

type CompileError = String;

pub fn compile(
    program_name: &str,
    src: Vec<parser::Statement>,
) -> Result<Vec<casl2::Statement>, CompileError> {
    if !casl2::Label::new(program_name).is_valid() {
        return Err(format!("invalid Program Name: {}", program_name));
    }

    let mut compiler = Compiler::new(program_name);

    for stmt in src.iter() {
        compiler.compile(stmt);
    }

    Ok(compiler.finish())
}

struct Compiler {
    int_var_id: usize,
    str_var_id: usize,
    jump_id: usize,
    statements: Vec<casl2::Statement>,
}

impl Compiler {
    fn new(program_name: &str) -> Self {
        Self {
            int_var_id: 0,
            str_var_id: 0,
            jump_id: 0,
            statements: vec![casl2::Statement::labeled(
                program_name,
                casl2::Command::Start { entry_point: None },
            )],
        }
    }

    fn finish(mut self) -> Vec<casl2::Statement> {
        self.statements
            .push(casl2::Statement::code(casl2::Command::End));
        self.statements
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
            Dim {
                var_name: _,
                var_type: _,
            } => todo!(),
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
}

#[cfg(test)]
mod test {

    #[test]
    fn it_works() {
        assert!(true);
    }
}
