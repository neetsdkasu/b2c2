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
        todo!();
    }

    Ok(compiler.finish())
}

struct Compiler {
    int_var_id: usize,
    str_var_id: usize,
    statements: Vec<casl2::Statement>,
}

impl Compiler {
    fn new(program_name: &str) -> Self {
        Self {
            int_var_id: 0,
            str_var_id: 0,
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
}

#[cfg(test)]
mod test {

    #[test]
    fn it_works() {
        assert!(true);
    }
}
