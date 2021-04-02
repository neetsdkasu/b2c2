// crate::compiler::ext

use super::*;

pub trait AddComment<T> {
    fn comment(&mut self, text: T);
}

pub trait AddCode<T> {
    fn code(&mut self, src: T);
}

pub trait AddLabeledCode<L> {
    fn labeled(&mut self, label: L, command: casl2::Command);
}

impl<T> AddComment<T> for Compiler
where
    Vec<casl2::Statement>: AddComment<T>,
{
    fn comment(&mut self, text: T) {
        self.statements.comment(text);
    }
}

impl AddComment<&str> for Vec<casl2::Statement> {
    fn comment(&mut self, text: &str) {
        self.push(casl2::Statement::comment_with_indent(35, text));
    }
}

impl AddComment<String> for Vec<casl2::Statement> {
    fn comment(&mut self, text: String) {
        self.push(casl2::Statement::Comment { indent: 35, text });
    }
}

impl<T> AddCode<T> for Compiler
where
    Vec<casl2::Statement>: AddCode<T>,
{
    fn code(&mut self, src: T) {
        self.statements.code(src);
    }
}

impl AddCode<&str> for Vec<casl2::Statement> {
    fn code(&mut self, src: &str) {
        self.extend(casl2::parse(src.trim_start_matches('\n').trim_end()).unwrap());
    }
}

impl AddCode<String> for Vec<casl2::Statement> {
    fn code(&mut self, src: String) {
        self.extend(casl2::parse(src.trim_start_matches('\n').trim_end()).unwrap());
    }
}

impl AddCode<&String> for Vec<casl2::Statement> {
    fn code(&mut self, src: &String) {
        self.extend(casl2::parse(src.trim_start_matches('\n').trim_end()).unwrap());
    }
}

impl AddCode<Vec<casl2::Statement>> for Vec<casl2::Statement> {
    fn code(&mut self, src: Vec<casl2::Statement>) {
        self.extend(src);
    }
}

impl AddCode<casl2::Statement> for Vec<casl2::Statement> {
    fn code(&mut self, src: casl2::Statement) {
        self.push(src);
    }
}

impl AddCode<casl2::Command> for Vec<casl2::Statement> {
    fn code(&mut self, src: casl2::Command) {
        self.push(casl2::Statement::code(src));
    }
}

impl<T> AddLabeledCode<T> for Compiler
where
    Vec<casl2::Statement>: AddLabeledCode<T> + AddCode<casl2::Command>,
{
    fn labeled(&mut self, label: T, command: casl2::Command) {
        self.statements.labeled(label, command);
    }
}

impl AddLabeledCode<&str> for Vec<casl2::Statement> {
    fn labeled(&mut self, label: &str, command: casl2::Command) {
        self.push(casl2::Statement::labeled(label, command));
    }
}

impl AddLabeledCode<String> for Vec<casl2::Statement> {
    fn labeled(&mut self, label: String, command: casl2::Command) {
        self.push(casl2::Statement::Code {
            label: Some(label.into()),
            command,
            comment: None,
        });
    }
}

impl AddLabeledCode<&String> for Vec<casl2::Statement> {
    fn labeled(&mut self, label: &String, command: casl2::Command) {
        self.push(casl2::Statement::Code {
            label: Some(label.clone().into()),
            command,
            comment: None,
        });
    }
}

impl AddLabeledCode<casl2::Label> for Vec<casl2::Statement> {
    fn labeled(&mut self, label: casl2::Label, command: casl2::Command) {
        self.push(casl2::Statement::Code {
            label: Some(label),
            command,
            comment: None,
        });
    }
}

impl AddLabeledCode<&casl2::Label> for Vec<casl2::Statement> {
    fn labeled(&mut self, label: &casl2::Label, command: casl2::Command) {
        self.push(casl2::Statement::Code {
            label: Some(label.clone()),
            command,
            comment: None,
        });
    }
}

impl<T> AddLabeledCode<Option<T>> for Vec<casl2::Statement>
where
    Self: AddLabeledCode<T> + AddCode<casl2::Command>,
{
    fn labeled(&mut self, label: Option<T>, command: casl2::Command) {
        if let Some(t) = label {
            self.labeled(t, command);
        } else {
            self.code(command);
        }
    }
}
