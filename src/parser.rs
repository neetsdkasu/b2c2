use crate::casl2::IndexRegister;
use crate::compiler::is_valid_program_name;
use crate::tokenizer::*;
use crate::SyntaxError;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::io::{self, BufRead};

#[cfg(test)]
mod test;

const MAX_ARRAY_SIZE: usize = 256;

pub fn parse<R: BufRead>(reader: R) -> io::Result<Result<Vec<Statement>, SyntaxError>> {
    let mut parser = Parser::new();

    for line in Tokenizer::new(reader) {
        let (line_number, pos_and_tokens) = match line? {
            Ok(values) => values,
            Err(error) => return Ok(Err(error)),
        };
        parser.line_number = line_number;

        match pos_and_tokens.first() {
            None => continue,
            Some((pos, Token::Name(name))) => {
                // bind (assign) statement
                parser.line_start_position = *pos;
                if let Err(error) = parser.parse_assign(name, &pos_and_tokens[1..]) {
                    return Ok(Err(error));
                }
            }
            Some((pos, Token::Keyword(keyword))) if keyword.is_toplevel_token() => {
                // command statement
                parser.line_start_position = *pos;
                if let Err(error) = parser.parse_command(keyword, &pos_and_tokens[1..]) {
                    return Ok(Err(error));
                }
            }
            Some((pos, token)) => {
                return Ok(Err(SyntaxError::new(
                    line_number,
                    *pos,
                    format!("not command: {:?}", token),
                )))
            }
        }
    }

    if !parser.is_valid() {
        return Ok(Err(SyntaxError::new(
            parser.line_number + 1,
            0,
            "invalid Source Code".into(),
        )));
    }

    Ok(Ok(parser.statements.pop().expect("BUG")))
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum EndProgramState {
    Unnecessary,
    Required,
    Satisfied,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum HeaderState {
    ExternSub,
    InExternSub,
    ProgramName,
    Argument,
    InArgument,
    Dim,
    NotHeader,
}

impl HeaderState {
    fn in_header(self) -> bool {
        !matches!(self, HeaderState::NotHeader)
    }

    fn in_defininition(self) -> bool {
        self.in_extern_sub() || self.in_argument()
    }

    fn in_extern_sub(self) -> bool {
        matches!(self, HeaderState::InExternSub)
    }

    fn in_argument(self) -> bool {
        matches!(self, HeaderState::InArgument)
    }

    fn can_command(self) -> bool {
        use HeaderState::*;
        matches!(self, ExternSub | ProgramName | Argument | Dim | NotHeader)
    }

    fn can_argument(self) -> bool {
        use HeaderState::*;
        matches!(self, ExternSub | ProgramName | Argument)
    }

    fn can_program_name(self) -> bool {
        use HeaderState::*;
        matches!(self, ExternSub | ProgramName)
    }

    fn can_extern_sub(self) -> bool {
        matches!(self, HeaderState::ExternSub)
    }

    fn can_dim(self) -> bool {
        use HeaderState::*;
        matches!(self, ExternSub | ProgramName | Argument | Dim)
    }
}

struct Parser {
    line_number: usize,
    line_start_position: usize,
    variables: HashMap<String, VarType>,
    statements: Vec<Vec<Statement>>,
    nest_of_do: Vec<usize>,
    nest_of_for: Vec<(usize, String)>,
    nest_of_select: Vec<usize>,
    provisionals: Vec<Statement>,
    exit_id: usize,
    is_select_head: bool,
    header_state: HeaderState,
    temp_argumets: Vec<ArgumentInfo>,
    temp_progam_name: Option<String>,
    callables: HashMap<String, Vec<ArgumentInfo>>,
    end_program_state: EndProgramState,
    in_call_with: bool,
    temp_call_with_arguments: Vec<(String, Expr)>,
}

impl Parser {
    fn new() -> Self {
        Self {
            line_number: 0,
            line_start_position: 0,
            variables: HashMap::new(),
            statements: vec![vec![]; 1],
            nest_of_do: Vec::new(),
            nest_of_for: Vec::new(),
            nest_of_select: Vec::new(),
            provisionals: Vec::new(),
            exit_id: 0,
            is_select_head: false,
            header_state: HeaderState::ExternSub,
            temp_argumets: Vec::new(),
            temp_progam_name: None,
            callables: HashMap::new(),
            end_program_state: EndProgramState::Unnecessary,
            in_call_with: false,
            temp_call_with_arguments: Vec::new(),
        }
    }

    fn is_valid(&self) -> bool {
        self.statements.len() == 1
            && self.nest_of_do.is_empty()
            && self.nest_of_for.is_empty()
            && self.nest_of_select.is_empty()
            && self.provisionals.is_empty()
            && !self.is_select_head
            && self.header_state.can_command()
            && !matches!(self.end_program_state, EndProgramState::Required)
            && !self.in_call_with
    }

    fn get_new_exit_id(&mut self) -> usize {
        let id = self.exit_id;
        self.exit_id += 1;
        id
    }

    fn syntax_error_pos(&self, position: usize, message: String) -> SyntaxError {
        SyntaxError::new(self.line_number, position, message)
    }

    fn syntax_error(&self, message: String) -> SyntaxError {
        SyntaxError::new(self.line_number, self.line_start_position, message)
    }

    fn add_statement(&mut self, statement: Statement) {
        self.statements.last_mut().expect("BUG").push(statement);
    }

    // Assign Variable
    fn parse_assign(
        &mut self,
        name: &str,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        if matches!(self.end_program_state, EndProgramState::Satisfied) {
            return Err(self.syntax_error("invalid Code statement".into()));
        }

        if self.in_call_with {
            return self.parse_assign_argument(name, pos_and_tokens);
        }

        if self.is_select_head {
            return Err(self.syntax_error("invalid Code statement".into()));
        }

        if !self.header_state.can_command() {
            return Err(self.syntax_error("invalid Code statement".into()));
        } else {
            self.header_state = HeaderState::NotHeader;
        }

        let var_type = self
            .variables
            .get(name)
            .cloned()
            .ok_or_else(|| self.syntax_error(format!("undefined variable: {}", name)))?;

        // var = expr
        // var += expr
        // var -= expr
        // array(expr) = expr
        // array(expr) += expr
        // array(expr) -= expr
        // string(expr) = expr

        match pos_and_tokens {
            // 代入
            [(_, Token::Operator(Operator::Equal)), rest @ ..]
                if matches!(
                    var_type,
                    VarType::Boolean
                        | VarType::Integer
                        | VarType::String
                        | VarType::RefBoolean
                        | VarType::RefInteger
                        | VarType::RefString
                ) =>
            {
                let expr = self.parse_expr(rest)?;
                match expr.return_type() {
                    ExprType::Boolean if matches!(var_type, VarType::Boolean) => {
                        self.add_statement(Statement::AssignBoolean {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    ExprType::Boolean if matches!(var_type, VarType::RefBoolean) => {
                        self.add_statement(Statement::AssignRefBoolean {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    ExprType::Integer if matches!(var_type, VarType::Integer) => {
                        self.add_statement(Statement::AssignInteger {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    ExprType::Integer if matches!(var_type, VarType::RefInteger) => {
                        self.add_statement(Statement::AssignRefInteger {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    ExprType::String if matches!(var_type, VarType::String) => {
                        self.add_statement(Statement::AssignString {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    ExprType::String if matches!(var_type, VarType::RefString) => {
                        self.add_statement(Statement::AssignRefString {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    _ => return Err(self.syntax_error("invalid Assign statement".into())),
                }
            }
            // 加算代入
            [(_, Token::Operator(Operator::AddInto)), rest @ ..]
                if matches!(var_type, VarType::Integer) =>
            {
                let expr = self.parse_expr(rest)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    self.add_statement(Statement::AssignAddInto {
                        var_name: name.into(),
                        value: expr,
                    });
                } else {
                    return Err(self.syntax_error("invalid Assign statement".into()));
                }
            }
            // 加算代入(左辺参照型)
            [(_, Token::Operator(Operator::AddInto)), rest @ ..]
                if matches!(var_type, VarType::RefInteger) =>
            {
                let expr = self.parse_expr(rest)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    self.add_statement(Statement::AssignRefAddInto {
                        var_name: name.into(),
                        value: expr,
                    });
                } else {
                    return Err(self.syntax_error("invalid Assign statement".into()));
                }
            }
            // 減算代入
            [(_, Token::Operator(Operator::SubInto)), rest @ ..]
                if matches!(var_type, VarType::Integer) =>
            {
                let expr = self.parse_expr(rest)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    self.add_statement(Statement::AssignSubInto {
                        var_name: name.into(),
                        value: expr,
                    });
                } else {
                    return Err(self.syntax_error("invalid Assign statement".into()));
                }
            }
            // 減算代入(左辺参照型)
            [(_, Token::Operator(Operator::SubInto)), rest @ ..]
                if matches!(var_type, VarType::RefInteger) =>
            {
                let expr = self.parse_expr(rest)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    self.add_statement(Statement::AssignRefSubInto {
                        var_name: name.into(),
                        value: expr,
                    });
                } else {
                    return Err(self.syntax_error("invalid Assign statement".into()));
                }
            }
            // 配列要素または文字列要素に代入
            [(_, Token::Operator(Operator::OpenBracket)), ..]
                if matches!(
                    var_type,
                    VarType::String
                        | VarType::ArrayOfBoolean(_)
                        | VarType::ArrayOfInteger(_)
                        | VarType::RefString
                        | VarType::RefArrayOfBoolean(_)
                        | VarType::RefArrayOfInteger(_)
                ) =>
            {
                return self.parse_assign_element(name, var_type, pos_and_tokens);
            }
            _ => {
                return Err(self.syntax_error("invalid Assign statement".into()));
            }
        }

        Ok(())
    }

    // Assign Elment of Array
    fn parse_assign_element(
        &mut self,
        name: &str,
        var_type: VarType,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        // array(expr) = expr
        // array(expr) += expr
        // array(expr) -= expr
        // string(expr) = expr

        let mut close_pos: Option<usize> = None;
        let mut bracket_count = 1;
        for (i, (_, token)) in pos_and_tokens.iter().enumerate().skip(1) {
            match token {
                Token::Operator(Operator::OpenBracket) => bracket_count += 1,
                Token::Operator(Operator::CloseBracket) => bracket_count -= 1,
                _ => {}
            }
            if bracket_count == 0 {
                close_pos = Some(i);
                break;
            }
        }
        let close_pos = close_pos
            .take()
            .ok_or_else(|| self.syntax_error("invalid Assign statement".into()))?;

        let (param, value) = pos_and_tokens.split_at(close_pos + 1);

        let param = if let [_, inner @ .., _] = param {
            self.parse_expr(inner)?
        } else {
            return Err(self.syntax_error("invalid Assign statement".into()));
        };
        if !matches!(param.return_type(), ExprType::Integer) {
            return Err(self.syntax_error("invalid Assign statement".into()));
        }

        match value {
            // 要素への代入
            [(_, Token::Operator(Operator::Equal)), rest @ ..] => {
                let expr = self.parse_expr(rest)?;
                match expr.return_type() {
                    ExprType::Boolean if matches!(var_type, VarType::ArrayOfBoolean(_)) => {
                        self.add_statement(Statement::AssignBooleanElement {
                            var_name: name.into(),
                            index: param,
                            value: expr,
                        });
                    }
                    ExprType::Boolean if matches!(var_type, VarType::RefArrayOfBoolean(_)) => {
                        self.add_statement(Statement::AssignRefBooleanElement {
                            var_name: name.into(),
                            index: param,
                            value: expr,
                        });
                    }
                    ExprType::Integer if matches!(var_type, VarType::ArrayOfInteger(_)) => {
                        self.add_statement(Statement::AssignIntegerElement {
                            var_name: name.into(),
                            index: param,
                            value: expr,
                        });
                    }
                    ExprType::Integer if matches!(var_type, VarType::RefArrayOfInteger(_)) => {
                        self.add_statement(Statement::AssignRefIntegerElement {
                            var_name: name.into(),
                            index: param,
                            value: expr,
                        });
                    }
                    ExprType::Integer if matches!(var_type, VarType::String) => {
                        self.add_statement(Statement::AssignCharacterElement {
                            var_name: name.into(),
                            index: param,
                            value: expr,
                        });
                    }
                    ExprType::Integer if matches!(var_type, VarType::RefString) => {
                        self.add_statement(Statement::AssignRefCharacterElement {
                            var_name: name.into(),
                            index: param,
                            value: expr,
                        });
                    }
                    _ => return Err(self.syntax_error("invalid Assign statement".into())),
                }
            }
            // 要素への加算代入
            [(_, Token::Operator(Operator::AddInto)), rest @ ..]
                if matches!(var_type, VarType::ArrayOfInteger(_)) =>
            {
                let expr = self.parse_expr(rest)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    self.add_statement(Statement::AssignAddIntoElement {
                        var_name: name.into(),
                        index: param,
                        value: expr,
                    });
                } else {
                    return Err(self.syntax_error("invalid Assign statement".into()));
                }
            }
            // 要素への加算代入(左辺参照型)
            [(_, Token::Operator(Operator::AddInto)), rest @ ..]
                if matches!(var_type, VarType::RefArrayOfInteger(_)) =>
            {
                let expr = self.parse_expr(rest)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    self.add_statement(Statement::AssignRefAddIntoElement {
                        var_name: name.into(),
                        index: param,
                        value: expr,
                    });
                } else {
                    return Err(self.syntax_error("invalid Assign statement".into()));
                }
            }
            // 要素への減算代入
            [(_, Token::Operator(Operator::SubInto)), rest @ ..]
                if matches!(var_type, VarType::ArrayOfInteger(_)) =>
            {
                let expr = self.parse_expr(rest)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    self.add_statement(Statement::AssignSubIntoElement {
                        var_name: name.into(),
                        index: param,
                        value: expr,
                    });
                } else {
                    return Err(self.syntax_error("invalid Assign statement".into()));
                }
            }
            // 要素への減算代入(左辺参照型)
            [(_, Token::Operator(Operator::SubInto)), rest @ ..]
                if matches!(var_type, VarType::RefArrayOfInteger(_)) =>
            {
                let expr = self.parse_expr(rest)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    self.add_statement(Statement::AssignRefSubIntoElement {
                        var_name: name.into(),
                        index: param,
                        value: expr,
                    });
                } else {
                    return Err(self.syntax_error("invalid Assign statement".into()));
                }
            }
            _ => return Err(self.syntax_error("invalid Assign statement".into())),
        }

        Ok(())
    }

    // Assign Argument
    fn parse_assign_argument(
        &mut self,
        name: &str,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        assert!(self.in_call_with);

        for (arg_name, _) in self.temp_call_with_arguments.iter() {
            if arg_name == name {
                return Err(self.syntax_error(format!("duplicate Call argument name: {}", name)));
            }
        }

        let value = if let [(_, Token::Operator(Operator::Equal)), rest @ ..] = pos_and_tokens {
            self.parse_expr(rest)?
        } else {
            return Err(self.syntax_error("invalid Call argument statement".into()));
        };

        if let Some(arg) = self
            .callables
            .get(self.temp_progam_name.as_ref().expect("BUG"))
            .expect("BUG")
            .iter()
            .find(|arg| arg.var_name == name)
        {
            if !arg.is_valid_type(&value) {
                return Err(self.syntax_error(format!("invalid Call argument: {} {}", name, value)));
            }
        } else {
            return Err(self.syntax_error(format!("invalid Call argument name: {}", name)));
        }

        self.temp_call_with_arguments
            .push((name.to_string(), value));

        Ok(())
    }

    // Command
    fn parse_command(
        &mut self,
        command: &Keyword,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        if matches!(self.end_program_state, EndProgramState::Satisfied) {
            return Err(self.syntax_error("invalid Code statement".into()));
        }

        if self.header_state.in_header() {
            match command {
                Keyword::Argument => return self.parse_command_argument(pos_and_tokens),
                Keyword::Dim => return self.parse_command_dim(pos_and_tokens),
                Keyword::Extern => return self.parse_command_extern_sub(pos_and_tokens),
                Keyword::Program => return self.parse_command_program_name(pos_and_tokens),
                _ => {}
            }
        }

        if self.header_state.in_defininition() {
            return match command {
                Keyword::ByRef => self.parse_command_byref(pos_and_tokens),
                Keyword::ByVal => self.parse_command_byval(pos_and_tokens),
                Keyword::End => self.parse_command_end(pos_and_tokens),
                _ => Err(self.syntax_error(format!("invalid {:?} statement", command))),
            };
        }

        if !self.header_state.can_command() {
            return Err(self.syntax_error(format!("invalid {:?} statement", command)));
        } else if self.header_state.in_header() {
            if let Some(name) = self.temp_progam_name.take() {
                self.callables.insert(name, Vec::new());
            }
            self.header_state = HeaderState::NotHeader;
        }

        if self.in_call_with {
            return if let Keyword::End = command {
                self.parse_command_end(pos_and_tokens)
            } else {
                Err(self.syntax_error(format!("invalid {:?} statement", command)))
            };
        }

        if self.is_select_head {
            return match command {
                Keyword::Case => self.parse_command_case(pos_and_tokens),
                Keyword::End => self.parse_command_end(pos_and_tokens),
                _ => Err(self.syntax_error(format!("invalid {:?} statement", command))),
            };
        }

        match command {
            Keyword::Call => self.parse_command_call_extern_sub(pos_and_tokens),
            Keyword::Case => self.parse_command_case(pos_and_tokens),
            Keyword::Continue => self.parse_command_continue(pos_and_tokens),
            Keyword::Do => self.parse_command_do(pos_and_tokens),
            Keyword::Else => self.parse_command_else(pos_and_tokens),
            Keyword::ElseIf => self.parse_command_elseif(pos_and_tokens),
            Keyword::End => self.parse_command_end(pos_and_tokens),
            Keyword::Exit => self.parse_command_exit(pos_and_tokens),
            Keyword::For => self.parse_command_for(pos_and_tokens),
            Keyword::If => self.parse_command_if(pos_and_tokens),
            Keyword::Input => self.parse_command_input(pos_and_tokens),
            Keyword::Loop => self.parse_command_loop(pos_and_tokens),
            Keyword::Mid => self.parse_command_mid(pos_and_tokens),
            Keyword::Next => self.parse_command_next(pos_and_tokens),
            Keyword::Print => self.parse_command_print(pos_and_tokens),
            Keyword::Select => self.parse_command_select(pos_and_tokens),

            Keyword::Argument
            | Keyword::ByRef
            | Keyword::ByVal
            | Keyword::Dim
            | Keyword::Extern
            | Keyword::Program => {
                Err(self.syntax_error(format!("invalid {:?} statement", command)))
            }

            Keyword::As
            | Keyword::From
            | Keyword::Rem
            | Keyword::Step
            | Keyword::Sub
            | Keyword::Then
            | Keyword::To
            | Keyword::Until
            | Keyword::With
            | Keyword::While => unreachable!("BUG"),
        }
    }

    // Call <name>
    // Call <name> (<arguments>, ..)
    // Call <name> With
    fn parse_command_call_extern_sub(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [(pn, Token::Name(name))] => {
                if let Some(args) = self.callables.get(name) {
                    if !args.is_empty() {
                        return Err(self.syntax_error_pos(*pn, "invalid Call arguments".into()));
                    }
                } else {
                    return Err(self.syntax_error_pos(*pn, format!("invalid Call name: {}", name)));
                }
                self.add_statement(Statement::Call {
                    name: name.clone(),
                    arguments: Vec::new(),
                });
            }
            [(pn, Token::Name(name)), (_, Token::Keyword(Keyword::With))] => {
                if !self.callables.contains_key(name) {
                    return Err(self.syntax_error_pos(*pn, format!("invalid Call name: {}", name)));
                }
                assert!(self.temp_progam_name.is_none());
                assert!(self.temp_call_with_arguments.is_empty());
                assert!(!self.in_call_with);
                self.temp_progam_name = Some(name.clone());
                self.in_call_with = true;
            }
            [(pn, Token::Name(name)), rest @ ..] => {
                let param = self.parse_expr(rest)?;
                let arguments = if let Some(args) = self.callables.get(name) {
                    if let Expr::ParamList(list) = param {
                        if list.len() != args.len() {
                            return Err(self.syntax_error("invalid Call arguments".into()));
                        }
                        let mut arguments = Vec::with_capacity(list.len());
                        for (arg, expr) in args.iter().zip(list) {
                            if !arg.is_valid_type(&expr) {
                                return Err(self.syntax_error(format!(
                                    "invalid Call argument: {}, {}",
                                    arg.var_name, expr
                                )));
                            }
                            arguments.push((arg.var_name.clone(), expr));
                        }
                        arguments
                    } else if args.len() == 1 {
                        let arg = args.first().unwrap();
                        if !arg.is_valid_type(&param) {
                            return Err(self.syntax_error(format!(
                                "invalid Call argument: {} {}",
                                arg.var_name, param
                            )));
                        }
                        vec![(arg.var_name.clone(), param)]
                    } else {
                        return Err(self.syntax_error_pos(*pn, "invalid Call arguments".into()));
                    }
                } else {
                    return Err(self.syntax_error_pos(*pn, format!("invalid Call name: {}", name)));
                };
                self.add_statement(Statement::Call {
                    name: name.clone(),
                    arguments,
                });
            }
            _ => return Err(self.syntax_error("invalid Call statement".into())),
        }
        Ok(())
    }

    // プログラム名の正当性チェック
    fn check_valid_program_name(&self, pn: usize, name: &str) -> Result<(), SyntaxError> {
        if !is_valid_program_name(name) {
            Err(self.syntax_error_pos(pn, format!("invalid Program Name: {}", name)))
        } else if self.callables.contains_key(name) {
            Err(self.syntax_error_pos(pn, format!("duplicate Program Name: {}", name)))
        } else {
            Ok(())
        }
    }

    // Program
    // Program <name>
    fn parse_command_program_name(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        if !self.header_state.can_program_name() {
            return Err(self.syntax_error("invalid Program statement".into()));
        }
        match pos_and_tokens {
            [] => {}
            [(pn, Token::Name(name))] => {
                self.check_valid_program_name(*pn, name)?;
                assert!(self.temp_progam_name.is_none());
                assert!(self.temp_argumets.is_empty());
                self.temp_progam_name = Some(name.clone());
                self.add_statement(Statement::ProgramName { name: name.clone() });
            }
            _ => return Err(self.syntax_error("invalid Program statement".into())),
        }
        self.header_state = HeaderState::Argument;
        self.end_program_state = EndProgramState::Required;
        Ok(())
    }

    // Extern Sub <name>
    // Extern Sub <name> With
    fn parse_command_extern_sub(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        if !self.header_state.can_extern_sub() {
            return Err(self.syntax_error("invalid Extern statement".into()));
        }
        match pos_and_tokens {
            [(_, Token::Keyword(Keyword::Sub)), (pn, Token::Name(name)), (_, Token::Keyword(Keyword::With))] =>
            {
                self.check_valid_program_name(*pn, name)?;
                assert!(self.temp_argumets.is_empty(), "BUG");
                self.temp_progam_name = Some(name.clone());
                self.header_state = HeaderState::InExternSub;
            }
            [(_, Token::Keyword(Keyword::Sub)), (pn, Token::Name(name))] => {
                self.check_valid_program_name(*pn, name)?;
                self.callables.insert(name.clone(), Vec::new());
                self.add_statement(Statement::ExternSub {
                    name: name.clone(),
                    arguments: Vec::new(),
                });
            }
            _ => return Err(self.syntax_error("invalid Extern statement".into())),
        }
        Ok(())
    }

    // ByRef <name> As {Boolean / Integer} { From / To } <register>
    // ByRef <name> As String { From / To } <register>,<register>
    // ByRef <name>(<ubound>) As {Boolean / Integer} { From / To } <register>
    fn parse_command_byref(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        use Keyword::As;
        use Operator::{CloseBracket as Cb, Comma as Co, OpenBracket as Ob};
        use Token::{Integer as I, Keyword as K, Name as N, Operator as Op, TypeName as T};
        use TypeName as Tn;
        let ((pn, var_name), var_type, (pf, flow), (pr1, reg1), reg2) = match pos_and_tokens {
            [(pn, N(name)), (_, K(As)), (_, T(Tn::Boolean)), (pf, K(flow)), (pr, N(reg))] => {
                ((pn, name), VarType::RefBoolean, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, K(As)), (_, T(Tn::Integer)), (pf, K(flow)), (pr, N(reg))] => {
                ((pn, name), VarType::RefInteger, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, K(As)), (_, T(Tn::String)), (pf, K(flow)), (pr1, N(reg1)), (_, Op(Co)), (pr2, N(reg2))] =>
            {
                let reg2 = Some((pr2, reg2));
                let var_type = VarType::RefString;
                ((pn, name), var_type, (pf, flow), (pr1, reg1), reg2)
            }
            [(pn, N(name)), (_, Op(Ob)), (pu, I(ubound)), (_, Op(Cb)), (_, K(As)), (_, T(Tn::Boolean)), (pf, K(flow)), (pr, N(reg))] =>
            {
                if !(0..MAX_ARRAY_SIZE as i32).contains(ubound) {
                    return Err(
                        self.syntax_error_pos(*pu, "invalid array size in ByRef statement".into())
                    );
                }
                let var_type = VarType::RefArrayOfBoolean(*ubound as usize + 1);
                ((pn, name), var_type, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, Op(Ob)), (pu, I(ubound)), (_, Op(Cb)), (_, K(As)), (_, T(Tn::Integer)), (pf, K(flow)), (pr, N(reg))] =>
            {
                if !(0..MAX_ARRAY_SIZE as i32).contains(ubound) {
                    return Err(
                        self.syntax_error_pos(*pu, "invalid array size in ByRef statement".into())
                    );
                }
                let var_type = VarType::RefArrayOfInteger(*ubound as usize + 1);
                ((pn, name), var_type, (pf, flow), (pr, reg), None)
            }
            _ => return Err(self.syntax_error("invalid ByRef statement".into())),
        };

        if self
            .temp_argumets
            .iter()
            .any(|arg| arg.var_name.eq(var_name))
        {
            return Err(
                self.syntax_error_pos(*pn, "dupulicate argument name in ByRef statement".into())
            );
        }

        match flow {
            Keyword::From if self.header_state.in_argument() => {}
            Keyword::To if self.header_state.in_extern_sub() => {}
            _ => return Err(self.syntax_error_pos(*pf, "invalid ByRef statement".into())),
        }

        let register1 = IndexRegister::try_from(reg1.as_str())
            .map_err(|_| self.syntax_error_pos(*pr1, "invalid register name".into()))?;
        if self
            .temp_argumets
            .iter()
            .any(|arg| arg.register1 == register1 || arg.register2 == Some(register1))
        {
            return Err(self.syntax_error_pos(*pr1, "duplicate register name".into()));
        }

        let register2 = if let Some((pr2, reg2)) = reg2 {
            let register2 = IndexRegister::try_from(reg2.as_str())
                .map_err(|_| self.syntax_error_pos(*pr2, "invalid register name".into()))?;
            if self
                .temp_argumets
                .iter()
                .any(|arg| arg.register1 == register2 || arg.register2 == Some(register2))
                || register1 == register2
            {
                return Err(self.syntax_error_pos(*pr2, "duplicate register name".into()));
            }
            Some(register2)
        } else {
            None
        };

        let arg_info = ArgumentInfo {
            var_name: var_name.clone(),
            var_type,
            register1,
            register2,
        };

        self.temp_argumets.push(arg_info);

        Ok(())
    }

    // ByVal <name> As {Boolean / Integer} { From / To } <register>
    // ByVal <name> As String { From / To } <register>,<register>
    // ByVal <name>(<ubound>) As {Boolean / Integer} { From / To } <register>
    fn parse_command_byval(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        use Keyword::As;
        use Operator::{CloseBracket as Cb, Comma as Co, OpenBracket as Ob};
        use Token::{Integer as I, Keyword as K, Name as N, Operator as Op, TypeName as T};
        use TypeName as Tn;
        let ((pn, var_name), var_type, (pf, flow), (pr1, reg1), reg2) = match pos_and_tokens {
            [(pn, N(name)), (_, K(As)), (_, T(Tn::Boolean)), (pf, K(flow)), (pr, N(reg))] => {
                ((pn, name), VarType::Boolean, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, K(As)), (_, T(Tn::Integer)), (pf, K(flow)), (pr, N(reg))] => {
                ((pn, name), VarType::Integer, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, K(As)), (_, T(Tn::String)), (pf, K(flow)), (pr1, N(reg1)), (_, Op(Co)), (pr2, N(reg2))] =>
            {
                let reg2 = Some((pr2, reg2));
                let var_type = VarType::String;
                ((pn, name), var_type, (pf, flow), (pr1, reg1), reg2)
            }
            [(pn, N(name)), (_, Op(Ob)), (pu, I(ubound)), (_, Op(Cb)), (_, K(As)), (_, T(Tn::Boolean)), (pf, K(flow)), (pr, N(reg))] =>
            {
                if !(0..MAX_ARRAY_SIZE as i32).contains(ubound) {
                    return Err(
                        self.syntax_error_pos(*pu, "invalid array size in ByVal statement".into())
                    );
                }
                let var_type = VarType::ArrayOfBoolean(*ubound as usize + 1);
                ((pn, name), var_type, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, Op(Ob)), (pu, I(ubound)), (_, Op(Cb)), (_, K(As)), (_, T(Tn::Integer)), (pf, K(flow)), (pr, N(reg))] =>
            {
                if !(0..MAX_ARRAY_SIZE as i32).contains(ubound) {
                    return Err(
                        self.syntax_error_pos(*pu, "invalid array size in ByVal statement".into())
                    );
                }
                let var_type = VarType::ArrayOfInteger(*ubound as usize + 1);
                ((pn, name), var_type, (pf, flow), (pr, reg), None)
            }
            _ => return Err(self.syntax_error("invalid ByVal statement".into())),
        };

        if self
            .temp_argumets
            .iter()
            .any(|arg| arg.var_name.eq(var_name))
        {
            return Err(
                self.syntax_error_pos(*pn, "dupulicate argument name in ByVal statement".into())
            );
        }

        match flow {
            Keyword::From if self.header_state.in_argument() => {}
            Keyword::To if self.header_state.in_extern_sub() => {}
            _ => return Err(self.syntax_error_pos(*pf, "invalid ByRef statement".into())),
        }

        let register1 = IndexRegister::try_from(reg1.as_str())
            .map_err(|_| self.syntax_error_pos(*pr1, "invalid register name".into()))?;
        if self
            .temp_argumets
            .iter()
            .any(|arg| arg.register1 == register1 || arg.register2 == Some(register1))
        {
            return Err(self.syntax_error_pos(*pr1, "duplicate register name".into()));
        }

        let register2 = if let Some((pr2, reg2)) = reg2 {
            let register2 = IndexRegister::try_from(reg2.as_str())
                .map_err(|_| self.syntax_error_pos(*pr2, "invalid register name".into()))?;
            if self
                .temp_argumets
                .iter()
                .any(|arg| arg.register1 == register2 || arg.register2 == Some(register2))
                || register1 == register2
            {
                return Err(self.syntax_error_pos(*pr2, "duplicate register name".into()));
            }
            Some(register2)
        } else {
            None
        };

        let arg_info = ArgumentInfo {
            var_name: var_name.clone(),
            var_type,
            register1,
            register2,
        };

        self.temp_argumets.push(arg_info);

        Ok(())
    }

    // Argumentステートメント
    fn parse_command_argument(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        if !self.header_state.can_argument() {
            return Err(self.syntax_error("invalid Argument statement".into()));
        }

        if !pos_and_tokens.is_empty() {
            return Err(self.syntax_error("invalid Argument statement".into()));
        }

        self.header_state = HeaderState::InArgument;

        Ok(())
    }

    // Mid(<var_str>,<offset>) = <str_value>
    // Mid(<var_str>,<offset>,<length>) = <str_value>
    fn parse_command_mid(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        // Midパラメータの括弧
        let mut close_pos: Option<usize> = None;
        let mut bracket_count = 1;
        for (i, (_, token)) in pos_and_tokens.iter().enumerate().skip(1) {
            match token {
                Token::Operator(Operator::OpenBracket) => bracket_count += 1,
                Token::Operator(Operator::CloseBracket) => bracket_count -= 1,
                _ => {}
            }
            if bracket_count == 0 {
                close_pos = Some(i);
                break;
            }
        }
        let close_pos = close_pos
            .take()
            .ok_or_else(|| self.syntax_error("invalid Mid statement".into()))?;

        let (param, value) = pos_and_tokens.split_at(close_pos + 1);

        let param = if let [_, inner @ .., _] = param {
            self.parse_expr(inner)?
        } else {
            return Err(self.syntax_error("invalid Mid statement".into()));
        };

        let mut list = if let Expr::ParamList(list) = param {
            list
        } else {
            return Err(self.syntax_error("invalid Mid statement".into()));
        };

        list.reverse();

        let (var_name, var_is_ref) = match list.pop() {
            Some(Expr::VarString(var_name)) => (var_name, false),
            Some(Expr::VarRefString(var_name)) => (var_name, true),
            _ => return Err(self.syntax_error("invalid Mid statement".into())),
        };

        let offset = match list.pop() {
            Some(offset) if matches!(offset.return_type(), ExprType::Integer) => offset,
            _ => return Err(self.syntax_error("invalid Mid statement".into())),
        };

        let length = match list.pop() {
            None => None,
            Some(expr) if matches!(expr.return_type(), ExprType::Integer) && list.is_empty() => {
                Some(expr)
            }
            Some(_) => return Err(self.syntax_error("invalid Mid statement".into())),
        };

        // 要素への代入
        if let [(_, Token::Operator(Operator::Equal)), rest @ ..] = value {
            let value = self.parse_expr(rest)?;
            if matches!(value.return_type(), ExprType::String) {
                self.add_statement(Statement::Mid {
                    var_name,
                    var_is_ref,
                    offset,
                    length,
                    value,
                });
            } else {
                return Err(self.syntax_error("invalid Mid statement".into()));
            }
        } else {
            return Err(self.syntax_error("invalid Mid statement".into()));
        }

        Ok(())
    }

    // End { Argument / Call / If / Program / Select / Sub }
    fn parse_command_end(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [(_, Token::Keyword(Keyword::Argument))] => self.compose_command_argument(),
            [(_, Token::Keyword(Keyword::Call))] => self.compose_command_call_with(),
            [(_, Token::Keyword(Keyword::If))] => self.compose_command_if(),
            [(_, Token::Keyword(Keyword::Program))] => {
                if matches!(self.end_program_state, EndProgramState::Required) {
                    self.end_program_state = EndProgramState::Satisfied;
                    Ok(())
                } else {
                    Err(self.syntax_error("invalid End statement".into()))
                }
            }
            [(_, Token::Keyword(Keyword::Select))] => self.compose_command_select(),
            [(_, Token::Keyword(Keyword::Sub))] => self.compose_command_sub(),
            _ => Err(self.syntax_error("invalid End statement".into())),
        }
    }

    // End Call
    fn compose_command_call_with(&mut self) -> Result<(), SyntaxError> {
        if !self.in_call_with {
            return Err(self.syntax_error("Invalid End statement".into()));
        }

        let name = self.temp_progam_name.take().expect("BUG");
        let arguments = self.temp_call_with_arguments.split_off(0);

        if self.callables.get(&name).expect("BUG").len() != arguments.len() {
            return Err(self.syntax_error("invalid Call arguments".into()));
        }

        self.add_statement(Statement::Call { name, arguments });

        self.in_call_with = false;

        Ok(())
    }

    // End Argument
    fn compose_command_argument(&mut self) -> Result<(), SyntaxError> {
        if !self.header_state.in_argument() {
            return Err(self.syntax_error("Invalid End statement".into()));
        }

        let arguments = self.temp_argumets.split_off(0);

        for arg in arguments.iter() {
            self.variables.insert(arg.var_name.clone(), arg.var_type);
        }

        if let Some(name) = self.temp_progam_name.take() {
            self.callables.insert(name, arguments.clone());
        }

        self.add_statement(Statement::Argument { arguments });

        self.header_state = HeaderState::Dim;

        Ok(())
    }

    // End Sub
    fn compose_command_sub(&mut self) -> Result<(), SyntaxError> {
        if !self.header_state.in_extern_sub() {
            return Err(self.syntax_error("Invalid End statement".into()));
        }

        let name = if let Some(name) = self.temp_progam_name.take() {
            name
        } else {
            unreachable!("BUG");
        };

        let arguments = self.temp_argumets.split_off(0);

        self.callables.insert(name.clone(), arguments.clone());

        self.add_statement(Statement::ExternSub { name, arguments });

        self.header_state = HeaderState::ExternSub;

        Ok(())
    }

    // End If
    fn compose_command_if(&mut self) -> Result<(), SyntaxError> {
        let block = self.statements.pop().expect("BUG"); // 常に self.statements.len() > 0 なので

        match self.provisionals.pop() {
            Some(Statement::ProvitionalIf { condition }) => {
                self.add_statement(Statement::If {
                    condition,
                    block,
                    else_blocks: Vec::new(),
                });
            }
            Some(Statement::ProvisionalElseIf { condition }) => {
                let else_if_statement = Statement::ElseIf { condition, block };
                if let Some(Statement::If {
                    condition,
                    block,
                    mut else_blocks,
                }) = self.provisionals.pop()
                {
                    else_blocks.push(else_if_statement);
                    self.add_statement(Statement::If {
                        condition,
                        block,
                        else_blocks,
                    });
                } else {
                    unreachable!("BUG");
                }
            }
            Some(Statement::ProvisionalElse) => {
                let else_statement = Statement::Else { block };
                if let Some(Statement::If {
                    condition,
                    block,
                    mut else_blocks,
                }) = self.provisionals.pop()
                {
                    else_blocks.push(else_statement);
                    self.add_statement(Statement::If {
                        condition,
                        block,
                        else_blocks,
                    });
                } else {
                    unreachable!("BUG");
                }
            }
            _ => return Err(self.syntax_error("invalid End statement".into())),
        }

        Ok(())
    }

    // End Select
    fn compose_command_select(&mut self) -> Result<(), SyntaxError> {
        let cur_exit_id = self
            .nest_of_select
            .pop()
            .ok_or_else(|| self.syntax_error("invalid End statement".into()))?;

        match self.provisionals.pop() {
            Some(Statement::ProvisionalSelectInteger { exit_id, value }) => {
                assert!(self.is_select_head, "BUG");
                assert!(exit_id == cur_exit_id, "BUG");
                self.add_statement(Statement::SelectInteger {
                    exit_id,
                    value,
                    case_blocks: Vec::new(),
                });
                self.is_select_head = false;
            }
            Some(Statement::ProvisionalSelectString { exit_id, value }) => {
                assert!(self.is_select_head, "BUG");
                assert!(exit_id == cur_exit_id, "BUG");
                self.add_statement(Statement::SelectString {
                    exit_id,
                    value,
                    case_blocks: Vec::new(),
                });
                self.is_select_head = false;
            }
            Some(Statement::ProvisionalCaseInteger { values }) => {
                assert!(!self.is_select_head, "BUG");
                let block = self.statements.pop().expect("BUG");
                if let Some(Statement::SelectInteger {
                    exit_id,
                    value,
                    mut case_blocks,
                }) = self.provisionals.pop()
                {
                    assert!(exit_id == cur_exit_id, "BUG");
                    case_blocks.push(Statement::CaseInteger { values, block });
                    self.add_statement(Statement::SelectInteger {
                        exit_id,
                        value,
                        case_blocks,
                    });
                } else {
                    unreachable!("BUG");
                }
            }
            Some(Statement::ProvisionalCaseString { values }) => {
                assert!(!self.is_select_head, "BUG");
                let block = self.statements.pop().expect("BUG");
                if let Some(Statement::SelectString {
                    exit_id,
                    value,
                    mut case_blocks,
                }) = self.provisionals.pop()
                {
                    assert!(exit_id == cur_exit_id, "BUG");
                    case_blocks.push(Statement::CaseString { values, block });
                    self.add_statement(Statement::SelectString {
                        exit_id,
                        value,
                        case_blocks,
                    });
                } else {
                    unreachable!("BUG");
                }
            }
            Some(Statement::ProvisionalCaseElse) => {
                assert!(!self.is_select_head, "BUG");
                let block = self.statements.pop().expect("BUG");
                match self.provisionals.pop() {
                    Some(Statement::SelectInteger {
                        exit_id,
                        value,
                        mut case_blocks,
                    }) => {
                        assert!(exit_id == cur_exit_id, "BUG");
                        case_blocks.push(Statement::CaseElse { block });
                        self.add_statement(Statement::SelectInteger {
                            exit_id,
                            value,
                            case_blocks,
                        });
                    }
                    Some(Statement::SelectString {
                        exit_id,
                        value,
                        mut case_blocks,
                    }) => {
                        assert!(exit_id == cur_exit_id, "BUG");
                        case_blocks.push(Statement::CaseElse { block });
                        self.add_statement(Statement::SelectString {
                            exit_id,
                            value,
                            case_blocks,
                        });
                    }
                    _ => unreachable!("BUG"),
                }
            }
            _ => return Err(self.syntax_error("invalid End statement".into())),
        }

        Ok(())
    }

    // Else
    fn parse_command_else(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [] => {}
            [(_, Token::Keyword(Keyword::If)), rest @ ..] => {
                return self.parse_command_elseif(rest)
            }
            _ => return Err(self.syntax_error("invalid Else statement".into())),
        }

        let block = self.statements.pop().expect("BUG"); // 常に self.statements.len() > 0 なので

        match self.provisionals.pop() {
            Some(Statement::ProvitionalIf { condition }) => {
                self.provisionals.push(Statement::If {
                    condition,
                    block,
                    else_blocks: Vec::new(),
                });
            }
            Some(Statement::ProvisionalElseIf { condition }) => {
                if let Some(Statement::If { else_blocks, .. }) = self.provisionals.last_mut() {
                    else_blocks.push(Statement::ElseIf { condition, block });
                } else {
                    unreachable!("BUG");
                }
            }
            _ => return Err(self.syntax_error("invalid Else statement".into())),
        }

        self.provisionals.push(Statement::ProvisionalElse);
        self.statements.push(Vec::new());

        Ok(())
    }

    // ElseIf <condition> Then
    fn parse_command_elseif(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        let block = self.statements.pop().expect("BUG"); // 常に self.statements.len() > 0 なので

        match self.provisionals.pop() {
            Some(Statement::ProvitionalIf { condition }) => {
                self.provisionals.push(Statement::If {
                    condition,
                    block,
                    else_blocks: Vec::new(),
                });
            }
            Some(Statement::ProvisionalElseIf { condition }) => {
                if let Some(Statement::If { else_blocks, .. }) = self.provisionals.last_mut() {
                    else_blocks.push(Statement::ElseIf { condition, block });
                } else {
                    unreachable!("BUG");
                }
            }
            _ => return Err(self.syntax_error("invalid ElseIf statement".into())),
        }

        let condition = if let [rest @ .., (_, Token::Keyword(Keyword::Then))] = pos_and_tokens {
            self.parse_expr(rest)?
        } else {
            return Err(self.syntax_error("invalid ElseIf statement".into()));
        };

        self.provisionals
            .push(Statement::ProvisionalElseIf { condition });
        self.statements.push(Vec::new());

        Ok(())
    }

    // If <condition> Then
    fn parse_command_if(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        let condition = if let [rest @ .., (_, Token::Keyword(Keyword::Then))] = pos_and_tokens {
            self.parse_expr(rest)?
        } else {
            return Err(self.syntax_error("invalid If statement".into()));
        };

        if !matches!(condition.return_type(), ExprType::Boolean) {
            return Err(self.syntax_error("invalid If statement".into()));
        }

        self.provisionals
            .push(Statement::ProvitionalIf { condition });
        self.statements.push(Vec::new());

        Ok(())
    }

    // Exit { Do / For / Program / Select }
    fn parse_command_exit(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [(_, Token::Keyword(Keyword::Do))] => {
                if let Some(&exit_id) = self.nest_of_do.last() {
                    self.add_statement(Statement::ExitDo { exit_id });
                } else {
                    return Err(self.syntax_error("invalid Exit statement".into()));
                }
            }
            [(_, Token::Keyword(Keyword::For))] => {
                if let Some((exit_id, _)) = self.nest_of_for.last() {
                    let exit_id = *exit_id;
                    self.add_statement(Statement::ExitFor { exit_id });
                } else {
                    return Err(self.syntax_error("invalid Exit statement".into()));
                }
            }
            [(_, Token::Keyword(Keyword::Program))] => self.add_statement(Statement::ExitProgram),
            [(_, Token::Keyword(Keyword::Select))] => {
                if let Some(&exit_id) = self.nest_of_select.last() {
                    self.add_statement(Statement::ExitSelect { exit_id });
                } else {
                    return Err(self.syntax_error("invalid Exit statement".into()));
                }
            }
            _ => return Err(self.syntax_error("invalid Exit statement".into())),
        }

        Ok(())
    }

    // Case <integer/character> [, <integer/character>]*
    // Case <string> [, <string>]*
    // Case Else
    fn parse_command_case(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        let select_type = match self.provisionals.pop() {
            Some(Statement::ProvisionalSelectInteger { exit_id, value }) => {
                assert!(self.is_select_head, "BUG");
                self.provisionals.push(Statement::SelectInteger {
                    exit_id,
                    value,
                    case_blocks: vec![],
                });
                self.is_select_head = false;
                ExprType::Integer
            }
            Some(Statement::ProvisionalSelectString { exit_id, value }) => {
                assert!(self.is_select_head, "BUG");
                self.provisionals.push(Statement::SelectString {
                    exit_id,
                    value,
                    case_blocks: vec![],
                });
                self.is_select_head = false;
                ExprType::String
            }
            Some(Statement::ProvisionalCaseInteger { values }) => {
                assert!(!self.is_select_head, "BUG");
                let block = self.statements.pop().expect("BUG");
                if let Some(Statement::SelectInteger { case_blocks, .. }) =
                    self.provisionals.last_mut()
                {
                    case_blocks.push(Statement::CaseInteger { values, block });
                } else {
                    unreachable!("BUG");
                }
                ExprType::Integer
            }
            Some(Statement::ProvisionalCaseString { values }) => {
                assert!(!self.is_select_head, "BUG");
                let block = self.statements.pop().expect("BUG");
                if let Some(Statement::SelectString { case_blocks, .. }) =
                    self.provisionals.last_mut()
                {
                    case_blocks.push(Statement::CaseString { values, block });
                } else {
                    unreachable!("BUG");
                }
                ExprType::String
            }
            _ => return Err(self.syntax_error("invalid Case statement".into())),
        };

        if matches!(pos_and_tokens, [(_, Token::Keyword(Keyword::Else))]) {
            self.provisionals.push(Statement::ProvisionalCaseElse);
            self.statements.push(Vec::new());
            return Ok(());
        }

        match self.parse_expr(pos_and_tokens)? {
            Expr::LitCharacter(value) => {
                if let Some(Statement::SelectInteger { case_blocks, .. }) = self.provisionals.last()
                {
                    let found_duplicate = case_blocks.iter().any(|s| {
                        if let Statement::CaseInteger { values, .. } = s {
                            values.iter().any(|v| {
                                if let CaseIntegerItem::Character(v) = v {
                                    *v == value
                                } else {
                                    false
                                }
                            })
                        } else {
                            unreachable!("BUG");
                        }
                    });
                    if found_duplicate {
                        return Err(self.syntax_error("invalid Case statement".into()));
                    }
                } else {
                    unreachable!("BUG");
                }
                self.provisionals.push(Statement::ProvisionalCaseInteger {
                    values: vec![CaseIntegerItem::Character(value)],
                });
            }
            Expr::LitInteger(value) => {
                if let Some(Statement::SelectInteger { case_blocks, .. }) = self.provisionals.last()
                {
                    let found_duplicate = case_blocks.iter().any(|s| {
                        if let Statement::CaseInteger { values, .. } = s {
                            values.iter().any(|v| {
                                if let CaseIntegerItem::Integer(v) = v {
                                    *v == value
                                } else {
                                    false
                                }
                            })
                        } else {
                            unreachable!("BUG");
                        }
                    });
                    if found_duplicate {
                        return Err(self.syntax_error("invalid Case statement".into()));
                    }
                } else {
                    unreachable!("BUG");
                }
                self.provisionals.push(Statement::ProvisionalCaseInteger {
                    values: vec![CaseIntegerItem::Integer(value)],
                });
            }
            Expr::LitString(value) => {
                if let Some(Statement::SelectString { case_blocks, .. }) = self.provisionals.last()
                {
                    let found_duplicate = case_blocks.iter().any(|s| {
                        if let Statement::CaseString { values, .. } = s {
                            values.iter().any(|v| v == &value)
                        } else {
                            unreachable!("BUG");
                        }
                    });
                    if found_duplicate {
                        return Err(self.syntax_error("invalid Case statement".into()));
                    }
                } else {
                    unreachable!("BUG");
                }
                self.provisionals.push(Statement::ProvisionalCaseString {
                    values: vec![value],
                });
            }
            Expr::ParamList(values) if matches!(select_type, ExprType::Integer) => {
                assert!(values.len() > 1, "BUG");
                let values = values
                    .into_iter()
                    .try_fold(vec![], |mut acc, expr| match expr {
                        Expr::LitInteger(value) => {
                            acc.push(CaseIntegerItem::Integer(value));
                            Ok(acc)
                        }
                        Expr::LitCharacter(value) => {
                            acc.push(CaseIntegerItem::Character(value));
                            Ok(acc)
                        }
                        _ => Err(self.syntax_error("invalid Case statement".into())),
                    })?;
                if let Some(Statement::SelectInteger { case_blocks, .. }) = self.provisionals.last()
                {
                    let found_duplicate = values.iter().any(|value| {
                        case_blocks.iter().any(|s| {
                            if let Statement::CaseInteger { values, .. } = s {
                                values.iter().any(|v| v == value)
                            } else {
                                unreachable!("BUG");
                            }
                        })
                    });
                    if found_duplicate {
                        return Err(self.syntax_error("invalid Case statement".into()));
                    }
                } else {
                    unreachable!("BUG");
                }
                self.provisionals
                    .push(Statement::ProvisionalCaseInteger { values });
            }
            Expr::ParamList(values) if matches!(select_type, ExprType::String) => {
                assert!(values.len() > 1, "BUG");
                let values = values.into_iter().try_fold(vec![], |mut acc, expr| {
                    if let Expr::LitString(value) = expr {
                        acc.push(value);
                        Ok(acc)
                    } else {
                        Err(self.syntax_error("invalid Case statement".into()))
                    }
                })?;
                if let Some(Statement::SelectString { case_blocks, .. }) = self.provisionals.last()
                {
                    let found_duplicate = values.iter().any(|value| {
                        case_blocks.iter().any(|s| {
                            if let Statement::CaseString { values, .. } = s {
                                values.iter().any(|v| v == value)
                            } else {
                                unreachable!("BUG");
                            }
                        })
                    });
                    if found_duplicate {
                        return Err(self.syntax_error("invalid Case statement".into()));
                    }
                } else {
                    unreachable!("BUG");
                }
                self.provisionals
                    .push(Statement::ProvisionalCaseString { values });
            }
            _ => return Err(self.syntax_error("invalid Case statement".into())),
        }

        self.statements.push(Vec::new());

        Ok(())
    }

    // Select Case <expr>
    fn parse_command_select(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        let exit_id = self.get_new_exit_id();

        let statement = match pos_and_tokens {
            [(_, Token::Keyword(Keyword::Case)), rest @ ..] | rest @ [_, ..] => {
                let value = self.parse_expr(rest)?;
                match value.return_type() {
                    ExprType::Integer => Statement::ProvisionalSelectInteger { exit_id, value },
                    ExprType::String => Statement::ProvisionalSelectString { exit_id, value },
                    _ => return Err(self.syntax_error("invalid Select statement".into())),
                }
            }
            [] => return Err(self.syntax_error("invalid Select statement".into())),
        };

        self.is_select_head = true;
        self.nest_of_select.push(exit_id);
        self.provisionals.push(statement);

        Ok(())
    }

    // Continue { Do / For }
    fn parse_command_continue(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [(_, Token::Keyword(Keyword::Do))] => {
                if let Some(&exit_id) = self.nest_of_do.last() {
                    self.add_statement(Statement::ContinueDo { exit_id });
                } else {
                    return Err(self.syntax_error("invalid Continue statement".into()));
                }
            }
            [(_, Token::Keyword(Keyword::For))] => {
                if let Some((exit_id, _)) = self.nest_of_for.last() {
                    let exit_id = *exit_id;
                    self.add_statement(Statement::ContinueFor { exit_id });
                } else {
                    return Err(self.syntax_error("invalid Continue statement".into()));
                }
            }
            _ => return Err(self.syntax_error("invalid Continue statement".into())),
        }

        Ok(())
    }

    // Loop [{ While / Until } <condition>]
    fn parse_command_loop(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        let cur_exit_id = self
            .nest_of_do
            .pop()
            .ok_or_else(|| self.syntax_error("invalid Loop statement".into()))?;

        let block = self.statements.pop().expect("BUG");

        if let Some(Statement::ProvisionalDo {
            exit_id,
            until_condition,
            while_condition,
        }) = self.provisionals.pop()
        {
            assert!(exit_id == cur_exit_id, "BUG");
            assert!(
                until_condition.is_none() || while_condition.is_none(),
                "BUG"
            );
            match pos_and_tokens {
                [] => {
                    if let Some(condition) = until_condition {
                        self.add_statement(Statement::DoUntilLoop {
                            exit_id,
                            condition,
                            block,
                        });
                    } else if let Some(condition) = while_condition {
                        self.add_statement(Statement::DoWhileLoop {
                            exit_id,
                            condition,
                            block,
                        });
                    } else {
                        self.add_statement(Statement::DoLoop { exit_id, block });
                    }
                }
                _ if until_condition.or(while_condition).is_some() => {
                    return Err(self.syntax_error("invalid Loop statement".into()))
                }
                [(pos, Token::Keyword(Keyword::Until)), rest @ ..] => {
                    let condition = self.parse_expr(rest)?;
                    if !matches!(condition.return_type(), ExprType::Boolean) {
                        return Err(self.syntax_error_pos(*pos, "invalid Loop statement".into()));
                    }
                    self.add_statement(Statement::DoLoopUntil {
                        exit_id,
                        condition,
                        block,
                    });
                }
                [(pos, Token::Keyword(Keyword::While)), rest @ ..] => {
                    let condition = self.parse_expr(rest)?;
                    if !matches!(condition.return_type(), ExprType::Boolean) {
                        return Err(self.syntax_error_pos(*pos, "invalid Loop statement".into()));
                    }
                    self.add_statement(Statement::DoLoopWhile {
                        exit_id,
                        condition,
                        block,
                    });
                }
                _ => return Err(self.syntax_error("invalid Loop statement".into())),
            }
        } else {
            return Err(self.syntax_error("invalid Loop statement".into()));
        }

        Ok(())
    }

    // Do [{ While / Until } <condition>]
    fn parse_command_do(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        let exit_id = self.get_new_exit_id();

        let statement = match pos_and_tokens {
            [] => Statement::ProvisionalDo {
                exit_id,
                until_condition: None,
                while_condition: None,
            },
            [(pos, Token::Keyword(Keyword::Until)), rest @ ..] => {
                let condition = self.parse_expr(rest)?;
                if !matches!(condition.return_type(), ExprType::Boolean) {
                    return Err(self.syntax_error_pos(*pos, "invalid Do statement".into()));
                }
                Statement::ProvisionalDo {
                    exit_id,
                    until_condition: Some(condition),
                    while_condition: None,
                }
            }
            [(pos, Token::Keyword(Keyword::While)), rest @ ..] => {
                let condition = self.parse_expr(rest)?;
                if !matches!(condition.return_type(), ExprType::Boolean) {
                    return Err(self.syntax_error_pos(*pos, "invalid Do statement".into()));
                }
                Statement::ProvisionalDo {
                    exit_id,
                    until_condition: None,
                    while_condition: Some(condition),
                }
            }
            _ => return Err(self.syntax_error("invalid Do statement".into())),
        };

        self.nest_of_do.push(exit_id);
        self.statements.push(Vec::new());
        self.provisionals.push(statement);

        Ok(())
    }

    // Next [<counter>]
    fn parse_command_next(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        let name = match pos_and_tokens {
            [] => None,
            [(_, Token::Name(name))] => Some(name),
            _ => return Err(self.syntax_error("invalid Next statment".into())),
        };

        let (cur_exit_id, _) = self
            .nest_of_for
            .pop()
            .ok_or_else(|| self.syntax_error("invalid Next statement".into()))?;

        let block = self.statements.pop().expect("BUG");

        if let Some(Statement::ProvisionalFor {
            exit_id,
            counter,
            counter_is_ref,
            init,
            end,
            step,
        }) = self.provisionals.pop()
        {
            assert!(exit_id == cur_exit_id, "BUG");
            if name.filter(|name| *name != &counter).is_some() {
                return Err(self.syntax_error("invalid Next statement".into()));
            }
            self.add_statement(Statement::For {
                exit_id,
                counter,
                counter_is_ref,
                init,
                end,
                step,
                block,
            });
        } else {
            return Err(self.syntax_error("invalid Next statement".into()));
        }

        Ok(())
    }

    // For <counter> = <init> To <end> [Step <step>]
    fn parse_command_for(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        let (name, rest) = if let [(_, Token::Name(name)), (_, Token::Operator(Operator::Equal)), rest @ ..] =
            pos_and_tokens
        {
            (name, rest)
        } else {
            return Err(self.syntax_error("invalid For statement".into()));
        };

        if self
            .nest_of_for
            .iter()
            .any(|(_, counter)| counter.as_str() == name.as_str())
        {
            return Err(self.syntax_error("invalid counter in For statement".into()));
        }

        let counter_is_ref = match self.variables.get(name) {
            Some(VarType::Integer) => false,
            Some(VarType::RefInteger) => true,
            _ => return Err(self.syntax_error("invalid For statement".into())),
        };

        let to_position = rest
            .iter()
            .enumerate()
            .find(|(_, (_, token))| matches!(token, Token::Keyword(Keyword::To)))
            .map(|(i, _)| i)
            .ok_or_else(|| self.syntax_error("invalid For statement".into()))?;

        let (init, rest) = rest.split_at(to_position);

        let step_position = rest
            .iter()
            .enumerate()
            .find(|(_, (_, token))| matches!(token, Token::Keyword(Keyword::Step)))
            .map_or(rest.len(), |(i, _)| i);

        let (end, step) = rest.split_at(step_position);

        let init = self.parse_expr(init)?;
        if !matches!(init.return_type(), ExprType::Integer) {
            return Err(self.syntax_error("invalid initial value in For statement".into()));
        }

        let end = self.parse_expr(&end[1..])?;
        if !matches!(end.return_type(), ExprType::Integer) {
            return Err(self.syntax_error("invalid end value in For statement".into()));
        }

        let step = if step.is_empty() {
            None
        } else {
            let step = self.parse_expr(&step[1..])?;
            if matches!(step.return_type(), ExprType::Integer) {
                Some(step)
            } else {
                return Err(self.syntax_error("invalid end value in For statement".into()));
            }
        };

        let exit_id = self.get_new_exit_id();
        self.nest_of_for.push((exit_id, name.clone()));
        self.statements.push(Vec::new());
        self.provisionals.push(Statement::ProvisionalFor {
            exit_id,
            counter: name.clone(),
            counter_is_ref,
            init,
            end,
            step,
        });

        Ok(())
    }

    // Dim <var_name> As { Boolean / Integer / String }
    // Dim <arr_name>(<ubound>) As { Boolean / Integer }
    fn parse_command_dim(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            // プリミティブ変数の宣言
            [(pos, Token::Name(name)), (_, Token::Keyword(Keyword::As)), (_, Token::TypeName(type_name))] =>
            {
                if self.variables.contains_key(name) {
                    return Err(
                        self.syntax_error_pos(*pos, format!("already defined variable: {}", name))
                    );
                }
                let var_type = match type_name {
                    TypeName::Boolean => VarType::Boolean,
                    TypeName::Integer => VarType::Integer,
                    TypeName::String => VarType::String,
                };
                self.variables.insert(name.clone(), var_type);
                self.add_statement(Statement::Dim {
                    var_name: name.clone(),
                    var_type,
                });
            }
            // 配列変数の宣言
            [(pos_n, Token::Name(name)), (_, Token::Operator(Operator::OpenBracket)), (pos_s, Token::Integer(size)), (_, Token::Operator(Operator::CloseBracket)), (_, Token::Keyword(Keyword::As)), (pos_t, Token::TypeName(type_name))] =>
            {
                if self.variables.contains_key(name) {
                    return Err(self
                        .syntax_error_pos(*pos_n, format!("already defined variable: {}", name)));
                }
                if !(0..MAX_ARRAY_SIZE as i32).contains(size) {
                    return Err(
                        self.syntax_error_pos(*pos_s, "invalid Array Size in Dim statement".into())
                    );
                }
                let size = *size as usize + 1;
                let var_type = match type_name {
                    TypeName::Boolean => VarType::ArrayOfBoolean(size),
                    TypeName::Integer => VarType::ArrayOfInteger(size),
                    TypeName::String => {
                        return Err(self.syntax_error_pos(
                            *pos_t,
                            "invalid Array Type in Dim statement".into(),
                        ))
                    }
                };
                self.variables.insert(name.clone(), var_type);
                self.add_statement(Statement::Dim {
                    var_name: name.clone(),
                    var_type,
                });
            }
            _ => return Err(self.syntax_error("invalid Dim statement".into())),
        }

        Ok(())
    }

    // Input { <var_name> / <arr_name>(<index>) }
    fn parse_command_input(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            // プリミティブ変数への入力
            [(pos, Token::Name(name))] => match self.variables.get(name) {
                Some(VarType::Integer) | Some(VarType::RefInteger) => {
                    self.add_statement(Statement::InputInteger {
                        var_name: name.clone(),
                    });
                }
                Some(VarType::String) | Some(VarType::RefString) => {
                    self.add_statement(Statement::InputString {
                        var_name: name.clone(),
                    });
                }
                Some(_) => {
                    return Err(
                        self.syntax_error_pos(*pos, "invalid Variable in Input statement".into())
                    )
                }
                None => {
                    return Err(self.syntax_error_pos(*pos, format!("undefined variable: {}", name)))
                }
            },
            // 整数配列の指定位置への入力
            [(pos, Token::Name(name)), (_, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                if !matches!(
                    self.variables.get(name),
                    Some(VarType::ArrayOfInteger(_)) | Some(VarType::RefArrayOfInteger(_))
                ) {
                    return Err(
                        self.syntax_error_pos(*pos, "invalid Variable in Input statement".into())
                    );
                }
                let param = self.parse_expr(inner)?;
                if !matches!(param.return_type(), ExprType::Integer) {
                    return Err(
                        self.syntax_error_pos(*pos, "invalid Variable in Input statement".into())
                    );
                }
                self.add_statement(Statement::InputElementInteger {
                    var_name: name.clone(),
                    index: param,
                });
            }
            _ => return Err(self.syntax_error("invalid Input statement".into())),
        }
        Ok(())
    }

    // Print [ <boolean> / <integer> / <string> / <var_name> / <expr> ]
    fn parse_command_print(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            // 空文字列の出力
            [] => {
                self.add_statement(Statement::PrintLitString { value: "".into() });
                return Ok(());
            }
            // 真理値リテラルの出力
            [(_, Token::Boolean(value))] => {
                self.add_statement(Statement::PrintLitBoolean { value: *value });
                return Ok(());
            }
            // 整数リテラルの出力
            [(pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(false, *value) {
                    self.add_statement(Statement::PrintLitInteger { value });
                    return Ok(());
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }
            // 負符号付きの整数リテラルの出力
            [(_, Token::Operator(Operator::Sub)), (pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(true, *value) {
                    self.add_statement(Statement::PrintLitInteger { value });
                    return Ok(());
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }
            // 文字列リテラルの出力
            [(_, Token::String(value))] => {
                self.add_statement(Statement::PrintLitString {
                    value: value.clone(),
                });
                return Ok(());
            }
            // プリミティブ変数の出力
            [(_, Token::Name(name))] => match self.variables.get(name) {
                Some(VarType::Boolean)
                | Some(VarType::RefBoolean)
                | Some(VarType::Integer)
                | Some(VarType::RefInteger)
                | Some(VarType::RefString) => {}
                Some(VarType::String) => {
                    self.add_statement(Statement::PrintVarString {
                        var_name: name.clone(),
                    });
                    return Ok(());
                }
                _ => return Err(self.syntax_error("invalid Argument in Print statement".into())),
            },
            _ => {}
        }
        // 計算結果の出力
        let expr = self.parse_expr(pos_and_tokens)?;
        match expr.return_type() {
            ExprType::Boolean => self.add_statement(Statement::PrintExprBoolan { value: expr }),
            ExprType::Integer => self.add_statement(Statement::PrintExprInteger { value: expr }),
            ExprType::String => self.add_statement(Statement::PrintExprString { value: expr }),
            ExprType::ParamList | ExprType::ReferenceOfVar(..) => {
                return Err(self.syntax_error("invalid Expression in Print statement".into()))
            }
        }
        Ok(())
    }

    // 式の分解
    fn parse_expr(&self, pos_and_tokens: &[(usize, Token)]) -> Result<Expr, SyntaxError> {
        // 項の最小単位
        match pos_and_tokens {
            // 空の項は存在してはダメ (構文エラー)
            [] => return Err(self.syntax_error("invalid Expression".into())),

            // 真理値リテラル
            [(_, Token::Boolean(value))] => return Ok(Expr::LitBoolean(*value)),

            // 文字リテラル
            [(_, Token::Character(value))] => return Ok(Expr::LitCharacter(*value)),

            // 整数リテラル
            [(pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(false, *value) {
                    return Ok(Expr::LitInteger(value));
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }

            // 文字列リテラル
            [(_, Token::String(value))] => return Ok(Expr::LitString(value.clone())),

            // プリミティブ変数
            [(pos, Token::Name(name))] => {
                let expr = match self.variables.get(name) {
                    Some(VarType::Boolean) => Expr::VarBoolean(name.clone()),
                    Some(VarType::Integer) => Expr::VarInteger(name.clone()),
                    Some(VarType::String) => Expr::VarString(name.clone()),
                    Some(VarType::RefBoolean) => Expr::VarRefBoolean(name.clone()),
                    Some(VarType::RefInteger) => Expr::VarRefInteger(name.clone()),
                    Some(VarType::RefString) => Expr::VarRefString(name.clone()),
                    Some(var_type) => {
                        assert!(matches!(
                            var_type,
                            VarType::ArrayOfBoolean(_)
                                | VarType::ArrayOfInteger(_)
                                | VarType::RefArrayOfBoolean(_)
                                | VarType::RefArrayOfInteger(_)
                        ));
                        Expr::ReferenceOfVar(name.clone(), *var_type)
                    }
                    None => {
                        return Err(
                            self.syntax_error_pos(*pos, format!("undefined variable: {}", name))
                        )
                    }
                };
                return Ok(expr);
            }

            // 単項演算子と真理値
            [(_, Token::Operator(op)), (_, Token::Boolean(value))] if op.can_be_unary_boolean() => {
                return Ok(Expr::UnaryOperatorBoolean(
                    *op,
                    Box::new(Expr::LitBoolean(*value)),
                ))
            }

            // 単項演算子マイナスと整数
            [(_, Token::Operator(Operator::Sub)), (pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(true, *value) {
                    return Ok(Expr::LitInteger(value));
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }

            // 単項演算子と整数
            [(_, Token::Operator(op)), (pos, Token::Integer(value))]
                if op.can_be_unary_integer() =>
            {
                if let Some(value) = validate_integer(false, *value) {
                    return Ok(Expr::UnaryOperatorInteger(
                        *op,
                        Box::new(Expr::LitInteger(value)),
                    ));
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }

            // 単項演算子と変数(真理値or整数)
            [(_, Token::Operator(op)), (pos, Token::Name(name))] if op.can_be_unary() => match self
                .variables
                .get(name)
            {
                Some(VarType::Boolean) if op.can_be_unary_boolean() => {
                    return Ok(Expr::UnaryOperatorBoolean(
                        *op,
                        Box::new(Expr::VarBoolean(name.clone())),
                    ))
                }
                Some(VarType::RefBoolean) if op.can_be_unary_boolean() => {
                    return Ok(Expr::UnaryOperatorBoolean(
                        *op,
                        Box::new(Expr::VarRefBoolean(name.clone())),
                    ))
                }
                Some(VarType::Integer) if op.can_be_unary_integer() => {
                    return Ok(Expr::UnaryOperatorInteger(
                        *op,
                        Box::new(Expr::VarInteger(name.clone())),
                    ))
                }
                Some(VarType::RefInteger) if op.can_be_unary_integer() => {
                    return Ok(Expr::UnaryOperatorInteger(
                        *op,
                        Box::new(Expr::VarRefInteger(name.clone())),
                    ))
                }
                Some(_) => return Err(self.syntax_error_pos(*pos, "invalid Expression".into())),
                None => {
                    return Err(self.syntax_error_pos(*pos, format!("undefined variable: {}", name)))
                }
            },

            // 存在してはいけないトークン列　(構文エラー)
            [(pos, token)] | [(pos, token), (_, _)] => {
                return Err(self
                    .syntax_error_pos(*pos, format!("invalid token in Expression: {:?}", token)))
            }

            // 他はここでは処理しない
            _ => {}
        }

        // 外側の演算子のうち一番右のを抽出する (括弧のネストに気をつける)
        //    項 op1 項 op2 項 op1 ... op2 項
        //   -> (項 op1 項 op2 項 op1 ...) op2 (項)
        //   分割して再帰的に処理していく

        // 分割点となる演算子の抜き出し
        let mut target_op: Option<(usize, Operator)> = None;
        let mut next_unary = true;
        let mut next_term = true;
        let mut bracket_count = 0;
        for (i, (pos, token)) in pos_and_tokens.iter().enumerate() {
            if bracket_count > 0 {
                match token {
                    Token::Operator(Operator::OpenBracket) => bracket_count += 1,
                    Token::Operator(Operator::CloseBracket) => bracket_count -= 1,
                    _ => {}
                }
                continue;
            }
            if let Token::Operator(op) = token {
                if matches!(op, Operator::OpenBracket) {
                    bracket_count += 1;
                    next_unary = false;
                    next_term = false;
                    continue;
                }
                if next_unary {
                    // 単項演算子をそのまま重ねることは禁止
                    //  NG:  Not Not Not -123
                    //  OK:  Not (Not (Not (-123)))
                    if op.can_be_unary() {
                        next_unary = false;
                    } else {
                        return Err(self.syntax_error_pos(*pos, "invalid Expression".into()));
                    }
                } else if op.can_be_binary() {
                    if target_op.filter(|(_, cur_op)| cur_op > op).is_none() {
                        target_op = Some((i, *op));
                    }
                    next_unary = true;
                    next_term = true;
                } else {
                    return Err(self.syntax_error_pos(*pos, "invalid Expression".into()));
                }
            } else {
                if !next_term {
                    return Err(self.syntax_error_pos(*pos, "invalid Expression".into()));
                }
                next_unary = false;
                next_term = false;
            }
        }

        if bracket_count > 0 || next_unary || next_term {
            return Err(self.syntax_error("invalid Expression".into()));
        }

        // 分割して再帰的に処理していく
        if let Some((i, op)) = target_op {
            let lhs = self.parse_expr(&pos_and_tokens[..i])?;
            let rhs = self.parse_expr(&pos_and_tokens[i + 1..])?;
            return Expr::binary(op, lhs, rhs).ok_or_else(|| {
                let (pos, _) = pos_and_tokens[i];
                self.syntax_error_pos(pos, "invalid Expression".into())
            });
        }

        // 外側の演算子がない場合、つまり、項が１つ
        // 構文ミスが無い場合は
        //    [unary-op] ( 式 )
        //    [unary-op] Array ( 引数 ) ' 要素取り出し
        //    [unary-op] Func ( 引数 )
        //    VarString ( 引数 ) ' 文字取り出し
        //    LitString ( 引数 ) ' 文字取り出し
        //    のどれかのハズ
        //    ※引数なし関数は定義しない ()
        //    ※配列自体や関数自体は計算結果として現れないため (CInt)("123")
        //    ※関数自体を持つ変数は作れないとするため 変数(引数) は現れない
        //    ※一時的に生成される文字列から文字取り出しはできないとする (s1 & s2 & "XYZ")(1) など
        // 構文ミスがある場合は例えば、項を繋ぐ演算子が無い場合
        //     値 ( 式 ) ( 式 ) 値 Func 値 ( 引数 ) unary-op 値

        match pos_and_tokens {
            // 単項演算子の分離
            [(pos, Token::Operator(op)), rest @ ..] if op.can_be_unary() => {
                let expr = self.parse_expr(rest)?;
                match expr.return_type() {
                    ExprType::Boolean if op.can_be_unary_boolean() => {
                        Ok(Expr::UnaryOperatorBoolean(*op, Box::new(expr)))
                    }
                    ExprType::Integer if op.can_be_unary_integer() => {
                        Ok(Expr::UnaryOperatorInteger(*op, Box::new(expr)))
                    }
                    _ => Err(self.syntax_error_pos(*pos, "invalid Expression".into())),
                }
            }

            // ( 式 )
            [(_, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] => {
                self.parse_expr(inner)
            }

            // 文字列リテラル ( 添え字 )
            [(_, Token::String(text)), (pos, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let expr = self.parse_expr(inner)?;
                if matches!(expr.return_type(), ExprType::Integer) {
                    Ok(Expr::CharOfLitString(text.clone(), Box::new(expr)))
                } else {
                    Err(self.syntax_error_pos(*pos, "invalid Expression".into()))
                }
            }

            // 配列変数 ( 添え字 )  もしくは  文字列変数 ( 添え字 )
            [(pos_n, Token::Name(name)), (pos_e, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let expr = self.parse_expr(inner)?;
                if !matches!(expr.return_type(), ExprType::Integer) {
                    Err(self.syntax_error_pos(*pos_e, "invalid Expression".into()))
                } else {
                    match self.variables.get(name) {
                        Some(VarType::ArrayOfBoolean(_)) => {
                            Ok(Expr::VarArrayOfBoolean(name.clone(), Box::new(expr)))
                        }
                        Some(VarType::RefArrayOfBoolean(_)) => {
                            Ok(Expr::VarRefArrayOfBoolean(name.clone(), Box::new(expr)))
                        }
                        Some(VarType::ArrayOfInteger(_)) => {
                            Ok(Expr::VarArrayOfInteger(name.clone(), Box::new(expr)))
                        }
                        Some(VarType::RefArrayOfInteger(_)) => {
                            Ok(Expr::VarRefArrayOfInteger(name.clone(), Box::new(expr)))
                        }
                        Some(VarType::String) => {
                            Ok(Expr::CharOfVarString(name.clone(), Box::new(expr)))
                        }
                        Some(VarType::RefString) => {
                            Ok(Expr::CharOfVarRefString(name.clone(), Box::new(expr)))
                        }
                        Some(_) => Err(self.syntax_error_pos(*pos_n, "invalid Expression".into())),
                        None => {
                            Err(self
                                .syntax_error_pos(*pos_n, format!("undefined Variable: {}", name)))
                        }
                    }
                }
            }

            // 関数 ( )  ※引数なし関数
            [(_, Token::Function(function)), (pos, Token::Operator(Operator::OpenBracket)), (_, Token::Operator(Operator::CloseBracket))] => {
                if matches!(function, Function::Eof) {
                    Ok(Expr::FunctionBoolean(
                        *function,
                        Box::new(Expr::LitInteger(0)),
                    ))
                } else {
                    Err(self.syntax_error_pos(*pos, "invalid Expression".into()))
                }
            }

            // 関数 ( 引数 )
            [(_, Token::Function(function)), (pos, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let param = self.parse_expr(inner)?;
                if !function.check_param(&param) {
                    Err(self.syntax_error_pos(*pos, "invalid Expression".into()))
                } else {
                    match function.return_type() {
                        ExprType::Boolean => Ok(Expr::FunctionBoolean(*function, Box::new(param))),
                        ExprType::Integer => Ok(Expr::FunctionInteger(*function, Box::new(param))),
                        ExprType::String => Ok(Expr::FunctionString(*function, Box::new(param))),
                        _ => Err(self.syntax_error_pos(*pos, "invalid Expression".into())),
                    }
                }
            }

            // 該当なし (構文エラー)
            _ => Err(self.syntax_error("invalid Expression".into())),
        }
    }
}

fn validate_integer(minus: bool, value: i32) -> Option<i32> {
    if minus {
        Some((-value) as i16 as i32)
    } else if value < (i16::MIN as i32).abs() {
        Some(value)
    } else {
        None
    }
}

impl Keyword {
    fn is_toplevel_token(&self) -> bool {
        use Keyword::*;
        match self {
            Argument | ByRef | ByVal | Call | Case | Continue | Else | ElseIf | End | Exit
            | Extern | Dim | Do | For | If | Input | Loop | Mid | Next | Print | Program
            | Select => true,

            As | From | Rem | Step | Sub | Then | To | Until | With | While => false,
        }
    }
}

impl Operator {
    fn can_be_binary(&self) -> bool {
        !matches!(
            self,
            Operator::Not
                | Operator::OpenBracket
                | Operator::CloseBracket
                | Operator::AddInto
                | Operator::SubInto
        )
    }

    fn can_be_unary(&self) -> bool {
        matches!(self, Operator::Sub | Operator::Not)
    }

    fn can_be_unary_integer(&self) -> bool {
        matches!(self, Operator::Sub | Operator::Not)
    }

    fn can_be_unary_boolean(&self) -> bool {
        matches!(self, Operator::Not)
    }

    fn priority(&self) -> i32 {
        use Operator::*;
        match self {
            OpenBracket | CloseBracket | AddInto | SubInto => 0,

            Not => 2,

            ShiftLeft | ShiftRight => 4,

            Mul | Div | Mod => 6,

            Add | Sub | Concat => 8,

            LessOrEequal | GreaterOrEqual | LessThan | GreaterThan => 12,

            Equal | NotEqual => 14,

            And | Or | Xor => 16,

            Comma => 18,
        }
    }
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.priority().cmp(&other.priority()))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ArgumentInfo {
    pub var_name: String,
    pub var_type: VarType,
    pub register1: IndexRegister,
    pub register2: Option<IndexRegister>,
}

impl ArgumentInfo {
    fn is_valid_type(&self, expr: &Expr) -> bool {
        match self.var_type {
            VarType::Boolean | VarType::RefBoolean => {
                matches!(expr.return_type(), ExprType::Boolean)
            }
            VarType::Integer | VarType::RefInteger => {
                matches!(expr.return_type(), ExprType::Integer)
            }
            VarType::String | VarType::RefString => matches!(expr.return_type(), ExprType::String),
            VarType::ArrayOfBoolean(size1) | VarType::RefArrayOfBoolean(size1) => match expr {
                Expr::ReferenceOfVar(_, VarType::ArrayOfBoolean(size2))
                | Expr::ReferenceOfVar(_, VarType::RefArrayOfBoolean(size2)) => size1 == *size2,
                _ => false,
            },
            VarType::ArrayOfInteger(size1) | VarType::RefArrayOfInteger(size1) => match expr {
                Expr::ReferenceOfVar(_, VarType::ArrayOfInteger(size2))
                | Expr::ReferenceOfVar(_, VarType::RefArrayOfInteger(size2)) => size1 == *size2,
                _ => false,
            },
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement {
    ProgramName {
        name: String,
    },
    ExitProgram,
    ExternSub {
        name: String,
        arguments: Vec<ArgumentInfo>,
    },
    Argument {
        arguments: Vec<ArgumentInfo>,
    },
    Call {
        name: String,
        arguments: Vec<(String, Expr)>,
    },
    AssignAddInto {
        var_name: String,
        value: Expr,
    },
    AssignRefAddInto {
        var_name: String,
        value: Expr,
    },
    AssignAddIntoElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignRefAddIntoElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignBoolean {
        var_name: String,
        value: Expr,
    },
    AssignRefBoolean {
        var_name: String,
        value: Expr,
    },
    AssignBooleanElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignRefBooleanElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignIntegerElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignRefIntegerElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignCharacterElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignRefCharacterElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignInteger {
        var_name: String,
        value: Expr,
    },
    AssignRefInteger {
        var_name: String,
        value: Expr,
    },
    AssignString {
        var_name: String,
        value: Expr,
    },
    AssignRefString {
        var_name: String,
        value: Expr,
    },
    AssignSubInto {
        var_name: String,
        value: Expr,
    },
    AssignRefSubInto {
        var_name: String,
        value: Expr,
    },
    AssignSubIntoElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignRefSubIntoElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    ContinueDo {
        exit_id: usize,
    },
    ContinueFor {
        exit_id: usize,
    },
    Dim {
        var_name: String,
        var_type: VarType,
    },
    Mid {
        var_name: String,
        var_is_ref: bool,
        offset: Expr,
        length: Option<Expr>,
        value: Expr,
    },
    DoLoop {
        exit_id: usize,
        block: Vec<Statement>,
    },
    DoLoopUntil {
        exit_id: usize,
        condition: Expr,
        block: Vec<Statement>,
    },
    DoLoopWhile {
        exit_id: usize,
        condition: Expr,
        block: Vec<Statement>,
    },
    DoUntilLoop {
        exit_id: usize,
        condition: Expr,
        block: Vec<Statement>,
    },
    DoWhileLoop {
        exit_id: usize,
        condition: Expr,
        block: Vec<Statement>,
    },
    ProvisionalDo {
        exit_id: usize,
        until_condition: Option<Expr>,
        while_condition: Option<Expr>,
    },
    ExitDo {
        exit_id: usize,
    },
    ExitFor {
        exit_id: usize,
    },
    ExitSelect {
        exit_id: usize,
    },
    For {
        exit_id: usize,
        counter: String,
        counter_is_ref: bool,
        init: Expr,
        end: Expr,
        step: Option<Expr>,
        block: Vec<Statement>,
    },
    ProvisionalFor {
        exit_id: usize,
        counter: String,
        counter_is_ref: bool,
        init: Expr,
        end: Expr,
        step: Option<Expr>,
    },
    If {
        condition: Expr,
        block: Vec<Statement>,
        else_blocks: Vec<Statement>,
    },
    ProvitionalIf {
        condition: Expr,
    },
    ElseIf {
        condition: Expr,
        block: Vec<Statement>,
    },
    ProvisionalElseIf {
        condition: Expr,
    },
    Else {
        block: Vec<Statement>,
    },
    ProvisionalElse,
    SelectInteger {
        exit_id: usize,
        value: Expr,
        case_blocks: Vec<Statement>,
    },
    ProvisionalSelectInteger {
        exit_id: usize,
        value: Expr,
    },
    CaseInteger {
        values: Vec<CaseIntegerItem>,
        block: Vec<Statement>,
    },
    ProvisionalCaseInteger {
        values: Vec<CaseIntegerItem>,
    },
    SelectString {
        exit_id: usize,
        value: Expr,
        case_blocks: Vec<Statement>,
    },
    ProvisionalSelectString {
        exit_id: usize,
        value: Expr,
    },
    CaseString {
        values: Vec<String>,
        block: Vec<Statement>,
    },
    ProvisionalCaseString {
        values: Vec<String>,
    },
    CaseElse {
        block: Vec<Statement>,
    },
    ProvisionalCaseElse,
    InputElementInteger {
        var_name: String,
        index: Expr,
    },
    InputInteger {
        var_name: String,
    },
    InputString {
        var_name: String,
    },
    PrintLitBoolean {
        value: bool,
    },
    PrintLitInteger {
        value: i32,
    },
    PrintLitString {
        value: String,
    },
    PrintVarString {
        var_name: String,
    },
    PrintExprBoolan {
        value: Expr,
    },
    PrintExprInteger {
        value: Expr,
    },
    PrintExprString {
        value: Expr,
    },
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum CaseIntegerItem {
    Integer(i32),
    Character(char),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum VarType {
    Boolean,
    Integer,
    String,
    ArrayOfBoolean(usize),
    ArrayOfInteger(usize),
    RefBoolean,
    RefInteger,
    RefString,
    RefArrayOfBoolean(usize),
    RefArrayOfInteger(usize),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ExprType {
    Boolean,
    Integer,
    String,
    ParamList,
    ReferenceOfVar(VarType),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expr {
    BinaryOperatorBoolean(Operator, Box<Expr>, Box<Expr>),
    BinaryOperatorInteger(Operator, Box<Expr>, Box<Expr>),
    BinaryOperatorString(Operator, Box<Expr>, Box<Expr>),
    CharOfLitString(String, Box<Expr>),
    CharOfVarString(String, Box<Expr>),
    CharOfVarRefString(String, Box<Expr>),
    FunctionBoolean(Function, Box<Expr>),
    FunctionInteger(Function, Box<Expr>),
    FunctionString(Function, Box<Expr>),
    LitBoolean(bool),
    LitInteger(i32),
    LitString(String),
    LitCharacter(char),
    UnaryOperatorInteger(Operator, Box<Expr>),
    UnaryOperatorBoolean(Operator, Box<Expr>),
    VarBoolean(String),
    VarInteger(String),
    VarString(String),
    VarRefBoolean(String),
    VarRefInteger(String),
    VarRefString(String),
    VarArrayOfBoolean(String, Box<Expr>),
    VarArrayOfInteger(String, Box<Expr>),
    VarRefArrayOfBoolean(String, Box<Expr>),
    VarRefArrayOfInteger(String, Box<Expr>),
    ReferenceOfVar(String, VarType),
    ParamList(Vec<Expr>),
}

impl Expr {
    fn binary(op: Operator, lhs: Expr, rhs: Expr) -> Option<Expr> {
        // op == , => ParamList, lhsとrhsは型が不一致でもOK
        // op != , => lhsとrhsは型が一致する必要がある
        // opが受け取れる型とopが返す型は様々

        use Operator::*;
        let bin_op_expr = match op {
            // binary operator ではないもの、ここに来たらどこかにバグ
            OpenBracket | CloseBracket | AddInto | SubInto | Not => unreachable!(),

            // 任意の結果を受け取ってパラメータリストとして返すもの
            Comma => {
                let v = match (lhs, rhs) {
                    (Expr::ParamList(mut v1), Expr::ParamList(mut v2)) => {
                        v1.append(&mut v2);
                        v1
                    }
                    (Expr::ParamList(mut v1), rhs) => {
                        v1.push(rhs);
                        v1
                    }
                    (lhs, Expr::ParamList(mut v2)) => {
                        v2.insert(0, lhs);
                        v2
                    }
                    (lhs, rhs) => vec![lhs, rhs],
                };
                return Some(Expr::ParamList(v));
            }

            // カンマ以外の演算子で左辺と右辺の型が異なるのはシンタックスエラー
            _ if lhs.return_type() != rhs.return_type() => return None,

            // 整数を受け取って整数を返すもの
            ShiftLeft | ShiftRight | Mul | Div | Mod | Add | Sub
                if matches!(lhs.return_type(), ExprType::Integer) =>
            {
                Expr::BinaryOperatorInteger
            }

            // 文字列を受け取って文字列を返すもの
            Concat if matches!(lhs.return_type(), ExprType::String) => Expr::BinaryOperatorString,

            // 整数または文字列を受け取って真理値を返すもの
            LessOrEequal | GreaterOrEqual | LessThan | GreaterThan
                if matches!(lhs.return_type(), ExprType::Integer | ExprType::String) =>
            {
                Expr::BinaryOperatorBoolean
            }

            // 真理値または整数または文字列を受け取って真理値を返すもの
            Equal | NotEqual
                if matches!(
                    lhs.return_type(),
                    ExprType::Boolean | ExprType::Integer | ExprType::String
                ) =>
            {
                Expr::BinaryOperatorBoolean
            }

            // 真理値を受け取って真理値を返す、または整数を受け取って整数を返すもの
            And | Or | Xor if matches!(lhs.return_type(), ExprType::Boolean) => {
                Expr::BinaryOperatorBoolean
            }
            And | Or | Xor if matches!(lhs.return_type(), ExprType::Integer) => {
                Expr::BinaryOperatorInteger
            }

            // 受け取る型に対応する演算子ではないのでシンタックスエラー
            // 漏れを無くすため全列挙…
            ShiftLeft | ShiftRight | Mul | Div | Mod | Add | Sub | Concat | LessOrEequal
            | GreaterOrEqual | LessThan | GreaterThan | Equal | NotEqual | And | Or | Xor => {
                return None
            }
        };

        Some(bin_op_expr(op, Box::new(lhs), Box::new(rhs)))
    }

    pub fn return_type(&self) -> ExprType {
        use Expr::*;
        match self {
            BinaryOperatorBoolean(..)
            | FunctionBoolean(..)
            | LitBoolean(..)
            | UnaryOperatorBoolean(..)
            | VarBoolean(..)
            | VarRefBoolean(..)
            | VarArrayOfBoolean(..)
            | VarRefArrayOfBoolean(..) => ExprType::Boolean,

            BinaryOperatorInteger(..)
            | FunctionInteger(..)
            | LitInteger(..)
            | LitCharacter(..)
            | UnaryOperatorInteger(..)
            | VarInteger(..)
            | VarRefInteger(..)
            | VarArrayOfInteger(..)
            | VarRefArrayOfInteger(..)
            | CharOfLitString(..)
            | CharOfVarString(..)
            | CharOfVarRefString(..) => ExprType::Integer,

            BinaryOperatorString(..)
            | FunctionString(..)
            | LitString(..)
            | VarString(..)
            | VarRefString(..) => ExprType::String,

            ParamList(..) => ExprType::ParamList,

            ReferenceOfVar(_, var_type) => ExprType::ReferenceOfVar(*var_type),
        }
    }
}

impl Function {
    fn return_type(&self) -> ExprType {
        use Function::*;
        match self {
            CBool | Eof => ExprType::Boolean,
            Abs | Asc | CInt | Len | Max | Min => ExprType::Integer,
            Chr | CStr | Mid | Space => ExprType::String,
        }
    }

    fn check_param(&self, param: &Expr) -> bool {
        use Function::*;
        match self {
            // 引数なし
            Eof => false,

            // 引数は整数1個
            Abs | CBool | Chr | Space => matches!(param.return_type(), ExprType::Integer),

            // 引数は真理値1個あるいは整数1個
            CInt => matches!(param.return_type(), ExprType::Boolean | ExprType::String),

            // 引数は真理値1個あるいは文字列1個
            CStr => matches!(param.return_type(), ExprType::Boolean | ExprType::Integer),

            // 引数は整数2個
            Max | Min => {
                if let Expr::ParamList(list) = param {
                    list.len() == 2
                        && list
                            .iter()
                            .all(|expr| matches!(expr.return_type(), ExprType::Integer))
                } else {
                    false
                }
            }

            // 引数は文字列1個と整数1個あるいは文字列1個と整数2個
            Mid => {
                if let Expr::ParamList(list) = param {
                    let list: Vec<_> = list.iter().map(|e| e.return_type()).collect();
                    matches!(
                        list.as_slice(),
                        [ExprType::String, ExprType::Integer]
                            | [ExprType::String, ExprType::Integer, ExprType::Integer]
                    )
                } else {
                    false
                }
            }

            // 引数は文字列1個
            Asc | Len => matches!(param.return_type(), ExprType::String),
        }
    }
}

impl std::fmt::Display for ArgumentInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self.var_type {
            VarType::Boolean => {
                format!("ByVal {} As Boolean With {}", self.var_name, self.register1)
            }
            VarType::Integer => {
                format!("ByVal {} As Integer With {}", self.var_name, self.register1)
            }
            VarType::String => format!(
                "ByVal {} As String With {},{}",
                self.var_name,
                self.register1,
                self.register2.expect("BUG")
            ),
            VarType::ArrayOfBoolean(size) => format!(
                "ByVal {}({}) As Boolean With {}",
                self.var_name, size, self.register1
            ),
            VarType::ArrayOfInteger(size) => format!(
                "ByVal {}({}) As Integer With {}",
                self.var_name, size, self.register1
            ),
            VarType::RefBoolean => {
                format!("ByRef {} As Boolean With {}", self.var_name, self.register1)
            }
            VarType::RefInteger => {
                format!("ByRef {} As Integer With {}", self.var_name, self.register1)
            }
            VarType::RefString => format!(
                "ByRef {} As String With {},{}",
                self.var_name,
                self.register1,
                self.register2.expect("BUG")
            ),
            VarType::RefArrayOfBoolean(size) => format!(
                "ByRef {}({}) As Boolean With {}",
                self.var_name, size, self.register1
            ),
            VarType::RefArrayOfInteger(size) => format!(
                "ByRef {}({}) As Integer With {}",
                self.var_name, size, self.register1
            ),
        };
        s.fmt(f)
    }
}

impl std::fmt::Display for CaseIntegerItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(value) => value.fmt(f),
            Self::Character('"') => r#"""""c"#.fmt(f),
            Self::Character(ch) => format!(r#""{}"c"#, ch).fmt(f),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expr::*;
        match self {
            BinaryOperatorBoolean(op, lhs, rhs)
            | BinaryOperatorInteger(op, lhs, rhs)
            | BinaryOperatorString(op, lhs, rhs) => format!(
                "({} {} {})",
                lhs.to_string(),
                op.to_string(),
                rhs.to_string()
            )
            .fmt(f),
            CharOfLitString(lit, index)
                if matches!(
                    index.as_ref(),
                    BinaryOperatorBoolean(..)
                        | BinaryOperatorInteger(..)
                        | BinaryOperatorString(..)
                ) =>
            {
                format!(r#""{}"{}"#, lit.replace('"', r#""""#), index.to_string()).fmt(f)
            }
            CharOfLitString(lit, index) => {
                format!(r#""{}"({})"#, lit.replace('"', r#""""#), index.to_string()).fmt(f)
            }
            VarArrayOfBoolean(var, index)
            | VarArrayOfInteger(var, index)
            | CharOfVarString(var, index)
            | VarRefArrayOfBoolean(var, index)
            | VarRefArrayOfInteger(var, index)
            | CharOfVarRefString(var, index)
                if matches!(
                    index.as_ref(),
                    BinaryOperatorBoolean(..)
                        | BinaryOperatorInteger(..)
                        | BinaryOperatorString(..)
                ) =>
            {
                format!("{}{}", var, index.to_string()).fmt(f)
            }
            VarArrayOfBoolean(var, index)
            | VarArrayOfInteger(var, index)
            | CharOfVarString(var, index)
            | VarRefArrayOfBoolean(var, index)
            | VarRefArrayOfInteger(var, index)
            | CharOfVarRefString(var, index) => format!("{}({})", var, index.to_string()).fmt(f),
            FunctionBoolean(func, param)
            | FunctionInteger(func, param)
            | FunctionString(func, param)
                if matches!(
                    param.as_ref(),
                    BinaryOperatorBoolean(..)
                        | BinaryOperatorInteger(..)
                        | BinaryOperatorString(..)
                ) =>
            {
                format!("{}{}", func.to_string(), param.to_string()).fmt(f)
            }
            FunctionBoolean(func, param)
            | FunctionInteger(func, param)
            | FunctionString(func, param) => {
                format!("{}({})", func.to_string(), param.to_string()).fmt(f)
            }
            LitBoolean(lit) => (if *lit { "True" } else { "False" }).fmt(f),
            LitInteger(lit) => lit.fmt(f),
            LitString(lit) => format!(r#""{}""#, lit.replace('"', r#""""#)).fmt(f),
            LitCharacter('"') => r#"""""c"#.fmt(f),
            LitCharacter(lit) => format!(r#""{}"c"#, lit).fmt(f),
            UnaryOperatorInteger(op, value) | UnaryOperatorBoolean(op, value) => {
                format!("{} {}", op.to_string(), value.to_string()).fmt(f)
            }
            VarBoolean(var)
            | VarInteger(var)
            | VarString(var)
            | VarRefBoolean(var)
            | VarRefInteger(var)
            | VarRefString(var)
            | ReferenceOfVar(var, ..) => var.fmt(f),
            ParamList(list) => list
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(", ")
                .fmt(f),
        }
    }
}
