use crate::casl2::IndexRegister;
use crate::compiler::{is_valid_program_name, MAX_ALLOCATION_SIZE, MAX_ARRAY_SIZE};
use crate::tokenizer::*;
use crate::SyntaxError;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::io::{self, BufRead};

#[cfg(test)]
mod test;

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
                    format!("不適切なトークンです: {:?}", token),
                )))
            }
        }
    }

    if !parser.is_valid_allocation() {
        return Ok(Err(SyntaxError::new(
            parser.line_number + 1,
            0,
            format!(
                "指定のアロケーションサイズが小さすぎます: {} / {}",
                parser.variable_area_size + parser.maximum_allocate_temporary_area_size,
                parser.maximum_variable_area_size
            ),
        )));
    }

    if !parser.is_valid() {
        return Ok(Err(SyntaxError::new(
            parser.line_number + 1,
            0,
            "不完全なソースコードのため正しく解釈できませんでした".into(),
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
    Option,
    ExternSub,
    InExternSub,
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

    fn can_option(self) -> bool {
        matches!(self, HeaderState::Option)
    }

    fn can_extern_sub(self) -> bool {
        use HeaderState::*;
        matches!(self, Option | ExternSub)
    }

    fn can_program_name(self) -> bool {
        use HeaderState::*;
        matches!(self, Option | ExternSub)
    }

    fn can_argument(self) -> bool {
        use HeaderState::*;
        matches!(self, Option | ExternSub | Argument)
    }

    fn can_dim(self) -> bool {
        use HeaderState::*;
        matches!(self, Option | ExternSub | Argument | Dim)
    }

    fn can_command(self) -> bool {
        use HeaderState::*;
        matches!(self, Option | ExternSub | Argument | Dim | NotHeader)
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
    declare_array_with_length: Option<bool>,
    use_bound_for_array_function: bool,
    maximum_variable_area_size: usize,
    variable_area_size: usize,
    maximum_allocate_temporary_area_size: usize,
    allocate_temporary_area_size: usize,
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
            header_state: HeaderState::Option,
            temp_argumets: Vec::new(),
            temp_progam_name: None,
            callables: HashMap::new(),
            end_program_state: EndProgramState::Unnecessary,
            in_call_with: false,
            temp_call_with_arguments: Vec::new(),
            declare_array_with_length: None,
            use_bound_for_array_function: false,
            maximum_variable_area_size: MAX_ALLOCATION_SIZE,
            variable_area_size: 0,
            maximum_allocate_temporary_area_size: 0,
            allocate_temporary_area_size: 0,
        }
    }

    fn is_valid_allocation(&self) -> bool {
        self.variable_area_size + self.maximum_allocate_temporary_area_size
            <= self.maximum_variable_area_size
    }

    fn parse_size_for_array_function(&self, size: usize, value: i32) -> Option<usize> {
        if self.use_bound_for_array_function {
            if (0..size as i32).contains(&value) {
                Some(value as usize + 1)
            } else {
                None
            }
        } else if (1..=size as i32).contains(&value) {
            Some(value as usize)
        } else {
            None
        }
    }

    fn parse_declare_array_size(&self, value: i32) -> Option<usize> {
        if let Some(true) = self.declare_array_with_length {
            if (1..=MAX_ARRAY_SIZE as i32).contains(&value) {
                Some(value as usize)
            } else {
                None
            }
        } else if (0..MAX_ARRAY_SIZE as i32).contains(&value) {
            Some(value as usize + 1)
        } else {
            None
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

    fn can_end_program(&self) -> bool {
        self.statements.len() == 1
            && self.nest_of_do.is_empty()
            && self.nest_of_for.is_empty()
            && self.nest_of_select.is_empty()
            && self.provisionals.is_empty()
            && !self.is_select_head
            && self.header_state.can_command()
            && matches!(self.end_program_state, EndProgramState::Required)
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
            return Err(self.syntax_error("この位置に代入のステートメントは置けません".into()));
        }

        if self.in_call_with {
            return self.parse_assign_argument(name, pos_and_tokens);
        }

        if self.is_select_head {
            return Err(self.syntax_error("この位置に代入のステートメントは置けません".into()));
        }

        if !self.header_state.can_command() {
            return Err(self.syntax_error("この位置に代入のステートメントは置けません".into()));
        } else if self.header_state.in_header() {
            if let Some(name) = self.temp_progam_name.take() {
                self.callables.insert(name, Vec::new());
            }
            self.header_state = HeaderState::NotHeader;
        }

        let var_type =
            self.variables.get(name).cloned().ok_or_else(|| {
                self.syntax_error(format!("宣言されていない変数名です: {}", name))
            })?;

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
                        | VarType::ArrayOfBoolean(..)
                        | VarType::ArrayOfInteger(..)
                        | VarType::RefArrayOfBoolean(..)
                        | VarType::RefArrayOfInteger(..)
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
                    ExprType::ReferenceOfVar(VarType::ArrayOfBoolean(size1))
                    | ExprType::ReferenceOfVar(VarType::RefArrayOfBoolean(size1))
                        if matches!(var_type, VarType::ArrayOfBoolean(size2) if size1 == size2) =>
                    {
                        self.add_statement(Statement::AssignBooleanArray {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    ExprType::ReferenceOfVar(VarType::ArrayOfBoolean(size1))
                    | ExprType::ReferenceOfVar(VarType::RefArrayOfBoolean(size1))
                        if matches!(var_type, VarType::RefArrayOfBoolean(size2) if size1 == size2) =>
                    {
                        self.add_statement(Statement::AssignRefBooleanArray {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    ExprType::ReferenceOfVar(VarType::ArrayOfInteger(size1))
                    | ExprType::ReferenceOfVar(VarType::RefArrayOfInteger(size1))
                        if matches!(var_type, VarType::ArrayOfInteger(size2) if size1 == size2) =>
                    {
                        self.add_statement(Statement::AssignIntegerArray {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    ExprType::ReferenceOfVar(VarType::ArrayOfInteger(size1))
                    | ExprType::ReferenceOfVar(VarType::RefArrayOfInteger(size1))
                        if matches!(var_type, VarType::RefArrayOfInteger(size2) if size1 == size2) =>
                    {
                        self.add_statement(Statement::AssignRefIntegerArray {
                            var_name: name.into(),
                            value: expr,
                        });
                    }
                    _ => return Err(self.syntax_error("代入の変数と値の型が一致しません".into())),
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
                    return Err(self.syntax_error("代入の変数と値の型が一致しません".into()));
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
                    return Err(self.syntax_error("代入の変数と値の型が一致しません".into()));
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
                    return Err(self.syntax_error("代入の変数と値の型が一致しません".into()));
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
                    return Err(self.syntax_error("代入の変数と値の型が一致しません".into()));
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
                return Err(self.syntax_error("不正な代入のステートメントです".into()));
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
            .ok_or_else(|| self.syntax_error("閉じ括弧が不足しています".into()))?;

        let (param, value) = pos_and_tokens.split_at(close_pos + 1);

        let param = if let [_, inner @ .., _] = param {
            self.parse_expr(inner)?
        } else {
            return Err(self.syntax_error("不正なインデックスの指定です".into()));
        };
        if !matches!(param.return_type(), ExprType::Integer) {
            return Err(self.syntax_error("インデックスの型が不正です".into()));
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
                    _ => return Err(self.syntax_error("代入の変数と値の型が一致しません".into())),
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
                    return Err(self.syntax_error("代入の変数と値の型が一致しません".into()));
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
                    return Err(self.syntax_error("代入の変数と値の型が一致しません".into()));
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
                    return Err(self.syntax_error("代入の変数と値の型が一致しません".into()));
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
                    return Err(self.syntax_error("代入の変数と値の型が一致しません".into()));
                }
            }
            _ => return Err(self.syntax_error("不正な代入のステートメントです".into())),
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
                return Err(self.syntax_error(format!("引数名が重複しています: {}", name)));
            }
        }

        let value = if let [(_, Token::Operator(Operator::Equal)), rest @ ..] = pos_and_tokens {
            self.parse_expr(rest)?
        } else {
            return Err(self.syntax_error("引数への値の割り当てのステートメントが不正です".into()));
        };

        let temp_area: usize;

        if let Some(arg) = self
            .callables
            .get(self.temp_progam_name.as_ref().expect("BUG"))
            .expect("BUG")
            .iter()
            .find(|arg| arg.var_name == name)
        {
            if !arg.is_valid_type(&value) {
                return Err(self.syntax_error(format!(
                    "引数への値の割り当ての型が一致しません: {} {}",
                    name, value
                )));
            }
            if !value.can_access_variable() && arg.var_type.is_reference() {
                if let Some(size) = arg.var_type.get_array_size() {
                    temp_area = size;
                } else if arg.var_type.is_string() {
                    temp_area = 257;
                } else if arg.var_type.is_boolean() || arg.var_type.is_integer() {
                    temp_area = 1;
                } else {
                    unreachable!("BUG");
                }
            } else {
                temp_area = 0;
            }
        } else {
            return Err(self.syntax_error(format!("引数名に誤りがあります: {}", name)));
        }

        self.allocate_temporary_area_size += temp_area;

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
            return Err(self.syntax_error("この位置にステートメントを置くことはできません".into()));
        }

        if self.header_state.in_header() {
            match command {
                Keyword::Argument => return self.parse_command_argument(pos_and_tokens),
                Keyword::Dim => return self.parse_command_dim(pos_and_tokens),
                Keyword::Extern => return self.parse_command_extern_sub(pos_and_tokens),
                Keyword::Option => return self.parse_option(pos_and_tokens),
                Keyword::Sub => return self.parse_command_program_name(pos_and_tokens),
                _ => {}
            }
        }

        if self.header_state.in_defininition() {
            return match command {
                Keyword::ByRef => self.parse_command_byref(pos_and_tokens),
                Keyword::ByVal => self.parse_command_byval(pos_and_tokens),
                Keyword::End => self.parse_command_end(pos_and_tokens),
                _ => Err(self.syntax_error(format!(
                    "この位置に{:?}ステートメントを置くことはできません",
                    command
                ))),
            };
        }

        if !self.header_state.can_command() {
            return Err(self.syntax_error(format!(
                "この位置に{:?}ステートメントを置くことはできません",
                command
            )));
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
                Err(self.syntax_error(format!(
                    "この位置に{:?}ステートメントを置くことはできません",
                    command
                )))
            };
        }

        if self.is_select_head {
            return match command {
                Keyword::Case => self.parse_command_case(pos_and_tokens),
                Keyword::End => self.parse_command_end(pos_and_tokens),
                _ => Err(self.syntax_error(format!(
                    "この位置に{:?}ステートメントを置くことはできません",
                    command
                ))),
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
            Keyword::Fill => self.parse_command_fill(pos_and_tokens),
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
            | Keyword::Option
            | Keyword::Sub => Err(self.syntax_error(format!(
                "この位置に{:?}ステートメントを置くことはできません",
                command
            ))),

            Keyword::As
            | Keyword::From
            | Keyword::Rem
            | Keyword::Step
            | Keyword::Then
            | Keyword::To
            | Keyword::Until
            | Keyword::With
            | Keyword::While => unreachable!("BUG"),
        }
    }

    // Option Array { UBound / Length } [ { All / Declare / Function } ]
    // Option EOF { Special / Common }
    // Option Allocator { Disabled / Enabled / Special }
    // Option Register { Restore / Dirty }
    // Option Variable { Initialize / Uninitialize }
    fn parse_option(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        if !self.header_state.can_option() {
            return Err(
                self.syntax_error("この位置にOptionステートメントを置くことはできません".into())
            );
        }
        let ((pt, target), (pv, value), extra) = match pos_and_tokens {
            [target, value] => (target, value, None),
            [target, value, extra] => (target, value, Some(extra)),
            _ => return Err(self.syntax_error("Optionの指定が不正です".into())),
        };

        match target {
            Token::Function(Function::Array) => {
                if self.declare_array_with_length.is_some() {
                    return Err(
                        self.syntax_error_pos(*pv, "Option Arrayは既に指定されています".into())
                    );
                }
                // value
                //   UBound .... 最大インデックス(境界値・上限値)でサイズ指定 (Bound,Bounds,Indexでも可)
                //   Length .... 配列長でサイズ指定 (Sizeでも可)
                // extra
                //   Declare ... Dim,ByRef,ByValだけ設定変更、CArray,SubArrayはデフォルトの設定
                //   Function .. CArray,SubArrayだけ設定変更、Dim,ByRef,ByValはデフォルトの設定
                //   All ....... Dim,ByRef,ByVal,CArray,SubArrayの全部の設定変更
                //  extraを省略した場合はAllがデフォルト
                let mut length = false;
                let mut all = true;
                match value {
                    Token::Name(value) if "Default".eq_ignore_ascii_case(value) => {
                        all = false;
                        if extra.is_some() {
                            return Err(
                                self.syntax_error_pos(*pv, "Option Arrayの指定が不正です".into())
                            );
                        }
                    }
                    Token::Name(value)
                        if "Bound".eq_ignore_ascii_case(value)
                            || "Bounds".eq_ignore_ascii_case(value)
                            || "Index".eq_ignore_ascii_case(value)
                            || "UBound".eq_ignore_ascii_case(value) =>
                    {
                        if let Some((pa, Token::Name(extra))) = extra {
                            if "Declare".eq_ignore_ascii_case(extra) {
                                all = false;
                            } else if !"Function".eq_ignore_ascii_case(extra)
                                && !"All".eq_ignore_ascii_case(extra)
                            {
                                return Err(self
                                    .syntax_error_pos(*pa, "Option Arrayの指定が不正です".into()));
                            }
                        }
                    }
                    Token::Name(value)
                        if "Length".eq_ignore_ascii_case(value)
                            || "Size".eq_ignore_ascii_case(value) =>
                    {
                        length = true;
                        if let Some((pa, Token::Name(extra))) = extra {
                            if "Declare".eq_ignore_ascii_case(extra) {
                                all = false;
                            } else if "Function".eq_ignore_ascii_case(extra) {
                                length = false;
                                all = false;
                            } else if !"All".eq_ignore_ascii_case(extra) {
                                return Err(self
                                    .syntax_error_pos(*pa, "Option Arrayの指定が不正です".into()));
                            }
                        }
                    }
                    _ => {
                        return Err(
                            self.syntax_error_pos(*pv, "Option Arrayの指定が不正です".into())
                        )
                    }
                }
                self.declare_array_with_length = Some(length);
                self.use_bound_for_array_function = length != all;
                self.add_statement(Statement::CompileOption {
                    option: CompileOption::ArraySize { length, all },
                });
            }
            Token::Name(target)
                if "Allocator".eq_ignore_ascii_case(target)
                    || "Allocate".eq_ignore_ascii_case(target)
                    || "Allocation".eq_ignore_ascii_case(target)
                    || "Recursion".eq_ignore_ascii_case(target)
                    || "Recursive".eq_ignore_ascii_case(target) =>
            {
                for stmt in self.statements.first().expect("BUG").iter() {
                    if let Statement::CompileOption {
                        option: CompileOption::Allocator { .. },
                    } = stmt
                    {
                        return Err(self.syntax_error_pos(
                            *pt,
                            format!("Option {}は既に指定されています", target),
                        ));
                    }
                }
                match value {
                    Token::Boolean(true) => {
                        if extra.is_some() {
                            return Err(self.syntax_error_pos(
                                *pv,
                                format!("Option {}の指定が不正です", target),
                            ));
                        }
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Allocator {
                                enabled: true,
                                common: true,
                                size: MAX_ALLOCATION_SIZE,
                            },
                        });
                    }
                    Token::Name(value)
                        if "Common".eq_ignore_ascii_case(value)
                            || "Shared".eq_ignore_ascii_case(value)
                            || "External".eq_ignore_ascii_case(value)
                            || "Global".eq_ignore_ascii_case(value)
                            || "Public".eq_ignore_ascii_case(value)
                            || "On".eq_ignore_ascii_case(value)
                            || "Enable".eq_ignore_ascii_case(value)
                            || "Enabled".eq_ignore_ascii_case(value) =>
                    {
                        if extra.is_some() {
                            return Err(self.syntax_error_pos(
                                *pv,
                                format!("Option {}の指定が不正です", target),
                            ));
                        }
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Allocator {
                                enabled: true,
                                common: true,
                                size: MAX_ALLOCATION_SIZE,
                            },
                        });
                    }
                    Token::Boolean(false) => {
                        if extra.is_some() {
                            return Err(self.syntax_error_pos(
                                *pv,
                                format!("Option {}の指定が不正です", target),
                            ));
                        }
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Allocator {
                                enabled: false,
                                common: false,
                                size: 0,
                            },
                        });
                    }
                    Token::Name(value)
                        if "Default".eq_ignore_ascii_case(value)
                            || "Disable".eq_ignore_ascii_case(value)
                            || "Disabled".eq_ignore_ascii_case(value)
                            || "Off".eq_ignore_ascii_case(value)
                            || "No".eq_ignore_ascii_case(value)
                            || "Nothing".eq_ignore_ascii_case(value)
                            || "None".eq_ignore_ascii_case(value) =>
                    {
                        if extra.is_some() {
                            return Err(self.syntax_error_pos(
                                *pv,
                                format!("Option {}の指定が不正です", target),
                            ));
                        }
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Allocator {
                                enabled: false,
                                common: false,
                                size: 0,
                            },
                        });
                    }
                    Token::Name(value)
                        if "Special".eq_ignore_ascii_case(value)
                            || "Intern".eq_ignore_ascii_case(value)
                            || "Internal".eq_ignore_ascii_case(value)
                            || "Local".eq_ignore_ascii_case(value)
                            || "Private".eq_ignore_ascii_case(value)
                            || "Personal".eq_ignore_ascii_case(value)
                            || "Own".eq_ignore_ascii_case(value)
                            || "Owned".eq_ignore_ascii_case(value) =>
                    {
                        match extra {
                            None => {
                                self.add_statement(Statement::CompileOption {
                                    option: CompileOption::Allocator {
                                        enabled: true,
                                        common: false,
                                        size: MAX_ALLOCATION_SIZE,
                                    },
                                });
                            }
                            Some((_, Token::Integer(size)))
                                if (1..=MAX_ALLOCATION_SIZE as i32).contains(size) =>
                            {
                                self.maximum_variable_area_size = *size as usize;
                                self.add_statement(Statement::CompileOption {
                                    option: CompileOption::Allocator {
                                        enabled: true,
                                        common: false,
                                        size: *size as usize,
                                    },
                                });
                            }
                            Some((pv, _)) => {
                                return Err(self.syntax_error_pos(
                                    *pv,
                                    format!("Option {}の指定が不正です", target),
                                ))
                            }
                        }
                    }
                    _ => {
                        return Err(self
                            .syntax_error_pos(*pt, format!("Option {}の指定が不正です", target)))
                    }
                }
            }
            _ if extra.is_some() => {
                let (pe, _) = extra.unwrap();
                return Err(self.syntax_error_pos(*pe, "Optionの指定が不正です".into()));
            }
            Token::Function(Function::Eof) => {
                for stmt in self.statements.first().expect("BUG").iter() {
                    if let Statement::CompileOption {
                        option: CompileOption::Eof { .. },
                    } = stmt
                    {
                        return Err(
                            self.syntax_error_pos(*pt, "Option EOFは既に指定されています".into())
                        );
                    }
                }
                match value {
                    Token::Keyword(Keyword::Extern) => {
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Eof { common: true },
                        });
                    }
                    Token::Name(value)
                        if "Common".eq_ignore_ascii_case(value)
                            || "Shared".eq_ignore_ascii_case(value)
                            || "External".eq_ignore_ascii_case(value)
                            || "Global".eq_ignore_ascii_case(value)
                            || "Public".eq_ignore_ascii_case(value) =>
                    {
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Eof { common: true },
                        });
                    }
                    Token::Name(value)
                        if "Default".eq_ignore_ascii_case(value)
                            || "Special".eq_ignore_ascii_case(value)
                            || "Intern".eq_ignore_ascii_case(value)
                            || "Internal".eq_ignore_ascii_case(value)
                            || "Local".eq_ignore_ascii_case(value)
                            || "Personal".eq_ignore_ascii_case(value)
                            || "Private".eq_ignore_ascii_case(value)
                            || "Own".eq_ignore_ascii_case(value)
                            || "Owned".eq_ignore_ascii_case(value) =>
                    {
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Eof { common: false },
                        });
                    }
                    _ => {
                        return Err(self.syntax_error_pos(*pv, "Option EOFの指定が不正です".into()))
                    }
                }
            }
            Token::Name(target) if "Register".eq_ignore_ascii_case(target) => {
                for stmt in self.statements.first().expect("BUG").iter() {
                    if let Statement::CompileOption {
                        option: CompileOption::Register { .. },
                    } = stmt
                    {
                        return Err(self.syntax_error_pos(
                            *pt,
                            "Option Registerは既に指定されています".into(),
                        ));
                    }
                }
                // Recover 呼び出し前に回復する
                // Restore 呼び出し前を復元する
                // Back    呼び出し前の状態に戻る
                // Break   破壊（呼び出し前の状態には戻らない)
                // Dirty　　　汚染(呼び出し前の状態には戻らない) (Pollutionでもいいかもしれない)
                // Unclean  汚染(呼び出し前の状態には戻らない) (反対がcleanにするわけではないので違和感はある)
                // Keep,Leave,Saveは対象がどうなるのか俺にはよくわからん
                // Keep は呼び出し前を維持するのか、終了状態を維持するのか、
                // Save は呼び出し前を保存するのか、終了状態を保存するのか、
                // Leave は呼び出し前を残しておくのか、終了状態を残しておくのか、(Leaveは後者な雰囲気はある)
                match value {
                    Token::Name(value)
                        if "Default".eq_ignore_ascii_case(value)
                            || "Recover".eq_ignore_ascii_case(value)
                            || "Restore".eq_ignore_ascii_case(value)
                            || "Back".eq_ignore_ascii_case(value) =>
                    {
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Register { restore: true },
                        });
                    }
                    Token::Name(value)
                        if "Break".eq_ignore_ascii_case(value)
                            || "Dirty".eq_ignore_ascii_case(value)
                            || "Unclear".eq_ignore_ascii_case(value)
                            || "Unclean".eq_ignore_ascii_case(value) =>
                    {
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Register { restore: false },
                        });
                    }
                    _ => {
                        return Err(
                            self.syntax_error_pos(*pv, "Option Registerの指定が不正です".into())
                        )
                    }
                }
            }
            Token::Name(target) if "Variable".eq_ignore_ascii_case(target) => {
                for stmt in self.statements.first().expect("BUG").iter() {
                    if let Statement::CompileOption {
                        option: CompileOption::Variable { .. },
                    } = stmt
                    {
                        return Err(self.syntax_error_pos(
                            *pt,
                            "Option Variableは既に指定されています".into(),
                        ));
                    }
                }
                match value {
                    Token::Name(value)
                        if "Default".eq_ignore_ascii_case(value)
                            || "Initialize".eq_ignore_ascii_case(value)
                            || "Format".eq_ignore_ascii_case(value)
                            || "Clear".eq_ignore_ascii_case(value)
                            || "Clean".eq_ignore_ascii_case(value)
                            || "Zero".eq_ignore_ascii_case(value) =>
                    {
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Variable { initialize: true },
                        });
                    }
                    Token::Name(value)
                        if "Uninitialize".eq_ignore_ascii_case(value)
                            || "Unclear".eq_ignore_ascii_case(value)
                            || "Unclean".eq_ignore_ascii_case(value)
                            || "Dirty".eq_ignore_ascii_case(value) =>
                    {
                        self.add_statement(Statement::CompileOption {
                            option: CompileOption::Variable { initialize: false },
                        });
                    }
                    _ => {
                        return Err(
                            self.syntax_error_pos(*pv, "Option Variableの指定が不正です".into())
                        )
                    }
                }
            }
            _ => return Err(self.syntax_error_pos(*pt, "Optionの指定が不正です".into())),
        }

        Ok(())
    }

    // Fill <bool_arr>, <value>
    // Fill <ref_bool_arr>, <value>
    // Fill <int_arr>, <value>
    // Fill <ref_int_arr>, <value>
    // Fill <str_var>, <char>
    // Fill <ref_str_var>, <char>
    fn parse_command_fill(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        let param = self.parse_expr(pos_and_tokens)?;
        let (var, value) = if let Expr::ParamList(list) = &param {
            if let [var, value] = list.as_slice() {
                (var, value)
            } else {
                return Err(self.syntax_error("不正なFillステートメントです".into()));
            }
        } else {
            return Err(self.syntax_error("不正なFillステートメントです".into()));
        };

        match var {
            Expr::VarString(var_name) if matches!(value.return_type(), ExprType::Integer) => {
                self.add_statement(Statement::FillString {
                    var_name: var_name.clone(),
                    value: value.clone(),
                });
            }
            Expr::VarRefString(var_name) if matches!(value.return_type(), ExprType::Integer) => {
                self.add_statement(Statement::FillRefString {
                    var_name: var_name.clone(),
                    value: value.clone(),
                });
            }
            Expr::ReferenceOfVar(var_name, VarType::ArrayOfBoolean(_))
                if matches!(value.return_type(), ExprType::Boolean) =>
            {
                self.add_statement(Statement::FillArrayOfBoolean {
                    var_name: var_name.clone(),
                    value: value.clone(),
                });
            }
            Expr::ReferenceOfVar(var_name, VarType::RefArrayOfBoolean(_))
                if matches!(value.return_type(), ExprType::Boolean) =>
            {
                self.add_statement(Statement::FillRefArrayOfBoolean {
                    var_name: var_name.clone(),
                    value: value.clone(),
                });
            }
            Expr::ReferenceOfVar(var_name, VarType::ArrayOfInteger(_))
                if matches!(value.return_type(), ExprType::Integer) =>
            {
                self.add_statement(Statement::FillArrayOfInteger {
                    var_name: var_name.clone(),
                    value: value.clone(),
                });
            }
            Expr::ReferenceOfVar(var_name, VarType::RefArrayOfInteger(_))
                if matches!(value.return_type(), ExprType::Integer) =>
            {
                self.add_statement(Statement::FillRefArrayOfInteger {
                    var_name: var_name.clone(),
                    value: value.clone(),
                });
            }
            _ => return Err(self.syntax_error("不正なFillステートメントです".into())),
        }

        Ok(())
    }

    // Call <name>
    // Call <name> (<arguments>, ..)
    // Call <name> With
    fn parse_command_call_extern_sub(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [(pn, Token::Name(name)), (_, Token::Operator(Operator::OpenBracket)), (_, Token::Operator(Operator::CloseBracket))]
            | [(pn, Token::Name(name))] => {
                if let Some(args) = self.callables.get(name) {
                    if !args.is_empty() {
                        return Err(self.syntax_error_pos(*pn, "引数の指定が必要です".into()));
                    }
                } else {
                    return Err(
                        self.syntax_error_pos(*pn, format!("サブルーチン{}が未定義です", name))
                    );
                }
                self.add_statement(Statement::Call {
                    name: name.clone(),
                    arguments: Vec::new(),
                });
            }
            [(pn, Token::Name(name)), (_, Token::Keyword(Keyword::With))] => {
                if !self.callables.contains_key(name) {
                    return Err(
                        self.syntax_error_pos(*pn, format!("サブルーチン{}が未定義です", name))
                    );
                }
                assert!(self.temp_progam_name.is_none());
                assert!(self.temp_call_with_arguments.is_empty());
                assert!(!self.in_call_with);
                self.temp_progam_name = Some(name.clone());
                self.in_call_with = true;
                self.allocate_temporary_area_size = 0;
            }
            [(pn, Token::Name(name)), rest @ ..] => {
                let param = self.parse_expr(rest)?;
                let mut temp_area: usize = 0;
                let arguments = if let Some(args) = self.callables.get(name) {
                    if let Expr::ParamList(list) = param {
                        if list.len() != args.len() {
                            return Err(self.syntax_error("引数の数が一致しません".into()));
                        }
                        let mut arguments = Vec::with_capacity(list.len());
                        for (arg, expr) in args.iter().zip(list) {
                            if !arg.is_valid_type(&expr) {
                                return Err(self.syntax_error(format!(
                                    "引数と値の型が一致しません: {} = {}",
                                    arg.var_name, expr
                                )));
                            }
                            if !expr.can_access_variable() && arg.var_type.is_reference() {
                                if let Some(size) = arg.var_type.get_array_size() {
                                    temp_area += size;
                                } else if arg.var_type.is_string() {
                                    temp_area += 257;
                                } else if arg.var_type.is_boolean() || arg.var_type.is_integer() {
                                    temp_area += 1;
                                } else {
                                    unreachable!("BUG");
                                }
                            }
                            arguments.push((arg.var_name.clone(), expr));
                        }
                        arguments
                    } else if args.len() == 1 {
                        let arg = args.first().unwrap();
                        if !arg.is_valid_type(&param) {
                            return Err(self.syntax_error(format!(
                                "引数と値の型が一致しません: {} = {}",
                                arg.var_name, param
                            )));
                        }
                        if !param.can_access_variable() && arg.var_type.is_reference() {
                            if let Some(size) = arg.var_type.get_array_size() {
                                temp_area += size;
                            } else if arg.var_type.is_string() {
                                temp_area += 257;
                            } else if arg.var_type.is_boolean() || arg.var_type.is_integer() {
                                temp_area += 1;
                            } else {
                                unreachable!("BUG");
                            }
                        }
                        vec![(arg.var_name.clone(), param)]
                    } else {
                        return Err(self.syntax_error_pos(*pn, "引数の数が一致しません".into()));
                    }
                } else {
                    return Err(
                        self.syntax_error_pos(*pn, format!("サブルーチン{}が未定義です", name))
                    );
                };
                self.maximum_allocate_temporary_area_size =
                    self.maximum_allocate_temporary_area_size.max(temp_area);
                self.add_statement(Statement::Call {
                    name: name.clone(),
                    arguments,
                });
            }
            _ => return Err(self.syntax_error("不正なCallステートメントです".into())),
        }
        Ok(())
    }

    // プログラム名の正当性チェック
    fn check_valid_program_name(&self, pn: usize, name: &str) -> Result<(), SyntaxError> {
        if !is_valid_program_name(name) {
            Err(self.syntax_error_pos(pn, format!("禁止されている名前です: {}", name)))
        } else if self.callables.contains_key(name) {
            Err(self.syntax_error_pos(pn, format!("既に使用されている名前です: {}", name)))
        } else {
            Ok(())
        }
    }

    // Program <name>
    fn parse_command_program_name(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        if !self.header_state.can_program_name() {
            return Err(
                self.syntax_error("この位置にSubステートメントを置くことはできません".into())
            );
        }
        if let [(pn, Token::Name(name))] = pos_and_tokens {
            self.check_valid_program_name(*pn, name)?;
            assert!(self.temp_progam_name.is_none());
            assert!(self.temp_argumets.is_empty());
            self.temp_progam_name = Some(name.clone());
            self.add_statement(Statement::ProgramName { name: name.clone() });
        } else {
            return Err(self.syntax_error("不正なSubステートメントです".into()));
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
            return Err(self
                .syntax_error("この位置にExtern Subステートメントを置くことはできません".into()));
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
            _ => return Err(self.syntax_error("不正なExtern Subステートメントです".into())),
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
        self.variable_area_size += 1;
        let ((pn, var_name), var_type, (pf, flow), (pr1, reg1), reg2) = match pos_and_tokens {
            [(pn, N(name)), (_, K(As)), (_, T(Tn::Boolean)), (pf, K(flow)), (pr, N(reg))] => {
                ((pn, name), VarType::RefBoolean, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, K(As)), (_, T(Tn::Integer)), (pf, K(flow)), (pr, N(reg))] => {
                ((pn, name), VarType::RefInteger, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, K(As)), (_, T(Tn::String)), (pf, K(flow)), (pr1, N(reg1)), (_, Op(Co)), (pr2, N(reg2))] =>
            {
                self.variable_area_size += 1;
                let reg2 = Some((pr2, reg2));
                let var_type = VarType::RefString;
                ((pn, name), var_type, (pf, flow), (pr1, reg1), reg2)
            }
            [(pn, N(name)), (_, Op(Ob)), (pu, I(ubound)), (_, Op(Cb)), (_, K(As)), (_, T(Tn::Boolean)), (pf, K(flow)), (pr, N(reg))] =>
            {
                let size = self.parse_declare_array_size(*ubound).ok_or_else(|| {
                    self.syntax_error_pos(*pu, "配列の大きさの指定が不正です".into())
                })?;
                let var_type = VarType::RefArrayOfBoolean(size);
                ((pn, name), var_type, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, Op(Ob)), (pu, I(ubound)), (_, Op(Cb)), (_, K(As)), (_, T(Tn::Integer)), (pf, K(flow)), (pr, N(reg))] =>
            {
                let size = self.parse_declare_array_size(*ubound).ok_or_else(|| {
                    self.syntax_error_pos(*pu, "配列の大きさの指定が不正です".into())
                })?;
                let var_type = VarType::RefArrayOfInteger(size);
                ((pn, name), var_type, (pf, flow), (pr, reg), None)
            }
            _ => return Err(self.syntax_error("不正なByRefステートメントです".into())),
        };

        if self
            .temp_argumets
            .iter()
            .any(|arg| arg.var_name.eq(var_name))
        {
            return Err(self.syntax_error_pos(*pn, format!("引数名が重複しています: {}", var_name)));
        }

        match flow {
            Keyword::From if self.header_state.in_argument() => {}
            Keyword::To if self.header_state.in_extern_sub() => {}
            _ => return Err(self.syntax_error_pos(*pf, "不正なByRefステートメントです".into())),
        }

        let register1 = IndexRegister::try_from(reg1.as_str()).map_err(|_| {
            self.syntax_error_pos(*pr1, format!("不正なレジスタ名です: {}", reg1.as_str()))
        })?;
        if self
            .temp_argumets
            .iter()
            .any(|arg| arg.register1 == register1 || arg.register2 == Some(register1))
        {
            return Err(
                self.syntax_error_pos(*pr1, format!("レジスタが重複しています: {}", register1))
            );
        }

        let register2 = if let Some((pr2, reg2)) = reg2 {
            let register2 = IndexRegister::try_from(reg2.as_str()).map_err(|_| {
                self.syntax_error_pos(*pr2, format!("不正なレジスタ名です: {}", reg2.as_str()))
            })?;
            if self
                .temp_argumets
                .iter()
                .any(|arg| arg.register1 == register2 || arg.register2 == Some(register2))
                || register1 == register2
            {
                return Err(self.syntax_error_pos(
                    *pr2,
                    format!("レジスタが重複しています: {}", reg2.as_str()),
                ));
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
                self.variable_area_size += 1;
                ((pn, name), VarType::Boolean, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, K(As)), (_, T(Tn::Integer)), (pf, K(flow)), (pr, N(reg))] => {
                self.variable_area_size += 1;
                ((pn, name), VarType::Integer, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, K(As)), (_, T(Tn::String)), (pf, K(flow)), (pr1, N(reg1)), (_, Op(Co)), (pr2, N(reg2))] =>
            {
                self.variable_area_size += 257;
                let reg2 = Some((pr2, reg2));
                let var_type = VarType::String;
                ((pn, name), var_type, (pf, flow), (pr1, reg1), reg2)
            }
            [(pn, N(name)), (_, Op(Ob)), (pu, I(ubound)), (_, Op(Cb)), (_, K(As)), (_, T(Tn::Boolean)), (pf, K(flow)), (pr, N(reg))] =>
            {
                let size = self.parse_declare_array_size(*ubound).ok_or_else(|| {
                    self.syntax_error_pos(*pu, "配列の大きさの指定が不正です".into())
                })?;
                self.variable_area_size += size;
                let var_type = VarType::ArrayOfBoolean(size);
                ((pn, name), var_type, (pf, flow), (pr, reg), None)
            }
            [(pn, N(name)), (_, Op(Ob)), (pu, I(ubound)), (_, Op(Cb)), (_, K(As)), (_, T(Tn::Integer)), (pf, K(flow)), (pr, N(reg))] =>
            {
                let size = self.parse_declare_array_size(*ubound).ok_or_else(|| {
                    self.syntax_error_pos(*pu, "配列の大きさの指定が不正です".into())
                })?;
                self.variable_area_size += size;
                let var_type = VarType::ArrayOfInteger(size);
                ((pn, name), var_type, (pf, flow), (pr, reg), None)
            }
            _ => return Err(self.syntax_error("不正なByValステートメントです".into())),
        };

        if self
            .temp_argumets
            .iter()
            .any(|arg| arg.var_name.eq(var_name))
        {
            return Err(self.syntax_error_pos(*pn, format!("引数名が重複しています: {}", var_name)));
        }

        match flow {
            Keyword::From if self.header_state.in_argument() => {}
            Keyword::To if self.header_state.in_extern_sub() => {}
            _ => return Err(self.syntax_error_pos(*pf, "不正なByValステートメントです".into())),
        }

        let register1 = IndexRegister::try_from(reg1.as_str()).map_err(|_| {
            self.syntax_error_pos(*pr1, format!("不正なレジスタ名です: {}", reg1.as_str()))
        })?;
        if self
            .temp_argumets
            .iter()
            .any(|arg| arg.register1 == register1 || arg.register2 == Some(register1))
        {
            return Err(
                self.syntax_error_pos(*pr1, format!("レジスタが重複しています: {}", reg1.as_str()))
            );
        }

        let register2 = if let Some((pr2, reg2)) = reg2 {
            let register2 = IndexRegister::try_from(reg2.as_str()).map_err(|_| {
                self.syntax_error_pos(*pr2, format!("不正なレジスタ名です: {}", reg2.as_str()))
            })?;
            if self
                .temp_argumets
                .iter()
                .any(|arg| arg.register1 == register2 || arg.register2 == Some(register2))
                || register1 == register2
            {
                return Err(self.syntax_error_pos(
                    *pr2,
                    format!("レジスタが重複しています: {}", reg2.as_str()),
                ));
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
            return Err(
                self.syntax_error("この位置にArgumentステートメントを置くことはできません".into())
            );
        }

        if !pos_and_tokens.is_empty() {
            return Err(self.syntax_error("不正なArgumentステートメントです".into()));
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
            .ok_or_else(|| self.syntax_error("閉じ括弧が不足しています".into()))?;

        let (param, value) = pos_and_tokens.split_at(close_pos + 1);

        let param = if let [_, inner @ .., _] = param {
            self.parse_expr(inner)?
        } else {
            return Err(self.syntax_error("不正なMidステートメントです".into()));
        };

        let mut list = if let Expr::ParamList(list) = param {
            list
        } else {
            return Err(self.syntax_error("不正なMidステートメントです".into()));
        };

        list.reverse();

        let (var_name, var_is_ref) = match list.pop() {
            Some(Expr::VarString(var_name)) => (var_name, false),
            Some(Expr::VarRefString(var_name)) => (var_name, true),
            _ => return Err(self.syntax_error("文字列変数名が指定されていません".into())),
        };

        let offset = match list.pop() {
            Some(offset) if matches!(offset.return_type(), ExprType::Integer) => offset,
            _ => return Err(self.syntax_error("不正なオフセット値の指定です".into())),
        };

        let length = match list.pop() {
            None => None,
            Some(expr) if matches!(expr.return_type(), ExprType::Integer) && list.is_empty() => {
                Some(expr)
            }
            Some(_) => return Err(self.syntax_error("不正な長さの指定です".into())),
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
                return Err(self.syntax_error("値の型が文字列ではありません".into()));
            }
        } else {
            return Err(self.syntax_error("不正なMidステートメントです".into()));
        }

        Ok(())
    }

    // End { Argument / Call / If / Program / Select / Sub }
    fn parse_command_end(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [(_, Token::Keyword(Keyword::Argument))] => self.compose_command_argument(),
            [(_, Token::Keyword(Keyword::Call))] => self.compose_command_call_with(),
            [(_, Token::Keyword(Keyword::If))] => self.compose_command_if(),
            [(_, Token::Keyword(Keyword::Select))] => self.compose_command_select(),
            [(_, Token::Keyword(Keyword::Sub))] => self.compose_command_sub(),
            _ => Err(self.syntax_error("不正なEndステートメントです".into())),
        }
    }

    // End Call
    fn compose_command_call_with(&mut self) -> Result<(), SyntaxError> {
        if !self.in_call_with {
            return Err(self.syntax_error("不正なEndステートメントです".into()));
        }

        let name = self.temp_progam_name.take().expect("BUG");
        let arguments = self.temp_call_with_arguments.split_off(0);

        if self.callables.get(&name).expect("BUG").len() != arguments.len() {
            return Err(self.syntax_error("引数が不足しています".into()));
        }

        self.maximum_allocate_temporary_area_size = self
            .maximum_allocate_temporary_area_size
            .max(self.allocate_temporary_area_size);

        self.add_statement(Statement::Call { name, arguments });

        self.in_call_with = false;

        Ok(())
    }

    // End Argument
    fn compose_command_argument(&mut self) -> Result<(), SyntaxError> {
        if !self.header_state.in_argument() {
            return Err(self.syntax_error("不正なEndステートメントです".into()));
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
            if self.can_end_program() {
                self.end_program_state = EndProgramState::Satisfied;
                return Ok(());
            } else {
                return Err(self.syntax_error("不正なEndステートメントです".into()));
            }
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
            _ => return Err(self.syntax_error("不正なEndステートメントです".into())),
        }

        Ok(())
    }

    // End Select
    fn compose_command_select(&mut self) -> Result<(), SyntaxError> {
        let cur_exit_id = self
            .nest_of_select
            .pop()
            .ok_or_else(|| self.syntax_error("不正なEndステートメントです".into()))?;

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
            _ => return Err(self.syntax_error("不正なEndステートメントです".into())),
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
            _ => return Err(self.syntax_error("不正なElseステートメントです".into())),
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
            _ => return Err(self.syntax_error("不正なElseステートメントです".into())),
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
            _ => return Err(self.syntax_error("不正なElse Ifステートメントです".into())),
        }

        let condition = if let [rest @ .., (_, Token::Keyword(Keyword::Then))] = pos_and_tokens {
            self.parse_expr(rest)?
        } else {
            return Err(self.syntax_error("不正なElse Ifステートメントです".into()));
        };

        if !matches!(condition.return_type(), ExprType::Boolean) {
            return Err(self.syntax_error("条件式の型が真理値ではありません".into()));
        }

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
            return Err(self.syntax_error("不正なIfステートメントです".into()));
        };

        if !matches!(condition.return_type(), ExprType::Boolean) {
            return Err(self.syntax_error("条件式の型が真理値ではありません".into()));
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
                    return Err(self.syntax_error("この位置にExit Doは置けません".into()));
                }
            }
            [(_, Token::Keyword(Keyword::For))] => {
                if let Some((exit_id, _)) = self.nest_of_for.last() {
                    let exit_id = *exit_id;
                    self.add_statement(Statement::ExitFor { exit_id });
                } else {
                    return Err(self.syntax_error("この位置にExit Forは置けません".into()));
                }
            }
            [(_, Token::Keyword(Keyword::Sub))] => self.add_statement(Statement::ExitProgram),
            [(_, Token::Keyword(Keyword::Select))] => {
                if let Some(&exit_id) = self.nest_of_select.last() {
                    self.add_statement(Statement::ExitSelect { exit_id });
                } else {
                    return Err(self.syntax_error("この位置にExit Selectは置けません".into()));
                }
            }
            _ => return Err(self.syntax_error("不正なExitステートメントです".into())),
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
            _ => return Err(self.syntax_error("不正なCaseステートメントです".into())),
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
                        return Err(self
                            .syntax_error("Caseステートメントで値の指定に重複があります".into()));
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
                        return Err(self
                            .syntax_error("Caseステートメントで値の指定に重複があります".into()));
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
                        return Err(self
                            .syntax_error("Caseステートメントで値の指定に重複があります".into()));
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
                        _ => {
                            Err(self.syntax_error("Caseステートメントの値の指定が不正です".into()))
                        }
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
                        return Err(self
                            .syntax_error("Caseステートメントで値の指定に重複があります".into()));
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
                        Err(self.syntax_error("Caseステートメントの値の指定が不正です".into()))
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
                        return Err(self
                            .syntax_error("Caseステートメントで値の指定に重複があります".into()));
                    }
                } else {
                    unreachable!("BUG");
                }
                self.provisionals
                    .push(Statement::ProvisionalCaseString { values });
            }
            _ => return Err(self.syntax_error("不正なCaseステートメントです".into())),
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
                    _ => return Err(self.syntax_error("不正なSelectステートメントです".into())),
                }
            }
            [] => return Err(self.syntax_error("不正なSelectステートメントです".into())),
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
                    return Err(self.syntax_error("この位置にContinue Doは置けません".into()));
                }
            }
            [(_, Token::Keyword(Keyword::For))] => {
                if let Some((exit_id, _)) = self.nest_of_for.last() {
                    let exit_id = *exit_id;
                    self.add_statement(Statement::ContinueFor { exit_id });
                } else {
                    return Err(self.syntax_error("この位置にContinue Forは置けません".into()));
                }
            }
            _ => return Err(self.syntax_error("不正なContinueステートメントです".into())),
        }

        Ok(())
    }

    // Loop [{ While / Until } <condition>]
    fn parse_command_loop(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        let cur_exit_id = self.nest_of_do.pop().ok_or_else(|| {
            self.syntax_error("対応するDoステートメントのない不正なLoopステートメントです".into())
        })?;

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
                    return Err(self.syntax_error("不正なLoopステートメントです".into()))
                }
                [(pos, Token::Keyword(Keyword::Until)), rest @ ..] => {
                    let condition = self.parse_expr(rest)?;
                    if !matches!(condition.return_type(), ExprType::Boolean) {
                        return Err(self.syntax_error_pos(
                            *pos,
                            "条件式には真理値式を置く必要があります".into(),
                        ));
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
                        return Err(self.syntax_error_pos(
                            *pos,
                            "条件式には真理値式を置く必要があります".into(),
                        ));
                    }
                    self.add_statement(Statement::DoLoopWhile {
                        exit_id,
                        condition,
                        block,
                    });
                }
                _ => return Err(self.syntax_error("不正なLoopステートメントです".into())),
            }
        } else {
            return Err(self.syntax_error("この位置にLoopステートメントは置けません".into()));
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
                    return Err(self
                        .syntax_error_pos(*pos, "条件式には真理値式を置く必要があります".into()));
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
                    return Err(self
                        .syntax_error_pos(*pos, "条件式には真理値式を置く必要があります".into()));
                }
                Statement::ProvisionalDo {
                    exit_id,
                    until_condition: None,
                    while_condition: Some(condition),
                }
            }
            _ => return Err(self.syntax_error("不正なDoステートメントです".into())),
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
            _ => return Err(self.syntax_error("不正なNextステートメントです".into())),
        };

        let (cur_exit_id, _) = self.nest_of_for.pop().ok_or_else(|| {
            self.syntax_error("対応するForのない不正なNextステートメントです".into())
        })?;

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
                return Err(self.syntax_error("カウンタの変数名が一致しません".into()));
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
            return Err(self.syntax_error("この位置にNextステートメントは置けません".into()));
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
            return Err(self.syntax_error("不正なForステートメントです".into()));
        };

        if self
            .nest_of_for
            .iter()
            .any(|(_, counter)| counter.as_str() == name.as_str())
        {
            return Err(self.syntax_error(format!(
                "同じカウンタ同士のForをネストすることはできません: {}",
                name
            )));
        }

        let counter_is_ref = match self.variables.get(name) {
            Some(VarType::Integer) => false,
            Some(VarType::RefInteger) => true,
            _ => return Err(self.syntax_error(format!("カウンタ名が不正です: {}", name))),
        };

        let to_position = rest
            .iter()
            .enumerate()
            .find(|(_, (_, token))| matches!(token, Token::Keyword(Keyword::To)))
            .map(|(i, _)| i)
            .ok_or_else(|| self.syntax_error("不正なForステートメントです".into()))?;

        let (init, rest) = rest.split_at(to_position);

        let step_position = rest
            .iter()
            .enumerate()
            .find(|(_, (_, token))| matches!(token, Token::Keyword(Keyword::Step)))
            .map_or(rest.len(), |(i, _)| i);

        let (end, step) = rest.split_at(step_position);

        let init = self.parse_expr(init)?;
        if !matches!(init.return_type(), ExprType::Integer) {
            return Err(self.syntax_error("カウンタの初期値の型が不正です".into()));
        }

        let end = self.parse_expr(&end[1..])?;
        if !matches!(end.return_type(), ExprType::Integer) {
            return Err(self.syntax_error("終端値の型が不正です".into()));
        }

        let step = if step.is_empty() {
            None
        } else {
            let step = self.parse_expr(&step[1..])?;
            if matches!(step.return_type(), ExprType::Integer) {
                Some(step)
            } else {
                return Err(self.syntax_error("Stepの値の型が不正です".into()));
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
        if !self.header_state.can_dim() {
            return Err(
                self.syntax_error("この位置にDimステートメントは置くことができません".into())
            );
        }
        match pos_and_tokens {
            // プリミティブ変数の宣言
            [(pos, Token::Name(name)), (_, Token::Keyword(Keyword::As)), (_, Token::TypeName(type_name))] =>
            {
                if self.variables.contains_key(name) {
                    return Err(
                        self.syntax_error_pos(*pos, format!("変数名が重複しています: {}", name))
                    );
                }
                self.variable_area_size += 1;
                let var_type = match type_name {
                    TypeName::Boolean => VarType::Boolean,
                    TypeName::Integer => VarType::Integer,
                    TypeName::String => {
                        self.variable_area_size += 256;
                        VarType::String
                    }
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
                    return Err(
                        self.syntax_error_pos(*pos_n, format!("変数名が重複しています: {}", name))
                    );
                }
                let size = self.parse_declare_array_size(*size).ok_or_else(|| {
                    self.syntax_error_pos(*pos_s, "配列の大きさの指定が不正です".into())
                })?;
                self.variable_area_size += size;
                let var_type = match type_name {
                    TypeName::Boolean => VarType::ArrayOfBoolean(size),
                    TypeName::Integer => VarType::ArrayOfInteger(size),
                    TypeName::String => {
                        return Err(
                            self.syntax_error_pos(*pos_t, "この型は配列にはできません".into())
                        )
                    }
                };
                self.variables.insert(name.clone(), var_type);
                self.add_statement(Statement::Dim {
                    var_name: name.clone(),
                    var_type,
                });
            }
            _ => return Err(self.syntax_error("不正なDimステートメントです".into())),
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
                Some(VarType::Integer) => {
                    self.add_statement(Statement::InputInteger {
                        var_name: name.clone(),
                    });
                }
                Some(VarType::RefInteger) => {
                    self.add_statement(Statement::InputRefInteger {
                        var_name: name.clone(),
                    });
                }
                Some(VarType::String) => {
                    self.add_statement(Statement::InputString {
                        var_name: name.clone(),
                    });
                }
                Some(VarType::RefString) => {
                    self.add_statement(Statement::InputRefString {
                        var_name: name.clone(),
                    });
                }
                Some(_) => {
                    return Err(self.syntax_error_pos(
                        *pos,
                        "Inputステートメントに指定できない型の変数です".into(),
                    ))
                }
                None => {
                    return Err(
                        self.syntax_error_pos(*pos, format!("存在しない変数名です: {}", name))
                    )
                }
            },
            // 整数配列の指定位置への入力
            [(pos, Token::Name(name)), (_, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let is_ref = match self.variables.get(name) {
                    Some(VarType::ArrayOfInteger(_)) => false,
                    Some(VarType::RefArrayOfInteger(_)) => true,
                    _ => {
                        return Err(self.syntax_error_pos(
                            *pos,
                            "要素へのInputステートメントは整数配列でのみ可能です".into(),
                        ))
                    }
                };
                let param = self.parse_expr(inner)?;
                if !matches!(param.return_type(), ExprType::Integer) {
                    return Err(self.syntax_error_pos(*pos, "インデックスの型が不正です".into()));
                }
                if is_ref {
                    self.add_statement(Statement::InputRefElementInteger {
                        var_name: name.clone(),
                        index: param,
                    });
                } else {
                    self.add_statement(Statement::InputElementInteger {
                        var_name: name.clone(),
                        index: param,
                    });
                }
            }
            _ => return Err(self.syntax_error("不正なInputステートメントです".into())),
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
                    return Err(
                        self.syntax_error_pos(*pos, format!("不正な整数リテラルです: {}", *value))
                    );
                }
            }
            // 負符号付きの整数リテラルの出力
            [(_, Token::Operator(Operator::Sub)), (pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(true, *value) {
                    self.add_statement(Statement::PrintLitInteger { value });
                    return Ok(());
                } else {
                    return Err(
                        self.syntax_error_pos(*pos, format!("不正な整数リテラルです: {}", *value))
                    );
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
                _ => return Err(self.syntax_error("不正なPrintステートメントです".into())),
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
                return Err(self.syntax_error("不正なPrintステートメントです".into()))
            }
        }
        Ok(())
    }

    // 式の分解
    fn parse_expr(&self, pos_and_tokens: &[(usize, Token)]) -> Result<Expr, SyntaxError> {
        // 項の最小単位
        match pos_and_tokens {
            // 空の項は存在してはダメ (構文エラー)
            [] => return Err(self.syntax_error("式がありません".into())),

            // 真理値リテラル
            [(_, Token::Boolean(value))] => return Ok(Expr::LitBoolean(*value)),

            // 文字リテラル
            [(_, Token::Character(value))] => return Ok(Expr::LitCharacter(*value)),

            // 整数リテラル
            [(pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(false, *value) {
                    return Ok(Expr::LitInteger(value));
                } else {
                    return Err(
                        self.syntax_error_pos(*pos, format!("不正な整数リテラルです: {}", *value))
                    );
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
                            self.syntax_error_pos(*pos, format!("存在しない変数名です: {}", name))
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
                    return Err(
                        self.syntax_error_pos(*pos, format!("不正な整数リテラルです: -{}", *value))
                    );
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
                    return Err(
                        self.syntax_error_pos(*pos, format!("不正な整数リテラルです: {}", *value))
                    );
                }
            }

            // 単項演算子と変数(真理値or整数)
            [(_, Token::Operator(op)), (pos, Token::Name(name))] if op.can_be_unary() => {
                match self.variables.get(name) {
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
                    Some(_) => return Err(self.syntax_error_pos(*pos, "不正な式です".into())),
                    None => {
                        return Err(
                            self.syntax_error_pos(*pos, format!("存在しない変数名です: {}", name))
                        )
                    }
                }
            }

            // 存在してはいけないトークン列　(構文エラー)
            [(pos, token)] | [(pos, token), (_, _)] => {
                return Err(self.syntax_error_pos(*pos, format!("不正な式です: {:?}", token)))
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
                        return Err(self.syntax_error_pos(*pos, "不正な式です".into()));
                    }
                } else if op.can_be_binary() {
                    if target_op.filter(|(_, cur_op)| cur_op > op).is_none() {
                        target_op = Some((i, *op));
                    }
                    next_unary = true;
                    next_term = true;
                } else {
                    return Err(self.syntax_error_pos(*pos, "不正な式です".into()));
                }
            } else {
                if !next_term {
                    return Err(self.syntax_error_pos(*pos, "不正な式です".into()));
                }
                next_unary = false;
                next_term = false;
            }
        }

        if bracket_count > 0 || next_unary || next_term {
            return Err(self.syntax_error("不正な式です".into()));
        }

        // 分割して再帰的に処理していく
        if let Some((i, op)) = target_op {
            let lhs = self.parse_expr(&pos_and_tokens[..i])?;
            let rhs = self.parse_expr(&pos_and_tokens[i + 1..])?;
            return Expr::binary(op, lhs, rhs).ok_or_else(|| {
                let (pos, _) = pos_and_tokens[i];
                self.syntax_error_pos(pos, "不正な式です".into())
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
                    _ => Err(self.syntax_error_pos(*pos, "不正な式です".into())),
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
                    Err(self.syntax_error_pos(*pos, "不正な式です".into()))
                }
            }

            // 配列変数 ( 添え字 )  もしくは  文字列変数 ( 添え字 )
            [(pos_n, Token::Name(name)), (pos_e, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let expr = self.parse_expr(inner)?;
                if !matches!(expr.return_type(), ExprType::Integer) {
                    Err(self.syntax_error_pos(*pos_e, "不正な式です".into()))
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
                        Some(_) => Err(self.syntax_error_pos(*pos_n, "不正な式です".into())),
                        None => Err(self
                            .syntax_error_pos(*pos_n, format!("存在しない変数名です: {}", name))),
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
                    Err(self.syntax_error_pos(*pos, "不正な式です".into()))
                }
            }

            // 関数 Array ( 引数 )
            [(_, Token::Function(Function::Array)), (pos, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let param = self.parse_expr(inner)?;
                match &param {
                    Expr::ParamList(list) if (1..=MAX_ARRAY_SIZE).contains(&list.len()) => {
                        let size = list.len();
                        if list
                            .iter()
                            .all(|expr| matches!(expr.return_type(), ExprType::Boolean))
                        {
                            return Ok(Expr::FunctionBooleanArray(
                                size,
                                Function::Array,
                                Box::new(param),
                            ));
                        } else if list
                            .iter()
                            .all(|expr| matches!(expr.return_type(), ExprType::Integer))
                        {
                            return Ok(Expr::FunctionIntegerArray(
                                size,
                                Function::Array,
                                Box::new(param),
                            ));
                        }
                    }
                    _ if matches!(param.return_type(), ExprType::Boolean) => {
                        return Ok(Expr::FunctionBooleanArray(
                            1,
                            Function::Array,
                            Box::new(param),
                        ));
                    }
                    _ if matches!(param.return_type(), ExprType::Integer) => {
                        return Ok(Expr::FunctionIntegerArray(
                            1,
                            Function::Array,
                            Box::new(param),
                        ));
                    }
                    _ => {}
                }
                Err(self.syntax_error_pos(*pos, "不正な式です".into()))
            }

            // 関数 SubArray ( 引数 )
            [(_, Token::Function(Function::SubArray)), (pos, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let mut param = self.parse_expr(inner)?;
                if let Expr::ParamList(list) = &mut param {
                    match list.as_mut_slice() {
                        [Expr::FunctionBooleanArray(size, ..), offset, Expr::LitInteger(len)]
                        | [Expr::ReferenceOfVar(_, VarType::ArrayOfBoolean(size)), offset, Expr::LitInteger(len)]
                        | [Expr::ReferenceOfVar(_, VarType::RefArrayOfBoolean(size)), offset, Expr::LitInteger(len)] => {
                            if matches!(offset.return_type(), ExprType::Integer) {
                                if let Some(size) = self.parse_size_for_array_function(*size, *len)
                                {
                                    *len = size as i32;
                                    return Ok(Expr::FunctionBooleanArray(
                                        size,
                                        Function::SubArray,
                                        Box::new(param),
                                    ));
                                }
                            }
                        }
                        [Expr::FunctionIntegerArray(size, ..), offset, Expr::LitInteger(len)]
                        | [Expr::ReferenceOfVar(_, VarType::ArrayOfInteger(size)), offset, Expr::LitInteger(len)]
                        | [Expr::ReferenceOfVar(_, VarType::RefArrayOfInteger(size)), offset, Expr::LitInteger(len)] => {
                            if matches!(offset.return_type(), ExprType::Integer) {
                                if let Some(size) = self.parse_size_for_array_function(*size, *len)
                                {
                                    *len = size as i32;
                                    return Ok(Expr::FunctionIntegerArray(
                                        size,
                                        Function::SubArray,
                                        Box::new(param),
                                    ));
                                }
                            }
                        }
                        [expr, offset, Expr::LitInteger(_)]
                            if matches!(offset.return_type(), ExprType::Integer)
                                && (expr.return_type().is_bool_array()
                                    || expr.return_type().is_int_array()) =>
                        {
                            unreachable!("BUG")
                        }
                        _ => {}
                    }
                }
                Err(self.syntax_error_pos(*pos, "不正な式です".into()))
            }

            // 関数 CArray ( 引数 )
            [(_, Token::Function(Function::CArray)), (pos, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let mut param = self.parse_expr(inner)?;
                if let Expr::ParamList(list) = &mut param {
                    if let [expr, Expr::LitInteger(len)] = list.as_mut_slice() {
                        if let Some(size) = self.parse_size_for_array_function(MAX_ARRAY_SIZE, *len)
                        {
                            if expr.return_type().is_bool_array() {
                                *len = size as i32;
                                return Ok(Expr::FunctionBooleanArray(
                                    size,
                                    Function::CArray,
                                    Box::new(param),
                                ));
                            } else if matches!(expr.return_type(), ExprType::String)
                                || expr.return_type().is_int_array()
                            {
                                *len = size as i32;
                                return Ok(Expr::FunctionIntegerArray(
                                    size,
                                    Function::CArray,
                                    Box::new(param),
                                ));
                            }
                        }
                    }
                }
                Err(self.syntax_error_pos(*pos, "不正な式です".into()))
            }

            // 関数 String( 引数 )
            [(_, Token::TypeName(TypeName::String)), (pos, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let param = self.parse_expr(inner)?;
                if let Expr::ParamList(list) = &param {
                    if list.len() == 2
                        && list
                            .iter()
                            .all(|expr| matches!(expr.return_type(), ExprType::Integer))
                    {
                        return Ok(Expr::FunctionString(Function::String, Box::new(param)));
                    }
                } else if param.return_type().is_int_array() {
                    return Ok(Expr::FunctionString(Function::String, Box::new(param)));
                }
                Err(self.syntax_error_pos(*pos, "不正な式です".into()))
            }

            // 関数 ( 引数 )
            [(_, Token::Function(function)), (pos, Token::Operator(Operator::OpenBracket)), inner @ .., (_, Token::Operator(Operator::CloseBracket))] =>
            {
                let param = self.parse_expr(inner)?;
                if !function.check_param(&param) {
                    Err(self.syntax_error_pos(*pos, "不正な式です".into()))
                } else {
                    match function.return_type() {
                        ExprType::Boolean => Ok(Expr::FunctionBoolean(*function, Box::new(param))),
                        ExprType::Integer => Ok(Expr::FunctionInteger(*function, Box::new(param))),
                        ExprType::String => Ok(Expr::FunctionString(*function, Box::new(param))),
                        _ => Err(self.syntax_error_pos(*pos, "不正な式です".into())),
                    }
                }
            }

            // 該当なし (構文エラー)
            _ => Err(self.syntax_error("不正な式です".into())),
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
            | Extern | Dim | Do | Fill | For | If | Input | Loop | Mid | Next | Option | Print
            | Select | Sub => true,

            As | From | Rem | Step | Then | To | Until | While | With => false,
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

            ShiftLeftArithmetic | ShiftRightArithmetic | ShiftLeftLogical | ShiftRightLogical => 4,

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
            VarType::ArrayOfBoolean(size1) | VarType::RefArrayOfBoolean(size1) => {
                match expr.return_type() {
                    ExprType::ReferenceOfVar(VarType::ArrayOfBoolean(size2))
                    | ExprType::ReferenceOfVar(VarType::RefArrayOfBoolean(size2)) => size1 == size2,
                    _ => false,
                }
            }
            VarType::ArrayOfInteger(size1) | VarType::RefArrayOfInteger(size1) => {
                match expr.return_type() {
                    ExprType::ReferenceOfVar(VarType::ArrayOfInteger(size2))
                    | ExprType::ReferenceOfVar(VarType::RefArrayOfInteger(size2)) => size1 == size2,
                    _ => false,
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum CompileOption {
    ArraySize {
        length: bool,
        all: bool,
    },
    Eof {
        common: bool,
    },
    Register {
        restore: bool,
    },
    Allocator {
        enabled: bool,
        common: bool,
        size: usize,
    },
    Variable {
        initialize: bool,
    },
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement {
    CompileOption {
        option: CompileOption,
    },
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
    AssignBooleanArray {
        var_name: String,
        value: Expr,
    },
    AssignRefBooleanArray {
        var_name: String,
        value: Expr,
    },
    AssignIntegerArray {
        var_name: String,
        value: Expr,
    },
    AssignRefIntegerArray {
        var_name: String,
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
    InputRefElementInteger {
        var_name: String,
        index: Expr,
    },
    InputRefInteger {
        var_name: String,
    },
    InputRefString {
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
    FillArrayOfBoolean {
        var_name: String,
        value: Expr,
    },
    FillRefArrayOfBoolean {
        var_name: String,
        value: Expr,
    },
    FillArrayOfInteger {
        var_name: String,
        value: Expr,
    },
    FillRefArrayOfInteger {
        var_name: String,
        value: Expr,
    },
    FillString {
        var_name: String,
        value: Expr,
    },
    FillRefString {
        var_name: String,
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
    ReferenceOfVar(VarType), // 現状は配列タイプのみ
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
    FunctionBooleanArray(usize, Function, Box<Expr>),
    FunctionIntegerArray(usize, Function, Box<Expr>),
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
    VarArrayOfBoolean(String, Box<Expr>), // 命名が悪い、配列の要素を返すExpr, (変数名,インデックス)
    VarArrayOfInteger(String, Box<Expr>), // 同上
    VarRefArrayOfBoolean(String, Box<Expr>), // 同上
    VarRefArrayOfInteger(String, Box<Expr>), // 同上
    ReferenceOfVar(String, VarType),      // 現状、配列への参照を持つ用 (変数名,型)
    ParamList(Vec<Expr>),
}

impl VarType {
    pub fn is_reference(&self) -> bool {
        use VarType::*;
        match self {
            Boolean | Integer | String | ArrayOfBoolean(..) | ArrayOfInteger(..) => false,

            RefBoolean | RefInteger | RefString | RefArrayOfBoolean(..) | RefArrayOfInteger(..) => {
                true
            }
        }
    }

    pub fn get_array_size(&self) -> Option<usize> {
        use VarType::*;
        match self {
            Boolean | Integer | String | RefBoolean | RefInteger | RefString => None,

            ArrayOfBoolean(size)
            | ArrayOfInteger(size)
            | RefArrayOfBoolean(size)
            | RefArrayOfInteger(size) => Some(*size),
        }
    }

    pub fn is_boolean(&self) -> bool {
        use VarType::*;
        match self {
            Boolean | RefBoolean => true,

            Integer
            | String
            | RefInteger
            | RefString
            | ArrayOfBoolean(..)
            | ArrayOfInteger(..)
            | RefArrayOfBoolean(..)
            | RefArrayOfInteger(..) => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        use VarType::*;
        match self {
            Integer | RefInteger => true,

            Boolean
            | RefBoolean
            | String
            | RefString
            | ArrayOfBoolean(..)
            | ArrayOfInteger(..)
            | RefArrayOfBoolean(..)
            | RefArrayOfInteger(..) => false,
        }
    }

    pub fn is_string(&self) -> bool {
        use VarType::*;
        match self {
            String | RefString => true,

            Boolean
            | RefBoolean
            | Integer
            | RefInteger
            | ArrayOfBoolean(..)
            | ArrayOfInteger(..)
            | RefArrayOfBoolean(..)
            | RefArrayOfInteger(..) => false,
        }
    }
}

impl ExprType {
    pub fn match_for_bin_op(&self, other: &Self) -> bool {
        match self {
            Self::ReferenceOfVar(VarType::ArrayOfBoolean(size1))
            | Self::ReferenceOfVar(VarType::RefArrayOfBoolean(size1)) => match other {
                Self::ReferenceOfVar(VarType::ArrayOfBoolean(size2))
                | Self::ReferenceOfVar(VarType::RefArrayOfBoolean(size2)) => size1 == size2,
                _ => false,
            },

            Self::ReferenceOfVar(VarType::ArrayOfInteger(size1))
            | Self::ReferenceOfVar(VarType::RefArrayOfInteger(size1)) => match other {
                Self::ReferenceOfVar(VarType::ArrayOfInteger(size2))
                | Self::ReferenceOfVar(VarType::RefArrayOfInteger(size2)) => size1 == size2,
                _ => false,
            },

            Self::Boolean => matches!(other, Self::Boolean),
            Self::Integer => matches!(other, Self::Integer),
            Self::String => matches!(other, Self::String),

            Self::ParamList | Self::ReferenceOfVar(..) => false,
        }
    }

    pub fn is_bool_array(&self) -> bool {
        match self {
            Self::ReferenceOfVar(VarType::ArrayOfBoolean(_))
            | Self::ReferenceOfVar(VarType::RefArrayOfBoolean(_)) => true,

            Self::Boolean
            | Self::Integer
            | Self::String
            | Self::ParamList
            | Self::ReferenceOfVar(..) => false,
        }
    }

    pub fn is_int_array(&self) -> bool {
        match self {
            Self::ReferenceOfVar(VarType::ArrayOfInteger(_))
            | Self::ReferenceOfVar(VarType::RefArrayOfInteger(_)) => true,

            Self::Boolean
            | Self::Integer
            | Self::String
            | Self::ParamList
            | Self::ReferenceOfVar(..) => false,
        }
    }
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

            // カンマ以外の演算子で左辺と右辺の型が非互換なのはシンタックスエラー
            _ if !lhs.return_type().match_for_bin_op(&rhs.return_type()) => return None,

            // 整数を受け取って整数を返すもの
            ShiftLeftArithmetic | ShiftRightArithmetic | ShiftLeftLogical | ShiftRightLogical
            | Mul | Div | Mod | Add | Sub
                if matches!(lhs.return_type(), ExprType::Integer) =>
            {
                Expr::BinaryOperatorInteger
            }

            // 文字列を受け取って文字列を返すもの
            Concat if matches!(lhs.return_type(), ExprType::String) => Expr::BinaryOperatorString,

            // 整数または文字列または配列を受け取って真理値を返すもの
            LessOrEequal | GreaterOrEqual | LessThan | GreaterThan
                if matches!(lhs.return_type(), ExprType::Integer | ExprType::String)
                    || lhs.return_type().is_int_array() =>
            {
                Expr::BinaryOperatorBoolean
            }

            // 真理値または整数または文字列または配列を受け取って真理値を返すもの
            Equal | NotEqual
                if matches!(lhs.return_type(), ExprType::Boolean)
                    || matches!(lhs.return_type(), ExprType::Integer)
                    || matches!(lhs.return_type(), ExprType::String)
                    || lhs.return_type().is_bool_array()
                    || lhs.return_type().is_int_array() =>
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
            ShiftLeftArithmetic | ShiftRightArithmetic | ShiftLeftLogical | ShiftRightLogical
            | Mul | Div | Mod | Add | Sub | Concat | LessOrEequal | GreaterOrEqual | LessThan
            | GreaterThan | Equal | NotEqual | And | Or | Xor => return None,
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

            FunctionBooleanArray(size, ..) => {
                ExprType::ReferenceOfVar(VarType::ArrayOfBoolean(*size))
            }

            FunctionIntegerArray(size, ..) => {
                ExprType::ReferenceOfVar(VarType::ArrayOfInteger(*size))
            }

            ReferenceOfVar(_, var_type) => ExprType::ReferenceOfVar(*var_type),
        }
    }

    pub fn can_access_variable(&self) -> bool {
        use Expr::*;
        match self {
            BinaryOperatorBoolean(..)
            | BinaryOperatorInteger(..)
            | BinaryOperatorString(..)
            | FunctionBoolean(..)
            | FunctionInteger(..)
            | FunctionString(..)
            | FunctionBooleanArray(..)
            | FunctionIntegerArray(..)
            | LitBoolean(..)
            | LitInteger(..)
            | LitCharacter(..)
            | LitString(..)
            | CharOfLitString(..)
            | UnaryOperatorBoolean(..)
            | UnaryOperatorInteger(..)
            | ParamList(..) => false,

            CharOfVarString(..)
            | CharOfVarRefString(..)
            | VarBoolean(..)
            | VarRefBoolean(..)
            | VarArrayOfBoolean(..)
            | VarRefArrayOfBoolean(..)
            | VarInteger(..)
            | VarRefInteger(..)
            | VarArrayOfInteger(..)
            | VarRefArrayOfInteger(..)
            | VarString(..)
            | VarRefString(..)
            | ReferenceOfVar(..) => true,
        }
    }
}

impl Function {
    fn return_type(&self) -> ExprType {
        use Function::*;
        match self {
            CBool | Eof => ExprType::Boolean,
            Abs | Asc | CInt | Len | Max | Min => ExprType::Integer,
            Chr | CStr | Mid | Space | String => ExprType::String,
            Array | CArray | SubArray => unreachable!("BUG"),
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
            Asc => matches!(param.return_type(), ExprType::String),

            // 引数は文字列1個または配列
            Len => {
                matches!(param.return_type(), ExprType::String)
                    || param.return_type().is_bool_array()
                    || param.return_type().is_int_array()
            }

            // 引数は整数配列か整数２個
            String => {
                if let Expr::ParamList(list) = param {
                    list.len() == 2
                        && list
                            .iter()
                            .all(|expr| matches!(expr.return_type(), ExprType::Integer))
                } else {
                    param.return_type().is_int_array()
                }
            }

            Array | CArray | SubArray => unreachable!("BUG"),
        }
    }
}

impl std::fmt::Display for ArgumentInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self.var_type {
            VarType::Boolean => {
                format!("ByVal {} As Boolean [{}]", self.var_name, self.register1)
            }
            VarType::Integer => {
                format!("ByVal {} As Integer [{}]", self.var_name, self.register1)
            }
            VarType::String => format!(
                "ByVal {} As String [{},{}]",
                self.var_name,
                self.register1,
                self.register2.expect("BUG")
            ),
            VarType::ArrayOfBoolean(size) => format!(
                "ByVal {}({}) As Boolean [{}]",
                self.var_name,
                size - 1,
                self.register1
            ),
            VarType::ArrayOfInteger(size) => format!(
                "ByVal {}({}) As Integer [{}]",
                self.var_name,
                size - 1,
                self.register1
            ),
            VarType::RefBoolean => {
                format!("ByRef {} As Boolean [{}]", self.var_name, self.register1)
            }
            VarType::RefInteger => {
                format!("ByRef {} As Integer [{}]", self.var_name, self.register1)
            }
            VarType::RefString => format!(
                "ByRef {} As String [{},{}]",
                self.var_name,
                self.register1,
                self.register2.expect("BUG")
            ),
            VarType::RefArrayOfBoolean(size) => format!(
                "ByRef {}({}) As Boolean [{}]",
                self.var_name,
                size - 1,
                self.register1
            ),
            VarType::RefArrayOfInteger(size) => format!(
                "ByRef {}({}) As Integer [{}]",
                self.var_name,
                size - 1,
                self.register1
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
            | FunctionBooleanArray(_, func, param)
            | FunctionIntegerArray(_, func, param)
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
            | FunctionString(func, param)
            | FunctionBooleanArray(_, func, param)
            | FunctionIntegerArray(_, func, param) => {
                format!("{}({})", func.to_string(), param.to_string()).fmt(f)
            }
            LitBoolean(lit) => (if *lit { "True" } else { "False" }).fmt(f),
            LitInteger(lit) => lit.fmt(f),
            LitString(lit) => format!(r#""{}""#, lit.replace('"', r#""""#)).fmt(f),
            LitCharacter('"') => r#"""""c"#.fmt(f),
            LitCharacter(lit) => format!(r#""{}"c"#, lit).fmt(f),
            UnaryOperatorInteger(op, value) | UnaryOperatorBoolean(op, value) => {
                format!("{}({})", op.to_string(), value.to_string()).fmt(f)
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
