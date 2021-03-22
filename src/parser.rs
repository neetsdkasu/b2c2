use crate::tokenizer::*;
use crate::SyntaxError;
use std::collections::HashMap;
use std::io::{self, BufRead};

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
            Some((pos, Token::Keyword(keyword))) if keyword.is_command() => {
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

struct Parser {
    line_number: usize,
    line_start_position: usize,
    variables: HashMap<String, VarType>,
    statements: Vec<Vec<Statement>>,
    nest_of_do: Vec<usize>,
    nest_of_for: Vec<usize>,
    nest_of_select: Vec<usize>,
    provisionals: Vec<Statement>,
    exit_id: usize,
    is_select_head: bool,
    is_dim_header: bool,
}

impl Parser {
    fn new() -> Self {
        Self {
            line_number: 0,
            line_start_position: 0,
            variables: HashMap::new(),
            statements: vec![vec![]; 1],
            nest_of_do: vec![],
            nest_of_for: vec![],
            nest_of_select: vec![],
            provisionals: vec![],
            exit_id: 0,
            is_select_head: false,
            is_dim_header: true,
        }
    }

    fn is_valid(&self) -> bool {
        self.statements.len() == 1
            && self.nest_of_do.is_empty()
            && self.nest_of_for.is_empty()
            && self.nest_of_select.is_empty()
            && self.provisionals.is_empty()
            && !self.is_select_head
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
        if self.is_select_head {
            return Err(self.syntax_error("invalid Code statement".into()));
        }
        if self.is_dim_header {
            self.is_dim_header = false;
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
                    VarType::Boolean | VarType::Integer | VarType::String
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
                    ExprType::Integer if matches!(var_type, VarType::Integer) => {
                        self.add_statement(Statement::AssignInteger {
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
            // 配列要素または文字列要素に代入
            [(_, Token::Operator(Operator::OpenBracket)), ..]
                if matches!(
                    var_type,
                    VarType::String | VarType::ArrayOfBoolean(_) | VarType::ArrayOfInteger(_)
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
                        self.add_statement(Statement::AssignElement {
                            var_name: name.into(),
                            index: param,
                            value: expr,
                        });
                    }
                    ExprType::Integer
                        if matches!(var_type, VarType::String | VarType::ArrayOfInteger(_)) =>
                    {
                        self.add_statement(Statement::AssignElement {
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
            _ => return Err(self.syntax_error("invalid Assign statement".into())),
        }

        Ok(())
    }

    // Command
    fn parse_command(
        &mut self,
        command: &Keyword,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        if self.is_dim_header && !matches!(command, Keyword::Dim) {
            self.is_dim_header = false;
        }
        match command {
            Keyword::Case => self.parse_command_case(pos_and_tokens),
            _ if self.is_select_head => Err(self.syntax_error("invalid Code statement".into())),
            Keyword::Continue => self.parse_command_continue(pos_and_tokens),
            Keyword::Dim if self.is_dim_header => self.parse_command_dim(pos_and_tokens),
            Keyword::Dim => Err(self.syntax_error("invalid Dim statement".into())),
            Keyword::Do => self.parse_command_do(pos_and_tokens),
            Keyword::Else => self.parse_command_else(pos_and_tokens),
            Keyword::ElseIf => self.parse_command_elseif(pos_and_tokens),
            Keyword::End => self.parse_command_end(pos_and_tokens),
            Keyword::Exit => self.parse_command_exit(pos_and_tokens),
            Keyword::For => self.parse_command_for(pos_and_tokens),
            Keyword::If => self.parse_command_if(pos_and_tokens),
            Keyword::Input => self.parse_command_input(pos_and_tokens),
            Keyword::Let => {
                if let [(_, Token::Name(name)), rest @ ..] = pos_and_tokens {
                    self.parse_assign(name, rest)
                } else {
                    Err(self.syntax_error("invalid Let statement".into()))
                }
            }
            Keyword::Loop => self.parse_command_loop(pos_and_tokens),
            Keyword::Next => self.parse_command_next(pos_and_tokens),
            Keyword::Print => self.parse_command_print(pos_and_tokens),
            Keyword::Select => self.parse_command_select(pos_and_tokens),
            _ if command.is_command() => unreachable!("BUG"),
            _ => Err(self.syntax_error("invalid Code statement".into())),
        }
    }

    // End { If / Select }
    fn parse_command_end(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [(_, Token::Keyword(Keyword::If))] => self.compose_command_if(),
            [(_, Token::Keyword(Keyword::Select))] => self.compose_command_select(),
            _ => Err(self.syntax_error("invalid End statement".into())),
        }
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

    // Exit { Do / For / Select }
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
                if let Some(&exit_id) = self.nest_of_for.last() {
                    self.add_statement(Statement::ExitFor { exit_id });
                } else {
                    return Err(self.syntax_error("invalid Exit statement".into()));
                }
            }
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

    // Case <integer> [, <integer>]*
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
            Expr::LitInteger(value) => {
                if let Some(Statement::SelectInteger { case_blocks, .. }) = self.provisionals.last()
                {
                    let found_duplicate = case_blocks.iter().any(|s| {
                        if let Statement::CaseInteger { values, .. } = s {
                            values.iter().any(|v| *v == value)
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
                    values: vec![value],
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
                let values = values.into_iter().try_fold(vec![], |mut acc, expr| {
                    if let Expr::LitInteger(value) = expr {
                        acc.push(value);
                        Ok(acc)
                    } else {
                        Err(self.syntax_error("invalid Case statement".into()))
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

        let statement = if let [(_, Token::Keyword(Keyword::Case)), rest @ ..] = pos_and_tokens {
            let value = self.parse_expr(rest)?;
            match value.return_type() {
                ExprType::Integer => Statement::ProvisionalSelectInteger { exit_id, value },
                ExprType::String => Statement::ProvisionalSelectString { exit_id, value },
                _ => return Err(self.syntax_error("invalid Select statement".into())),
            }
        } else {
            return Err(self.syntax_error("invalid Select statement".into()));
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
                if let Some(&exit_id) = self.nest_of_for.last() {
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

        let cur_exit_id = self
            .nest_of_for
            .pop()
            .ok_or_else(|| self.syntax_error("invalid Next statement".into()))?;

        let block = self.statements.pop().expect("BUG");

        if let Some(Statement::ProvisionalFor {
            exit_id,
            counter,
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

        if !matches!(self.variables.get(name), Some(VarType::Integer)) {
            return Err(self.syntax_error("invalid For statement".into()));
        }

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
        self.nest_of_for.push(exit_id);
        self.statements.push(Vec::new());
        self.provisionals.push(Statement::ProvisionalFor {
            exit_id,
            counter: name.clone(),
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
                if !(0..=255).contains(size) {
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
                Some(VarType::Integer) => {
                    self.add_statement(Statement::InputInteger {
                        var_name: name.clone(),
                    });
                }
                Some(VarType::String) => {
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
                if !matches!(self.variables.get(name), Some(VarType::ArrayOfInteger(_))) {
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
                Some(VarType::Boolean) => {
                    self.add_statement(Statement::PrintVarBoolean {
                        var_name: name.clone(),
                    });
                    return Ok(());
                }
                Some(VarType::Integer) => {
                    self.add_statement(Statement::PrintVarInteger {
                        var_name: name.clone(),
                    });
                    return Ok(());
                }
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
            ExprType::ParamList => {
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
            [(pos, Token::Name(name))] => match self.variables.get(name) {
                Some(VarType::Boolean) => return Ok(Expr::VarBoolean(name.clone())),
                Some(VarType::Integer) => return Ok(Expr::VarInteger(name.clone())),
                Some(VarType::String) => return Ok(Expr::VarString(name.clone())),
                Some(_) => {
                    return Err(
                        self.syntax_error_pos(*pos, "invalid Array Variable in Expression".into())
                    )
                }
                None => {
                    return Err(self.syntax_error_pos(*pos, format!("undefined variable: {}", name)))
                }
            },

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
                Some(VarType::Integer) if op.can_be_unary_integer() => {
                    return Ok(Expr::UnaryOperatorInteger(
                        *op,
                        Box::new(Expr::VarInteger(name.clone())),
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

        // 外側の演算子のうち一番左のを抽出する (括弧のネストに気をつける)
        //    項 op1 項 op2 項 op1 ... op2 項
        //   -> (項 op1 項) op2 (項 op1 ... op2 項)
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
                    if target_op.filter(|(_, cur_op)| cur_op >= op).is_none() {
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
                        Some(VarType::ArrayOfInteger(_)) => {
                            Ok(Expr::VarArrayOfInteger(name.clone(), Box::new(expr)))
                        }
                        Some(VarType::String) => {
                            Ok(Expr::CharOfVarString(name.clone(), Box::new(expr)))
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
            [(_, Token::Function(_function)), (pos, Token::Operator(Operator::OpenBracket)), (_, Token::Operator(Operator::CloseBracket))] => {
                Err(self.syntax_error_pos(*pos, "invalid Expression".into()))
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
    fn is_command(&self) -> bool {
        use Keyword::*;
        match self {
            Case | Continue | Else | ElseIf | End | Exit | Dim | Do | For | If | Input | Let
            | Loop | Next | Print | Select => true,

            As | Step | Then | To | Until | While => false,
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
pub enum Statement {
    AssignAddInto {
        var_name: String,
        value: Expr,
    },
    AssignAddIntoElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignBoolean {
        var_name: String,
        value: Expr,
    },
    AssignElement {
        var_name: String,
        index: Expr,
        value: Expr,
    },
    AssignInteger {
        var_name: String,
        value: Expr,
    },
    AssignString {
        var_name: String,
        value: Expr,
    },
    AssignSubInto {
        var_name: String,
        value: Expr,
    },
    AssignSubIntoElement {
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
        init: Expr,
        end: Expr,
        step: Option<Expr>,
        block: Vec<Statement>,
    },
    ProvisionalFor {
        exit_id: usize,
        counter: String,
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
        values: Vec<i32>,
        block: Vec<Statement>,
    },
    ProvisionalCaseInteger {
        values: Vec<i32>,
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
    PrintVarBoolean {
        var_name: String,
    },
    PrintVarInteger {
        var_name: String,
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
pub enum VarType {
    Boolean,
    Integer,
    String,
    ArrayOfBoolean(usize),
    ArrayOfInteger(usize),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ExprType {
    Boolean,
    Integer,
    String,
    ParamList,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expr {
    BinaryOperatorBoolean(Operator, Box<Expr>, Box<Expr>),
    BinaryOperatorInteger(Operator, Box<Expr>, Box<Expr>),
    BinaryOperatorString(Operator, Box<Expr>, Box<Expr>),
    CharOfLitString(String, Box<Expr>),
    CharOfVarString(String, Box<Expr>),
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
    VarArrayOfBoolean(String, Box<Expr>),
    VarArrayOfInteger(String, Box<Expr>),
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

    fn return_type(&self) -> ExprType {
        use Expr::*;
        match self {
            BinaryOperatorBoolean(_, _, _)
            | FunctionBoolean(_, _)
            | LitBoolean(_)
            | UnaryOperatorBoolean(_, _)
            | VarBoolean(_)
            | VarArrayOfBoolean(_, _) => ExprType::Boolean,

            BinaryOperatorInteger(_, _, _)
            | FunctionInteger(_, _)
            | LitInteger(_)
            | LitCharacter(_)
            | UnaryOperatorInteger(_, _)
            | VarInteger(_)
            | VarArrayOfInteger(_, _)
            | CharOfLitString(_, _)
            | CharOfVarString(_, _) => ExprType::Integer,

            BinaryOperatorString(_, _, _) | FunctionString(_, _) | LitString(_) | VarString(_) => {
                ExprType::String
            }

            ParamList(_) => ExprType::ParamList,
        }
    }
}

impl Function {
    fn return_type(&self) -> ExprType {
        use Function::*;
        match self {
            CBool => ExprType::Boolean,
            CInt | Len | Max | Min => ExprType::Integer,
            CStr => ExprType::String,
        }
    }

    fn check_param(&self, param: &Expr) -> bool {
        use Function::*;
        match self {
            CBool => matches!(param.return_type(), ExprType::Integer),
            CInt => matches!(param.return_type(), ExprType::Boolean | ExprType::String),
            CStr => matches!(param.return_type(), ExprType::Boolean | ExprType::Integer),
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
            Len => matches!(param.return_type(), ExprType::String),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io;

    #[test]
    fn parse_works() -> io::Result<()> {
        let src1 = io::Cursor::new(SRC1);
        let statements1 = parse(src1)?.unwrap();
        assert_eq!(statements1, get_src1_statements());

        let src2 = io::Cursor::new(SRC2);
        let statements2 = parse(src2)?.unwrap();
        assert_eq!(statements2, get_src2_statements());

        Ok(())
    }

    #[test]
    fn parser_parse_expr_works() {
        let mut parser = Parser::new();

        let bool_var1 = "boolVar1";
        let int_var1 = "intVar1";
        let str_var1 = "strVar1";
        let int_arr1 = "intArr1";

        [
            (bool_var1, VarType::Boolean),
            (int_var1, VarType::Integer),
            (str_var1, VarType::String),
            (int_arr1, VarType::ArrayOfInteger(5)),
        ]
        .iter()
        .for_each(|(v, t)| {
            parser.variables.insert(v.to_string(), *t);
        });

        let src = vec![
            // True
            (vec![(1, Token::Boolean(true))], Expr::LitBoolean(true)),
            // 1234
            (vec![(1, Token::Integer(1234))], Expr::LitInteger(1234)),
            // "text"
            (
                vec![(1, Token::String("text".into()))],
                Expr::LitString("text".into()),
            ),
            // boolVar1
            (
                vec![(1, Token::Name(bool_var1.into()))],
                Expr::VarBoolean(bool_var1.into()),
            ),
            // intVar1
            (
                vec![(1, Token::Name(int_var1.into()))],
                Expr::VarInteger(int_var1.into()),
            ),
            // strVar1
            (
                vec![(1, Token::Name(str_var1.into()))],
                Expr::VarString(str_var1.into()),
            ),
            // Not true
            (
                vec![
                    (1, Token::Operator(Operator::Not)),
                    (2, Token::Boolean(true)),
                ],
                Expr::UnaryOperatorBoolean(Operator::Not, Box::new(Expr::LitBoolean(true))),
            ),
            // -1234
            (
                vec![
                    (1, Token::Operator(Operator::Sub)),
                    (2, Token::Integer(1234)),
                ],
                Expr::LitInteger(-1234),
            ),
            // -&h8000
            (
                vec![
                    (1, Token::Operator(Operator::Sub)),
                    (2, Token::Integer(0x8000)),
                ],
                Expr::LitInteger(-0x8000),
            ),
            // Not 1234
            (
                vec![
                    (1, Token::Operator(Operator::Not)),
                    (2, Token::Integer(1234)),
                ],
                Expr::UnaryOperatorInteger(Operator::Not, Box::new(Expr::LitInteger(1234))),
            ),
            // Not boolVar1
            (
                vec![
                    (1, Token::Operator(Operator::Not)),
                    (2, Token::Name(bool_var1.into())),
                ],
                Expr::UnaryOperatorBoolean(
                    Operator::Not,
                    Box::new(Expr::VarBoolean(bool_var1.into())),
                ),
            ),
            // Not intVar1
            (
                vec![
                    (1, Token::Operator(Operator::Not)),
                    (2, Token::Name(int_var1.into())),
                ],
                Expr::UnaryOperatorInteger(
                    Operator::Not,
                    Box::new(Expr::VarInteger(int_var1.into())),
                ),
            ),
            // -intVar1
            (
                vec![
                    (1, Token::Operator(Operator::Sub)),
                    (2, Token::Name(int_var1.into())),
                ],
                Expr::UnaryOperatorInteger(
                    Operator::Sub,
                    Box::new(Expr::VarInteger(int_var1.into())),
                ),
            ),
            // 123 + 456
            (
                vec![
                    (1, Token::Integer(123)),
                    (2, Token::Operator(Operator::Add)),
                    (3, Token::Integer(456)),
                ],
                Expr::BinaryOperatorInteger(
                    Operator::Add,
                    Box::new(Expr::LitInteger(123)),
                    Box::new(Expr::LitInteger(456)),
                ),
            ),
            // -123 + -456
            (
                vec![
                    (1, Token::Operator(Operator::Sub)),
                    (2, Token::Integer(123)),
                    (3, Token::Operator(Operator::Add)),
                    (4, Token::Operator(Operator::Sub)),
                    (5, Token::Integer(456)),
                ],
                Expr::BinaryOperatorInteger(
                    Operator::Add,
                    Box::new(Expr::LitInteger(-123)),
                    Box::new(Expr::LitInteger(-456)),
                ),
            ),
            // 12 + -34 * 56
            (
                vec![
                    (1, Token::Integer(12)),
                    (2, Token::Operator(Operator::Add)),
                    (3, Token::Operator(Operator::Sub)),
                    (4, Token::Integer(34)),
                    (5, Token::Operator(Operator::Mul)),
                    (6, Token::Integer(56)),
                ],
                Expr::BinaryOperatorInteger(
                    Operator::Add,
                    Box::new(Expr::LitInteger(12)),
                    Box::new(Expr::BinaryOperatorInteger(
                        Operator::Mul,
                        Box::new(Expr::LitInteger(-34)),
                        Box::new(Expr::LitInteger(56)),
                    )),
                ),
            ),
            // intVar1 = 12 Or boolVar1
            (
                vec![
                    (1, Token::Name(int_var1.into())),
                    (2, Token::Operator(Operator::Equal)),
                    (3, Token::Integer(12)),
                    (4, Token::Operator(Operator::Or)),
                    (5, Token::Name(bool_var1.into())),
                ],
                Expr::BinaryOperatorBoolean(
                    Operator::Or,
                    Box::new(Expr::BinaryOperatorBoolean(
                        Operator::Equal,
                        Box::new(Expr::VarInteger(int_var1.into())),
                        Box::new(Expr::LitInteger(12)),
                    )),
                    Box::new(Expr::VarBoolean(bool_var1.into())),
                ),
            ),
            // 3 * intArr1(5 * 10) And 4
            (
                vec![
                    (1, Token::Integer(3)),
                    (2, Token::Operator(Operator::Mul)),
                    (3, Token::Name(int_arr1.into())),
                    (4, Token::Operator(Operator::OpenBracket)),
                    (5, Token::Integer(5)),
                    (6, Token::Operator(Operator::Mul)),
                    (7, Token::Integer(10)),
                    (8, Token::Operator(Operator::CloseBracket)),
                    (9, Token::Operator(Operator::And)),
                    (10, Token::Integer(4)),
                ],
                Expr::BinaryOperatorInteger(
                    Operator::And,
                    Box::new(Expr::BinaryOperatorInteger(
                        Operator::Mul,
                        Box::new(Expr::LitInteger(3)),
                        Box::new(Expr::VarArrayOfInteger(
                            int_arr1.into(),
                            Box::new(Expr::BinaryOperatorInteger(
                                Operator::Mul,
                                Box::new(Expr::LitInteger(5)),
                                Box::new(Expr::LitInteger(10)),
                            )),
                        )),
                    )),
                    Box::new(Expr::LitInteger(4)),
                ),
            ),
            // -Max(123, 456) + -(987 - 321) * Not intVar1
            (
                vec![
                    (1, Token::Operator(Operator::Sub)),
                    (2, Token::Function(Function::Max)),
                    (3, Token::Operator(Operator::OpenBracket)),
                    (4, Token::Integer(123)),
                    (5, Token::Operator(Operator::Comma)),
                    (6, Token::Integer(456)),
                    (7, Token::Operator(Operator::CloseBracket)),
                    (8, Token::Operator(Operator::Add)),
                    (9, Token::Operator(Operator::Sub)),
                    (10, Token::Operator(Operator::OpenBracket)),
                    (11, Token::Integer(987)),
                    (12, Token::Operator(Operator::Sub)),
                    (13, Token::Integer(321)),
                    (14, Token::Operator(Operator::CloseBracket)),
                    (15, Token::Operator(Operator::Mul)),
                    (16, Token::Operator(Operator::Not)),
                    (17, Token::Name(int_var1.into())),
                ],
                Expr::BinaryOperatorInteger(
                    Operator::Add,
                    Box::new(Expr::UnaryOperatorInteger(
                        Operator::Sub,
                        Box::new(Expr::FunctionInteger(
                            Function::Max,
                            Box::new(Expr::ParamList(vec![
                                Expr::LitInteger(123),
                                Expr::LitInteger(456),
                            ])),
                        )),
                    )),
                    Box::new(Expr::BinaryOperatorInteger(
                        Operator::Mul,
                        Box::new(Expr::UnaryOperatorInteger(
                            Operator::Sub,
                            Box::new(Expr::BinaryOperatorInteger(
                                Operator::Sub,
                                Box::new(Expr::LitInteger(987)),
                                Box::new(Expr::LitInteger(321)),
                            )),
                        )),
                        Box::new(Expr::UnaryOperatorInteger(
                            Operator::Not,
                            Box::new(Expr::VarInteger(int_var1.into())),
                        )),
                    )),
                ),
            ),
        ];

        for (tokens, expr) in src {
            assert_eq!(parser.parse_expr(&tokens).unwrap(), expr);
        }
    }

    const SRC1: &str = r#"
Dim i As Integer
Dim c As Integer
Print "Limit?"
Input c
c = Max(1, Min(100, c))
For i = 1 To c Step 1
    Select Case i Mod 15
        Case 0
            Print "FizzBuzz"
        Case 3, 6, 9, 12
            Print "Fizz"
        Case 5, 10
            Print "Buzz"
        Case Else
            Print i
    End Select
Next i
"#;

    const SRC2: &str = r#"
Dim s As String
Dim n As Integer
Do
    Print "Number?"
    Input s
    If s = "end" Then
        Exit Do
    End If
    n = CInt(s)
    If n < 1 Then
        Print "Invalid Input"
        Continue Do
    End If
    If n Mod 15 = 0 Then
        s = "FizzBuzz"
    ElseIf n Mod 3 = 0 Then
        s = "Fizz"
    ElseIf n Mod 5 = 0 Then
        s = "Buzz"
    End If
    Print s
Loop
"#;

    fn get_src1_statements() -> Vec<Statement> {
        vec![
            // Dim i As Integer
            Statement::Dim {
                var_name: "i".into(),
                var_type: VarType::Integer,
            },
            // Dim c As Integer
            Statement::Dim {
                var_name: "c".into(),
                var_type: VarType::Integer,
            },
            // Print "Limit?"
            Statement::PrintLitString {
                value: "Limit?".into(),
            },
            // Input c
            Statement::InputInteger {
                var_name: "c".into(),
            },
            // c = Max(1, Min(100, c))
            Statement::AssignInteger {
                var_name: "c".into(),
                value: Expr::FunctionInteger(
                    Function::Max,
                    Box::new(Expr::ParamList(vec![
                        Expr::LitInteger(1),
                        Expr::FunctionInteger(
                            Function::Min,
                            Box::new(Expr::ParamList(vec![
                                Expr::LitInteger(100),
                                Expr::VarInteger("c".into()),
                            ])),
                        ),
                    ])),
                ),
            },
            // For i = 1 To c Step 1
            Statement::For {
                exit_id: 0,
                counter: "i".into(),
                init: Expr::LitInteger(1),
                end: Expr::VarInteger("c".into()),
                step: Some(Expr::LitInteger(1)),
                block: vec![
                    // Select Case i Mod 15
                    Statement::SelectInteger {
                        exit_id: 1,
                        value: Expr::BinaryOperatorInteger(
                            Operator::Mod,
                            Box::new(Expr::VarInteger("i".into())),
                            Box::new(Expr::LitInteger(15)),
                        ),
                        case_blocks: vec![
                            // Case 0
                            Statement::CaseInteger {
                                values: vec![0],
                                block: vec![
                                    // Print "FizzBuzz"
                                    Statement::PrintLitString {
                                        value: "FizzBuzz".into(),
                                    },
                                ],
                            },
                            // Case 3, 6, 9, 12
                            Statement::CaseInteger {
                                values: vec![3, 6, 9, 12],
                                block: vec![
                                    // Print "Fizz"
                                    Statement::PrintLitString {
                                        value: "Fizz".into(),
                                    },
                                ],
                            },
                            // Case 5, 10
                            Statement::CaseInteger {
                                values: vec![5, 10],
                                block: vec![
                                    // Print "Buzz"
                                    Statement::PrintLitString {
                                        value: "Buzz".into(),
                                    },
                                ],
                            },
                            // Case Else
                            Statement::CaseElse {
                                block: vec![
                                    // Print i
                                    Statement::PrintVarInteger {
                                        var_name: "i".into(),
                                    },
                                ],
                            },
                            // End Select
                        ],
                    },
                    // Next i
                ],
            },
        ]
    }

    fn get_src2_statements() -> Vec<Statement> {
        vec![
            // Dim s As String
            Statement::Dim {
                var_name: "s".into(),
                var_type: VarType::String,
            },
            // Dim n As Integer
            Statement::Dim {
                var_name: "n".into(),
                var_type: VarType::Integer,
            },
            // Do
            Statement::DoLoop {
                exit_id: 0,
                block: vec![
                    // Print "Number?"
                    Statement::PrintLitString {
                        value: "Number?".into(),
                    },
                    // Input s
                    Statement::InputString {
                        var_name: "s".into(),
                    },
                    // If s = "end" Then
                    Statement::If {
                        condition: Expr::BinaryOperatorBoolean(
                            Operator::Equal,
                            Box::new(Expr::VarString("s".into())),
                            Box::new(Expr::LitString("end".into())),
                        ),
                        block: vec![
                            // Exit Do
                            Statement::ExitDo { exit_id: 0 },
                        ],
                        else_blocks: vec![
                            // End If
                        ],
                    },
                    // n = CInt(s)
                    Statement::AssignInteger {
                        var_name: "n".into(),
                        value: Expr::FunctionInteger(
                            Function::CInt,
                            Box::new(Expr::VarString("s".into())),
                        ),
                    },
                    // If n < 1 Then
                    Statement::If {
                        condition: Expr::BinaryOperatorBoolean(
                            Operator::LessThan,
                            Box::new(Expr::VarInteger("n".into())),
                            Box::new(Expr::LitInteger(1)),
                        ),
                        block: vec![
                            // Print "Invalid Input"
                            Statement::PrintLitString {
                                value: "Invalid Input".into(),
                            },
                            // Continue Do
                            Statement::ContinueDo { exit_id: 0 },
                        ],
                        else_blocks: vec![
                            // End If
                        ],
                    },
                    // If n Mod 15 = 0 Then
                    Statement::If {
                        condition: Expr::BinaryOperatorBoolean(
                            Operator::Equal,
                            Box::new(Expr::BinaryOperatorInteger(
                                Operator::Mod,
                                Box::new(Expr::VarInteger("n".into())),
                                Box::new(Expr::LitInteger(15)),
                            )),
                            Box::new(Expr::LitInteger(0)),
                        ),
                        block: vec![
                            // s = "FizzBuzz"
                            Statement::AssignString {
                                var_name: "s".into(),
                                value: Expr::LitString("FizzBuzz".into()),
                            },
                        ],
                        else_blocks: vec![
                            // ElseIf n Mod 3 = 0 Then
                            Statement::ElseIf {
                                condition: Expr::BinaryOperatorBoolean(
                                    Operator::Equal,
                                    Box::new(Expr::BinaryOperatorInteger(
                                        Operator::Mod,
                                        Box::new(Expr::VarInteger("n".into())),
                                        Box::new(Expr::LitInteger(3)),
                                    )),
                                    Box::new(Expr::LitInteger(0)),
                                ),
                                block: vec![
                                    // s = "Fizz"
                                    Statement::AssignString {
                                        var_name: "s".into(),
                                        value: Expr::LitString("Fizz".into()),
                                    },
                                ],
                            },
                            // ElseIf n Mod 5 = 0 Then
                            Statement::ElseIf {
                                condition: Expr::BinaryOperatorBoolean(
                                    Operator::Equal,
                                    Box::new(Expr::BinaryOperatorInteger(
                                        Operator::Mod,
                                        Box::new(Expr::VarInteger("n".into())),
                                        Box::new(Expr::LitInteger(5)),
                                    )),
                                    Box::new(Expr::LitInteger(0)),
                                ),
                                block: vec![
                                    // s = "Buzz"
                                    Statement::AssignString {
                                        var_name: "s".into(),
                                        value: Expr::LitString("Buzz".into()),
                                    },
                                ],
                            },
                            // End If
                        ],
                    },
                    // Print s
                    Statement::PrintVarString {
                        var_name: "s".into(),
                    },
                    // Loop
                ],
            },
        ]
    }
}
