use crate::tokenizer::*;
use crate::SyntaxError;
use std::collections::HashMap;
use std::io::{self, BufRead};

fn parse<R: BufRead>(reader: R) -> io::Result<Result<(), SyntaxError>> {
    let mut perser = Parser::new();

    for line in Tokenizer::new(reader) {
        let (line_number, pos_and_tokens) = match line? {
            Ok(values) => values,
            Err(error) => return Ok(Err(error)),
        };
        perser.line_number = line_number;

        match pos_and_tokens.first() {
            None => continue,
            Some((pos, Token::Name(name))) => {
                // bind (assign) statement
                perser.line_start_position = *pos;
                if let Err(error) = perser.parse_assign(name, &pos_and_tokens[1..]) {
                    return Ok(Err(error));
                }
            }
            Some((pos, Token::Keyword(keyword))) if keyword.is_command() => {
                // command statement
                perser.line_start_position = *pos;
                if let Err(error) = perser.parse_command(keyword, &pos_and_tokens[1..]) {
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

    Ok(Ok(()))
}

struct Parser {
    line_number: usize,
    line_start_position: usize,
    variables: HashMap<String, VarType>,
    statements: Vec<Vec<Statement>>,
    count_do: usize,
    count_for: usize,
}

impl Parser {
    fn new() -> Self {
        Self {
            line_number: 0,
            line_start_position: 0,
            variables: HashMap::new(),
            statements: vec![vec![]; 1],
            count_do: 0,
            count_for: 0,
        }
    }

    fn syntax_error_pos(&self, position: usize, message: String) -> SyntaxError {
        SyntaxError::new(self.line_number, position, message)
    }

    fn syntax_error(&self, message: String) -> SyntaxError {
        SyntaxError::new(self.line_number, self.line_start_position, message)
    }

    fn add_statement(&mut self, statement: Statement) {
        if let Some(block) = self.statements.last_mut() {
            block.push(statement);
        }
    }

    fn parse_assign(
        &mut self,
        name: &str,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        if !self.variables.contains_key(name) {
            return Err(self.syntax_error(format!("undefined variable: {}", name)));
        }

        // var = expr
        // var += expr
        // var -= expr
        // array(expr) = expr
        // array(expr) += expr
        // array(expr) -= expr

        // TODO

        match pos_and_tokens.first() {
            Some((_, Token::Operator(Operator::Equal)))
            | Some((_, Token::Operator(Operator::AddInto)))
            | Some((_, Token::Operator(Operator::SubInto))) => {}
            _ => {
                return Err(self.syntax_error("invalid statement".into()));
            }
        }

        Ok(())
    }

    fn parse_command(
        &mut self,
        command: &Keyword,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        match command {
            Keyword::Dim => self.parse_command_dim(pos_and_tokens),
            Keyword::Input => self.parse_command_input(pos_and_tokens),
            Keyword::Print => self.parse_command_print(pos_and_tokens),
            _ => Ok(()),
        }
    }

    fn parse_command_dim(&mut self, pos_and_tokens: &[(usize, Token)]) -> Result<(), SyntaxError> {
        match pos_and_tokens {
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
                self.add_statement(Statement::Dim(name.clone(), var_type));
            }
            [(pos_n, Token::Name(name)), (_, Token::Operator(Operator::OpenBracket)), (pos_s, Token::Integer(size)), (_, Token::Operator(Operator::CloseBracket)), (_, Token::Keyword(Keyword::As)), (pos_t, Token::TypeName(type_name))] =>
            {
                if self.variables.contains_key(name) {
                    return Err(self
                        .syntax_error_pos(*pos_n, format!("already defined variable: {}", name)));
                }
                if *size < 0 || *size > 256 {
                    return Err(
                        self.syntax_error_pos(*pos_s, "invalid Array Size in Dim statement".into())
                    );
                }
                let size = *size as usize;
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
                self.add_statement(Statement::Dim(name.clone(), var_type));
            }
            _ => return Err(self.syntax_error("invalid Dim statement".into())),
        }

        Ok(())
    }

    fn parse_command_input(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        let name = if let [(_, Token::Name(name))] = pos_and_tokens {
            name
        } else {
            return Err(self.syntax_error("invalid Input statement".into()));
        };
        match self.variables.get(name) {
            Some(VarType::Integer) => {
                self.add_statement(Statement::InputInteger(name.clone()));
            }
            Some(VarType::String) => {
                self.add_statement(Statement::InputString(name.clone()));
            }
            Some(_) => {
                return Err(self.syntax_error("invalid Variable Type in Input statement".into()))
            }
            None => return Err(self.syntax_error(format!("undefined variable: {}", name))),
        }

        Ok(())
    }

    fn parse_command_print(
        &mut self,
        pos_and_tokens: &[(usize, Token)],
    ) -> Result<(), SyntaxError> {
        match pos_and_tokens {
            [] => {
                self.add_statement(Statement::PrintLitString("".into()));
                return Ok(());
            }
            [(_, Token::Boolean(value))] => {
                self.add_statement(Statement::PrintLitBoolean(*value));
                return Ok(());
            }
            [(pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(false, *value) {
                    self.add_statement(Statement::PrintLitInteger(value));
                    return Ok(());
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }
            [(_, Token::Operator(Operator::Sub)), (pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(true, *value) {
                    self.add_statement(Statement::PrintLitInteger(value));
                    return Ok(());
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }
            [(_, Token::String(value))] => {
                self.add_statement(Statement::PrintLitString(value.clone()));
                return Ok(());
            }
            [(_, Token::Name(name))] => match self.variables.get(name) {
                Some(VarType::Boolean) => {
                    self.add_statement(Statement::PrintVarBoolean(name.clone()));
                    return Ok(());
                }
                Some(VarType::Integer) => {
                    self.add_statement(Statement::PrintVarInteger(name.clone()));
                    return Ok(());
                }
                Some(VarType::String) => {
                    self.add_statement(Statement::PrintVarString(name.clone()));
                    return Ok(());
                }
                _ => return Err(self.syntax_error("invalid Argument in Print statement".into())),
            },
            _ => {}
        }
        let expr = self.parse_expr(pos_and_tokens)?;
        match expr.return_type() {
            ExprType::Boolean => self.add_statement(Statement::PrintExprBoolan(expr)),
            ExprType::Integer => self.add_statement(Statement::PrintExprInteger(expr)),
            ExprType::String => self.add_statement(Statement::PrintExprString(expr)),
            ExprType::ParamList => {
                return Err(self.syntax_error("invalid Expression in Print statement".into()))
            }
        }
        Ok(())
    }

    fn parse_expr(&self, pos_and_tokens: &[(usize, Token)]) -> Result<Expr, SyntaxError> {
        match pos_and_tokens {
            [] => return Err(self.syntax_error("invalid Expression".into())),
            [(_, Token::Boolean(value))] => return Ok(Expr::LitBoolean(*value)),
            [(pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(false, *value) {
                    return Ok(Expr::LitInteger(value));
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }
            [(_, Token::String(value))] => return Ok(Expr::LitString(value.clone())),
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
            [(_, Token::Operator(op)), (_, Token::Boolean(value))] if op.can_be_unary_boolean() => {
                return Ok(Expr::UnaryOperatorBoolean(
                    *op,
                    Box::new(Expr::LitBoolean(*value)),
                ))
            }
            [(_, Token::Operator(Operator::Sub)), (pos, Token::Integer(value))] => {
                if let Some(value) = validate_integer(true, *value) {
                    return Ok(Expr::LitInteger(value));
                } else {
                    return Err(self.syntax_error_pos(*pos, format!("invalid integer: {}", *value)));
                }
            }
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
            [(pos, token)] | [(pos, token), (_, _)] => {
                return Err(self
                    .syntax_error_pos(*pos, format!("invalid token in Expression: {:?}", token)))
            }
            _ => {}
        }

        let mut target_op: Option<(usize, Operator)> = None;
        let mut next_unary = true;
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
                    continue;
                }
                if next_unary {
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
                } else {
                    return Err(self.syntax_error_pos(*pos, "invalid Expression".into()));
                }
            } else {
                next_unary = false;
            }
        }

        if bracket_count > 0 || next_unary {
            return Err(self.syntax_error("invalid Expression".into()));
        }

        if let Some((i, op)) = target_op {
            let lhs = self.parse_expr(&pos_and_tokens[..i])?;
            let rhs = self.parse_expr(&pos_and_tokens[i + 1..])?;
            return Expr::binary(op, lhs, rhs).ok_or_else(|| {
                let (pos, _) = pos_and_tokens[i];
                self.syntax_error_pos(pos, "invalid Expression".into())
            });
        }

        // 外側の演算子のうち一番左のを抽出する (括弧のネストに気をつける)
        //    項 op1 項 op2 項 op1 ... op2 項
        //   -> (項 op1 項) op2 (項 op1 ... op2 項)
        //   分割して再帰的に処理していく
        // 外側の演算子がない場合、つまり、項が１つ
        //    [unary-op] ( 式 )
        //    [unary-op] Array ( 引数 )
        //    [unary-op] Func ( 引数 )
        //  のどれかのハズ
        todo!();
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
            Case | Continue | Else | ElseIf | End | Exit | Dim | Do | For | If | Input | Loop
            | Next | Print | Select => true,

            As | Step | Then | To | While => false,
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
enum Statement {
    ContinueDo,
    ContinueFor,
    Dim(String, VarType),
    ExitDo,
    ExitFor,
    InputInteger(String),
    InputString(String),
    PrintLitBoolean(bool),
    PrintLitInteger(i32),
    PrintLitString(String),
    PrintVarBoolean(String),
    PrintVarInteger(String),
    PrintVarString(String),
    PrintExprBoolan(Expr),
    PrintExprInteger(Expr),
    PrintExprString(Expr),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum VarType {
    Boolean,
    Integer,
    String,
    ArrayOfBoolean(usize),
    ArrayOfInteger(usize),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum ExprType {
    Boolean,
    Integer,
    String,
    ParamList,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Expr {
    BinaryOperatorBoolean(Operator, Box<Expr>, Box<Expr>),
    BinaryOperatorInteger(Operator, Box<Expr>, Box<Expr>),
    BinaryOperatorString(Operator, Box<Expr>, Box<Expr>),
    FunctionBoolean(Function, Box<Expr>),
    FunctionInteger(Function, Box<Expr>),
    FunctionString(Function, Box<Expr>),
    LitBoolean(bool),
    LitInteger(i32),
    LitString(String),
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
        assert!(op.can_be_binary());
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
            | UnaryOperatorInteger(_, _)
            | VarInteger(_)
            | VarArrayOfInteger(_, _) => ExprType::Integer,

            BinaryOperatorString(_, _, _) | FunctionString(_, _) | LitString(_) | VarString(_) => {
                ExprType::String
            }

            ParamList(_) => ExprType::ParamList,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io;

    #[test]
    fn it_works() -> io::Result<()> {
        let src = io::Cursor::new(SRC);

        parse(src)?.unwrap();

        Ok(())
    }

    #[test]
    fn parser_parse_expr_works() {
        let mut parser = Parser::new();

        let bool_var1 = "boolVar1";
        let int_var1 = "intVar1";
        let str_var1 = "strVar1";

        [
            (bool_var1, VarType::Boolean),
            (int_var1, VarType::Integer),
            (str_var1, VarType::String),
        ]
        .iter()
        .for_each(|(v, t)| {
            parser.variables.insert(v.to_string(), *t);
        });

        let src = vec![
            (vec![(1, Token::Boolean(true))], Expr::LitBoolean(true)),
            (vec![(1, Token::Integer(1234))], Expr::LitInteger(1234)),
            (
                vec![(1, Token::String("text".into()))],
                Expr::LitString("text".into()),
            ),
            (
                vec![(1, Token::Name(bool_var1.into()))],
                Expr::VarBoolean(bool_var1.into()),
            ),
            (
                vec![(1, Token::Name(int_var1.into()))],
                Expr::VarInteger(int_var1.into()),
            ),
            (
                vec![(1, Token::Name(str_var1.into()))],
                Expr::VarString(str_var1.into()),
            ),
            (
                vec![
                    (1, Token::Operator(Operator::Not)),
                    (2, Token::Boolean(true)),
                ],
                Expr::UnaryOperatorBoolean(Operator::Not, Box::new(Expr::LitBoolean(true))),
            ),
            (
                vec![
                    (1, Token::Operator(Operator::Sub)),
                    (2, Token::Integer(1234)),
                ],
                Expr::LitInteger(-1234),
            ),
            (
                vec![
                    (1, Token::Operator(Operator::Sub)),
                    (2, Token::Integer(0x8000)),
                ],
                Expr::LitInteger(-0x8000),
            ),
            (
                vec![
                    (1, Token::Operator(Operator::Not)),
                    (2, Token::Integer(1234)),
                ],
                Expr::UnaryOperatorInteger(Operator::Not, Box::new(Expr::LitInteger(1234))),
            ),
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
        ];

        for (tokens, expr) in src {
            assert_eq!(parser.parse_expr(&tokens).unwrap(), expr);
        }
    }

    const SRC: &str = r#"
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
}
