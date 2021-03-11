use std::io::{self, BufRead};
use std::result;

type Result = result::Result<Vec<Token>, String>;

struct Tokenizer<R> {
    line_number: usize,
    reader: R,
}

impl<R> Tokenizer<R> {
    fn new(reader: R) -> Self {
        Self {
            line_number: 0,
            reader,
        }
    }
}

impl<R: BufRead> Iterator for Tokenizer<R> {
    type Item = io::Result<Result>;
    fn next(&mut self) -> Option<Self::Item> {
        self.line_number += 1;
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Err(error) => Some(Err(error)),
            Ok(0) => None,
            Ok(_) => Some(Ok(self.parse_line(&line))),
        }
    }
}

impl<R> Tokenizer<R> {
    fn parse_line(&self, src: &str) -> Result {
        let mut line = src.trim_start();
        let mut ret: Vec<Token> = vec![];
        while !line.is_empty() && !is_comment(line) {
            match take_token(line) {
                Some((token, rest)) => {
                    ret.push(token);
                    line = rest.trim_start();
                }
                None => {
                    let pos = src.len() - line.len() + 1;
                    return Err(format!(
                        "Syntax Error [in {}:{}]: {}",
                        self.line_number, pos, line
                    ));
                }
            }
        }
        Ok(ret)
    }
}

fn take_token(line: &str) -> Option<(Token, &str)> {
    [
        take_word_token,
        take_operator_token,
        take_string_token,
        take_integer_token,
    ]
    .iter()
    .find_map(|f| f(line))
}

fn is_comment(line: &str) -> bool {
    line.starts_with('\'')
        || take_word(line)
            .filter(|(word, _)| "Rem".eq_ignore_ascii_case(word))
            .is_some()
}

fn take_integer_token(s: &str) -> Option<(Token, &str)> {
    let split_position = s
        .char_indices()
        .find(|(_, ch)| !ch.is_ascii_digit())
        .map_or(s.len(), |(p, _)| p);
    let (number, rest) = s.split_at(split_position);
    number
        .parse::<i32>()
        .ok()
        .filter(|n| *n <= (i16::MIN as i32).abs())
        .map(|n| (Token::Integer(n), rest))
}

fn take_string_token(s: &str) -> Option<(Token, &str)> {
    let mut char_indices = s.char_indices();
    char_indices.next().filter(|(_, ch)| *ch == '"')?;
    let mut quotation = false;
    let mut split_position = s.len();
    let mut text = String::new();
    for (p, ch) in char_indices {
        if quotation {
            if ch == '"' {
                quotation = false;
                text.push('"');
            } else {
                split_position = p;
                break;
            }
        } else if ch == '"' {
            quotation = true;
        } else {
            text.push(ch);
        }
    }
    if !quotation {
        return None;
    }
    let (_, rest) = s.split_at(split_position);
    Some((Token::String(text), rest))
}

fn take_operator_token(s: &str) -> Option<(Token, &str)> {
    let mut char_indices = s.char_indices().take(4);
    char_indices
        .next()
        .filter(|(_, ch)| !ch.is_ascii_alphanumeric())?;
    char_indices
        .filter_map(|(p, _)| {
            let (word, rest) = s.split_at(p);
            Operator::parse(word).map(|token| (token, rest))
        })
        .last()
}

fn take_word(s: &str) -> Option<(&str, &str)> {
    let mut char_indices = s.char_indices();
    char_indices
        .next()
        .filter(|(_, head)| head.is_ascii_alphabetic())?;
    let split_position = char_indices
        .find(|(_, ch)| !(ch.is_ascii_alphanumeric() || *ch == '_'))
        .map_or(s.len(), |(p, _)| p);
    Some(s.split_at(split_position))
}

fn take_word_token(s: &str) -> Option<(Token, &str)> {
    take_word(s).and_then(|(word, rest)| {
        [
            Keyword::parse,
            Function::parse,
            TypeName::parse,
            Operator::parse,
        ]
        .iter()
        .find_map(|f| f(word))
        .or_else(|| Some(Token::Name(word.into())))
        .map(|token| (token, rest))
    })
}

#[derive(PartialEq, Eq, Debug)]
enum Token {
    Name(String),
    Keyword(Keyword),
    Function(Function),
    TypeName(TypeName),
    Operator(Operator),
    String(String),
    Integer(i32),
}

macro_rules! enumdef {
    ($v:ident,) => (1);
    ($v:ident, $($vs:ident,)*) => (1 + enumdef!($($vs,)*));
    ($name:ident; $array:ident;; $($value:ident,)* ) => {
        #[derive(PartialEq,Eq,Clone,Copy,Debug)]
        enum $name {
            $($value,)*
        }
        static $array: [$name; enumdef!($($value,)*)] = [
            $($name::$value,)*
        ];
        impl std::convert::TryFrom<&str> for $name {
            type Error = i32;
            fn try_from(token: &str) -> std::result::Result<Self, Self::Error> {
                $array
                    .iter()
                    .find(|v| v.to_str().eq_ignore_ascii_case(token))
                    .cloned()
                    .ok_or(0)
            }
        }
        impl Into<Token> for $name {
            fn into(self) -> Token {
                Token::$name(self)
            }
        }
        impl $name {
            fn parse(token: &str) -> Option<Token> {
                use std::convert::*;
                $name::try_from(token).map(Into::into).ok()
            }
        }
    };
    ($name:ident; $array:ident; $($value:ident,)* ) => {
        enumdef!($name; $array;; $($value,)*);
        impl $name {
            fn to_str(&self) -> &str {
                match self {
                    $($name::$value => stringify!($value),)*
                }
            }
        }
    };
}

enumdef!(
    Keyword;
    KEYWORDS;
    As,
    Case,
    Continue,
    Else,
    ElseIf,
    End,
    Exit,
    Dim,
    Do,
    For,
    If,
    Input,
    Loop,
    Next,
    Print,
    Select,
    Step,
    Then,
    To,
    While,
);

enumdef!(
    Function;
    FUNCTIONS;
    CInt,
    Max,
    Min,
);

enumdef!(
    TypeName;
    TYPE_NAMES;
    Integer,
    String,
);

enumdef!(
    Operator;
    OPERATORS;;
    Mod,
    Not,
    And,
    Xor,
    Or,
    NotEqual,
    LessOrEequal,
    GreaterOrEqual,
    ShiftLeft,
    ShiftRight,
    AddInto,
    SubInto,
    Equal,
    LessThan,
    GreaterThan,
    Add,
    Sub,
    Mul,
    Div,
    Concat,
    OpenBracket,
    CloseBracket,
    Comma,
);

impl Operator {
    fn to_str(&self) -> &str {
        use Operator::*;
        match self {
            Mod => "Mod",
            Not => "Not",
            And => "And",
            Xor => "Xor",
            Or => "Or",
            NotEqual => "<>",
            LessOrEequal => "<=",
            GreaterOrEqual => ">=",
            ShiftLeft => "<<",
            ShiftRight => ">>",
            AddInto => "+=",
            SubInto => "-=",
            Equal => "=",
            LessThan => "<",
            GreaterThan => ">",
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "\\",
            Concat => "&",
            OpenBracket => "(",
            CloseBracket => ")",
            Comma => ",",
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io;

    #[test]
    fn tokenizer_works() -> io::Result<()> {
        let c = io::Cursor::new(SRC);
        let t = Tokenizer::new(c);
        for (line, src_tokens) in t.zip(SrcTokens::new()) {
            assert_eq!(line?.unwrap(), src_tokens);
        }
        Ok(())
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

    struct SrcTokens {
        pos: usize,
    }

    impl SrcTokens {
        fn new() -> Self {
            Self { pos: 0 }
        }
    }

    impl Iterator for SrcTokens {
        type Item = Vec<Token>;

        fn next(&mut self) -> Option<Self::Item> {
            let v = match self.pos {
                //
                0 => vec![],
                // Dim i As Integer
                1 => vec![
                    Token::Keyword(Keyword::Dim),
                    Token::Name("i".into()),
                    Token::Keyword(Keyword::As),
                    Token::TypeName(TypeName::Integer),
                ],
                // Dim c As Integer
                2 => vec![
                    Token::Keyword(Keyword::Dim),
                    Token::Name("c".into()),
                    Token::Keyword(Keyword::As),
                    Token::TypeName(TypeName::Integer),
                ],
                // Print "Limit?"
                3 => vec![
                    Token::Keyword(Keyword::Print),
                    Token::String("Limit?".into()),
                ],
                // Input c
                4 => vec![Token::Keyword(Keyword::Input), Token::Name("c".into())],
                // c = Max(1, Min(100, c))
                5 => vec![
                    Token::Name("c".into()),
                    Token::Operator(Operator::Equal),
                    Token::Function(Function::Max),
                    Token::Operator(Operator::OpenBracket),
                    Token::Integer(1),
                    Token::Operator(Operator::Comma),
                    Token::Function(Function::Min),
                    Token::Operator(Operator::OpenBracket),
                    Token::Integer(100),
                    Token::Operator(Operator::Comma),
                    Token::Name("c".into()),
                    Token::Operator(Operator::CloseBracket),
                    Token::Operator(Operator::CloseBracket),
                ],
                // For i = 1 To c Step 1
                6 => vec![
                    Token::Keyword(Keyword::For),
                    Token::Name("i".into()),
                    Token::Operator(Operator::Equal),
                    Token::Integer(1),
                    Token::Keyword(Keyword::To),
                    Token::Name("c".into()),
                    Token::Keyword(Keyword::Step),
                    Token::Integer(1),
                ],
                // Select Case i Mod 15
                7 => vec![
                    Token::Keyword(Keyword::Select),
                    Token::Keyword(Keyword::Case),
                    Token::Name("i".into()),
                    Token::Operator(Operator::Mod),
                    Token::Integer(15),
                ],
                // Case 0
                8 => vec![Token::Keyword(Keyword::Case), Token::Integer(0)],
                // Print "FizzBuzz"
                9 => vec![
                    Token::Keyword(Keyword::Print),
                    Token::String("FizzBuzz".into()),
                ],
                // Case 3, 6, 9, 12
                10 => vec![
                    Token::Keyword(Keyword::Case),
                    Token::Integer(3),
                    Token::Operator(Operator::Comma),
                    Token::Integer(6),
                    Token::Operator(Operator::Comma),
                    Token::Integer(9),
                    Token::Operator(Operator::Comma),
                    Token::Integer(12),
                ],
                // Print "Fizz"
                11 => vec![Token::Keyword(Keyword::Print), Token::String("Fizz".into())],
                // Case 5, 10
                12 => vec![
                    Token::Keyword(Keyword::Case),
                    Token::Integer(5),
                    Token::Operator(Operator::Comma),
                    Token::Integer(10),
                ],
                // Print "Buzz"
                13 => vec![Token::Keyword(Keyword::Print), Token::String("Buzz".into())],
                // Case Else
                14 => vec![Token::Keyword(Keyword::Case), Token::Keyword(Keyword::Else)],
                // Print i
                15 => vec![Token::Keyword(Keyword::Print), Token::Name("i".into())],
                // End Select
                16 => vec![
                    Token::Keyword(Keyword::End),
                    Token::Keyword(Keyword::Select),
                ],
                // Next i
                17 => vec![Token::Keyword(Keyword::Next), Token::Name("i".into())],
                //
                18 => vec![],
                // Dim s As String
                19 => vec![
                    Token::Keyword(Keyword::Dim),
                    Token::Name("s".into()),
                    Token::Keyword(Keyword::As),
                    Token::TypeName(TypeName::String),
                ],
                // Dim n As Integer
                20 => vec![
                    Token::Keyword(Keyword::Dim),
                    Token::Name("n".into()),
                    Token::Keyword(Keyword::As),
                    Token::TypeName(TypeName::Integer),
                ],
                // Do
                21 => vec![Token::Keyword(Keyword::Do)],
                // Print "Number?"
                22 => vec![
                    Token::Keyword(Keyword::Print),
                    Token::String("Number?".into()),
                ],
                // Input s
                23 => vec![Token::Keyword(Keyword::Input), Token::Name("s".into())],
                // If s = "end" Then
                24 => vec![
                    Token::Keyword(Keyword::If),
                    Token::Name("s".into()),
                    Token::Operator(Operator::Equal),
                    Token::String("end".into()),
                    Token::Keyword(Keyword::Then),
                ],
                // Exit Do
                25 => vec![Token::Keyword(Keyword::Exit), Token::Keyword(Keyword::Do)],
                // End If
                26 => vec![Token::Keyword(Keyword::End), Token::Keyword(Keyword::If)],
                // n = CInt(s)
                27 => vec![
                    Token::Name("n".into()),
                    Token::Operator(Operator::Equal),
                    Token::Function(Function::CInt),
                    Token::Operator(Operator::OpenBracket),
                    Token::Name("s".into()),
                    Token::Operator(Operator::CloseBracket),
                ],
                // If n < 1 Then
                28 => vec![
                    Token::Keyword(Keyword::If),
                    Token::Name("n".into()),
                    Token::Operator(Operator::LessThan),
                    Token::Integer(1),
                    Token::Keyword(Keyword::Then),
                ],
                // Print "Invalid Input"
                29 => vec![
                    Token::Keyword(Keyword::Print),
                    Token::String("Invalid Input".into()),
                ],
                // Continue Do
                30 => vec![
                    Token::Keyword(Keyword::Continue),
                    Token::Keyword(Keyword::Do),
                ],
                // End If
                31 => vec![Token::Keyword(Keyword::End), Token::Keyword(Keyword::If)],
                // If n Mod 15 = 0 Then
                32 => vec![
                    Token::Keyword(Keyword::If),
                    Token::Name("n".into()),
                    Token::Operator(Operator::Mod),
                    Token::Integer(15),
                    Token::Operator(Operator::Equal),
                    Token::Integer(0),
                    Token::Keyword(Keyword::Then),
                ],
                // s = "FizzBuzz"
                33 => vec![
                    Token::Name("s".into()),
                    Token::Operator(Operator::Equal),
                    Token::String("FizzBuzz".into()),
                ],
                // ElseIf n Mod 3 = 0 Then
                34 => vec![
                    Token::Keyword(Keyword::ElseIf),
                    Token::Name("n".into()),
                    Token::Operator(Operator::Mod),
                    Token::Integer(3),
                    Token::Operator(Operator::Equal),
                    Token::Integer(0),
                    Token::Keyword(Keyword::Then),
                ],
                // s = "Fizz"
                35 => vec![
                    Token::Name("s".into()),
                    Token::Operator(Operator::Equal),
                    Token::String("Fizz".into()),
                ],
                // ElseIf n Mod 5 = 0 Then
                36 => vec![
                    Token::Keyword(Keyword::ElseIf),
                    Token::Name("n".into()),
                    Token::Operator(Operator::Mod),
                    Token::Integer(5),
                    Token::Operator(Operator::Equal),
                    Token::Integer(0),
                    Token::Keyword(Keyword::Then),
                ],
                // s = "Buzz"
                37 => vec![
                    Token::Name("s".into()),
                    Token::Operator(Operator::Equal),
                    Token::String("Buzz".into()),
                ],
                // End If
                38 => vec![Token::Keyword(Keyword::End), Token::Keyword(Keyword::If)],
                // Print s
                39 => vec![Token::Keyword(Keyword::Print), Token::Name("s".into())],
                // Loop
                40 => vec![Token::Keyword(Keyword::Loop)],
                _ => return None,
            };
            self.pos += 1;
            Some(v)
        }
    }
}
