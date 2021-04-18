// use crate::jis_x_201;
use crate::SyntaxError;
use std::io::{self, BufRead};
use std::result;

#[cfg(test)]
mod test;

type Result = result::Result<(usize, Vec<(usize, Token)>), SyntaxError>;

pub struct Tokenizer<R> {
    line_number: usize,
    reader: R,
}

impl<R> Tokenizer<R> {
    pub fn new(reader: R) -> Self {
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
        let mut ret: Vec<(usize, Token)> = vec![];
        while !line.is_empty() && !is_comment(line, ret.is_empty()) {
            let pos = src.len() - line.len() + 1;
            match take_token(line) {
                Some((Token::Keyword(Keyword::Mid), rest)) if !ret.is_empty() => {
                    ret.push((pos, Token::Function(Function::Mid)));
                    line = rest.trim_start();
                }
                Some((token, rest)) => {
                    ret.push((pos, token));
                    line = rest.trim_start();
                }
                None => {
                    return Err(SyntaxError::new(
                        self.line_number,
                        pos,
                        format!("invalid token: {}", line),
                    ))
                }
            }
        }
        Ok((self.line_number, ret))
    }
}

fn is_comment(line: &str, is_toplevel: bool) -> bool {
    line.starts_with('\'')
        || (is_toplevel
            && take_word(line)
                .filter(|(word, _)| "Rem".eq_ignore_ascii_case(word))
                .is_some())
}

fn take_token(line: &str) -> Option<(Token, &str)> {
    [
        take_word_token,
        take_hex_integer_token,
        take_integer_token,
        take_char_token,
        take_string_token,
        take_operator_token,
    ]
    .iter()
    .find_map(|f| f(line))
}

fn take_hex_integer_token(s: &str) -> Option<(Token, &str)> {
    let mut char_indices = s.char_indices();
    char_indices.next().filter(|(_, ch)| *ch == '&')?;
    char_indices
        .next()
        .filter(|(_, ch)| 'H'.eq_ignore_ascii_case(ch))?;
    let mut char_indices = char_indices.peekable();
    let &(prefix_position, _) = char_indices.peek()?;
    let split_position = char_indices
        .find(|(_, ch)| !ch.is_ascii_hexdigit())
        .map_or(s.len(), |(p, _)| p);
    let (num, rest) = s.split_at(split_position);
    let (_prefix, num) = num.split_at(prefix_position);
    u16::from_str_radix(num, 16)
        .ok()
        .map(|n| (Token::Integer(n as i16 as i32), rest))
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

fn take_char_token(s: &str) -> Option<(Token, &str)> {
    let mut char_indices = s.char_indices();
    char_indices.next().filter(|(_, ch)| *ch == '"')?;
    let mut quotation = false;
    let mut split_position = s.len();
    let mut text: Option<char> = None;
    for (p, ch) in char_indices {
        // if !jis_x_201::contains(ch) {
        // return None;
        // }
        if quotation {
            if ch == '"' {
                if text.is_some() {
                    return None;
                }
                quotation = false;
                text = Some('"');
            } else if text.is_some() {
                split_position = p;
                break;
            } else {
                return None;
            }
        } else if ch == '"' {
            quotation = true;
        } else if text.is_none() {
            text = Some(ch);
        } else {
            return None;
        }
    }
    if !quotation {
        return None;
    }
    let ch = text.take()?;
    let (_, rest) = s.split_at(split_position);
    let (suffix, rest) = take_word(rest)?;
    if !"c".eq_ignore_ascii_case(suffix) {
        return None;
    }
    Some((Token::Character(ch), rest))
}

fn take_string_token(s: &str) -> Option<(Token, &str)> {
    let mut char_indices = s.char_indices();
    char_indices.next().filter(|(_, ch)| *ch == '"')?;
    let mut quotation = false;
    let mut split_position = s.len();
    let mut text = String::new();
    for (p, ch) in char_indices {
        // if !jis_x_201::contains(ch) {
        // return None;
        // }
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
    if !quotation || text.chars().count() > 256 {
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
        .chain(vec![(s.len(), '\n')])
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
    Some(s.split_at(split_position)).filter(|(word, _)| word.chars().count() <= 30)
}

fn parse_boolean(token: &str) -> Option<Token> {
    // [("True", true), ("False", false)]
    // .iter()
    // .find(|(lit, _)| lit.eq_ignore_ascii_case(token))
    // .map(|(_, b)| Token::Boolean(*b))
    token
        .to_ascii_lowercase()
        .parse::<bool>()
        .map(Token::Boolean)
        .ok()
}

fn take_word_token(s: &str) -> Option<(Token, &str)> {
    take_word(s).and_then(|(word, rest)| {
        [
            parse_boolean,
            Keyword::parse,
            TypeName::parse,
            Function::parse,
            Operator::parse,
        ]
        .iter()
        .find_map(|f| f(word))
        .or_else(|| Some(Token::Name(word.into())))
        .map(|token| (token, rest))
    })
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Token {
    Name(String),
    Keyword(Keyword),
    Function(Function),
    TypeName(TypeName),
    Operator(Operator),
    String(String),
    Integer(i32),
    Boolean(bool),
    Character(char),
}

macro_rules! enumdef {
    ($v:ident,) => (1);
    ($v:ident, $($vs:ident,)*) => (1 + enumdef!($($vs,)*));
    ($name:ident; $array:ident;; $($value:ident,)* ) => {
        #[derive(PartialEq,Eq,Clone,Copy,Debug)]
        pub enum $name {
            $($value,)*
        }
        static $array: [$name; enumdef!($($value,)*)] = [
            $($name::$value,)*
        ];
        impl std::convert::TryFrom<&str> for $name {
            type Error = ();
            fn try_from(token: &str) -> std::result::Result<Self, Self::Error> {
                $array
                    .iter()
                    .find(|v| v.to_str().eq_ignore_ascii_case(token))
                    .cloned()
                    .ok_or(())
            }
        }
        impl From<$name> for Token {
            fn from(v: $name) -> Token {
                Token::$name(v)
            }
        }
        impl $name {
            fn parse(token: &str) -> Option<Token> {
                use std::convert::*;
                $name::try_from(token).map(Into::into).ok()
            }
        }
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.to_str().fmt(f)
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
    Argument,
    As,
    ByRef,
    ByVal,
    Call,
    Case,
    Continue,
    Else,
    ElseIf,
    End,
    Exit,
    Extern,
    Dim,
    Do,
    Fill,
    For,
    From,
    If,
    Input,
    Loop,
    Mid,
    Next,
    Option,
    Print,
    Program,
    Rem,
    Select,
    Step,
    Sub,
    Then,
    To,
    Until,
    While,
    With,
);

enumdef!(
    Function;
    FUNCTIONS;
    Abs,
    Array,
    Asc,
    CArray,
    CBool,
    Chr,
    CInt,
    CStr,
    Eof,
    Len,
    Max,
    Mid,
    Min,
    Space,
    String,
    SubArray,
);

enumdef!(
    TypeName;
    TYPE_NAMES;
    Boolean,
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
