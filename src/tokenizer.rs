// use crate::jis_x_201;
use crate::SyntaxError;
use std::io::{self, BufRead};
use std::result;

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
            type Error = i32;
            fn try_from(token: &str) -> std::result::Result<Self, Self::Error> {
                $array
                    .iter()
                    .find(|v| v.to_str().eq_ignore_ascii_case(token))
                    .cloned()
                    .ok_or(0)
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
    Let,
    Loop,
    Next,
    Print,
    Rem,
    Select,
    Step,
    Then,
    To,
    Until,
    While,
);

enumdef!(
    Function;
    FUNCTIONS;
    CBool,
    CInt,
    CStr,
    Len,
    Max,
    Min,
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

    #[test]
    fn is_comment_works() {
        // comment
        assert!(is_comment("' hoge", true));
        assert!(is_comment("'hoge", true));
        assert!(is_comment("''hoge", true));
        assert!(is_comment("'", true));
        assert!(is_comment("' hoge", false));
        assert!(is_comment("'hoge", false));
        assert!(is_comment("''hoge", false));
        assert!(is_comment("'", false));
        assert!(is_comment("Rem hoge", true));
        assert!(is_comment("Rem'hoge", true));
        assert!(is_comment("Rem.hoge", true));
        assert!(is_comment("rem hoge", true));
        assert!(is_comment("REM'hoge", true));
        assert!(is_comment("rEm.hoge", true));
        // not comment
        assert!(!is_comment("hoge", true));
        assert!(!is_comment("h'oge", true));
        assert!(!is_comment("&hoge", true));
        assert!(!is_comment(" ' hoge", true));
        assert!(!is_comment(" Rem", true));
        assert!(!is_comment("R e m", true));
        assert!(!is_comment("\"rem\"", true));
        assert!(!is_comment("Reminder", true));
        assert!(!is_comment("rem1234", true));
        assert!(!is_comment("Rem hoge", false));
        assert!(!is_comment("Rem'hoge", false));
        assert!(!is_comment("Rem.hoge", false));
        assert!(!is_comment("rem hoge", false));
        assert!(!is_comment("REM'hoge", false));
        assert!(!is_comment("rEm.hoge", false));
    }

    #[test]
    fn parse_boolean_works() {
        let src = [
            ("true", Some(Token::Boolean(true))),
            ("false", Some(Token::Boolean(false))),
            ("True", Some(Token::Boolean(true))),
            ("False", Some(Token::Boolean(false))),
            ("TRUE", Some(Token::Boolean(true))),
            ("FALSE", Some(Token::Boolean(false))),
            ("t", None),
            ("f", None),
            (" true", None),
            (" false", None),
            ("true1", None),
            ("false1", None),
        ];
        for (s, res) in &src {
            assert_eq!(parse_boolean(s), *res);
        }
    }

    #[test]
    fn take_hex_integer_token_works() {
        let src = [
            ("&H000000", Some((Token::Integer(0x0), ""))),
            ("&H00001", Some((Token::Integer(0x1), ""))),
            ("&h10", Some((Token::Integer(0x10), ""))),
            ("&H1000", Some((Token::Integer(0x1000), ""))),
            ("&HABCZ", Some((Token::Integer(0xABC), "Z"))),
            ("&hFF", Some((Token::Integer(0xFF), ""))),
            ("&H10 0", Some((Token::Integer(0x10), " 0"))),
            ("&H10xx", Some((Token::Integer(0x10), "xx"))),
            ("&h1000.00", Some((Token::Integer(0x1000), ".00"))),
            ("&H7fff+1", Some((Token::Integer(0x7FFF), "+1"))),
            ("&H8000-2", Some((Token::Integer(-0x8000), "-2"))),
            ("&", None),
            ("&H", None),
            ("&Hx", None),
            ("&H 0", None),
            ("&012", None),
            ("H012", None),
            ("0x12", None),
            ("ABC", None),
            (" &h01", None),
            ("&h10000", None),
            ("&h123456789", None),
            ("+&h100", None),
            ("-&h100", None),
        ];
        for (s, res) in &src {
            assert_eq!(take_hex_integer_token(s), *res);
        }
    }

    #[test]
    fn take_integer_token_works() {
        let src = [
            ("000000", Some((Token::Integer(0), ""))),
            ("001", Some((Token::Integer(1), ""))),
            ("1", Some((Token::Integer(1), ""))),
            ("12345#", Some((Token::Integer(12345), "#"))),
            ("12345L", Some((Token::Integer(12345), "L"))),
            ("12345+", Some((Token::Integer(12345), "+"))),
            ("12345.6", Some((Token::Integer(12345), ".6"))),
            ("12 345", Some((Token::Integer(12), " 345"))),
            ("32767", Some((Token::Integer(32767), ""))),
            ("32768", Some((Token::Integer(32768), ""))),
            ("32769", None),
            ("123456789", None),
            ("&H1", None),
            ("ABC", None),
            (" 123", None),
            ("+123", None),
            ("-123", None),
        ];
        for (s, res) in &src {
            assert_eq!(take_integer_token(s), *res);
        }
    }

    #[test]
    fn take_char_token_works() {
        let src = [
            (r#""A"c"#, Some((Token::Character('A'), ""))),
            (r#""A"C"#, Some((Token::Character('A'), ""))),
            (r#""A"c+"#, Some((Token::Character('A'), "+"))),
            (r#"""""c"#, Some((Token::Character('"'), ""))),
            (r#"""c"#, None),
            (r#""AB"c"#, None),
            (r#""A""#, None),
            (r#""A"Cc"#, None),
            (r#""A"c1"#, None),
        ];
        for (s, res) in &src {
            assert_eq!(take_char_token(s), *res);
        }
    }

    #[test]
    fn take_string_token_works() {
        let src = [
            (r#""ABCD""#, Some((Token::String("ABCD".into()), ""))),
            (r#""AB CD""#, Some((Token::String("AB CD".into()), ""))),
            (r#""AB""CD""#, Some((Token::String("AB\"CD".into()), ""))),
            (
                r#""AB""""CD""#,
                Some((Token::String("AB\"\"CD".into()), "")),
            ),
            (r#""AB"CD""#, Some((Token::String("AB".into()), "CD\""))),
            (
                r#""AB" "CD""#,
                Some((Token::String("AB".into()), " \"CD\"")),
            ),
            (r#""ABCD"xyz"#, Some((Token::String("ABCD".into()), "xyz"))),
            (r#""ABCD"&x"#, Some((Token::String("ABCD".into()), "&x"))),
            (r#""""#, Some((Token::String("".into()), ""))),
            (r#"""#, None),
            (r#"ABCD""#, None),
            (r#""ABCD"#, None),
            (r#"ABCD"#, None),
        ];
        for (s, res) in &src {
            assert_eq!(take_string_token(s), *res);
        }
    }

    #[test]
    fn take_operator_token_works() {
        let src = [
            ("<<", Some((Token::Operator(Operator::ShiftLeft), ""))),
            ("<", Some((Token::Operator(Operator::LessThan), ""))),
            ("<=", Some((Token::Operator(Operator::LessOrEequal), ""))),
            ("<>", Some((Token::Operator(Operator::NotEqual), ""))),
            ("<>>", Some((Token::Operator(Operator::NotEqual), ">"))),
            ("==", Some((Token::Operator(Operator::Equal), "="))),
            ("+=", Some((Token::Operator(Operator::AddInto), ""))),
            ("+", Some((Token::Operator(Operator::Add), ""))),
            ("mod", None),
            ("And", None),
            ("Xor", None),
            ("'", None),
            ("|", None),
            (" + ", None),
        ];
        for (s, res) in &src {
            assert_eq!(take_operator_token(s), *res);
        }
    }

    #[test]
    fn take_word_works() {
        let src = [
            ("ABC", Some(("ABC", ""))),
            ("abc", Some(("abc", ""))),
            ("ABC123", Some(("ABC123", ""))),
            ("AB_C12_3", Some(("AB_C12_3", ""))),
            ("AB*C12*3", Some(("AB", "*C12*3"))),
            ("123ABC", None),
            ("_123ABC", None),
            (" ABC", None),
            ("_ABC", None),
            ("*ABC", None),
        ];
        for (s, res) in &src {
            assert_eq!(take_word(s), *res);
        }
    }

    #[test]
    fn take_word_token_works() {
        let src = [
            ("ABC", Some((Token::Name("ABC".into()), ""))),
            ("ABC123", Some((Token::Name("ABC123".into()), ""))),
            ("else", Some((Token::Keyword(Keyword::Else), ""))),
            ("MAX", Some((Token::Function(Function::Max), ""))),
            ("integer", Some((Token::TypeName(TypeName::Integer), ""))),
            ("xoR", Some((Token::Operator(Operator::Xor), ""))),
            ("else if", Some((Token::Keyword(Keyword::Else), " if"))),
            ("MAX++", Some((Token::Function(Function::Max), "++"))),
            (
                "integer(x)",
                Some((Token::TypeName(TypeName::Integer), "(x)")),
            ),
            ("MAXX", Some((Token::Name("MAXX".into()), ""))),
            ("Integerr", Some((Token::Name("Integerr".into()), ""))),
            ("Inte", Some((Token::Name("Inte".into()), ""))),
            ("X or", Some((Token::Name("X".into()), " or"))),
            ("True,", Some((Token::Boolean(true), ","))),
            ("false @", Some((Token::Boolean(false), " @"))),
            ("TrueTears", Some((Token::Name("TrueTears".into()), ""))),
            (" Abc", None),
            ("_Abc", None),
            ("+Abc", None),
            ("123", None),
            ("+", None),
            ("<>", None),
        ];
        for (s, res) in &src {
            assert_eq!(take_word_token(s), *res);
        }
    }

    #[test]
    fn take_token_works() {
        let src = [
            ("ABC", Some((Token::Name("ABC".into()), ""))),
            ("ABC123", Some((Token::Name("ABC123".into()), ""))),
            ("else", Some((Token::Keyword(Keyword::Else), ""))),
            ("MAX", Some((Token::Function(Function::Max), ""))),
            ("integer", Some((Token::TypeName(TypeName::Integer), ""))),
            ("xoR", Some((Token::Operator(Operator::Xor), ""))),
            ("else if", Some((Token::Keyword(Keyword::Else), " if"))),
            ("MAX++", Some((Token::Function(Function::Max), "++"))),
            (
                "integer(x)",
                Some((Token::TypeName(TypeName::Integer), "(x)")),
            ),
            ("MAXX", Some((Token::Name("MAXX".into()), ""))),
            ("Integerr", Some((Token::Name("Integerr".into()), ""))),
            ("Inte", Some((Token::Name("Inte".into()), ""))),
            ("X or", Some((Token::Name("X".into()), " or"))),
            ("==", Some((Token::Operator(Operator::Equal), "="))),
            ("+=", Some((Token::Operator(Operator::AddInto), ""))),
            ("+", Some((Token::Operator(Operator::Add), ""))),
            ("+Abc", Some((Token::Operator(Operator::Add), "Abc"))),
            ("&H7fff+1", Some((Token::Integer(0x7FFF), "+1"))),
            ("&H8000-2", Some((Token::Integer(-0x8000), "-2"))),
            ("&", Some((Token::Operator(Operator::Concat), ""))),
            ("&H", Some((Token::Operator(Operator::Concat), "H"))),
            ("&Hx", Some((Token::Operator(Operator::Concat), "Hx"))),
            (r#""ABCD"&x"#, Some((Token::String("ABCD".into()), "&x"))),
            (r#""""#, Some((Token::String("".into()), ""))),
            (r#"ABCD""#, Some((Token::Name("ABCD".into()), "\""))),
            ("<=", Some((Token::Operator(Operator::LessOrEequal), ""))),
            ("<>", Some((Token::Operator(Operator::NotEqual), ""))),
            ("<>>", Some((Token::Operator(Operator::NotEqual), ">"))),
            ("True,", Some((Token::Boolean(true), ","))),
            ("false @", Some((Token::Boolean(false), " @"))),
            ("12 345", Some((Token::Integer(12), " 345"))),
            ("32767", Some((Token::Integer(32767), ""))),
            ("32768", Some((Token::Integer(32768), ""))),
            (r#""A"c+"#, Some((Token::Character('A'), "+"))),
            (r#"""""c"#, Some((Token::Character('"'), ""))),
            (r#"""c"#, Some((Token::String("".into()), "c"))),
            (r#""AB"c"#, Some((Token::String("AB".into()), "c"))),
            ("32769", None),
            (r#"""#, None),
            (" Abc", None),
            ("_Abc", None),
        ];
        for (s, res) in &src {
            assert_eq!(take_token(s), *res);
        }
        for keyword in &KEYWORDS {
            assert_eq!(take_token(keyword.to_str()), Some(((*keyword).into(), "")));
        }
        for function in &FUNCTIONS {
            assert_eq!(
                take_token(function.to_str()),
                Some(((*function).into(), ""))
            );
        }
        for type_name in &TYPE_NAMES {
            assert_eq!(
                take_token(type_name.to_str()),
                Some(((*type_name).into(), ""))
            );
        }
        for operator in &OPERATORS {
            assert_eq!(
                take_token(operator.to_str()),
                Some(((*operator).into(), ""))
            );
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
        type Item = (usize, Vec<(usize, Token)>);

        fn next(&mut self) -> Option<Self::Item> {
            self.pos += 1;
            let v = match self.pos {
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //
                1 => vec![],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Dim i As Integer
                2 => vec![
                    (1, Token::Keyword(Keyword::Dim)),
                    (5, Token::Name("i".into())),
                    (7, Token::Keyword(Keyword::As)),
                    (10, Token::TypeName(TypeName::Integer)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Dim c As Integer
                3 => vec![
                    (1, Token::Keyword(Keyword::Dim)),
                    (5, Token::Name("c".into())),
                    (7, Token::Keyword(Keyword::As)),
                    (10, Token::TypeName(TypeName::Integer)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Print "Limit?"
                4 => vec![
                    (1, Token::Keyword(Keyword::Print)),
                    (7, Token::String("Limit?".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Input c
                5 => vec![
                    (1, Token::Keyword(Keyword::Input)),
                    (7, Token::Name("c".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // c = Max(1, Min(100, c))
                6 => vec![
                    (1, Token::Name("c".into())),
                    (3, Token::Operator(Operator::Equal)),
                    (5, Token::Function(Function::Max)),
                    (8, Token::Operator(Operator::OpenBracket)),
                    (9, Token::Integer(1)),
                    (10, Token::Operator(Operator::Comma)),
                    (12, Token::Function(Function::Min)),
                    (15, Token::Operator(Operator::OpenBracket)),
                    (16, Token::Integer(100)),
                    (19, Token::Operator(Operator::Comma)),
                    (21, Token::Name("c".into())),
                    (22, Token::Operator(Operator::CloseBracket)),
                    (23, Token::Operator(Operator::CloseBracket)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // For i = 1 To c Step 1
                7 => vec![
                    (1, Token::Keyword(Keyword::For)),
                    (5, Token::Name("i".into())),
                    (7, Token::Operator(Operator::Equal)),
                    (9, Token::Integer(1)),
                    (11, Token::Keyword(Keyword::To)),
                    (14, Token::Name("c".into())),
                    (16, Token::Keyword(Keyword::Step)),
                    (21, Token::Integer(1)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     Select Case i Mod 15
                8 => vec![
                    (5, Token::Keyword(Keyword::Select)),
                    (12, Token::Keyword(Keyword::Case)),
                    (17, Token::Name("i".into())),
                    (19, Token::Operator(Operator::Mod)),
                    (23, Token::Integer(15)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         Case 0
                9 => vec![(9, Token::Keyword(Keyword::Case)), (14, Token::Integer(0))],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //             Print "FizzBuzz"
                10 => vec![
                    (13, Token::Keyword(Keyword::Print)),
                    (19, Token::String("FizzBuzz".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         Case 3, 6, 9, 12
                11 => vec![
                    (9, Token::Keyword(Keyword::Case)),
                    (14, Token::Integer(3)),
                    (15, Token::Operator(Operator::Comma)),
                    (17, Token::Integer(6)),
                    (18, Token::Operator(Operator::Comma)),
                    (20, Token::Integer(9)),
                    (21, Token::Operator(Operator::Comma)),
                    (23, Token::Integer(12)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //             Print "Fizz"
                12 => vec![
                    (13, Token::Keyword(Keyword::Print)),
                    (19, Token::String("Fizz".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         Case 5, 10
                13 => vec![
                    (9, Token::Keyword(Keyword::Case)),
                    (14, Token::Integer(5)),
                    (15, Token::Operator(Operator::Comma)),
                    (17, Token::Integer(10)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //             Print "Buzz"
                14 => vec![
                    (13, Token::Keyword(Keyword::Print)),
                    (19, Token::String("Buzz".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         Case Else
                15 => vec![
                    (9, Token::Keyword(Keyword::Case)),
                    (14, Token::Keyword(Keyword::Else)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //             Print i
                16 => vec![
                    (13, Token::Keyword(Keyword::Print)),
                    (19, Token::Name("i".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     End Select
                17 => vec![
                    (5, Token::Keyword(Keyword::End)),
                    (9, Token::Keyword(Keyword::Select)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Next i
                18 => vec![
                    (1, Token::Keyword(Keyword::Next)),
                    (6, Token::Name("i".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //
                19 => vec![],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Dim s As String
                20 => vec![
                    (1, Token::Keyword(Keyword::Dim)),
                    (5, Token::Name("s".into())),
                    (7, Token::Keyword(Keyword::As)),
                    (10, Token::TypeName(TypeName::String)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Dim n As Integer
                21 => vec![
                    (1, Token::Keyword(Keyword::Dim)),
                    (5, Token::Name("n".into())),
                    (7, Token::Keyword(Keyword::As)),
                    (10, Token::TypeName(TypeName::Integer)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Do
                22 => vec![(1, Token::Keyword(Keyword::Do))],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     Print "Number?"
                23 => vec![
                    (5, Token::Keyword(Keyword::Print)),
                    (11, Token::String("Number?".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     Input s
                24 => vec![
                    (5, Token::Keyword(Keyword::Input)),
                    (11, Token::Name("s".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     If s = "end" Then
                25 => vec![
                    (5, Token::Keyword(Keyword::If)),
                    (8, Token::Name("s".into())),
                    (10, Token::Operator(Operator::Equal)),
                    (12, Token::String("end".into())),
                    (18, Token::Keyword(Keyword::Then)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         Exit Do
                26 => vec![
                    (9, Token::Keyword(Keyword::Exit)),
                    (14, Token::Keyword(Keyword::Do)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     End If
                27 => vec![
                    (5, Token::Keyword(Keyword::End)),
                    (9, Token::Keyword(Keyword::If)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     n = CInt(s)
                28 => vec![
                    (5, Token::Name("n".into())),
                    (7, Token::Operator(Operator::Equal)),
                    (9, Token::Function(Function::CInt)),
                    (13, Token::Operator(Operator::OpenBracket)),
                    (14, Token::Name("s".into())),
                    (15, Token::Operator(Operator::CloseBracket)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     If n < 1 Then
                29 => vec![
                    (5, Token::Keyword(Keyword::If)),
                    (8, Token::Name("n".into())),
                    (10, Token::Operator(Operator::LessThan)),
                    (12, Token::Integer(1)),
                    (14, Token::Keyword(Keyword::Then)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         Print "Invalid Input"
                30 => vec![
                    (9, Token::Keyword(Keyword::Print)),
                    (15, Token::String("Invalid Input".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         Continue Do
                31 => vec![
                    (9, Token::Keyword(Keyword::Continue)),
                    (18, Token::Keyword(Keyword::Do)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     End If
                32 => vec![
                    (5, Token::Keyword(Keyword::End)),
                    (9, Token::Keyword(Keyword::If)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     If n Mod 15 = 0 Then
                33 => vec![
                    (5, Token::Keyword(Keyword::If)),
                    (8, Token::Name("n".into())),
                    (10, Token::Operator(Operator::Mod)),
                    (14, Token::Integer(15)),
                    (17, Token::Operator(Operator::Equal)),
                    (19, Token::Integer(0)),
                    (21, Token::Keyword(Keyword::Then)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         s = "FizzBuzz"
                34 => vec![
                    (9, Token::Name("s".into())),
                    (11, Token::Operator(Operator::Equal)),
                    (13, Token::String("FizzBuzz".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     ElseIf n Mod 3 = 0 Then
                35 => vec![
                    (5, Token::Keyword(Keyword::ElseIf)),
                    (12, Token::Name("n".into())),
                    (14, Token::Operator(Operator::Mod)),
                    (18, Token::Integer(3)),
                    (20, Token::Operator(Operator::Equal)),
                    (22, Token::Integer(0)),
                    (24, Token::Keyword(Keyword::Then)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         s = "Fizz"
                36 => vec![
                    (9, Token::Name("s".into())),
                    (11, Token::Operator(Operator::Equal)),
                    (13, Token::String("Fizz".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     ElseIf n Mod 5 = 0 Then
                37 => vec![
                    (5, Token::Keyword(Keyword::ElseIf)),
                    (12, Token::Name("n".into())),
                    (14, Token::Operator(Operator::Mod)),
                    (18, Token::Integer(5)),
                    (20, Token::Operator(Operator::Equal)),
                    (22, Token::Integer(0)),
                    (24, Token::Keyword(Keyword::Then)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //         s = "Buzz"
                38 => vec![
                    (9, Token::Name("s".into())),
                    (11, Token::Operator(Operator::Equal)),
                    (13, Token::String("Buzz".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     End If
                39 => vec![
                    (5, Token::Keyword(Keyword::End)),
                    (9, Token::Keyword(Keyword::If)),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                //     Print s
                40 => vec![
                    (5, Token::Keyword(Keyword::Print)),
                    (11, Token::Name("s".into())),
                ],
                //          111111111122222222223333333333
                // 123456789012345678901234567890123456789
                // Loop
                41 => vec![(1, Token::Keyword(Keyword::Loop))],
                _ => return None,
            };
            Some((self.pos, v))
        }
    }
}
