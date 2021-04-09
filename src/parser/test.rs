// crate::parser::test

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
        (
            vec![(1, Token::Boolean(true))],
            Expr::LitBoolean(true),
            "True",
        ),
        // 1234
        (
            vec![(1, Token::Integer(1234))],
            Expr::LitInteger(1234),
            "1234",
        ),
        // "text"
        (
            vec![(1, Token::String("text".into()))],
            Expr::LitString("text".into()),
            r#""text""#,
        ),
        // boolVar1
        (
            vec![(1, Token::Name(bool_var1.into()))],
            Expr::VarBoolean(bool_var1.into()),
            "boolVar1",
        ),
        // intVar1
        (
            vec![(1, Token::Name(int_var1.into()))],
            Expr::VarInteger(int_var1.into()),
            "intVar1",
        ),
        // strVar1
        (
            vec![(1, Token::Name(str_var1.into()))],
            Expr::VarString(str_var1.into()),
            "strVar1",
        ),
        // Not true
        (
            vec![
                (1, Token::Operator(Operator::Not)),
                (2, Token::Boolean(true)),
            ],
            Expr::UnaryOperatorBoolean(Operator::Not, Box::new(Expr::LitBoolean(true))),
            "Not True",
        ),
        // -1234
        (
            vec![
                (1, Token::Operator(Operator::Sub)),
                (2, Token::Integer(1234)),
            ],
            Expr::LitInteger(-1234),
            "-1234",
        ),
        // -&h8000
        (
            vec![
                (1, Token::Operator(Operator::Sub)),
                (2, Token::Integer(0x8000)),
            ],
            Expr::LitInteger(-0x8000),
            "-32768",
        ),
        // Not 1234
        (
            vec![
                (1, Token::Operator(Operator::Not)),
                (2, Token::Integer(1234)),
            ],
            Expr::UnaryOperatorInteger(Operator::Not, Box::new(Expr::LitInteger(1234))),
            "Not 1234",
        ),
        // Not boolVar1
        (
            vec![
                (1, Token::Operator(Operator::Not)),
                (2, Token::Name(bool_var1.into())),
            ],
            Expr::UnaryOperatorBoolean(Operator::Not, Box::new(Expr::VarBoolean(bool_var1.into()))),
            "Not boolVar1",
        ),
        // Not intVar1
        (
            vec![
                (1, Token::Operator(Operator::Not)),
                (2, Token::Name(int_var1.into())),
            ],
            Expr::UnaryOperatorInteger(Operator::Not, Box::new(Expr::VarInteger(int_var1.into()))),
            "Not intVar1",
        ),
        // -intVar1
        (
            vec![
                (1, Token::Operator(Operator::Sub)),
                (2, Token::Name(int_var1.into())),
            ],
            Expr::UnaryOperatorInteger(Operator::Sub, Box::new(Expr::VarInteger(int_var1.into()))),
            "- intVar1",
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
            "(123 + 456)",
        ),
        // 1 + 1 + 1 + 1 + 1
        (
            vec![
                (1, Token::Integer(1)),
                (2, Token::Operator(Operator::Add)),
                (3, Token::Integer(1)),
                (4, Token::Operator(Operator::Add)),
                (5, Token::Integer(1)),
                (6, Token::Operator(Operator::Add)),
                (7, Token::Integer(1)),
                (8, Token::Operator(Operator::Add)),
                (9, Token::Integer(1)),
            ],
            Expr::BinaryOperatorInteger(
                Operator::Add,
                Box::new(Expr::BinaryOperatorInteger(
                    Operator::Add,
                    Box::new(Expr::BinaryOperatorInteger(
                        Operator::Add,
                        Box::new(Expr::BinaryOperatorInteger(
                            Operator::Add,
                            Box::new(Expr::LitInteger(1)),
                            Box::new(Expr::LitInteger(1)),
                        )),
                        Box::new(Expr::LitInteger(1)),
                    )),
                    Box::new(Expr::LitInteger(1)),
                )),
                Box::new(Expr::LitInteger(1)),
            ),
            "((((1 + 1) + 1) + 1) + 1)",
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
            "(-123 + -456)",
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
            "(12 + (-34 * 56))",
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
            "((intVar1 = 12) Or boolVar1)",
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
            "((3 * intArr1(5 * 10)) And 4)",
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
            "(- Max(123, 456) + (- (987 - 321) * Not intVar1))",
        ),
    ];

    for (tokens, expr, disp) in src {
        assert_eq!(parser.parse_expr(&tokens).unwrap(), expr);
        assert_eq!(expr.to_string(), disp);
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
                            values: vec![CaseIntegerItem::Integer(0)],
                            block: vec![
                                // Print "FizzBuzz"
                                Statement::PrintLitString {
                                    value: "FizzBuzz".into(),
                                },
                            ],
                        },
                        // Case 3, 6, 9, 12
                        Statement::CaseInteger {
                            values: vec![
                                CaseIntegerItem::Integer(3),
                                CaseIntegerItem::Integer(6),
                                CaseIntegerItem::Integer(9),
                                CaseIntegerItem::Integer(12),
                            ],
                            block: vec![
                                // Print "Fizz"
                                Statement::PrintLitString {
                                    value: "Fizz".into(),
                                },
                            ],
                        },
                        // Case 5, 10
                        Statement::CaseInteger {
                            values: vec![CaseIntegerItem::Integer(5), CaseIntegerItem::Integer(10)],
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
                                Statement::PrintExprInteger {
                                    value: Expr::VarInteger("i".into()),
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

#[test]
fn parse_argument_works() -> io::Result<()> {
    let src = r#"
Argument
    ByRef argInt1 As Integer With GR1
    ByRef argStr1 As String With GR2,GR3
    ByVal argStr2 As String With GR4,GR5
    ByRef argArr1(10) As Integer With GR6
    ByVal argArr2(10) As Integer With GR7
End Argument
Extern Sub FOOBAR
    ByRef fbInt1 As Integer With GR1
    ByRef fbStr1 As String With GR2,GR3
    ByVal fbStr2 As String With GR4,GR5
    ByRef fbArr1(10) As Integer With GR6
    ByVal fbArr2(10) As Integer With GR7
End Sub
Dim int1 As Integer
Dim str1 As String
int1 = argInt1 + 1
argInt1 = int1 * 2
str1 = argStr1 & "x"
argStr1 = "x" & str1
argStr1(0) = "A"c
argStr1(1) = "A"c
argStr1(int1) = "A"c
int1 = argStr(0) 
int1 = argStr(1) 
int1 = argStr(int1) 
int1 = argArr1(0)
int1 = argArr1(1)
int1 = argArr1(int1)
argArr1(0) = int1
argArr1(1) = int1
argArr1(int1) = int1
Call FOOBAR
"#;

    let src = io::Cursor::new(src);
    let statements = parse(src)?.unwrap();

    for stmt in statements {
        eprintln!("{:?}", stmt);
    }

    Ok(())
}
