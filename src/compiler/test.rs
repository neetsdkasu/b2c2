// crate::compiler::test

use super::*;

#[test]
fn it_works() {
    let src = r#"
        Rem TEST PROGRAM
        Dim bool1 As Boolean
        Dim int1 As Integer
        Dim str1 As String
        Dim intArr1(10) As Integer
        Dim boolArr1(10) As Boolean
        Dim i As Integer
        Dim j As Integer
        Input int1
        ' Input intArr1(3)
        Input str1
        Print False
        Print 1234
        Print "Text"
        Print bool1
        Print int1
        Print str1
        Print 1 + 2 + 3 + int1
        Print (123 < 10)
        Print CStr(123 < 999)
        Let int1 = (1 + int1) + 2
        Let int1 = (1 - int1) - 2
        Let int1 = (1 << int1) << 2
        Let int1 = (1 >> int1) >> 2
        Let int1 = (1 And int1) And 2
        Let int1 = (1 Or int1) Or 2
        Let int1 = (1 Xor int1) Xor 2
        Let int1 = (1 * int1) * 2
        Let int1 = (1 \ int1) \ 2
        Let int1 = (1 Mod int1) Mod 2
        ' Let int1 = - (-int1 + -1)
        ' Let int1 = Not (Not int1 + Not 1)
        Let int1 = Len(str1)
        Let int1 = CInt(bool1)
        Let int1 = CInt(str1)
        Let int1 = int1
        Let int1 = 123
        ' Let bool1 = CBool(int1)
        Let bool1 = 123 < int1
        Let bool1 = bool1
        Let bool1 = True
        Let str1 = CStr(bool1)
        Let str1 = CStr(int1)
        Let str1 = str1
        Let str1 = "XYZ ABC"
        Let str1 = "prefix" & (str1 & "suffix")
        Let intArr1(1 * 2) = 123 * int1
        Let boolArr1(5 - 3) = True
        Let str1(3 * 0) = "A"c
        Let int1 = intArr1(0 + 1) * str1(3 + 1)
        Let bool1 = boolArr1(5 * 1) And Not bool1 Or False Xor True
        int1 += 123 * 5
        intArr1(5 + 1) += 123 - 4
        int1 -= 123 \ 3
        intArr1(4 - 3) -= 123 Mod 2
        For i = 1 To 10
        Print "X"
        Next i
        For i = int1 To int1 Step -5
        Print "X"
        Next i
        For i = int1 - 3 To (int1 << 4) - 2 Step int1 + 1
        Print "X"
        Next i
        For i = 1 To 10
        For j = 1 To 10
            Print "X"
        Next j
        Next i
        Select Case i
        Case 1, 2
        Print "X"
        Case 3, 4, "A"C, "B"C, 55
        Print "X"
        Case Else
        Print "X"
        End Select
        Select Case i
        Case 10, 11, 12
        Print "X"
        End Select
        Select Case i + 10
        Case Else
        Print "X"
        End Select
        Select Case i * i
        End Select
        Select i
        Case 1, 2, 3
        Print "X"
        End Select
        Select i << 1
        Case 1, 2, 4
        Print "X"
        Case Else
        Print "X"
        End Select
        Select Case str1
        Case "ABC","XYZ","EFG"
        Print "X"
        Case "1234"
        Print "X"
        Case Else
        Print "X"
        End Select
        Do
        Continue Do
        Exit Do
        Loop
        Do
        int1 += 1
        Loop Until int1 > 10
        Do
        int1 += 1
        Loop While int1 < 20
        Do Until int1 > 30
        int1 += 1
        Loop
        Do While int1 < 40
        int1 += 1
        Loop
        If int1 = 123 Then
        Print "X"
        ' ElseIf int1 + 3 <> (999 * 10) Then
        Print "X"
        ElseIf int1 - 4 < (543 + 123) Then
        Print "X"
        ElseIf int1 - 4 <= (543 + 123) Then
        Print "X"
        ElseIf int1 - 4 > (543 + 123) Then
        Print "X"
        ElseIf int1 - 4 >= (543 + 123) Then
        Print "X"
        Else If str1 = "xyz" Then
        Print "X"
        ' ElseIf str1 <> "xyz" Then
        Print "X"
        ' ElseIf str1 < "xyz" Then
        Print "X"
        ' ElseIf str1 <= "xyz" Then
        Print "X"
        ' ElseIf str1 > "xyz" Then
        Print "X"
        ' ElseIf str1 >= "xyz" Then
        Print "X"
        ' ElseIf Not bool1 = False Then
        Print "X"
        ' ElseIf bool1 <> Not False Then
        Print "X"
        Else
        Print "X"
        End If
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("TEST", &code[..]).unwrap();

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn compiler_is_valid_program_name_works() {
    assert!(Compiler::is_valid_program_name("TEST"));
    assert!(Compiler::is_valid_program_name("X123"));
    assert!(Compiler::is_valid_program_name("B"));
    assert!(Compiler::is_valid_program_name("B123XY"));

    assert!(!Compiler::is_valid_program_name("GR3")); // register name is BAD
    assert!(!Compiler::is_valid_program_name("")); // empty is BAD
    assert!(!Compiler::is_valid_program_name("FOOBARBAZ")); // too long (require len <= 8)
    assert!(!Compiler::is_valid_program_name("Test")); // lowercase is BAD
    assert!(!Compiler::is_valid_program_name("123TEST")); // digit start is BAD
    assert!(!Compiler::is_valid_program_name("TEST$")); // all chars must be ascii digits or ascii uppercases

    // compiler using names
    assert!(!Compiler::is_valid_program_name("B123"));
    assert!(!Compiler::is_valid_program_name("I123"));
    assert!(!Compiler::is_valid_program_name("SL123"));
    assert!(!Compiler::is_valid_program_name("SB123"));
    assert!(!Compiler::is_valid_program_name("BA123"));
    assert!(!Compiler::is_valid_program_name("IA123"));
}

#[test]
fn compiler_get_lit_str_labels_works() {
    let mut compiler = Compiler::new("TEST").unwrap();

    assert_eq!(
        compiler.get_lit_str_labels("-123"),
        StrLabels {
            len: "LL1".into(),
            buf: "LB1".into(),
            label_type: StrLabelType::Const
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("A b c"),
        StrLabels {
            len: "LL2".into(),
            buf: "LB2".into(),
            label_type: StrLabelType::Const
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("XYZ"),
        StrLabels {
            len: "LL3".into(),
            buf: "LB3".into(),
            label_type: StrLabelType::Const
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("Test@1234"),
        StrLabels {
            len: "LL4".into(),
            buf: "LB4".into(),
            label_type: StrLabelType::Const
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("A b c"),
        StrLabels {
            len: "LL2".into(),
            buf: "LB2".into(),
            label_type: StrLabelType::Const
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("XYZ"),
        StrLabels {
            len: "LL3".into(),
            buf: "LB3".into(),
            label_type: StrLabelType::Const
        }
    );

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
       RET
LL1    DC     4
LB1    DC     '-123'
LL2    DC     5
LB2    DC     'A b c'
LL3    DC     3
LB3    DC     'XYZ'
LL4    DC     9
LB4    DC     'Test@1234'
       END
            "#
            .trim()
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_dim_works() {
    let mut compiler = Compiler::new("TEST").unwrap();

    compiler.compile_dim("strVar1", &parser::VarType::String);
    compiler.compile_dim("boolVar1", &parser::VarType::Boolean);
    compiler.compile_dim("intVar2", &parser::VarType::Integer);
    compiler.compile_dim("boolArr1", &parser::VarType::ArrayOfBoolean(32));
    compiler.compile_dim("boolVar2", &parser::VarType::Boolean);
    compiler.compile_dim("strVar2", &parser::VarType::String);
    compiler.compile_dim("intArr1", &parser::VarType::ArrayOfInteger(155));
    compiler.compile_dim("intVar1", &parser::VarType::Integer);

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
       RET
                                   ; Dim boolVar1 As Boolean
B2     DS     1
                                   ; Dim boolVar2 As Boolean
B5     DS     1
                                   ; Dim intVar2 As Integer
I3     DS     1
                                   ; Dim intVar1 As Integer
I8     DS     1
                                   ; Dim strVar1 As String
SL1    DS     1
SB1    DS     256
                                   ; Dim strVar2 As String
SL6    DS     1
SB6    DS     256
                                   ; Dim boolArr1(31) As Boolean
BA4    DS     32
                                   ; Dim intArr1(154) As Integer
IA7    DS     155
       END
            "#
            .trim()
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_print_lit_boolean_works() {
    let mut compiler = Compiler::new("TEST").unwrap();

    compiler.compile_print_lit_boolean(true);
    compiler.compile_print_lit_boolean(false);
    compiler.compile_print_lit_boolean(false);
    compiler.compile_print_lit_boolean(true);

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
                                   ; Print True
       OUT    LB1,LL1
                                   ; Print False
       OUT    LB2,LL2
                                   ; Print False
       OUT    LB2,LL2
                                   ; Print True
       OUT    LB1,LL1
       RET
LL1    DC     4
LB1    DC     'True'
LL2    DC     5
LB2    DC     'False'
       END
            "#
            .trim()
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_print_lit_integer_works() {
    let mut compiler = Compiler::new("TEST").unwrap();

    compiler.compile_print_lit_integer(1234);
    compiler.compile_print_lit_integer(999);
    compiler.compile_print_lit_integer(-100);
    compiler.compile_print_lit_integer(1234);

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
                                   ; Print 1234
       OUT    LB1,LL1
                                   ; Print 999
       OUT    LB2,LL2
                                   ; Print -100
       OUT    LB3,LL3
                                   ; Print 1234
       OUT    LB1,LL1
       RET
LL1    DC     4
LB1    DC     '1234'
LL2    DC     3
LB2    DC     '999'
LL3    DC     4
LB3    DC     '-100'
       END
            "#
            .trim()
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_print_lit_string_works() {
    let mut compiler = Compiler::new("TEST").unwrap();

    compiler.compile_print_lit_string("ABCD");
    compiler.compile_print_lit_string("hey you!");
    compiler.compile_print_lit_string("");
    compiler.compile_print_lit_string("ABCD");

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
                                   ; Print "ABCD"
       OUT    LB1,LL1
                                   ; Print "hey you!"
       OUT    LB2,LL2
                                   ; Print ""
       OUT    LB3,LL3
                                   ; Print "ABCD"
       OUT    LB1,LL1
       RET
LL1    DC     4
LB1    DC     'ABCD'
LL2    DC     8
LB2    DC     'hey you!'
LL3    DC     0
LB3    DS     0
       END
            "#
            .trim()
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_print_var_string_works() {
    let mut compiler = Compiler::new("TEST").unwrap();

    compiler.compile_dim("strVar1", &parser::VarType::String);
    compiler.compile_dim("strVar2", &parser::VarType::String);
    compiler.compile_dim("strVar3", &parser::VarType::String);
    compiler.compile_print_var_string("strVar3");
    compiler.compile_print_var_string("strVar2");
    compiler.compile_print_var_string("strVar1");

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
                                   ; Print strVar3
       OUT    SB3,SL3
                                   ; Print strVar2
       OUT    SB2,SL2
                                   ; Print strVar1
       OUT    SB1,SL1
       RET
                                   ; Dim strVar1 As String
SL1    DS     1
SB1    DS     256
                                   ; Dim strVar2 As String
SL2    DS     1
SB2    DS     256
                                   ; Dim strVar3 As String
SL3    DS     1
SB3    DS     256
       END
            "#
            .trim()
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_input_string_works() {
    let mut compiler = Compiler::new("TEST").unwrap();

    compiler.compile_dim("strVar1", &parser::VarType::String);
    compiler.compile_dim("strVar2", &parser::VarType::String);
    compiler.compile_dim("strVar3", &parser::VarType::String);
    compiler.compile_input_string("strVar3");
    compiler.compile_input_string("strVar2");
    compiler.compile_input_string("strVar1");

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
                                   ; Input strVar3
       IN     SB3,SL3
       LD     GR0,SL3
       JPL    J1
       XOR    GR0,GR0
       ST     GR0,SL3
J1     NOP
                                   ; Input strVar2
       IN     SB2,SL2
       LD     GR0,SL2
       JPL    J2
       XOR    GR0,GR0
       ST     GR0,SL2
J2     NOP
                                   ; Input strVar1
       IN     SB1,SL1
       LD     GR0,SL1
       JPL    J3
       XOR    GR0,GR0
       ST     GR0,SL1
J3     NOP
       RET
                                   ; Dim strVar1 As String
SL1    DS     1
SB1    DS     256
                                   ; Dim strVar2 As String
SL2    DS     1
SB2    DS     256
                                   ; Dim strVar3 As String
SL3    DS     1
SB3    DS     256
       END
            "#
            .trim()
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_input_integer_works() {
    let mut compiler = Compiler::new("TEST").unwrap();

    compiler.compile_dim("intVar1", &parser::VarType::Integer);

    compiler.compile_input_integer("intVar1");

    assert_eq!(compiler.subroutine_codes.len(), 2);
    assert_eq!(compiler.temp_str_var_labels.len(), 1);

    struct T {
        v: Vec<&'static str>,
    }

    impl subroutine::Gen for T {
        fn jump_label(&mut self) -> String {
            self.v.pop().unwrap().to_string()
        }
        fn var_label(&mut self) -> String {
            unreachable!()
        }
    }

    let max = subroutine::Id::FuncMax;
    let cint = subroutine::Id::FuncCInt;
    let mut t = T {
        v: vec!["J4", "J3", "J2", "J1"],
    };

    let mut statements = casl2::parse(
        format!(
            r#"
TEST   START
                                   ; Input intVar1
       IN     TB1,TL1
       XOR    GR1,GR1
       LD     GR2,TL1
       CALL   {max}
       LD     GR2,GR0
       LAD    GR1,TB1
       CALL   {cint}
       ST     GR0,I1
       RET
            "#,
            max = max.label(),
            cint = cint.label()
        )
        .trim(),
    )
    .unwrap();

    statements.extend(
        casl2::parse(
            r#"
                                   ; Dim intVar1 As Integer
I1     DS     1
TL1    DS     1
TB1    DS     256
            "#
            .trim_start_matches('\n')
            .trim_end(),
        )
        .unwrap(),
    );

    let max_src = subroutine::get_src(&mut t, max).statements;
    let cint_src = subroutine::get_src(&mut t, cint).statements;

    statements.extend(cint_src);
    statements.extend(max_src);

    statements.push(casl2::Statement::code(casl2::Command::End));

    assert_eq!(compiler.finish(), statements);
}

#[test]
fn for_statement_without_step_works() {
    let src = r#"
            Dim i As Integer
            For i = 1 To 10
            Next i
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("TEST", &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                                   ; For i = 1 To 10 Step 1
                     LAD    GR7,10
                     ST     GR7,T1
                     LAD    GR7,1
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,T1
                     JPL    J3
                                   ; Next i
J2                   NOP
                     LD     GR1,I1
                     LAD    GR1,1,GR1
                     ST     GR1,I1
                     JUMP   J1
J3                   NOP
                     RET
                                   ; Dim i As Integer
I1                   DS 1
T1                   DS 1
                     END
"#
        )
        .unwrap()
    );
}

#[test]
fn for_statement_positive_step_works() {
    let src = r#"
            Dim i As Integer
            For i = 1 To 10 Step 1
            Next i
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("TEST", &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                                   ; For i = 1 To 10 Step 1
                     LAD    GR7,10
                     ST     GR7,T1
                     LAD    GR7,1
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,T1
                     JPL    J3
                                   ; Next i
J2                   NOP
                     LD     GR1,I1
                     LAD    GR1,1,GR1
                     ST     GR1,I1
                     JUMP   J1
J3                   NOP
                     RET
                                   ; Dim i As Integer
I1                   DS 1
T1                   DS 1
                     END
"#
        )
        .unwrap()
    );
}

#[test]
fn for_statement_negative_step_works() {
    let src = r#"
            Dim i As Integer
            For i = 24 To 8 Step -2
            Next i
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("TEST", &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                                   ; For i = 24 To 8 Step -2
                     LAD    GR7,8
                     ST     GR7,T1
                     LAD    GR7,24
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,T1
                     JMI    J3
                                   ; Next i
J2                   NOP
                     LD     GR1,I1
                     LAD    GR1,-2,GR1
                     ST     GR1,I1
                     JUMP   J1
J3                   NOP
                     RET
                                   ; Dim i As Integer
I1                   DS 1
T1                   DS 1
                     END
"#
        )
        .unwrap()
    );
}

#[test]
fn for_statement_expr_step_works() {
    let src = r#"
            Dim S As Integer
            Dim I As Integer
            For I = 1 To 10 Step S
            Next I
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("TEST", &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                                   ; For I = 1 To 10 Step S
                     LD     GR7,I1
                     ST     GR7,T1
                     LAD    GR7,10
                     ST     GR7,T2
                     LAD    GR7,1
                     ST     GR7,I2
J1                   NOP
                     LD     GR1,T1
                     JMI    J2
                     LD     GR1,I2
                     CPA    GR1,T2
                     JUMP   J3
J2                   LD     GR1,T2
                     CPA    GR1,I2
J3                   NOP
                     JPL    J5
                                   ; Next I
J4                   NOP
                     LD     GR1,I2
                     ADDA   GR1,T1
                     ST     GR1,I2
                     JUMP   J1
J5                   NOP
                     RET
                                   ; Dim S As Integer
I1                   DS 1
                                   ; Dim I As Integer
I2                   DS 1
T1                   DS 1
T2                   DS 1
                     END
"#
        )
        .unwrap()
    );
}

#[test]
fn expr_add_literal_int_rhs_works() {
    let src = r#"
            Dim x As Integer
            x = 11 + 22
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("TEST", &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                                   ; x = (11 + 22)
                     LAD    GR7,11
                     LAD    GR7,22,GR7
                     ST     GR7,I1
                     RET
                                   ; Dim x As Integer
I1                   DS 1
                     END
"#
        )
        .unwrap()
    );
}

#[test]
fn expr_add_variable_rhs_works() {
    let src = r#"
            Dim x As Integer
            Dim y As Integer
            x = 11 + y
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("TEST", &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                                   ; x = (11 + y)
                     LAD    GR7,11
                     LD     GR6,I2
                     ADDA   GR7,GR6
                     ST     GR7,I1
                     RET
                                   ; Dim x As Integer
I1                   DS 1
                                   ; Dim y As Integer
I2                   DS 1
                     END
"#
        )
        .unwrap()
    );
}

#[test]
fn fizzbuzz_1_works() {
    let src = r#"
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

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("FIZZBUZZ", &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn fizzbuzz_2_works() {
    let src = r#"
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
    Else
        s = CStr(n)
    End If
    Print s
Loop
"#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("FIZZBUZZ", &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn print_primes_works() {
    let src = r#"
' *** PRINT PRIMES ***
Dim flag(255) As Boolean
Dim prime(90) As Integer
Dim count As Integer
Dim i As Integer
Dim j As Integer
Dim s As String
For i = 2 To 255
    flag(i) = False
Next i
count = 0
For i = 2 To 255
    If flag(i) Then
        Continue For
    End If
    prime(count) = i
    count += 1
    For j = i + i To 255 Step i
        flag(j) = True
    Next j
Next i
Print "PRIMES: " & CStr(count)
s = ""
For i = 0 To count - 1
    If prime(i) < 10 Then
        s = s & "  "
    ElseIf prime(i) < 100 Then
        s = s & " "
    End If
    s = s & CStr(prime(i)) & ","
    If i Mod 10 = 9 Then
        Print s
        s = ""
    End If
Next i
If Len(s) > 0 Then
    Print s
End If
"#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("PRIMES", &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn swapcase_works() {
    let src = r#"
' *** SWAPCASE ***
Dim s As String
Dim i As Integer
Print "Swapcase Alphabet"
Input s
For i = 0 To Len(s) - 1
    If s(i) >= "A"c And s(i) <= "Z"c Then
        s(i) = s(i) + "a"c - "A"c
    ElseIf Not (s(i) < "a"c Or s(i) > "z"c) Then
        s(i) = s(i) + "A"c - "a"c
    End If
Next i
Print s
"#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile("SWAPCASE", &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    assert!(!statements.is_empty()); // dummy assert
}
