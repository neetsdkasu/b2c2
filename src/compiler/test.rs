// crate::compiler::test

use super::*;

struct Gen {
    jump: Vec<&'static str>,
    var: Vec<&'static str>,
}

impl subroutine::Gen for Gen {
    fn jump_label(&mut self) -> String {
        self.jump.pop().unwrap().to_string()
    }
    fn var_label(&mut self) -> String {
        self.var.pop().unwrap().to_string()
    }
}

#[test]
fn it_works() {
    let src = r#"
    Rem TEST PROGRAM
    Rem コンパイル通るかのチェックだけで、正しくコンパイルされてるかはチェックしていないという…
    Option Allocator Default  ' Off / On / Special
    Option Array     Default  ' UBound / Length
    Option EOF       Default  ' Internal / External
    Option Register  Default  ' Restore / Dirty
    Option Variable  Default  ' Initialize / Uninitialize
    Extern Sub PROC1
    Extern Sub PROC2 With
    End Sub
    Extern Sub PROC3 With
        ByVal arg1 As Boolean To GR3
        ByRef arg2 As Boolean To GR7
        ByVal arg3 As Integer To GR1
        ByRef arg4 As Integer To GR5
    End Sub
    Extern Sub PROC4 With
        ByVal arg1(3) As Boolean To GR7
        ByRef arg2(3) As Boolean To GR6
        ByVal arg3(3) As Integer To GR5
        ByRef arg4(3) As Integer To GR4
    End Sub
    Extern Sub PROC5 With
        ByVal arg1 As String To GR1,GR4
        ByRef arg2 As String To GR6,GR7
    End Sub
    Program TEST
        Argument
            ByRef argBool    As Boolean From GR1
            ByRef argInt     As Integer From GR2
            ByRef argStr     As String  From GR3,GR4
            ByRef argBArr(3) As Boolean From GR5
            ByRef argIArr(3) As Integer From GR6
        End Argument
        Dim bool1 As Boolean
        Dim int1 As Integer
        Dim str1 As String
        Dim intArr1(10) As Integer
        Dim boolArr1(10) As Boolean
        Dim intArr2(3) As Integer
        Dim boolArr2(3) As Boolean
        Dim intArr3(3) As Integer
        Dim boolArr3(3) As Boolean
        Dim i As Integer
        Dim j As Integer
        Input int1
        Input intArr1(3)
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
        int1 = (1 + int1) + 2
        int1 = (1 - int1) - 2
        int1 = (1 << int1) << 2
        int1 = (1 >> int1) >> 2
        int1 = (1 And int1) And 2
        int1 = (1 Or int1) Or 2
        int1 = (1 Xor int1) Xor 2
        int1 = (1 * int1) * 2
        int1 = (1 \ int1) \ 2
        int1 = (1 Mod int1) Mod 2
        int1 = - (-int1 + -1)
        int1 = Not (Not int1 + Not 1)
        int1 = Len(str1)
        int1 = CInt(bool1)
        int1 = CInt(str1)
        int1 = int1
        int1 = 123
        bool1 = CBool(int1)
        bool1 = 123 < int1
        bool1 = bool1
        bool1 = True
        str1 = CStr(bool1)
        str1 = CStr(int1)
        str1 = str1
        str1 = "XYZ ABC"
        str1 = "prefix" & (str1 & "suffix")
        intArr1(1 * 2) = 123 * int1
        boolArr1(5 - 3) = True
        str1(3 * 0) = "A"c
        int1 = intArr1(0 + 1) * str1(3 + 1)
        bool1 = boolArr1(5 * 1) And Not bool1 Or False Xor True
        int1 += 123 * 5
        intArr1(5 + 1) += 123 - 4
        int1 -= 123 \ 3
        intArr1(4 - 3) -= 123 Mod 2
        int1 = Abs(-123) + Abs(int1)
        str1 = Space(120)
        bool1 = EOF()
        int1 = (1+(1+(1+(1+(1+(1+(1+(1+((1+(1+(1+(1+((1+(1+(1+(1+(1+(1+(1+(1+1))))))))+(1+(1+(1+1))))))))+((1+(1+(1+(1+(1+(1+(1+(1+1))))))))+(1+(1+(1+(1+(1+(1+(1+((1+(1+(1+(1+((1+(1+(1+(1+(1+(1+(1+(1+1))))))))+(1+(1+(1+1))))))))+1))))))))))+1))))))))
        int1 = Asc("ABC")
        str1 = Chr("A"c)
        Mid(str1,0) = "X"
        Mid(str1,0,1) = "Z"
        bool1 = Mid("ABCD",2) = "CD"
        bool1 = Mid("ABCD",1,2) = "BC"
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
        ElseIf int1 + 3 <> (999 * 10) Then
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
        ElseIf str1 <> "xyz" Then
            Print "X"
        ElseIf str1 < "xyz" Then
            Print "X"
        ElseIf str1 <= "xyz" Then
            Print "X"
        ElseIf str1 > "xyz" Then
            Print "X"
        ElseIf str1 >= "xyz" Then
            Print "X"
        ElseIf Not bool1 = False Then
            Print "X"
        ElseIf bool1 <> Not False Then
            Print "X"
        Else
            Print "X"
        End If
        int1 = Max((((123,45)))) ' これも合法ｗｗ、パラメータリスト・タプルのような感じになってるおｗｗ
        Exit Program
        argBool = Not argBool
        argInt = argInt * 3 + Len(argStr)
        argInt += int1 + CInt(argStr)
        argInt -= int1
        argStr = argStr & "," & Mid(argStr, 1, 3)
        argStr(1) = argStr(1) + 1
        argStr(int1) = argStr(int1) + 1
        argBArr(1) = Not argBArr(0)
        argBArr(int1) = Not argBArr(int1)
        argIArr(1) = 2 * argIArr(1)
        argIArr(1) += 2 * argIArr(1)
        argIArr(1) -= 2 * argIArr(1)
        argIArr(int1) = 2 * argIArr(int1)
        argIArr(int1) += 2 * argIArr(int1)
        argIArr(int1) -= 2 * argIArr(int1)
        For argInt = 1 To 5
            Print argBool
            Print argInt
            Print argStr
            Input argInt
            Input argStr
            Input argIArr(1)
            Input argIArr(int1)
        Next argInt
        bool1 = (boolArr2 = boolArr2) Or (boolArr2 <> boolArr2)
        bool1 = (argBArr  = argBArr ) Or (argBArr  <> argBArr )
        bool1 = (boolArr2 = argBArr ) Or (boolArr2 <> argBArr )
        bool1 = (argBArr  = boolArr2) Or (argBArr  <> boolArr2)
        bool1 = (intArr2 = intArr2) Or (intArr2 <> intArr2)
        bool1 = (argIArr = argIArr) Or (argIArr <> argIArr)
        bool1 = (intArr2 = argIArr) Or (intArr2 <> argIArr)
        bool1 = (argIArr = intArr2) Or (argIArr <> intArr2)
        bool1 = (intArr2 >= intArr2) Or (intArr2 <= intArr2)
        bool1 = (argIArr >= argIArr) Or (argIArr <= argIArr)
        bool1 = (intArr2 >= argIArr) Or (intArr2 <= argIArr)
        bool1 = (argIArr >= intArr2) Or (argIArr <= intArr2)
        bool1 = (intArr2 > intArr2) Or (intArr2 < intArr2)
        bool1 = (argIArr > argIArr) Or (argIArr < argIArr)
        bool1 = (intArr2 > argIArr) Or (intArr2 < argIArr)
        bool1 = (argIArr > intArr2) Or (argIArr < intArr2)
        Call PROC1
        Call PROC1()
        Call PROC1 With
        End Call
        Call PROC2
        Call PROC2()
        Call PROC2 With
        End Call
        Call PROC3(True, False, 123, -123)
        Call PROC3  (True, False, 123, -123)
        Call PROC3  True, False, 123, -123
        Call PROC3  (((True, False, 123, -123))) '　ParamList雑解釈その1
        Call PROC3  (True, False), (123, -123)   '　ParamList雑解釈その2
        Call PROC3  (True, False, 123), -123
        Call PROC3  True, ((False, 123), -123)   ' ParamListの結合法則(笑)
        Call PROC3(bool1, bool1, int1, int1)
        Call PROC3 With
            arg1 = bool1
            arg2 = bool1
            arg3 = int1
            arg4 = int1
        End Call
        Call PROC3(boolArr1(int1), boolArr1(int1), intArr1(int1), intArr1(int1))
        Call PROC3 With
            arg1 = boolArr1(int1)
            arg2 = boolArr1(int1)
            arg3 = intArr1(int1)
            arg4 = intArr1(int1)
        End Call
        Call PROC3(True, True, 1, 1)
        Call PROC3 With
            arg1 = True
            arg2 = True
            arg3 = 1
            arg4 = 1
        End Call
        Call PROC3(True, True, str1(int1), str1(int1))
        Call PROC3 With
            arg1 = True
            arg2 = True
            arg3 = str1(int1)
            arg4 = str1(int1)
        End Call
        Call PROC3 With
            arg4 = 1
            arg1 = True
            arg3 = 1
            arg2 = True
        End Call
        Call PROC3(argBool, argBool, argInt, argInt)
        Call PROC3 With
            arg1 = argBool
            arg2 = argBool
            arg3 = argInt
            arg4 = argInt
        End Call
        Call PROC3(argBool, argBool, argStr(int1), argStr(int1))
        Call PROC3 With
            arg1 = argBool
            arg2 = argBool
            arg3 = argStr(int1)
            arg4 = argStr(int1)
        End Call
        Call PROC3(argBArr(int1+1), argBArr(int1+1), argIArr(int1+1), argIArr(int1+1))
        Call PROC3 With
            arg1 = argBArr(int1+1)
            arg2 = argBArr(int1+1)
            arg3 = argIArr(int1+1)
            arg4 = argIArr(int1+1)
        End Call
        Call PROC4(boolArr2, boolArr2, intArr2, intArr2)
        Call PROC4 With
            arg1 = boolArr2
            arg2 = boolArr2
            arg3 = intArr2
            arg4 = intArr2
        End Call
        Call PROC4(argBArr, argBArr, argIArr, argIArr)
        Call PROC4 With
            arg1 = argBArr
            arg2 = argBArr
            arg3 = argIArr
            arg4 = argIArr
        End Call
        Call PROC5("ABC", "XYZ")
        Call PROC5 With
            arg1 = "ABC"
            arg2 = "XYZ"
        End Call
        Call PROC5("ABC" & "123", "XYZ" & "123")
        Call PROC5 With
            arg1 = "ABC" & "123"
            arg2 = "XYZ" & "123"
        End Call
        Call PROC5(str1, str1)
        Call PROC5 With
            arg1 = str1
            arg2 = str1
        End Call
        Call PROC5(argStr, argStr)
        Call PROC5 With
            arg1 = argStr
            arg2 = argStr
        End Call
        boolArr2 = boolArr3
        boolArr2 = argBArr
        argBArr = boolArr2
        intArr2 = intArr3
        intArr2 = argIArr
        argIArr = intArr2
        boolArr2 = Array(True, False, True, True)
        intArr2 = Array(1, 2, 3, 4)
        boolArr2 = SubArray(boolArr1, 0, 4)
        boolArr2 = SubArray(boolArr1, int1, 4)
        intArr2 = SubArray(intArr1, 0, 4)
        intArr2 = SubArray(intArr1, int1, 4)
        boolArr2 = CArray(boolArr1, 4)
        boolArr1 = CArray(boolArr2, 11)
        intArr2 = CArray(intArr1, 4)
        intArr1 = CArray(intArr2, 11)
        intArr1 = CArray(str1, 11)
        intArr2 = CArray("ABCD", 4)
        str1 = String(intArr1)
        str1 = String(Array("A"c, "B"c, "C"c, "D"c))
        str1 = String(10, "A"c)
        int1 = Len(boolArr1)
        int1 = Len(argBArr)
        int1 = Len(intArr1)
        int1 = Len(argIArr)
        int1 = Len(Array(1,2,3,4))
        Fill (boolArr1, True)
        Fill boolArr2, Not False
        Fill argBArr, 1 <> int1
        Fill intArr1, 12*34+int1
        Fill argIArr, 99
        Fill str1, "X"c
        Fill argStr, "e"c
    End Program
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile(Some("TEST".into()), &code[..]).unwrap();

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn compiler_is_valid_program_name_works() {
    assert!(is_valid_program_name("TEST"));
    assert!(is_valid_program_name("X123"));
    assert!(is_valid_program_name("B"));
    assert!(is_valid_program_name("B123XY"));

    assert!(!is_valid_program_name("GR3")); // register name is BAD
    assert!(!is_valid_program_name("")); // empty is BAD
    assert!(!is_valid_program_name("FOOBARBAZ")); // too long (require len <= 8)
    assert!(!is_valid_program_name("Test")); // lowercase is BAD
    assert!(!is_valid_program_name("123TEST")); // digit start is BAD
    assert!(!is_valid_program_name("TEST$")); // all chars must be ascii digits or ascii uppercases

    // compiler using names
    assert!(!is_valid_program_name("B123"));
    assert!(!is_valid_program_name("I123"));
    assert!(!is_valid_program_name("SL123"));
    assert!(!is_valid_program_name("SB123"));
    assert!(!is_valid_program_name("BA123"));
    assert!(!is_valid_program_name("IA123"));
}

#[test]
fn compiler_get_lit_str_labels_works() {
    let mut compiler = Compiler::new(Some("TEST".into())).unwrap();

    assert_eq!(
        compiler.get_lit_str_labels("-123"),
        StrLabels {
            len: "LL1".into(),
            pos: "LB1".into(),
            label_type: StrLabelType::Const("-123".into())
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("A b c"),
        StrLabels {
            len: "LL2".into(),
            pos: "LB2".into(),
            label_type: StrLabelType::Const("A b c".into())
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("XYZ"),
        StrLabels {
            len: "LL3".into(),
            pos: "LB3".into(),
            label_type: StrLabelType::Const("XYZ".into())
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("Test@1234"),
        StrLabels {
            len: "LL4".into(),
            pos: "LB4".into(),
            label_type: StrLabelType::Const("Test@1234".into())
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("A b c"),
        StrLabels {
            len: "LL2".into(),
            pos: "LB2".into(),
            label_type: StrLabelType::Const("A b c".into())
        }
    );
    assert_eq!(
        compiler.get_lit_str_labels("XYZ"),
        StrLabels {
            len: "LL3".into(),
            pos: "LB3".into(),
            label_type: StrLabelType::Const("XYZ".into())
        }
    );

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
       RPUSH
EXIT   NOP
       RPOP
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
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_dim_works() {
    let mut compiler = Compiler::new(Some("TEST".into())).unwrap();

    compiler.compile_dim("strVar1", &parser::VarType::String);
    compiler.compile_dim("boolVar1", &parser::VarType::Boolean);
    compiler.compile_dim("intVar2", &parser::VarType::Integer);
    compiler.compile_dim("boolArr1", &parser::VarType::ArrayOfBoolean(32));
    compiler.compile_dim("boolVar2", &parser::VarType::Boolean);
    compiler.compile_dim("strVar2", &parser::VarType::String);
    compiler.compile_dim("intArr1", &parser::VarType::ArrayOfInteger(155));
    compiler.compile_dim("intVar1", &parser::VarType::Integer);

    let fill = subroutine::Id::UtilFill;

    let mut statements = casl2::parse(&format!(
        r#"
TEST   START
       RPUSH
                                   ; Init Variables
       LAD    GR1,B2
       XOR    GR2,GR2
       LAD    GR3,705
       CALL   {fill}
EXIT   NOP
       RPOP
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
            "#,
        fill = fill.label()
    ))
    .unwrap();

    let mut gen = Gen {
        jump: vec!["J2", "J1"],
        var: vec![],
    };

    statements.extend(subroutine::get_src(&mut gen, fill).statements);
    statements.push(casl2::Statement::code(casl2::Command::End));

    assert_eq!(compiler.finish(), statements);
}

#[test]
fn compiler_compile_print_lit_boolean_works() {
    let mut compiler = Compiler::new(Some("TEST".into())).unwrap();

    compiler.compile_print_lit_boolean(true);
    compiler.compile_print_lit_boolean(false);
    compiler.compile_print_lit_boolean(false);
    compiler.compile_print_lit_boolean(true);

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
       RPUSH
                                   ; Print True
       OUT    LB1,LL1
                                   ; Print False
       OUT    LB2,LL2
                                   ; Print False
       OUT    LB2,LL2
                                   ; Print True
       OUT    LB1,LL1
EXIT   NOP
       RPOP
       RET
LL1    DC     4
LB1    DC     'True'
LL2    DC     5
LB2    DC     'False'
       END
"#
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_print_lit_integer_works() {
    let mut compiler = Compiler::new(Some("TEST".into())).unwrap();

    compiler.compile_print_lit_integer(1234);
    compiler.compile_print_lit_integer(999);
    compiler.compile_print_lit_integer(-100);
    compiler.compile_print_lit_integer(1234);

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
       RPUSH
                                   ; Print 1234
       OUT    LB1,LL1
                                   ; Print 999
       OUT    LB2,LL2
                                   ; Print -100
       OUT    LB3,LL3
                                   ; Print 1234
       OUT    LB1,LL1
EXIT   NOP
       RPOP
       RET
LL1    DC     4
LB1    DC     '1234'
LL2    DC     3
LB2    DC     '999'
LL3    DC     4
LB3    DC     '-100'
       END
"#
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_print_lit_string_works() {
    let mut compiler = Compiler::new(Some("TEST".into())).unwrap();

    compiler.compile_print_lit_string("ABCD");
    compiler.compile_print_lit_string("hey you!");
    compiler.compile_print_lit_string("");
    compiler.compile_print_lit_string("ABCD");

    assert_eq!(
        compiler.finish(),
        casl2::parse(
            r#"
TEST   START
       RPUSH
                                   ; Print "ABCD"
       OUT    LB1,LL1
                                   ; Print "hey you!"
       OUT    LB2,LL2
                                   ; Print ""
       OUT    LB3,LL3
                                   ; Print "ABCD"
       OUT    LB1,LL1
EXIT   NOP
       RPOP
       RET
LL1    DC     4
LB1    DC     'ABCD'
LL2    DC     8
LB2    DC     'hey you!'
LL3    DC     0
LB3    DS     0
       END
"#
        )
        .unwrap()
    );
}

#[test]
fn compiler_compile_print_var_string_works() {
    let mut compiler = Compiler::new(Some("TEST".into())).unwrap();

    compiler.compile_dim("strVar1", &parser::VarType::String);
    compiler.compile_dim("strVar2", &parser::VarType::String);
    compiler.compile_dim("strVar3", &parser::VarType::String);
    compiler.compile_print_var_string("strVar3");
    compiler.compile_print_var_string("strVar2");
    compiler.compile_print_var_string("strVar1");

    let fill = subroutine::Id::UtilFill;

    let mut statements = casl2::parse(&format!(
        r#"
TEST   START
       RPUSH
                                   ; Init Variables
       LAD    GR1,SL1
       XOR    GR2,GR2
       LAD    GR3,771
       CALL   {fill}
                                   ; Print strVar3
       OUT    SB3,SL3
                                   ; Print strVar2
       OUT    SB2,SL2
                                   ; Print strVar1
       OUT    SB1,SL1
EXIT   NOP
       RPOP
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
"#,
        fill = fill.label()
    ))
    .unwrap();

    let mut gen = Gen {
        jump: vec!["J2", "J1"],
        var: vec![],
    };

    statements.extend(subroutine::get_src(&mut gen, fill).statements);
    statements.push(casl2::Statement::code(casl2::Command::End));

    assert_eq!(compiler.finish(), statements);
}

#[test]
fn compiler_compile_input_string_works() {
    let mut compiler = Compiler::new(Some("TEST".into())).unwrap();

    compiler.compile_dim("strVar1", &parser::VarType::String);
    compiler.compile_dim("strVar2", &parser::VarType::String);
    compiler.compile_dim("strVar3", &parser::VarType::String);
    compiler.compile_input_string("strVar3");
    compiler.compile_input_string("strVar2");
    compiler.compile_input_string("strVar1");

    let fill = subroutine::Id::UtilFill;

    let mut statements = casl2::parse(&format!(
        r#"
TEST   START
       RPUSH
                                   ; Init Variables
       LAD    GR1,SL1
       XOR    GR2,GR2
       LAD    GR3,771
       CALL   {fill}
                                   ; Input strVar3
       IN     SB3,SL3
       XOR    GR0,GR0
       ST     GR0,EOF
       LD     GR0,SL3
       JPL    J1
       JZE    J1
       ST     GR0,EOF
       XOR    GR0,GR0
       ST     GR0,SL3
J1     NOP
                                   ; Input strVar2
       IN     SB2,SL2
       XOR    GR0,GR0
       ST     GR0,EOF
       LD     GR0,SL2
       JPL    J2
       JZE    J2
       ST     GR0,EOF
       XOR    GR0,GR0
       ST     GR0,SL2
J2     NOP
                                   ; Input strVar1
       IN     SB1,SL1
       XOR    GR0,GR0
       ST     GR0,EOF
       LD     GR0,SL1
       JPL    J3
       JZE    J3
       ST     GR0,EOF
       XOR    GR0,GR0
       ST     GR0,SL1
J3     NOP
EXIT   NOP
       RPOP
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
EOF    DS     1
"#,
        fill = fill.label()
    ))
    .unwrap();

    let mut gen = Gen {
        jump: vec!["J5", "J4"],
        var: vec![],
    };

    statements.extend(subroutine::get_src(&mut gen, fill).statements);
    statements.push(casl2::Statement::code(casl2::Command::End));

    assert_eq!(compiler.finish(), statements);
}

#[test]
fn compiler_compile_input_integer_works() {
    let mut compiler = Compiler::new(Some("TEST".into())).unwrap();

    compiler.compile_dim("intVar1", &parser::VarType::Integer);

    compiler.compile_input_integer("intVar1");

    assert_eq!(compiler.subroutine_codes.len(), 1);
    assert_eq!(compiler.temp_str_var_labels.len(), 1);

    let cint = subroutine::Id::FuncCInt;

    let mut statements = casl2::parse(&format!(
        r#"
TEST   START
       RPUSH
                                   ; Init Variable
       XOR    GR0,GR0
       ST     GR0,I1
                                   ; Input intVar1
       IN     TB1,TL1
       XOR    GR0,GR0
       ST     GR0,EOF
       LAD    GR1,TB1
       LD     GR2,TL1
       JPL    J4
       JZE    J4
       ST     GR2,EOF
       XOR    GR2,GR2
J4     CALL   {cint}
       ST     GR0,I1
EXIT   NOP
       RPOP
       RET
                                   ; Dim intVar1 As Integer
I1     DS     1
EOF    DS     1
TL1    DS     1
TB1    DS     256
"#,
        cint = cint.label()
    ))
    .unwrap();

    let mut gen = Gen {
        jump: vec!["J6", "J5", "J3", "J2", "J1"],
        var: vec![],
    };

    let cint_src = subroutine::get_src(&mut gen, cint).statements;

    statements.extend(cint_src);

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

    let statements = compile(Some("TEST".into()), &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                     RPUSH
                                   ; Init Variable
                     XOR    GR0,GR0
                     ST     GR0,I1
                                   ; For i = 1 To 10 Step 1
                     LAD    GR7,1
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,=10
                     JPL    J3
                                   ; Next i
J2                   NOP
                     LAD    GR1,I1
                     LD     GR2,0,GR1
                     LAD    GR2,1,GR2
                     ST     GR2,0,GR1
                     JUMP   J1
J3                   NOP
EXIT                 NOP
                     RPOP
                     RET
                                   ; Dim i As Integer
I1                   DS 1
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

    let statements = compile(Some("TEST".into()), &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                     RPUSH
                                   ; Init Variable
                     XOR    GR0,GR0
                     ST     GR0,I1
                                   ; For i = 1 To 10 Step 1
                     LAD    GR7,1
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,=10
                     JPL    J3
                                   ; Next i
J2                   NOP
                     LAD    GR1,I1
                     LD     GR2,0,GR1
                     LAD    GR2,1,GR2
                     ST     GR2,0,GR1
                     JUMP   J1
J3                   NOP
EXIT                 NOP
                     RPOP
                     RET
                                   ; Dim i As Integer
I1                   DS 1
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

    let statements = compile(Some("TEST".into()), &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                     RPUSH
                                   ; Init Variable
                     XOR    GR0,GR0
                     ST     GR0,I1
                                   ; For i = 24 To 8 Step -2
                     LAD    GR7,24
                     ST     GR7,I1
J1                   NOP
                     LD     GR1,I1
                     CPA    GR1,=8
                     JMI    J3
                                   ; Next i
J2                   NOP
                     LAD    GR1,I1
                     LD     GR2,0,GR1
                     LAD    GR2,-2,GR2
                     ST     GR2,0,GR1
                     JUMP   J1
J3                   NOP
EXIT                 NOP
                     RPOP
                     RET
                                   ; Dim i As Integer
I1                   DS 1
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

    let statements = compile(Some("TEST".into()), &code[..]).unwrap();

    let fill = subroutine::Id::UtilFill;

    let mut right_statements = casl2::parse(&format!(
        r#"TEST      START
                     RPUSH
                                   ; Init Variables
                     LAD    GR1,I1
                     XOR    GR2,GR2
                     LAD    GR3,2
                     CALL   {fill}
                                   ; For I = 1 To 10 Step S
                     LD     GR7,I1
                     ST     GR7,T1
                     LAD    GR7,1
                     ST     GR7,I2
J1                   NOP
                     LD     GR1,T1
                     JMI    J2
                     LD     GR1,I2
                     CPA    GR1,=10
                     JUMP   J3
J2                   LAD    GR0,10
                     LAD    GR1,I2
                     CPA    GR0,0,GR1
J3                   NOP
                     JPL    J5
                                   ; Next I
J4                   NOP
                     LAD    GR1,I2
                     LD     GR0,0,GR1
                     ADDA   GR0,T1
                     ST     GR0,0,GR1
                     JUMP   J1
J5                   NOP
EXIT                 NOP
                     RPOP
                     RET
                                   ; Dim S As Integer
I1                   DS 1
                                   ; Dim I As Integer
I2                   DS 1
T1                   DS 1
"#,
        fill = fill.label()
    ))
    .unwrap();

    let mut gen = Gen {
        jump: vec!["J7", "J6"],
        var: vec![],
    };

    right_statements.extend(subroutine::get_src(&mut gen, fill).statements);
    right_statements.push(casl2::Statement::code(casl2::Command::End));

    assert_eq!(statements, right_statements);
}

#[test]
fn expr_add_literal_int_rhs_works() {
    let src = r#"
            Dim x As Integer
            x = 11 + 22
        "#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile(Some("TEST".into()), &code[..]).unwrap();

    assert_eq!(
        statements,
        casl2::parse(
            r#"TEST  START
                     RPUSH
                                   ; Init Variable
                     XOR    GR0,GR0
                     ST     GR0,I1
                                   ; x = (11 + 22)
                     LAD    GR7,11
                     LAD    GR7,22,GR7
                     ST     GR7,I1
EXIT                 NOP
                     RPOP
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

    let statements = compile(Some("TEST".into()), &code[..]).unwrap();

    let fill = subroutine::Id::UtilFill;

    let mut right_statements = casl2::parse(&format!(
        r#"TEST      START
                     RPUSH
                                   ; Init Variables
                     LAD    GR1,I1
                     XOR    GR2,GR2
                     LAD    GR3,2
                     CALL   {fill}
                                   ; x = (11 + y)
                     LAD    GR7,11
                     LD     GR6,I2
                     ADDA   GR7,GR6
                     ST     GR7,I1
EXIT                 NOP
                     RPOP
                     RET
                                   ; Dim x As Integer
I1                   DS 1
                                   ; Dim y As Integer
I2                   DS 1
"#,
        fill = fill.label()
    ))
    .unwrap();

    let mut gen = Gen {
        jump: vec!["J2", "J1"],
        var: vec![],
    };

    right_statements.extend(subroutine::get_src(&mut gen, fill).statements);
    right_statements.push(casl2::Statement::code(casl2::Command::End));

    assert_eq!(statements, right_statements);
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

    let statements = compile(Some("FIZZBUZZ".into()), &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    eprintln!("{}", crate::stat::analyze(&statements));

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

    let statements = compile(Some("FIZZBUZZ".into()), &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    eprintln!("{}", crate::stat::analyze(&statements));

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
        s = s & Space(2)
    ElseIf prime(i) < 100 Then
        s = s & Space(1)
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

    let statements = compile(Some("PRIMES".into()), &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    eprintln!("{}", crate::stat::analyze(&statements));

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

    let statements = compile(Some("SWAPCASE".into()), &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    eprintln!("{}", crate::stat::analyze(&statements));

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn factors_works() {
    let src = r#"
' *** FACTORS ***
Dim factor(20) As Integer
Dim count As Integer
Dim i As Integer
Dim n As Integer
Dim s As String
Print "Number?"
Input n
Print "Number is " & CStr(n)
If n < 2 Then
    Print "Invalid Number"
Else
    count = 0
    i = 2
    Do
        Do Until n Mod i <> 0
            factor(count) = i
            count += 1
            n = n \ i
        Loop
        i += 1
    Loop While i * i <= n  ' i=181のときi*i=32761(ギリギリ32768未満),i=182以上は無限ループ(32761～32678に素数がないので無限ループはしない)
    If n > 1 Then
        factor(count) = n
        count += 1
    End If
    Print "FACTORS: " & CStr(count)
    s = ""
    For i = 0 To count - 1
        s = s & CStr(factor(i)) & ", "
        If Len(s) > 60 Then
            Print s
            s = ""
        End If
    Next i
    If Len(s) > 0 Then
        Print s
    End If
End If
"#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile(Some("FACTORS".into()), &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    eprintln!("{}", crate::stat::analyze(&statements));

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn string_sorting_works() {
    let src = r#"
' *** STRSORT ***
Dim i As Integer
Dim p0 As Integer
Dim p1 As Integer
Dim p2 As Integer
Dim cnt As Integer
Dim ub As Integer
Dim s As String
Dim w1 As String
Dim w2 As String
Print "WORDS?"
Input s
ub = Len(s) - 1
For i = 0 To ub
    If s(i) = ","c Then
        cnt += 1
    End If
Next i
For i = 1 To cnt
    p0 = 0
    For p1 = 0 To ub
        If s(p1) = ","c Then
            Exit For
        End If
    Next p1
    For p2 = p1 + 1 To ub
        If s(p2) = ","c Then
            w1 = Mid(s, p0, p1 - p0)
            w2 = Mid(s, p1 + 1, p2 - (p1 + 1))
            If w1 > w2 Then
                Mid(s, p0) = w2 & "," & w1
                p0 += Len(w2) + 1
            Else
                p0 = p1 + 1
            End If
            p1 = p2
        End If
    Next p2
    w1 = Mid(s, p0, p1 - p0)
    w2 = Mid(s, p1 + 1, p2 - (p1 + 1))
    If w1 > w2 Then
        Mid(s, p0) = w2 & "," & w1
    End If
Next i
Print s
"#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile(Some("STRSORT".into()), &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    eprintln!("{}", crate::stat::analyze(&statements));

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn integer_sorting_works() {
    let src = r#"
' *** INTSORT ***
Dim c As Integer
Dim i As Integer
Dim j As Integer
Dim p As Integer
Dim s As String
Dim t As String
Dim arr(255) As Integer
Print "NUMBERS?"
Input s
arr(0) = CInt(s)
For i = 0 To Len(s) - 1
    If s(i) = ","c Then
        c += 1
        arr(c) = CInt(Mid(s, i + 1))
    End If
Next i
For i = 0 To c
    For j = 0 To c - 1 - i
        p = arr(j + 1)
        If arr(j) < p Then
            arr(j + 1) = arr(j)
            arr(j) = p
        End If
    Next j
    t = t & CStr(arr(j)) & ","
Next i
Print t
"#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile(Some("INTSORT".into()), &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    eprintln!("{}", crate::stat::analyze(&statements));

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn brainfck_works() {
    let src = r#"
' *** BRAINFCK ***
' ++++++++[>+++++++++[>+>++<<-]++[>>---<<-]<-]>>.>+++++.+++++++..+++.
Dim mem(255) As Integer
Dim mp As Integer
Dim pc As Integer
Dim cmd As String
Dim i As Integer
Dim brc As Integer
Dim inStr As String
Dim outStr As String
Print "Command?"
Input cmd
i = -1
Do While pc < Len(cmd)
    Select Case cmd(pc)
        Case "+"c
            mem(mp) += 1
        Case "-"c
            mem(mp) -= 1
        Case "<"c
            mp -= 1
        Case ">"c
            mp += 1
        Case "["c
            If mem(mp) = 0 Then
                Do
                    Select Case cmd(pc)
                        Case "["c
                            brc += 1
                        Case "]"c
                            brc -= 1
                    End Select
                    pc += 1
                Loop While brc <> 0 And pc < Len(cmd)
                Continue Do
            End If
        Case "]"c
            Do
                Select Case cmd(pc)
                Case "["c
                    brc += 1
                Case "]"c
                    brc -= 1
                End Select
                pc -= 1
            Loop While brc <> 0 And pc >= 0
            If brc <> 0 Then
                Exit Do
            End If
        Case ","c
            If i < 0 Then
                Input inStr
                If Len(inStr) = 0 And Not EOF() Then
                    mem(mp) = 13
                    i = -1
                Else
                    mem(mp) = inStr(0)
                    i = 1
                End If
            ElseIf i >= Len(inStr) Then
                mem(mp) = 13
                i = -1
            Else
                mem(mp) = inStr(i)
                i += 1
            End If
        Case "."c
            If mem(mp) = 13 Then
                Print outStr
                outStr = ""
            Else
                outStr = outStr & Chr(mem(mp))
                If Len(outStr) >= 40 Then
                    Print outStr
                    outStr = ""
                End If
            End If
    End Select
    pc += 1
Loop
Print outStr
"#;

    let mut cursor = std::io::Cursor::new(src);

    let code = parser::parse(&mut cursor).unwrap().unwrap();

    let statements = compile(Some("BRAINFCK".into()), &code[..]).unwrap();

    statements.iter().for_each(|line| {
        eprintln!("{}", line);
    });

    eprintln!("{}", crate::stat::analyze(&statements));

    eprintln!("TEST CODE");
    eprintln!("++++++++[>+++++++++[>+>++<<-]++[>>---<<-]<-]>>.>+++++.+++++++..+++.");

    assert!(!statements.is_empty()); // dummy assert
}

#[test]
fn with_arguments_1_works() {
    let src1 = r#"
Extern Sub FIZZ With
    ByVal num As Integer To GR1
    ByRef fizzBuzz As String To GR2,GR3
End Sub
Program MAIN
    Dim i As Integer
    Dim s As String
    Dim t As String
    t = ""
    For i = 1 To 20
        Call FIZZ (i, s)
        t = t & s & ", "
    Next i
    Print t
End Program
"#;

    let src2 = r#"
Program FIZZ
    Argument
        ByVal num As Integer From GR1
        ByRef fizzBuzz As String From GR2,GR3
    End Argument
    Select Case num Mod 15
    Case 0
        fizzBuzz = "FizzBuzz"
    Case 3, 6, 9, 12
        fizzBuzz = "FIZZ"
    Case 5, 10
        fizzBuzz = "BUZZ"
    Case Else
        fizzBuzz = CStr(num)
    End Select
End Program
"#;

    for src in [src1, src2].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }
}

#[test]
fn with_arguments_2_works() {
    let src1 = r#"
Extern Sub FOO With
    ByVal arg(3) As Boolean To GR1
End Sub
Extern Sub BAR With
    ByRef arg(3) As Boolean To GR1
End Sub
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByRef arg(3) As Boolean To GR1
End Sub
Program TEST
    Dim arr(3) As Boolean
    Dim big(5) As Boolean
    big = Array(True, False, True, False, True, False)

    Call BAZ("TEST 1", arr)

    arr = Array(False, True, True, False)
    Call BAZ("TEST 2", arr)

    arr = SubArray(big, 1, 4)
    Call BAZ("TEST 3", arr)

    arr = CArray(big, 4)
    Call BAZ("TEST 4", arr)

    Fill arr, True
    Call BAZ("TEST 5", arr)

    Call FOO(arr)
    Call BAZ("TEST 6", arr)

    Call BAR(arr)
    Call BAZ("TEST 7", arr)
End Program
"#;

    let src2 = r#"
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByRef arg(3) As Boolean To GR1
End Sub
Program FOO
    Argument
        ByVal arr(3) As Boolean From GR1
    End Argument
    Dim big(5) As Boolean
    big = Array(True, True, True, False, False, False)

    Call BAZ("FOO 1", arr)

    arr = Array(False, True, True, False)
    Call BAZ("FOO 2", arr)

    Fill arr, True
    Call BAZ("FOO 3", arr)

    arr = SubArray(big, 2, 4)
    Call BAZ("FOO 4", arr)

    arr = CArray(big, 4)
    Call BAZ("FOO 5", arr)
End Program
"#;

    let src3 = r#"
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByRef arg(3) As Boolean To GR1
End Sub
Program BAR
    Argument
        ByRef arr(3) As Boolean From GR1
    End Argument
    Dim big(5) As Boolean
    big = Array(False, False, True, True, False, False)

    Call BAZ("BAR 1", arr)

    arr = Array(False, True, True, False)
    Call BAZ("BAR 2", arr)

    Fill arr, True
    Call BAZ("BAR 3", arr)

    arr = SubArray(big, 2, 4)
    Call BAZ("BAR 4", arr)

    arr = CArray(big, 4)
    Call BAZ("BAR 5", arr)
End Program
"#;

    let src4 = r#"
Program BAZ
    Argument
        ByRef msg As String From GR2,GR3
        ByRef arr(3) As Boolean From GR1
    End Argument
    Dim i As Integer
    Dim s As String
    s = ""
    For i = 0 To Len(arr) - 1
        s = s & CStr(arr(i)) & ", "
    Next i
    Print msg
    Print s
End Program
"#;

    for src in [src1, src2, src3, src4].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }
}

#[test]
fn with_arguments_3_works() {
    let src1 = r#"
Extern Sub FOO With
    ByVal arg(3) As Integer To GR1
End Sub
Extern Sub BAR With
    ByRef arg(3) As Integer To GR1
End Sub
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByRef arg(3) As Integer To GR1
End Sub
Program TEST
    Dim arr(3) As Integer
    Dim big(5) As Integer
    big = Array(1, 2, 3, 4, 5, 6)

    Call BAZ("TEST 1", arr)

    arr = Array(100, 200, 300, 400)
    Call BAZ("TEST 2", arr)

    arr = SubArray(big, 1, 4)
    Call BAZ("TEST 3", arr)

    arr = CArray(big, 4)
    Call BAZ("TEST 4", arr)

    arr = CArray("ABCD", 4)
    Call BAZ("TEST 5", arr)

    Fill arr, 123
    Call BAZ("TEST 6", arr)

    Call FOO(arr)
    Call BAZ("TEST 7", arr)

    Call BAR(arr)
    Call BAZ("TEST 8", arr)
End Program
"#;

    let src2 = r#"
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByRef arg(3) As Integer To GR1
End Sub
Program FOO
    Argument
        ByVal arr(3) As Integer From GR1
    End Argument
    Dim big(5) As Integer
    big = Array(11, 22, 33, 44, 55, 66)

    Call BAZ("FOO 1", arr)

    arr = Array(12, 23, 34, 45)
    Call BAZ("FOO 2", arr)

    arr = CArray("abcd", 4)
    Call BAZ("FOO 3", arr)

    Fill arr, 789
    Call BAZ("FOO 4", arr)

    arr = SubArray(big, 2, 4)
    Call BAZ("FOO 5", arr)

    arr = CArray(big, 4)
    Call BAZ("FOO 6", arr)
End Program
"#;

    let src3 = r#"
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByRef arg(3) As Integer To GR1
End Sub
Program BAR
    Argument
        ByRef arr(3) As Integer From GR1
    End Argument
    Dim big(5) As Integer
    big = Array(999, 888, 777, 666, 555, 444)

    Call BAZ("BAR 1", arr)

    arr = Array(21, 43, 65, 87)
    Call BAZ("BAR 2", arr)

    arr = CArray("1234", 4)
    Call BAZ("BAR 3", arr)

    Fill arr, 456
    Call BAZ("BAR 4", arr)

    arr = SubArray(big, 2, 4)
    Call BAZ("BAR 5", arr)

    arr = CArray(big, 4)
    Call BAZ("BAR 6", arr)
End Program
"#;

    let src4 = r#"
Program BAZ
    Argument
        ByRef msg As String From GR2,GR3
        ByRef arr(3) As Integer From GR1
    End Argument
    Dim i As Integer
    Dim s As String
    s = ""
    For i = 0 To Len(arr) - 1
        s = s & CStr(arr(i)) & ", "
    Next i
    Print msg
    Print s
End Program
"#;

    for src in [src1, src2, src3, src4].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }
}

#[test]
fn with_arguments_4_works() {
    let src1 = r#"
Extern Sub FOO With
    ByVal arg As String To GR1,GR2
End Sub
Extern Sub BAR With
    ByRef arg As String To GR1,GR2
End Sub
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByVal arg As String To GR1,GR4
End Sub
Program TEST
    Dim str As String
    Dim arr(5) As Integer
    arr = Array(65, 66, 67, 65, 65, 70)

    Call BAZ("TEST 1", str)

    str = String(10, "?"c)
    Call BAZ("TEST 2", str)

    str = String(arr)
    Call BAZ("TEST 3", str)

    Fill str, "@"c
    Call BAZ("TEST 4", str)

    str = String(SubArray(arr, 2, 3))
    Call BAZ("TEST 5", str)

    Call FOO(str)
    Call BAZ("TEST 6", str)

    Call BAR(str)
    Call BAZ("TEST 7", str)
End Program
"#;

    let src2 = r#"
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByVal arg As String To GR1,GR4
End Sub
Program FOO
    Argument
        ByVal str As String From GR1,GR2
    End Argument
    Dim arr(5) As Integer
    arr = Array(97, 98, 104, 105, 98, 98)

    Call BAZ("FOO 1", str)

    str = String(10, "!"c)
    Call BAZ("FOO 2", str)

    str = String(arr)
    Call BAZ("FOO 3", str)

    Fill str, "%"c
    Call BAZ("FOO 4", str)

    str = String(SubArray(arr, 2, 4))
    Call BAZ("FOO 5", str)
End Program
"#;

    let src3 = r#"
Extern Sub BAZ With
    ByRef msg As String To GR2,GR3
    ByVal arg As String To GR1,GR4
End Sub
Program BAR
    Argument
        ByRef str As String From GR1,GR2
    End Argument
    Dim arr(5) As Integer
    arr = Array(97, 65, 98, 66, 99, 67)

    Call BAZ("BAR 1", str)

    str = String(10, "("c)
    Call BAZ("BAR 2", str)

    str = String(arr)
    Call BAZ("BAR 3", str)

    Fill str, ">"c
    Call BAZ("BAR 4", str)

    str = String(SubArray(arr, 1, 4))
    Call BAZ("BAR 5", str)
End Program
"#;

    let src4 = r#"
Program BAZ
    Argument
        ByRef msg As String From GR2,GR3
        ByVal arg As String From GR1,GR4
    End Argument
    Dim i As Integer
    Dim s As String
    s = ""
    For i = 0 To Len(arg) - 1
        s = s & CStr(arg(i)) & ", "
    Next i
    Print msg
    Print Len(arg)
    Print arg
    Print s
End Program
"#;

    for src in [src1, src2, src3, src4].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }
}

#[test]
fn with_arguments_5_works() {
    let src1 = r#"
Option Array UBound
Extern Sub FOO With
    ByVal arg1(5) As Boolean To GR1
    ByRef arg2(5) As Boolean To GR2
    ByVal arg3(5) As Integer To GR3
    ByRef arg4(5) As Integer To GR4
End Sub
Program TEST
    Dim foo(5) As Boolean
    Dim bar(5) As Integer
    foo = CArray(Array(True, True), 5)
    bar = SubArray(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 1, 5)
    Print "TEST"
    Print Len(foo)
    Print Len(bar)
    Call FOO(foo, foo, bar, bar)
End Program
"#;

    let src2 = r#"
Option Array Length
Program FOO
    Argument
        ByVal arg1(6) As Boolean From GR1
        ByRef arg2(6) As Boolean From GR2
        ByVal arg3(6) As Integer From GR3
        ByRef arg4(6) As Integer From GR4
    End Argument
    Dim foo(6) As Boolean
    Dim bar(6) As Integer
    foo = CArray(Array(True, True), 6)
    bar = SubArray(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 1, 6)
    Print "FOO"
    Print Len(foo)
    Print Len(bar)
    Print Len(arg1)
    Print Len(arg2)
    Print Len(arg3)
    Print Len(arg4)
End Program
"#;

    for src in [src1, src2].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }
}

#[test]
fn eof_shared_works() {
    let src1 = r#"
Option EOF Shared
Option Register Dirty
Option Variable Uninitialize
Extern Sub FOO
Program TEST
    Call  FOO
    Print CInt(EOF())
    Print EOF()
End Program
"#;

    let src2 = r#"
Option EOF Shared
Option Register Restore
Option Variable Uninitialize
Program FOO
    Dim s As String
    Input s
End Program
"#;

    for src in [src1, src2].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }

    let eof_stmt = subroutine::get_util_eof_store_code();
    eof_stmt.iter().for_each(|line| {
        eprintln!("{}", line);
    });
}

#[test]
fn danger_recursion_works() {
    let src1 = r#"
Option Register Dirty
Option Variable Uninitialize ' Initializeするとx=0のままなので無限ループする…
Program TEST
    Dim x As Integer
    Print "Before: " & CStr(x)
    If x < 10 Then
        x += 1
        Call TEST
        x -= 1
    End If
    Print "After: " & CStr(x)
End Program
"#;

    for src in [src1].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }
}

#[test]
fn allocator_1_works() {
    let src1 = r#"
OPTION ALLOCATOR INTERNAL 2000
PROGRAM TEST
    ARGUMENT
        BYVAL X AS INTEGER FROM GR1
    END ARGUMENT
    DIM B AS BOOLEAN
    DIM I AS INTEGER
    DIM S AS STRING
    B = X = 0
    I = 123 + X
    S = "ABC" & CSTR(X)
    IF X <> 987 THEN
        IF X > 0 THEN
            CALL TEST(987)
        ELSE
            CALL TEST(777)
            CALL TEST(7)
        END IF
    END IF
    PRINT B
    PRINT I
    PRINT S
    PRINT X
END PROGRAM
"#;

    for src in [src1].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }
}

#[test]
fn allocator_2_works() {
    let src1 = r#"
Extern Sub FIB With
    ByVal n As Integer To GR1
    ByRef r As Integer To GR2
End Sub
Program TEST
    Dim i As Integer
    Dim x As Integer
    For i = 0 To 20
        Call FIB(i, x)
        Print "FIB(" & CStr(i) & ") = " & CStr(x)
    Next i
End Program
"#;

    let src2 = r#"
Option Allocator Internal 100
Program FIB
    Argument
        ByVal n As Integer From GR1
        ByRef r As Integer From GR2
    End Argument
    Dim t As Integer
    If n <= 0 Then
        r = 0
    ElseIf n = 1 Then
        r = 1
    Else
        Call FIB(n - 1, r)
        Call FIB(n - 2, t)
        r += t
    End If
End Program
"#;

    for src in [src1, src2].iter() {
        let mut cursor = std::io::Cursor::new(src);

        let code = parser::parse(&mut cursor).unwrap().unwrap();

        let statements = compile(None, &code[..]).unwrap();

        statements.iter().for_each(|line| {
            eprintln!("{}", line);
        });

        eprintln!("{}", crate::stat::analyze(&statements));

        assert!(!statements.is_empty()); // dummy assert
    }
}
