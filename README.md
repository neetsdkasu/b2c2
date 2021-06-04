# b2c2

BASIC言語風の独自のプログラミング言語(?)のソースコードファイルからCASL2のソースコードファイルを生成しました  

-------------------------------------------------------------------------------

 - 生成されたCASL2コードの動作確認にはCASL2アセンブラ/COMET2シミュレータ等を別途用意する必要があります 
 - 生成コードの最適化等はしてません(無意味・無駄・冗長な処理が複数あります。例: 分岐によるジャンプ処理などはラベルをつけた`NOP`で実現、など)
 - リポジトリの*basic_src_examples*にあるCASL2コードはIPAのCASL II シミュレータ（Java,V2.00(2005/05/27))で一応は動作確認済みです  

-------------------------------------------------------------------------------

### Example: Fizz Buzz (その1)
リポジトリの*basic_src_examples/fizz_buzz_1/ *のディレクトリに入力のBASICファイルと出力のCASL2ファイルがあります  

生成コマンド: `cargo run -- -src fizzbuzz.bas` (basic_src_examples/fizz_buzz_1/での実行を想定)  

入力ファイル: *basic_src_examples/fizz_buzz_1/fizzbuzz.bas*
```vb
'
' FIZZ BUZZ (1)
'
DIM I AS INTEGER
DIM C AS INTEGER
PRINT "LIMIT?"
INPUT C
C = MAX(1, MIN(100, C))
FOR I = 1 TO C STEP 1
    SELECT CASE I MOD 15
        CASE 0
            PRINT "FIZZBUZZ"
        CASE 3, 6, 9, 12
            PRINT "FIZZ"
        CASE 5, 10
            PRINT "BUZZ"
        CASE ELSE
            PRINT I
    END SELECT
NEXT I
```
<details>
<summary>出力ファイル: <i>basic_src_examples/fizz_buzz_1/MAIN.cas</i></summary>
<pre><code>MAIN      START
          RPUSH
                                   ; Init Variables
          LAD       GR1,I001
          XOR       GR2,GR2
          LAD       GR3,2
          CALL      C010
                                   ; Print "LIMIT?"
          OUT       LB001,LL001
                                   ; Input C
          IN        TB001,TL001
          XOR       GR0,GR0
          ST        GR0,EOF
          LAD       GR1,TB001
          LD        GR2,TL001
          JPL       J004
          JZE       J004
          ST        GR2,EOF
          XOR       GR2,GR2
J004      CALL      C000
          ST        GR0,I002
                                   ; C = Max(1, Min(100, C))
          LAD       GR7,1
          LAD       GR6,100
          LD        GR5,I002
          CPA       GR6,GR5
          JMI       J005
          LD        GR6,GR5
J005      NOP
          CPA       GR7,GR6
          JPL       J006
          LD        GR7,GR6
J006      NOP
          ST        GR7,I002
                                   ; For I = 1 To C Step 1
          LD        GR7,I002
          ST        GR7,T001
          LAD       GR7,1
          ST        GR7,I001
J007      NOP
          LD        GR1,I001
          CPA       GR1,T001
          JPL       J009
                                   ; Select Case (I Mod 15)
          LD        GR7,I001
          LAD       GR6,15
          LD        GR3,GR6
          LD        GR2,GR7
          CALL      C009
          LD        GR0,GR1
          LD        GR7,GR0
          CPA       GR7,=0
          JZE       J010
          CPA       GR7,=3
          JZE       J011
          CPA       GR7,=6
          JZE       J011
          CPA       GR7,=9
          JZE       J011
          CPA       GR7,=12
          JZE       J011
          CPA       GR7,=5
          JZE       J012
          CPA       GR7,=10
          JZE       J012
          JUMP      J013
                                   ; Case 0
J010      NOP
                                   ; Print "FIZZBUZZ"
          OUT       LB002,LL002
          JUMP      J029
                                   ; Case 3, 6, 9, 12
J011      NOP
                                   ; Print "FIZZ"
          OUT       LB003,LL003
          JUMP      J029
                                   ; Case 5, 10
J012      NOP
                                   ; Print "BUZZ"
          OUT       LB004,LL004
          JUMP      J029
                                   ; Case Else
J013      NOP
                                   ; Print I
          LD        GR7,I001
          LD        GR3,GR7
          LAD       GR1,TB001
          LAD       GR2,TL001
          CALL      C002
          OUT       TB001,TL001
                                   ; End Select
J029      NOP
                                   ; Next I
J008      NOP
          LAD       GR1,I001
          LD        GR2,0,GR1
          LAD       GR2,1,GR2
          ST        GR2,0,GR1
          JUMP      J007
J009      NOP
EXIT      NOP
          RPOP
          RET
                                   ; Dim I As Integer
I001      DS        1
                                   ; Dim C As Integer
I002      DS        1
EOF       DS        1
T001      DS        1
TL001     DS        1
TB001     DS        256
LL001     DC        6
LB001     DC        'LIMIT?'
LL002     DC        8
LB002     DC        'FIZZBUZZ'
LL003     DC        4
LB003     DC        'FIZZ'
LL004     DC        4
LB004     DC        'BUZZ'
                                   ; FuncCInt
C000      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          ADDL      GR2,GR1
          XOR       GR0,GR0
          XOR       GR4,GR4
          CPL       GR1,GR2
          JZE       J001
          LD        GR3,0,GR1
          CPL       GR3,='+'
          JNZ       J003
          LAD       GR1,1,GR1
          JUMP      J002
J003      CPL       GR3,='-'
          JNZ       J002
          LAD       GR4,-1
          LAD       GR1,1,GR1
J002      CPL       GR1,GR2
          JZE       J001
          LD        GR3,0,GR1
          SUBL      GR3,='0'
          JMI       J001
          CPL       GR3,=9
          JPL       J001
          LD        GR5,GR0
          SLL       GR0,3
          ADDL      GR0,GR5
          ADDL      GR0,GR5
          ADDL      GR0,GR3
          LAD       GR1,1,GR1
          JUMP      J002
J001      XOR       GR0,GR4
          SUBL      GR0,GR4
          POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          POP       GR1
          RET
                                   ; FuncCStrArgInt
C002      CPL       GR3,=#8000
          JNZ       J030
          PUSH      0,GR3
          PUSH      0,GR4
          LAD       GR3,='-32768'
          LAD       GR4,6
          CALL      C007
          POP       GR4
          POP       GR3
          RET
J030      AND       GR3,GR3
          JNZ       J031
          LAD       GR3,1
          ST        GR3,0,GR2
          LD        GR3,='0'
          ST        GR3,0,GR1
          XOR       GR3,GR3
          RET
J031      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          JPL       J032
          LD        GR4,='-'
          ST        GR4,0,GR1
          LAD       GR1,1,GR1
          XOR       GR3,=#FFFF
          LAD       GR3,1,GR3
J032      LAD       GR4,V001
          LD        GR5,GR1
          LD        GR2,GR3
          LAD       GR3,10
J033      CALL      C009
          ADDL      GR1,='0'
          ST        GR1,0,GR4
          LAD       GR4,1,GR4
          LD        GR2,GR0
          JPL       J033
          LAD       GR2,V001
          LAD       GR4,-1,GR4
J034      LD        GR1,0,GR4
          ST        GR1,0,GR5
          LAD       GR5,1,GR5
          LAD       GR4,-1,GR4
          CPL       GR4,GR2
          JPL       J034
          JZE       J034
          LD        GR0,GR5
          POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          POP       GR1
          SUBL      GR0,GR1
          ST        GR0,0,GR2
          RET
V001      DS        6
                                   ; UtilCopyStr
C007      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          ST        GR4,0,GR2
          AND       GR4,GR4
          JZE       J036
J035      LD        GR2,0,GR3
          ST        GR2,0,GR1
          LAD       GR3,1,GR3
          LAD       GR1,1,GR1
          SUBL      GR4,=1
          JPL       J035
J036      POP       GR4
          POP       GR3
          POP       GR2
          POP       GR1
          RET
                                   ; UtilDivMod
C009      AND       GR3,GR3
          JNZ       J016
          XOR       GR0,GR0
          LAD       GR1,-1
          RET
J016      PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          LD        GR4,GR2
          LD        GR5,GR2
          JPL       J014
          XOR       GR5,GR5
          SUBA      GR5,GR2
J014      LD        GR1,GR3
          JPL       J015
          XOR       GR1,GR1
          SUBA      GR1,GR3
J015      LAD       GR0,1
J017      ADDL      GR1,GR1
          JOV       J018
          ADDL      GR0,GR0
          JUMP      J017
J018      SRL       GR1,1
          LAD       GR1,#8000,GR1
          XOR       GR2,GR2
J019      CPL       GR5,GR1
          JMI       J020
          SUBL      GR5,GR1
          ADDL      GR2,GR0
J020      SRL       GR0,1
          JZE       J021
          SRL       GR1,1
          JUMP      J019
J021      LD        GR5,GR4
          XOR       GR5,GR3
          SRA       GR5,15
          XOR       GR2,GR5
          SUBA      GR2,GR5
          CALL      C012
          LD        GR1,GR4
          SUBA      GR1,GR0
          LD        GR0,GR2
          POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          RET
                                   ; UtilFill
C010      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          ADDL      GR3,GR1
J037      CPL       GR1,GR3
          JZE       J038
          ST        GR2,0,GR1
          LAD       GR1,1,GR1
          JUMP      J037
J038      POP       GR3
          POP       GR2
          POP       GR1
          RET
                                   ; UtilMul
C012      PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          XOR       GR0,GR0
          XOR       GR1,GR1
          LD        GR4,GR2
          LD        GR5,GR3
J022      SRL       GR2,1
          JOV       J023
          JNZ       J025
          JUMP      J026
J023      ADDL      GR0,GR3
          JOV       J024
          JUMP      J025
J024      LAD       GR1,1,GR1
J025      SLL       GR3,1
          JUMP      J022
J026      SRL       GR5,1
          SLL       GR4,1
          JOV       J027
          JNZ       J026
          JUMP      J028
J027      ADDL      GR1,GR5
          JUMP      J026
J028      POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          RET
          END
</code></pre>
</details>

<details>
<summary>シミュレータ等による実行例</summary>
<pre><samp>LIMIT?
<i>?</i> <b>20</b>
1
2
FIZZ
4
BUZZ
FIZZ
7
8
FIZZ
BUZZ
11
FIZZ
13
14
FIZZBUZZ
16
17
FIZZ
19
BUZZ
</samp></pre>
</details>

-------------------------------------------------------------------------------

### Example: Fizz Buzz (その2)
リポジトリの*basic_src_examples/fizz_buzz_2/ *のディレクトリに入力のBASICファイルと出力のCASL2ファイルがあります  

生成コマンド: `cargo run -- -src fizzbuzz.bas`  (basic_src_examples/fizz_buzz_2/での実行を想定)    

入力ファイル: *basic_src_examples/fizz_buzz_2/fizzbuzz.bas*
```vb
'
' FIZZ BUZZ (2)
'
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
```

<details>
<summary>出力ファイル: <i>basic_src_examples/fizz_buzz_2/MAIN.cas</i></summary>
<pre><code>MAIN      START
          RPUSH
                                   ; Init Variables
          LAD       GR1,I002
          XOR       GR2,GR2
          LAD       GR3,258
          CALL      C010
                                   ; Do
J001      NOP
                                   ; Print "Number?"
          OUT       LB001,LL001
                                   ; Input s
          IN        SB001,SL001
          XOR       GR0,GR0
          ST        GR0,EOF
          LD        GR0,SL001
          JPL       J003
          JZE       J003
          ST        GR0,EOF
          XOR       GR0,GR0
          ST        GR0,SL001
J003      NOP
                                   ; If (s = "end") Then
          LAD       GR1,SB001
          LD        GR2,SL001
          LAD       GR3,='end'
          LAD       GR4,3
          CALL      C004
          SLL       GR0,15
          SRA       GR0,15
          XOR       GR0,=#FFFF
          LD        GR7,GR0
          AND       GR7,GR7
          JZE       J004
                                   ; Exit Do
          JUMP      J002
                                   ; End If
J004      NOP
                                   ; n = CInt(s)
          LAD       GR1,SB001
          LD        GR2,SL001
          CALL      C000
          LD        GR7,GR0
          ST        GR7,I002
                                   ; If (n < 1) Then
          LD        GR7,I002
          LAD       GR6,1
          LAD       GR0,#FFFF
          CPA       GR7,GR6
          JMI       J014
          XOR       GR0,GR0
J014      LD        GR7,GR0
          AND       GR7,GR7
          JZE       J013
                                   ; Print "Invalid Input"
          OUT       LB002,LL002
                                   ; Continue Do
          JUMP      J001
                                   ; End If
J013      NOP
                                   ; If ((n Mod 15) = 0) Then
          LD        GR7,I002
          LAD       GR6,15
          LD        GR3,GR6
          LD        GR2,GR7
          CALL      C009
          LD        GR0,GR1
          LD        GR7,GR0
          XOR       GR6,GR6
          SUBA      GR7,GR6
          JZE       J034
          LAD       GR7,#FFFF
J034      XOR       GR7,=#FFFF
          AND       GR7,GR7
          JZE       J016
                                   ; s = "FizzBuzz"
          LAD       GR1,SB001
          LAD       GR2,SL001
          LAD       GR3,='FizzBuzz'
          LAD       GR4,8
          CALL      C007
          JUMP      J015
                                   ; ElseIf ((n Mod 3) = 0) Then
J016      NOP
          LD        GR7,I002
          LAD       GR6,3
          LD        GR3,GR6
          LD        GR2,GR7
          CALL      C009
          LD        GR0,GR1
          LD        GR7,GR0
          XOR       GR6,GR6
          SUBA      GR7,GR6
          JZE       J037
          LAD       GR7,#FFFF
J037      XOR       GR7,=#FFFF
          AND       GR7,GR7
          JZE       J017
                                   ; s = "Fizz"
          LAD       GR1,SB001
          LAD       GR2,SL001
          LAD       GR3,='Fizz'
          LAD       GR4,4
          CALL      C007
          JUMP      J015
                                   ; ElseIf ((n Mod 5) = 0) Then
J017      NOP
          LD        GR7,I002
          LAD       GR6,5
          LD        GR3,GR6
          LD        GR2,GR7
          CALL      C009
          LD        GR0,GR1
          LD        GR7,GR0
          XOR       GR6,GR6
          SUBA      GR7,GR6
          JZE       J038
          LAD       GR7,#FFFF
J038      XOR       GR7,=#FFFF
          AND       GR7,GR7
          JZE       J018
                                   ; s = "Buzz"
          LAD       GR1,SB001
          LAD       GR2,SL001
          LAD       GR3,='Buzz'
          LAD       GR4,4
          CALL      C007
          JUMP      J015
                                   ; Else
J018      NOP
                                   ; s = CStr(n)
          LD        GR7,I002
          LD        GR3,GR7
          LAD       GR1,TB001
          LAD       GR2,TL001
          CALL      C002
          LAD       GR1,SB001
          LAD       GR2,SL001
          LAD       GR3,TB001
          LD        GR4,TL001
          CALL      C007
                                   ; End If
J015      NOP
                                   ; Print s
          OUT       SB001,SL001
                                   ; Loop
          JUMP      J001
J002      NOP
EXIT      NOP
          RPOP
          RET
                                   ; Dim n As Integer
I002      DS        1
                                   ; Dim s As String
SL001     DS        1
SB001     DS        256
EOF       DS        1
TL001     DS        1
TB001     DS        256
LL001     DC        7
LB001     DC        'Number?'
LL002     DC        13
LB002     DC        'Invalid Input'
                                   ; FuncCInt
C000      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          ADDL      GR2,GR1
          XOR       GR0,GR0
          XOR       GR4,GR4
          CPL       GR1,GR2
          JZE       J010
          LD        GR3,0,GR1
          CPL       GR3,='+'
          JNZ       J012
          LAD       GR1,1,GR1
          JUMP      J011
J012      CPL       GR3,='-'
          JNZ       J011
          LAD       GR4,-1
          LAD       GR1,1,GR1
J011      CPL       GR1,GR2
          JZE       J010
          LD        GR3,0,GR1
          SUBL      GR3,='0'
          JMI       J010
          CPL       GR3,=9
          JPL       J010
          LD        GR5,GR0
          SLL       GR0,3
          ADDL      GR0,GR5
          ADDL      GR0,GR5
          ADDL      GR0,GR3
          LAD       GR1,1,GR1
          JUMP      J011
J010      XOR       GR0,GR4
          SUBL      GR0,GR4
          POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          POP       GR1
          RET
                                   ; FuncCStrArgInt
C002      CPL       GR3,=#8000
          JNZ       J039
          PUSH      0,GR3
          PUSH      0,GR4
          LAD       GR3,='-32768'
          LAD       GR4,6
          CALL      C007
          POP       GR4
          POP       GR3
          RET
J039      AND       GR3,GR3
          JNZ       J040
          LAD       GR3,1
          ST        GR3,0,GR2
          LD        GR3,='0'
          ST        GR3,0,GR1
          XOR       GR3,GR3
          RET
J040      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          JPL       J041
          LD        GR4,='-'
          ST        GR4,0,GR1
          LAD       GR1,1,GR1
          XOR       GR3,=#FFFF
          LAD       GR3,1,GR3
J041      LAD       GR4,V001
          LD        GR5,GR1
          LD        GR2,GR3
          LAD       GR3,10
J042      CALL      C009
          ADDL      GR1,='0'
          ST        GR1,0,GR4
          LAD       GR4,1,GR4
          LD        GR2,GR0
          JPL       J042
          LAD       GR2,V001
          LAD       GR4,-1,GR4
J043      LD        GR1,0,GR4
          ST        GR1,0,GR5
          LAD       GR5,1,GR5
          LAD       GR4,-1,GR4
          CPL       GR4,GR2
          JPL       J043
          JZE       J043
          LD        GR0,GR5
          POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          POP       GR1
          SUBL      GR0,GR1
          ST        GR0,0,GR2
          RET
V001      DS        6
                                   ; UtilCompareStr
C004      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          XOR       GR0,GR0
J005      AND       GR2,GR2
          JPL       J006
          CPL       GR2,GR4
          JNZ       J007
          JUMP      J009
J006      AND       GR4,GR4
          JZE       J008
          LD        GR5,0,GR1
          CPL       GR5,0,GR3
          JMI       J007
          JPL       J008
          LAD       GR1,1,GR1
          LAD       GR2,-1,GR2
          LAD       GR3,1,GR3
          LAD       GR4,-1,GR4
          JUMP      J005
J007      LAD       GR0,-1
J008      OR        GR0,=1
J009      POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          POP       GR1
          RET
                                   ; UtilCopyStr
C007      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          ST        GR4,0,GR2
          AND       GR4,GR4
          JZE       J036
J035      LD        GR2,0,GR3
          ST        GR2,0,GR1
          LAD       GR3,1,GR3
          LAD       GR1,1,GR1
          SUBL      GR4,=1
          JPL       J035
J036      POP       GR4
          POP       GR3
          POP       GR2
          POP       GR1
          RET
                                   ; UtilDivMod
C009      AND       GR3,GR3
          JNZ       J021
          XOR       GR0,GR0
          LAD       GR1,-1
          RET
J021      PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          LD        GR4,GR2
          LD        GR5,GR2
          JPL       J019
          XOR       GR5,GR5
          SUBA      GR5,GR2
J019      LD        GR1,GR3
          JPL       J020
          XOR       GR1,GR1
          SUBA      GR1,GR3
J020      LAD       GR0,1
J022      ADDL      GR1,GR1
          JOV       J023
          ADDL      GR0,GR0
          JUMP      J022
J023      SRL       GR1,1
          LAD       GR1,#8000,GR1
          XOR       GR2,GR2
J024      CPL       GR5,GR1
          JMI       J025
          SUBL      GR5,GR1
          ADDL      GR2,GR0
J025      SRL       GR0,1
          JZE       J026
          SRL       GR1,1
          JUMP      J024
J026      LD        GR5,GR4
          XOR       GR5,GR3
          SRA       GR5,15
          XOR       GR2,GR5
          SUBA      GR2,GR5
          CALL      C012
          LD        GR1,GR4
          SUBA      GR1,GR0
          LD        GR0,GR2
          POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          RET
                                   ; UtilFill
C010      PUSH      0,GR1
          PUSH      0,GR2
          PUSH      0,GR3
          ADDL      GR3,GR1
J044      CPL       GR1,GR3
          JZE       J045
          ST        GR2,0,GR1
          LAD       GR1,1,GR1
          JUMP      J044
J045      POP       GR3
          POP       GR2
          POP       GR1
          RET
                                   ; UtilMul
C012      PUSH      0,GR2
          PUSH      0,GR3
          PUSH      0,GR4
          PUSH      0,GR5
          XOR       GR0,GR0
          XOR       GR1,GR1
          LD        GR4,GR2
          LD        GR5,GR3
J027      SRL       GR2,1
          JOV       J028
          JNZ       J030
          JUMP      J031
J028      ADDL      GR0,GR3
          JOV       J029
          JUMP      J030
J029      LAD       GR1,1,GR1
J030      SLL       GR3,1
          JUMP      J027
J031      SRL       GR5,1
          SLL       GR4,1
          JOV       J032
          JNZ       J031
          JUMP      J033
J032      ADDL      GR1,GR5
          JUMP      J031
J033      POP       GR5
          POP       GR4
          POP       GR3
          POP       GR2
          RET
          END
</code></pre>
</details>

<details>
<summary>シミュレータ等による実行例</summary>
<pre><samp>Number?
<i>?</i> <b>1</b>
1
Number?
<i>?</i> <b>2</b>
2
Number?
<i>?</i> <b>3</b>
Fizz
Number?
<i>?</i> <b>4</b>
4
Number?
<i>?</i> <b>5</b>
Buzz
Number?
<i>?</i> <b>11</b>
11
Number?
<i>?</i> <b>15</b>
FizzBuzz
Number?
<i>?</i> <b>end</b>
</samp></pre>
</details>

-------------------------------------------------------------------------------

### その他

##### b2c2の実行に関して
 - b2c2の実行バイナリの提供はありません。ソースコードからビルドして実行してください
 - b2c2はCLI(command-line interface)のアプリです。出力するメッセージに日本語が含まれることがあるため日本語表示が可能なシェルやコマンドプロンプトを使用する必要があります
 - 網羅的にチェックはしてないのでBASICコード次第では期待どおりの動作をしないCASL2コードが生成される可能性もあります    
 - BASICソースコードは**UTF-8**(BOMなし)で文字エンコーディングされたファイルとみなして読み込みます  
 - 生成のCASL2ソースコードは**UTF-8**(BOMなし)の文字エンコーディングで出力されます
 - 文字数の埋め込みがある場合はUnicodeスカラ値の個数を文字数として埋め込みます
 - b2c2のBASIC言語についてはリポジトリの*doc/language.md*に概要が書いてあります


##### b2c2のビルドに関して
 - rustcのバージョンは**1.52.1**、cargoのバージョンは**1.52.0**を使用しました (他のバージョンでコンパイル可能かは未確認です)
 - toolchainは**stable-i686-pc-windows-msvc**を使用しました (toolchainは実行環境や開発環境に合わせて選択するものですが、他のtoolchainでb2c2をコンパイル可能かは未確認です)  
 - Editionは**2018**を使用。標準ライブラリを使用しており、外部ライブラリは直接使用してません


##### b2c2のビルドと実行の例

ソースコードを取得してFizz Buzz (その1)を変換しFizz Buzz (その2)を変換するまでの一連のコマンドライン  
※gitとcargoが両方ともインストール済みとします  
※この例の場合では初回の`cargo run`実行時にb2c2が暗黙的にビルドされます  
```bash
git clone https://github.com/neetsdkasu/b2c2
cd b2c2
cd basic_src_examples
cd fizz_buzz_1
cargo run -- -src fizzbuzz.bas
cd ..
cd fizz_buzz_2
cargo run -- -src fizzbuzz.bas
```


##### 重要ではない話

 - 事前の設計などはなく思いつくまま実装していったためスパゲティコード化しておりメンテナンスや改造は難しいです  
 - プログラミング言語の作り方やコンパイラの作り方は不勉強であり、b2c2の作成にあたってはかなりデタラメです (一般的にはコンパイラコンパイラなどを使用するものと思われます)  
 - BASICのコードからCASL2のコードに変換に際し中間コードのようなものは使用してません (CASL2変換後にメタ情報が失われるためそのままでの最適化は難しいかもしれません)  
 - テストコードがいくつかありますが`cargo run`の代わりに`cargo test`を実行していたようなものなので本来のテストと呼ばれるものとは異なる使い方をしてます (全てのテストがpassedしても動作を保証するものではありません)  
 - Rustに関しても不勉強なため無駄な処理や無意味な処理も多いかもしれません  
 - b2c2の作成にRustを選択しておりますが特に理由はありません。標準入出力処理とファイル操作さえ出来れば十分でしたのでRustを選択する必要性は皆無で私のその時の気分でRustを選んだにすぎません
 - b2c2のソースコードでは**unsafe**キーワードは使用してません

 