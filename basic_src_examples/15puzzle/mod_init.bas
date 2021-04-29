'
' 盤面の初期化
'
Option Register Dirty
Option Variable Uninitialize

Extern Sub RAND With
    ByVal init As Boolean To GR1
    ByRef value As Integer To GR2
End Sub

Program INIT
    Argument
        ByRef field(15) As Integer From GR1
    End Argument

    Dim i As Integer
    Dim seed As Integer
    Dim x As Integer
    Dim y As Integer
    Dim temp As Integer

    Print "Seed? (1 - 999)"
    Input seed
    Call RAND(True, Max(1, Min(999, seed)))
    For i = 0 To 15
        field(i) = (i + 1) And 15
    Next i
    For i = 0 To 100
        Call RAND(False, x)
        Call RAND(False, y)
        temp = field(x)
        field(x) = field(y)
        field(y) = temp
    Next i

End Program