'
' 15 Puzzle メインルーチン
'
Option Variable Uninitialize

Extern Sub INIT With
    ByRef field(15) As Integer To GR1
End Sub

Extern Sub SHOW With
    ByVal steps As Integer To GR1
    ByRef field(15) As Integer To GR2
End Sub

Extern Sub CMD With
    ByRef cmd As Integer To GR1
End Sub

Extern Sub MOVE With
    ByVal cmd As Integer To GR1
    ByRef steps As Integer To GR2
    ByRef field(15) As Integer To GR3
End Sub

Extern Sub CHECK With
    ByRef ok As Boolean To GR1
    ByVal steps As Integer To GR2
    ByRef field(15) As Integer To GR3
End Sub

Sub MAIN

    Dim steps As Integer
    Dim field(15) As Integer
    Dim ok As Boolean
    Dim cmd As Integer

    Call INIT(field)
    Do
        Call SHOW(steps, field)
        Call CHECK(ok, steps, field)
        If ok Then
            Print "Complete!"
            Exit Do
        End If
        Call CMD(cmd)
        If cmd < 0 Then
            Exit Do
        End If
        Call MOVE(cmd, steps, field)
    Loop

End Sub