'
' 盤面の完成確認
'
Option Register Dirty
Option Variable Uninitialize

Sub CHECK
    Argument
        ByRef ok As Boolean From GR1
        ByVal steps As Integer From GR2
        ByRef field(15) As Integer From GR3
    End Argument

    Dim i As Integer

    For i = 0 To 15
        If field(i) <> ((i + 1) And 15) Then
            ok = False
            Exit Sub
        End If
    Next i
    ok = True

End Sub
