'
' コマンドの入力
'
Option Register Dirty
Option Variable Uninitialize

Extern Sub LOWER With
    ByRef s As String To GR1,GR2
End Sub

Program CMD
    Argument
        ByRef cmd As Integer From GR1
    End Argument

    Dim s As String

    cmd = 0
    Do
        Print "command? (up,right,down,left,end)"
        Input s
        Call LOWER(s)
        Select Case s
            Case "end"
                Exit Do
            Case "up", "u"
                cmd = -15
                Exit Do
            Case "right", "r"
                cmd = 1
                Exit Do
            Case "down", "d"
                cmd = 15
                Exit Do
            Case "left", "l"
                cmd = -1
                Exit Do
            Case "view", "v"
                cmd = 1000
                Exit Do
        End Select
    Loop Until EOF()

End Program
