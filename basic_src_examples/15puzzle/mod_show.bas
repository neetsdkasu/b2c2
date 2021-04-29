'
' 盤面を表示
'
Option Register Dirty
Option Variable Uninitialize

Sub SHOW
    Argument
        ByVal steps As Integer From GR1
        ByRef field(15) As Integer From GR2
    End Argument

    Dim row As Integer
    Dim col As Integer
    Dim index As Integer
    Dim f As Integer
    Dim s As String
    Dim hline As String

    hline = String(21, "-"c)
    index = 0
    Print " " & CStr(steps) & " steps"
    For row = 0 To 3
        Print hline
        s = "|"
        For col = 0 To 3
            f = field(index)
            If f = 0 Then
                s = s & "  * |"
            ElseIf f < 10 Then
                s = s & "  " & CStr(f) & " |"
            Else
                s = s & " " & CStr(f) & " |"
            End If
            index += 1
        Next col
        Print s
    Next row
    Print hline

End Sub