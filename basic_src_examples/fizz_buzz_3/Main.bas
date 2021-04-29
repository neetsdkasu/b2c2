'
' Main Program
'
Extern Sub FIZZBUZZ With
    ByVal num As Integer To GR1
    ByRef fizzBuzzStr As String To GR2,GR3
End Sub
Sub MAIN
    Dim i As Integer
    Dim s As String
    Dim t As String
    t = ""
    For i = 1 To 20
        Call FIZZBUZZ (i, s)
        t = t & s & ", "
    Next i
    Print t
End Sub
