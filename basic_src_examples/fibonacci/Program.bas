'
' Main Sub
'
Extern Sub FIB With
    ByVal n As Integer To GR1
    ByRef r As Integer To GR2
End Sub
Dim i As Integer
Dim x As Integer
For i = 0 To 20
    Call FIB(i, x)
    Print "FIB(" & CStr(i) & ") = " & CStr(x)
Next i
