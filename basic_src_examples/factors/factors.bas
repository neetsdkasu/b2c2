'
' FACTORS
'
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
    Loop While i * i <= n
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