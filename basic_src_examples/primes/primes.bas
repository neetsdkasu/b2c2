'
' PRINT PRIMES
'
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