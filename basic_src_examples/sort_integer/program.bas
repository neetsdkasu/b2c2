'
' SORT NUMBERS
'
Dim c As Integer
Dim i As Integer
Dim j As Integer
Dim p As Integer
Dim s As String
Dim t As String
Dim arr(255) As Integer
Print "NUMBERS? (comma separated)"
Input s
arr(0) = CInt(s)
For i = 0 To Len(s) - 1
    If s(i) = ","c Then
        c += 1
        arr(c) = CInt(Mid(s, i + 1))
    End If
Next i
For i = 0 To c
    For j = 0 To c - 1 - i
        p = arr(j + 1)
        If arr(j) < p Then
            arr(j + 1) = arr(j)
            arr(j) = p
        End If
    Next j
    t = t & CStr(arr(j)) & ","
Next i
Print t
