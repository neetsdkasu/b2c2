'
' SORT WORDS
'
Dim i As Integer
Dim p0 As Integer
Dim p1 As Integer
Dim p2 As Integer
Dim cnt As Integer
Dim ub As Integer
Dim s As String
Dim w1 As String
Dim w2 As String
Print "WORDS? (comma separated)"
Input s
ub = Len(s) - 1
For i = 0 To ub
    If s(i) = ","c Then
        cnt += 1
    End If
Next i
For i = 1 To cnt
    p0 = 0
    For p1 = 0 To ub
        If s(p1) = ","c Then
            Exit For
        End If
    Next p1
    For p2 = p1 + 1 To ub
        If s(p2) = ","c Then
            w1 = Mid(s, p0, p1 - p0)
            w2 = Mid(s, p1 + 1, p2 - (p1 + 1))
            If w1 > w2 Then
                Mid(s, p0) = w2 & "," & w1
                p0 += Len(w2) + 1
            Else
                p0 = p1 + 1
            End If
            p1 = p2
        End If
    Next p2
    w1 = Mid(s, p0, p1 - p0)
    w2 = Mid(s, p1 + 1, p2 - (p1 + 1))
    If w1 > w2 Then
        Mid(s, p0) = w2 & "," & w1
    End If
Next i
Print s