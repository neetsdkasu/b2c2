'
' レーベンシュタイン距離
'
Dim i As Integer
Dim k As Integer
Dim ch As Integer
Dim cost As Integer
Dim distance As Integer
Dim text1 As String
Dim text2 As String
Dim dp1(255) As Integer
Dim dp2(255) As Integer
Dim dp10 As Integer
Dim dp20 As Integer
Dim turn As Boolean

Print "Text1?"
Input text1

Print "Text2?"
Input text2

If Len(text1) = 0 Or Len(text2) = 0 Then
    distance = Len(text1) + Len(text2)
    Print "Edit Distance: " & CStr(distance)
    Exit Sub
End If

dp10 = 0
For i = 0 To Len(text1) - 1
    dp1(i) = i + 1
Next i

turn = True
For i = 0 To Len(text2) - 1
    ch = text2(i)
    If turn Then
        dp20 = i + 1
        If text1(0) = ch Then
            cost = 0
        Else
            cost = 1
        End If
        dp2(0) = Min(dp20 + 1, Min(dp10 + cost, dp1(0) + 1))
        For k = 1 To Len(text1) - 1
            If text1(k) = ch Then
                cost = 0
            Else
                cost = 1
            End If
            dp2(k) = Min(dp2(k - 1) + 1, Min(dp1(k - 1) + cost, dp1(k) + 1))
        Next k
    Else
        dp10 = i + 1
        If text1(0) = ch Then
            cost = 0
        Else
            cost = 1
        End If
        dp1(0) = Min(dp10 + 1, Min(dp20 + cost, dp2(0) + 1))
        For k = 1 To Len(text1) - 1
            If text1(k) = ch Then
                cost = 0
            Else
                cost = 1
            End If
            dp1(k) = Min(dp1(k - 1) + 1, Min(dp2(k - 1) + cost, dp2(k) + 1))
        Next k
    End If
    turn = Not turn
Next i

If turn Then
    distance = dp1(Len(text1) - 1)
Else
    distance = dp2(Len(text1) - 1)
End If
Print "Edit Distance: " & CStr(distance)
