'
' SWAP CASE
'
Dim s As String
Dim i As Integer
Print "Swapcase Alphabet"
Input s
For i = 0 To Len(s) - 1
    If s(i) >= "A"c And s(i) <= "Z"c Then
        s(i) = s(i) + "a"c - "A"c
    ElseIf Not (s(i) < "a"c Or s(i) > "z"c) Then
        s(i) = s(i) + "A"c - "a"c
    End If
Next i
Print s
