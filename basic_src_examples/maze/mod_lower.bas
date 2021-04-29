'
' 大文字を小文字に変換
'
Option Register Dirty
Option Variable Uninitialize

Program LOWER
    Argument
        ByRef s As String From GR1,GR2
    End Argument

    Dim i As Integer
    Dim ch As Integer

    For i = 0 To Len(s) - 1
        ch = s(i)
        If "A"c <= ch And ch <= "Z"c Then
            s(i) = ch - "A"c + "a"c
        End If
    Next i

End Program
