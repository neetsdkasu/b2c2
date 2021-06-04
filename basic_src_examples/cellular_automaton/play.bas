'
' 次の状態を求める
'
Sub PLAY
    Argument
        ByRef field1 As String From GR1,GR2
        ByRef field2 As String From GR3,GR4
    End Argument

    Dim i As Integer

    For i = 0 To Len(field1) - 1
        If field1(i - 1) + field1(i + 1) = "*"c + "."c Then
            field2(i) = "*"c
        Else
            field2(i) = "."c
        End If
    Next i

End Sub