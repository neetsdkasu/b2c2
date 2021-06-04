'
' セル・オートマトン
'

Extern Sub INIT With
    ByRef field1 As String To GR1,GR2
    ByRef field2 As String To GR3,GR4
    ByRef speed As Integer To GR5
End Sub

Extern Sub PLAY With
    ByRef field1 As String To GR1,GR2
    ByRef field2 As String To GR3,GR4
End Sub

Extern Sub CHECK With
    ByRef field As String To GR1,GR2
    ByRef over As Boolean To GR3
End Sub

Sub MAIN

    Dim i As Integer
    Dim k As Integer
    Dim speed As Integer
    Dim field1 As String
    Dim field2 As String
    Dim turn As Boolean
    Dim over As Boolean

    Call INIT(field1, field2, speed)
    turn = True

    Print field1

    For i = 0 To 1000
        If turn Then
            Call PLAY(field1, field2)
            Call CHECK(field2, over)
            Print field2
        Else
            Call PLAY(field2, field1)
            Call CHECK(field1, over)
            Print field1
        End If
        If over Then
            Exit For
        End If
        turn = Not turn
        For k = speed To 30000
            ' 無意味な処理で時間稼ぎ
            turn = Not (Not (Not (Not turn)))
        Next k
    Next i

End Sub