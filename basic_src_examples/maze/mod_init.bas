'
' 初期化
'

Extern Sub RAND With
    ByVal init As Boolean To GR1
    ByRef value As Integer To GR2
End Sub

Sub INIT
    Argument
        ByRef maze As String From GR1,GR2
    End Argument

    Dim i As Integer
    Dim x As Integer

    Print "Seed? (1 - 999)"
    Input x
    Call RAND(True, Max(101, Min(1099, x + 100)))

    maze = Space(225)

    Mid(maze, 0, 15) = String(15, "#"c)
    Mid(maze, 210, 15) = String(15, "#"c)

    For i = 15 To 210 Step 15
        maze(i) = "#"c
        maze(i + 14) = "#"c
    Next i

End Sub
