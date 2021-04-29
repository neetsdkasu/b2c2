'
' 迷路生成
'

Extern Sub RAND With
    ByVal init As Boolean To GR1
    ByRef value As Integer To GR2
End Sub

Extern Sub DFSFILL With
    ByRef s As String To GR1,GR2
    ByVal pos As Integer To GR3
End Sub

Extern Sub DFSFAR With
    ByRef maze As String To GR1,GR2
    ByVal pos As Integer To GR3
    ByVal depth As Integer To GR4
    ByRef maxDepth As Integer To GR5
    ByRef deepestPos As Integer To GR6
End Sub

Program MAKE
    Argument
        ByRef maze As String From GR1,GR2
        ByRef sPos As Integer From GR3
        ByRef best As Integer From GR4
    End Argument

    Dim i As Integer
    Dim k As Integer
    Dim x As Integer
    Dim gPos As Integer

    For k = 30 To 195 Step 30
        For i = 2 To 12 Step 2
            maze(k + i) = "#"c
            Call RAND(False, x)
            Select Case x
                Case 0
                    x = k + i - 15
                Case 1
                    x = k + i + 1
                Case 2
                    x = k + i + 15
                Case 3
                    x = k + i - 1
            End Select
            maze(x) = "#"c
        Next i
    Next k

    For i = 73 To 223 Step 15
        If maze(i) = " "c Then
            Call DFSFILL(maze, i)
            gPos = i
            Exit For
        End If
    Next i

    For i = 0 To 224
        If maze(i) <> "#"c Then
            maze(i) = " "c
        End If
    Next i

    Call DFSFAR(maze, gPos, 0, 0, sPos)
    best = 0
    Call DFSFAR(maze, sPos, 0, best, gPos)

    maze(sPos) = "S"c
    maze(gPos) = "G"c

End Program
