'
' 深さ優先探索
'
' 迷路のループ箇所を埋める処理
'

Option Allocator Internal 2000
Option Register Dirty
Option Variable Uninitialize

Sub DFSFILL
    Argument
        ByRef maze As String From GR1,GR2
        ByVal pos As Integer From GR3
    End Argument

    Dim x As Integer

    maze(pos) = "."c

    x = pos - 15
    Select Case maze(x)
        Case " "c
            maze(pos) = "U"c
            Call DFSFILL(maze, x)
        Case "U"c, "L"c, "R"c
            maze(pos) = "#"c
            Exit Sub
    End Select

    x = pos + 1
    Select Case maze(x)
        Case " "c
            maze(pos) = "R"c
            Call DFSFILL(maze, x)
        Case "U"c, "R"c, "D"c
            maze(pos) = "#"c
            Exit Sub
    End Select

    x = pos + 15
    Select Case maze(x)
        Case " "c
            maze(pos) = "D"c
            Call DFSFILL(maze, x)
        Case "R"c, "L"c, "D"c
            maze(pos) = "#"c
            Exit Sub
    End Select

    x = pos - 1
    Select Case maze(x)
        Case " "c
            maze(pos) = "L"c
            Call DFSFILL(maze, x)
        Case "L"c, "U"c, "D"c
            maze(pos) = "#"c
            Exit Sub
    End Select

End Sub
