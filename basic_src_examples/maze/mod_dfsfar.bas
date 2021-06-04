'
' 深さ優先探索
'
' 迷路は木構造(ループなし)が前提
' 最も遠い位置を見つける処理
'

Option Allocator Internal 3000
Option Register Dirty
Option Variable Uninitialize

Sub DFSFAR
    Argument
        ByRef maze As String From GR1,GR2
        ByVal pos As Integer From GR3
        ByVal depth As Integer From GR4
        ByRef maxDepth As Integer From GR5
        ByRef deepestPos As Integer From GR6
    End Argument

    Dim x As Integer

    maze(pos) = "."c
    depth += 1
    If depth > maxDepth Then
        maxDepth = depth
        deepestPos = pos
    End If

    x = pos - 15
    If maze(x) = " "c Then
        Call DFSFAR(maze, x, depth, maxDepth, deepestPos)
    End If

    x = pos + 1
    If maze(x) = " "c Then
        Call DFSFAR(maze, x, depth, maxDepth, deepestPos)
    End If

    x = pos + 15
    If maze(x) = " "c Then
        Call DFSFAR(maze, x, depth, maxDepth, deepestPos)
    End If

    x = pos - 1
    If maze(x) = " "c Then
        Call DFSFAR(maze, x, depth, maxDepth, deepestPos)
    End If

    maze(pos) = " "c

End Sub
