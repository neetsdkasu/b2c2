'
' 迷路
'

Extern Sub INIT With
    ByRef maze As String To GR1,GR2
End Sub

Extern Sub MAKE With
    ByRef maze As String To GR1,GR2
    ByRef sPos As Integer To GR3
    ByRef best As Integer To GR4
End Sub

Extern Sub CMD With
    ByRef cmd As Integer To GR1
End Sub


Dim i As Integer
Dim k As Integer
Dim maze As String
Dim field As String
Dim pos As Integer
Dim best As Integer
Dim cmd As Integer
Dim steps As Integer
Dim view As Integer

Call INIT(maze)

field = maze
For i = 15 To 210
    If field(i) = " "c Then
        field(i) = "*"c
    End If
Next i

Call MAKE(maze, pos, best)

view = 0
Do
    For i = (-1 << view) To (1 << view)
        For k = (-15 << view) To (15 << view) Step 15
            field(pos + i + k) = maze(pos + i + k)
        Next k
    Next i

    field(pos) = "@"c

    Print CStr(steps) & " steps"
    For i = 0 To 210 Step 15
        Print Mid(field, i, 15)
    Next i

    If maze(pos) = "G"c Then
        Print "GOAL! " & CStr(steps) & " / " & CStr(best)
        Exit Do
    End If

    Call CMD(cmd)
    If cmd = 0 Then
        Exit Do
    ElseIf cmd = 1000 Then
        view = 1 - view
        steps += view << 3
    ElseIf field(pos + cmd) = "#"c Then
        Print "CANNOT MOVE"
    Else
        pos += cmd
        steps += 1
    End If
Loop
