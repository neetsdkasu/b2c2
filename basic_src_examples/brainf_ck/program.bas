'
' BRAINF*CK
'
' Example Brainf*ck Program (Output "Hello")
' ++++++++[>+++++++++[>+>++<<-]++[>>---<<-]<-]>>.>+++++.+++++++..+++.
'
Dim mem(255) As Integer
Dim mp As Integer
Dim pc As Integer
Dim cmd As String
Dim i As Integer
Dim brc As Integer
Dim inStr As String
Dim outStr As String
Print "Program?"
Input cmd
i = -1
Do While pc < Len(cmd)
    Select Case cmd(pc)
        Case "+"c
            mem(mp) += 1
        Case "-"c
            mem(mp) -= 1
        Case "<"c
            mp -= 1
        Case ">"c
            mp += 1
        Case "["c
            If mem(mp) = 0 Then
                Do
                    Select Case cmd(pc)
                        Case "["c
                            brc += 1
                        Case "]"c
                            brc -= 1
                    End Select
                    pc += 1
                Loop While brc <> 0 And pc < Len(cmd)
                Continue Do
            End If
        Case "]"c
            Do
                Select Case cmd(pc)
                Case "["c
                    brc += 1
                Case "]"c
                    brc -= 1
                End Select
                pc -= 1
            Loop While brc <> 0 And pc >= 0
            If brc <> 0 Then
                Exit Do
            End If
        Case ","c
            If i < 0 Then
                Input inStr
                If Len(inStr) = 0 And Not EOF() Then
                    mem(mp) = 13
                    i = -1
                Else
                    mem(mp) = inStr(0)
                    i = 1
                End If
            ElseIf i >= Len(inStr) Then
                mem(mp) = 13
                i = -1
            Else
                mem(mp) = inStr(i)
                i += 1
            End If
        Case "."c
            If mem(mp) = 13 Then
                Print outStr
                outStr = ""
            Else
                outStr = outStr & Chr(mem(mp))
                If Len(outStr) >= 40 Then
                    Print outStr
                    outStr = ""
                End If
            End If
    End Select
    pc += 1
Loop
Print outStr