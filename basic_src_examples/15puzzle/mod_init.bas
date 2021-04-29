'
' 盤面の初期化
'
Option Register Dirty
Option Variable Uninitialize

Extern Sub RAND With
    ByVal init As Boolean To GR1
    ByRef value As Integer To GR2
End Sub

Program INIT
    Argument
        ByRef field(15) As Integer From GR1
    End Argument

    Dim i As Integer
    Dim k as Integer
    Dim seed As Integer
    Dim cmd As Integer
    Dim temp As Integer
    Dim pos As Integer
    Dim count As Integer

    Print "Seed? (1 - 999)"
    Input seed
    Call RAND(True, Max(1, Min(999, seed)))
    For i = 0 To 15
        field(i) = (i + 1) And 15
    Next i
    pos = 15
    temp = pos
    For i = 0 To 1000
        Call RAND(False, cmd)
        Call RAND(False, count)
        count = count Xor (count >> 1)
        For k = 0 To count
            Select Case cmd
                Case 0
                    If temp >= 4 Then
                        temp -= 4
                    End If
                Case 1
                    If (temp And 3) <> 3 Then
                        temp += 1
                    End If
                Case 2
                    If temp < 12 Then
                        temp += 4
                    End If
                Case 3
                    If (temp And 3) <> 0 Then
                        temp -= 1
                    End If
            End Select
            If temp = pos Then
                Exit For
            Else
                field(pos) = field(temp)
                pos = temp
                field(pos) = 0
            End If
        Next k
    Next i

End Program