'
' 空きマスの移動
'
Option Register Dirty
Option Variable Uninitialize

Program MOVE
    Argument
        ByVal cmd As Integer From GR1
        ByRef steps As Integer From GR2
        ByRef field(15) As Integer From GR3
    End Argument

    Dim pos As Integer
    Dim i As Integer

    For i = 0 To 15
        If field(i) = 0 Then
            pos = i
            Select Case cmd
                Case 0  ' up
                    If i >= 4 Then
                        pos = i - 4
                    End If
                Case 1  ' right
                    If (i And 3) <> 3 Then
                        pos = i + 1
                    End If
                Case 2  ' down
                    If i < 12 Then
                        pos = i + 4
                    End if
                Case 3  ' left
                    If (i And 3) <> 0 Then
                        pos = i - 1
                    End If
            End Select
            If pos = i Then
                Print "CANNOT MOVE"
            Else
                steps += 1
                field(i) = field(pos)
                field(pos) = 0
            End If
            Exit For
        End If
    Next i

End Program
