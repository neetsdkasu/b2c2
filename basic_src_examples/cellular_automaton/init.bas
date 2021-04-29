'
' 初期化
'

Extern Sub RAND With
    ByVal init As Boolean To GR1
    ByRef value As Integer To GR2
End Sub

Extern Sub CHECK With
    ByRef field As String To GR1,GR2
    ByRef over As Boolean To GR3
End Sub

Sub INIT
    Argument
        ByRef field1 As String From GR1,GR2
        ByRef field2 As String From GR3,GR4
        ByRef speed As Integer From GR5
    End Argument

    Dim i As Integer
    Dim k As Integer
    Dim seed As Integer
    Dim value As Integer
    Dim over As Boolean

    Print "Seed? (1 - 999)"
    Input seed
    Call RAND(True, Max(101, Min(1099, seed + 100)))

    field1 = String(64, "."c)
    field2 = field1

    For k = 1 To 100
        For i = 0 To 63
            value = 1000
            Call RAND(False, value)
            If (value And 28) = 0 Then
                field1(i And 63) = "*"c
            Else
                field1(i And 63) = "."c
            End If
            seed = seed Xor value
        Next i
        Call CHECK(field1, over)
        If Not over Then
            Exit For
        End If
        If (k And 3) = 0 Then
            Call RAND(True, seed)
        End If
    Next k

    Print "Speed? (1 - 30000)"
    Input speed
    speed = Max(1, Min(30000, speed))

End Sub