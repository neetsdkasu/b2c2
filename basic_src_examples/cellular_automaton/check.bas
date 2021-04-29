'
' 終了判定
'
Program CHECK
    Argument
        ByRef field As String From GR1,GR2
        ByRef over As Boolean From GR3
    End Argument
    
    Dim i As Integer
    Dim x As Integer
    
    x = field(0)
    For i = 1 To Len(field) - 1
        If x <> field(i) Then
            over = False
            Exit Program
        End If
    Next i
    over = True
    
End Program