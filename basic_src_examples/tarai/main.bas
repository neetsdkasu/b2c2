'
' ベンチマーク
'
Extern Sub TARAI With
    ByVal x As Integer To GR1
    ByVal y As Integer To GR2
    ByVal z As Integer To GR3
    ByRef ret As Integer To GR4
End Sub
Sub MAIN
    Dim ret As Integer
    Call TARAI(10, 5, 0, ret)
End Sub