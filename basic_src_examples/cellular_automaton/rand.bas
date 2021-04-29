'
' 乱数生成 (線形合同法を使用)
'
' 1236未満の乱数を生成する（たぶん)
'
Option Variable Uninitialize

Sub RAND
    Argument
        ByVal init As Boolean From GR1
        ByRef value As Integer From GR2
    End Argument

    Dim seed As Integer

    If init Then
        seed = (17 * value + 23) Mod 1236
    Else
        seed = (17 * seed + 23) Mod 1236
        value = seed Mod value
    End If

End Sub
