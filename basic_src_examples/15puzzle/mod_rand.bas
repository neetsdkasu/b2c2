'
' 乱数生成 (線形合同法を使用)
'
' 0～3の乱数を生成する
'
Option Register Dirty
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
        value = (seed >> 2) And 3
    End If

End Sub
