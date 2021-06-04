'
' 竹内関数(たらい関数)
'
Option Allocator Internal
Option Variable  Uninitialize
Option Register  Dirty
Sub TARAI
    Argument
        ByVal x As Integer From GR1
        ByVal y As Integer From GR2
        ByVal z As Integer From GR3
        ByRef ret As Integer From GR4
    End Argument
    Dim a As Integer
    Dim b As Integer
    Dim c As Integer
    If x <= y Then
        ret = y
    Else
        Call TARAI(x - 1, y, z, a)
        Call TARAI(y - 1, z, x, b)
        Call TARAI(z - 1, x, y, c)
        Call TARAI(a, b, c, ret)
    End If
End Sub