'
' Calc Fibonacci Number
'
Option Allocator Internal 100
Sub FIB
    Argument
        ByVal n As Integer From GR1
        ByRef r As Integer From GR2
    End Argument
    Dim t As Integer
    If n <= 0 Then
        r = 0
    ElseIf n = 1 Then
        r = 1
    Else
        Call FIB(n - 1, r)
        Call FIB(n - 2, t)
        r += t
    End If
End Sub
