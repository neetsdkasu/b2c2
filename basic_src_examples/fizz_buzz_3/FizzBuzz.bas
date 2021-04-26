'
' FizzBuzz Program
'
Program FIZZBUZZ
    Argument
        ByVal num As Integer From GR1
        ByRef fizzBuzzStr As String From GR2,GR3
    End Argument
    Select Case num Mod 15
    Case 0
        fizzBuzzStr = "FizzBuzz"
    Case 3, 6, 9, 12
        fizzBuzzStr = "FIZZ"
    Case 5, 10
        fizzBuzzStr = "BUZZ"
    Case Else
        fizzBuzzStr = CStr(num)
    End Select
End Program
