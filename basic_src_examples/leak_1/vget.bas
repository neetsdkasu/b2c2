'
' アドレスからデータ読み出し
'
' 呼び出し側はアドレスをByValで渡す
' extern sub VGET with
'     byval adr as integer to gr1
'     byref value as integer to gr2
' end sub
'
sub VGET
    argument
        byref var as integer from gr1
        byref value as integer from gr2
    end argument
    value = var
end sub
