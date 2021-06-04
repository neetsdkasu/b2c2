'
' アドレス先にデータを設定
'
' 呼び出し側はByValでアドレスを渡す
' extern sub VSET with
'     byval adr as integer to gr1
'     byval value as integer to gr2
' end sub
'
sub VSET
    argument
        byref var as integer from gr1
        byval value as integer from gr2
    end argument
    var = value
end sub
