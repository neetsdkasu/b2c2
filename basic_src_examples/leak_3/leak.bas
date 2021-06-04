'
' アドレス流出
'
' 呼び出し側はByRefで変数を渡すようにする
' extern sub LEAK with
'     byref var as integer to gr1
'     byref adr as integer to gr2
' end sub
'
sub LEAK
    argument
        byval var_adr as integer from gr1
        byref adr as integer from gr2
    end argument
    adr = var_adr
end sub
