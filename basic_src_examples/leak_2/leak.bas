'
' アドレス流出
'
' 呼び出し側をByValにして内部バッファのアドレスを取得する
' extern sub LEAK with
'     byref pseudo_arr as integer to gr1
'     byval arr_len    as integer to gr2
' end sub
'
extern sub VGET with
    byval adr as integer to gr1
    byref value as integer to gr2
end sub
extern sub VSET with
    byval adr as integer to gr1
    byval value as integer to gr2
end sub
sub LEAK
    argument
        byval arr_adr as integer from gr1
        byval arr_len as integer from gr2
    end argument

    dim i as integer
    dim v as integer
    dim sum1 as integer
    dim sum2 as integer

    sum1 = 0
    for i = 0 to arr_len - 1
        call VSET(arr_adr + i, i + i)
        sum1 += i + i
    next i

    sum2 = 0
    for i = 0 to arr_len - 1
        call VGET(arr_adr + i, v)
        sum2 += v
    next i

    print (sum1 = sum2)

end sub
