'
' leak_3
'
' b2c2によるメモリ配置的に
' 内部アロケータと組み合わせることで
' 長さが256より大きな配列を実現できる
' が、扱いは難しい
'
' 確保した領域を呼び出し元側が使用するパターン
'
extern sub ARR1000 with
    byref arr_adr as integer to gr1
end sub
extern sub VGET with
    byval adr as integer to gr1
    byref value as integer to gr2
end sub
extern sub VSET with
    byval adr as integer to gr1
    byval value as integer to gr2
end sub
sub MAIN
    dim arr_adr as integer
    dim i as integer
    dim v as integer
    dim sum1 as integer
    dim sum2 as integer

    call ARR1000(arr_adr)

    sum1 = 0
    for i = 0 to 999
        call VSET(arr_adr + i, i + i)
        sum1 += i + i
    next i

    sum2 = 0
    for i = 0 to 999
        call VGET(arr_adr + i, v)
        sum2 += v
    next i

    print (sum1 = sum2)

end sub
