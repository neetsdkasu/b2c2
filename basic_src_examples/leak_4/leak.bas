'
' 文字列をアドレスから操作する
'
' 呼び出し側は文字列をByRefで渡し、LEAK側で別々に受け取り処理する
' extern sub LEAK with
'     byref s as string to gr1,gr2
'     byval size as integer to gr3
' end sub
'
extern sub VSET with
    byval adr as integer to gr1
    byval value as integer to gr2
end sub
sub LEAK
    argument
        byref str_len as integer from gr1
        byval str_adr as integer from gr2
        byval size as integer from gr3
    end argument
    
    dim i as integer
    dim c as integer
    dim x as integer

    str_len = size
    
    c = "A"c
    x = 99
    
    ' 100文字ごとにアルファベットを入れる
    for i = 0 to size - 1
        if i = x then
            call VSET(str_adr + i, c)
            x += 100
            c += 1
        else
            call VSET(str_adr + i, "."c)
        end if
    next i
    
end sub