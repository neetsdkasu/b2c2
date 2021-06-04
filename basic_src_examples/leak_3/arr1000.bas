'
' 内部アロケータが持つ領域のアドレスを流出させる
'
option allocator internal 1001
extern sub LEAK with
    byref var as integer to gr1
    byref adr as integer to gr2
end sub
sub ARR1000
    argument
        byref adr as integer from gr1
    end argument

    dim pseudo_arr as integer

    call LEAK(pseudo_arr, adr)

end sub
