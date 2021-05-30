'
' leak_2
'
' b2c2によるメモリ配置的に
' 内部アロケータと組み合わせることで
' 長さが256より大きな配列を実現できる
' が、扱いは難しい
'
' 確保した領域を呼び出し先側が使用するパターン
'
option allocator internal 1000
extern sub LEAK with
    byref pseudo_arr as integer to gr1
    byval arr_len   as integer to gr2
end sub
sub MAIN
    dim pseudo_arr as integer
    
    call LEAK(pseudo_arr, 1000) 
    
end sub
