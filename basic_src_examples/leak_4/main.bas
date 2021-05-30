'
' leak_4
'
' b2c2によって
' 連続して宣言された文字列は宣言順に連続する領域が割り当てられるので
' 先頭の文字列のアドレスを利用することによって256文字を超える文字列を扱う
' (文字列長が最大256なのはb2c2による制限でしかない)
' (CASL2の仕様ではINは256文字の入力と制限はあるが、OUTには制限がない)
'
' CASL2のOUT命令にはラベルしか渡せないため
' b2c2が文字列とラベルを１対１で保持するパターンのときのみ実行が可能である
' (引数の文字列やアロケータを使用する場合などはOUTのためのコピーがあるため
'　　長さが256を超える不正な文字列を与えるとバッファオーバーフローを起こして壊れる)
'
extern sub LEAK with
    byref s as string to gr1,gr2
    byval size as integer to gr3
end sub
sub MAIN
    dim s as string
    dim s_dummy1 as string
    dim s_dummy2 as string

    call LEAK(s, 600)

    print s

end sub