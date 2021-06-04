'
' leak_1
'
' extern subとのやりとりで
' ByRefとByValを呼び出し先と呼び出し元であえて入れ替えて参照させることにより
' 変数のアドレスを取得して操作をできるようにする
'
extern sub LEAK with
    byref var as integer to gr1
    byref adr as integer to gr2
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
    dim var as integer
    dim adr as integer
    dim value as integer
    
    ' varのアドレスを取得
    call LEAK(var, adr)
    
    var = 123
    
    ' アドレスからvarの値を取得
    call VGET(adr, value)
    
    print (123 = value)
    
    ' アドレスからvarに値を設定
    call VSET(adr, 987)
    
    print (987 = var)
    
end sub
