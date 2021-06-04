# fibonacci

20番目までのフィボナッチ数を列挙  
単純な再帰呼び出しで実装  


| BASICソース  | 変換後CASL2 | 概要                     |
|:-------------|:------------|:-------------------------|
| Program.bas  | MAIN.cas    | メイン部                 |
| Library.bas  | FIB.cas     | フィボナッチ数計算部     |


変換
```
cargo run -- -src Program.bas
cargo run -- -src Library.bas
```


実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
FIB(0) = 0
FIB(1) = 1
FIB(2) = 1
FIB(3) = 2
FIB(4) = 3
FIB(5) = 5
FIB(6) = 8
FIB(7) = 13
FIB(8) = 21
FIB(9) = 34
FIB(10) = 55
FIB(11) = 89
FIB(12) = 144
FIB(13) = 233
FIB(14) = 377
FIB(15) = 610
FIB(16) = 987
FIB(17) = 1597
FIB(18) = 2584
FIB(19) = 4181
FIB(20) = 6765
```
