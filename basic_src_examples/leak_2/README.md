# leak_2

配列長1000の整数配列を生成・操作 (その1)  
`Option Allocator Internal 1000`によって予約された1000語の領域の先頭アドレスを利用する  
MAINで予約された1000語の領域の先頭アドレスをLEAK側で操作する  
ビルトイン・サブルーチンALLOCの仕組みに依存している  


| BASICソース   | 変換後CASL2 | 概要                        |
|:--------------|:------------|:----------------------------|
| main.bas      | MAIN.cas    | メイン部                    |
| leak.bas      | LEAK.cas    | 1000語領域の操作            |
| vset.bas      | VSET.cas    | アドレスに整数を格納        |
| vget.bas      | VGET.cas    | アドレスから整数を取得      |


変換  
```
cargo run -- -src main.bas
cargo run -- -src leak.bas
cargo run -- -src vset.bas
cargo run -- -src vget.bas
```


実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
True
```
