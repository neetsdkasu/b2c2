# leak_3

配列長1000の整数配列を生成・操作 (その2)  
`Option Allocator Internal 1001`によって予約された領域を利用する  
*ARR1000*で予約された領域のアドレスを*LEAK*側で取得し*MAIN*で操作する  
ビルトイン・サブルーチン*ALLOC*の仕組みに依存している  


| BASICソース   | 変換後CASL2 | 概要                        |
|:--------------|:------------|:----------------------------|
| main.bas      | MAIN.cas    | メイン部                    |
| arr1000.bas   | ARR1000.cas | 1000語の領域の保持          |
| leak.bas      | LEAK.cas    | 領域のアドレスを取得        |
| vset.bas      | VSET.cas    | アドレスに整数を格納        |
| vget.bas      | VGET.cas    | アドレスから整数を取得      |


変換  
```
cargo run -- -src main.bas
cargo run -- -src arr1000.bas
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
※*VSET*によって領域に書き込まれた値の和と*VGET*によって領域から取得した値の和の比較結果を出力している  

