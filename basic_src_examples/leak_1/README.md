# leak_1

整数変数のアドレス取得・操作  
b2c2にはアドレス操作をする仕組みは用意さてないが不正なコードで実現するという話  
呼び出し側と呼び出され側の引数のByVal,ByRefを一致させないことでメモリアドレスを取得する  


| BASICソース   | 変換後CASL2 | 概要                        |
|:--------------|:------------|:----------------------------|
| main.bas      | MAIN.cas    | メイン部                    |
| leak.bas      | LEAK.cas    | 整数変数のアドレス取得      |
| vget.bas      | VGET.cas    | アドレスから整数を取得      |
| vset.bas      | VSET.cas    | アドレスに整数を格納        |


変換  
```
cargo run -- -src main.bas
cargo run -- -src leak.bas
cargo run -- -src vget.bas
cargo run -- -src vset.bas
```

実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
True
True
```
※1行目は*VGET*によって取得した値と元の変数の値の比較した結果を出力している  
※2行目は*VSET*によって格納した値と元の変数の値の比較した結果を出力している  
