# leak_4

長さ600の文字列を操作・表示  
b2c2では文字列変数は256文字までしか扱えないが不正なコードで257文字以上を扱う(※あくまでb2c2のBASICコードのみで解決しようする場合の話)  
複数の文字列変数を宣言した場合に連続した領域に確保されることを利用している   
CASL2の`OUT`命令にはラベルを指定する必要があるため変数とラベルの1対1の対応が必要  
`s_dummy1`と`s_dummy2`は長さデータが破壊されるため文字列として使用しようとするとプログラムが暴走する可能性がある   
b2c2の変換の仕組みに依存している  


| BASICソース   | 変換後CASL2 | 概要                        |
|:--------------|:------------|:----------------------------|
| main.bas      | MAIN.cas    | メイン部                    |
| leak.bas      | LEAK.cas    | 受け取ったアドレスから操作  |
| vset.bas      | VSET.cas    | アドレスに整数を格納        |


変換  
```
cargo run -- -src main.bas
cargo run -- -src leak.bas
cargo run -- -src vset.bas
```

実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
...................................................................................................A...................................................................................................B...................................................................................................C...................................................................................................D...................................................................................................E...................................................................................................F

```
※100文字ごとにアルファベットを配置  