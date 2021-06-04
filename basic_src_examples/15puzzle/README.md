# 15puzzle

15パズル・ゲーム  


| BASICソース   | 変換後CASL2 | 概要                        |
|:--------------|:------------|:----------------------------|
| program.bas   | MAIN.cas    | メイン部                    |
| mod_init.bas  | INIT.cas    | パズル生成                  |
| mod_rand.bas  | RAND.cas    | 乱数生成                    |
| mod_show.bas  | SHOW.cas    | パズルを表示                |
| mod_check.bas | CHECK.cas   | パズル完成の判定            |
| mod_cmd.bas   | CMD.cas     | コマンドの受け取り          |
| mod_lower.bas | LOWER.cas   | 文字列の英字を小文字に変換  |
| mod_move.bas  | MOVE.cas    | 空マスの移動処理            |


変換  
```
cargo run -- -src program.bas
cargo run -- -src mod_init.bas
cargo run -- -src mod_rand.bas
cargo run -- -src mod_show.bas
cargo run -- -src mod_check.bas
cargo run -- -src mod_cmd.bas
cargo run -- -src mod_lower.bas
cargo run -- -src mod_move.bas
```

実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
Seed? (1 - 999)
? 123
 0 steps
---------------------
|  * |  1 |  9 |  4 |
---------------------
|  6 |  2 |  7 |  5 |
---------------------
| 12 |  8 | 11 |  3 |
---------------------
| 13 | 10 | 14 | 15 |
---------------------
command? (up,right,down,left,end)
? right
 1 steps
---------------------
|  1 |  * |  9 |  4 |
---------------------
|  6 |  2 |  7 |  5 |
---------------------
| 12 |  8 | 11 |  3 |
---------------------
| 13 | 10 | 14 | 15 |
---------------------
command? (up,right,down,left,end)
? down
 2 steps
---------------------
|  1 |  2 |  9 |  4 |
---------------------
|  6 |  * |  7 |  5 |
---------------------
| 12 |  8 | 11 |  3 |
---------------------
| 13 | 10 | 14 | 15 |
---------------------
command? (up,right,down,left,end)
? right
 3 steps
---------------------
|  1 |  2 |  9 |  4 |
---------------------
|  6 |  7 |  * |  5 |
---------------------
| 12 |  8 | 11 |  3 |
---------------------
| 13 | 10 | 14 | 15 |
---------------------
command? (up,right,down,left,end)
? end
```
