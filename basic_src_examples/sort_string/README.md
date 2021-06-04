# sort_string

単語をソート  
カンマ区切りで単語を入力する  
ソートアルゴリズムはたぶんバブルソート(?)  


| BASICソース   | 変換後CASL2 | 概要                        |
|:--------------|:------------|:----------------------------|
| program.bas   | MAIN.cas    | メイン部                    |


変換  
```
cargo run -- -src program.bas
```

実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
WORDS? (comma separated)
? wolf,fish,cat,dog,sheep,ox,cow,duck,bird,horse,fox
bird,cat,cow,dog,duck,fish,fox,horse,ox,sheep,wolf
```
