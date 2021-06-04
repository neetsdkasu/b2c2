# edit_distance

2つの文字列の編集距離(レーベンシュタイン距離)を計算する  
挿入・削除・置換のそれぞれのコストは1として計算  


| BASICソース  | 変換後CASL2 | 概要                     |
|:-------------|:------------|:-------------------------|
| main.bas     | MAIN.cas    | メイン部                 |


変換
```
cargo run -- -src main.bas
```


実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
Text1?
? kitten
Text2?
? sitting
Edit Distance: 3
```

