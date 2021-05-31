# factors

入力された数値を素因数分解する  
入力は1～32767の範囲の数値  


| BASICソース  | 変換後CASL2 | 概要                     |
|:-------------|:------------|:-------------------------|
| factors.bas  | MAIN.cas    | メイン部                 |


変換
```
cargo run -- -src factors.bas
```


実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
Number?
? 32760
Number is 32760
FACTORS: 8
2, 2, 2, 3, 3, 5, 7, 13,
```
