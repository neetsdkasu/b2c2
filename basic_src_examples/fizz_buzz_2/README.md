# fizz_buzz_2

Fizz Buzz (その2)  
入力された数値のFizz Buzzを出力する  
数値は1～32767の範囲で入力  
endの入力で終了  


| BASICソース  | 変換後CASL2 | 概要                     |
|:-------------|:------------|:-------------------------|
| fizzbuzz.bas | MAIN.cas    | メイン部                 |


変換
```
cargo run -- -src fizzbuzz.bas
```


実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
Number?
? 1
1
Number?
? 2
2
Number?
? 3
Fizz
Number?
? 4
4
Number?
? 5
Buzz
Number?
? 6
Fizz
Number?
? 15
FizzBuzz
Number?
? 5250
FizzBuzz
Number?
? end
```
