# fizz_buzz_3

Fizz Buzz (その3)  
1から20までのFizz Buzzを出力する  


| BASICソース  | 変換後CASL2  | 概要                     |
|:-------------|:-------------|:-------------------------|
| Main.bas     | MAIN.cas     | メイン部                 |
| FizzBuzz.bas | FIZZBUZZ.cas | FizzBuzzを求める         |


変換
```
cargo run -- -src fizzbuzz.bas
```


実行  
 - MAIN.casをプログラムのスタートとして実行する


実行例  
```
1, 2, FIZZ, 4, BUZZ, FIZZ, 7, 8, FIZZ, BUZZ, 11, FIZZ, 13, 14, FizzBuzz, 16, 17, FIZZ, 19, BUZZ, 
```
