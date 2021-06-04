# fizz_buzz_1

Fizz Buzz (その1)  
1から指定された数値までのFizz Buzzを出力する  
数値は最大100まで指定可能  


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
LIMIT?
? 40
1
2
FIZZ
4
BUZZ
FIZZ
7
8
FIZZ
BUZZ
11
FIZZ
13
14
FIZZBUZZ
16
17
FIZZ
19
BUZZ
FIZZ
22
23
FIZZ
BUZZ
26
FIZZ
28
29
FIZZBUZZ
31
32
FIZZ
34
BUZZ
FIZZ
37
38
FIZZ
BUZZ
```
