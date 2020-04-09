# 型付きラムダ計算

標準入力に渡された項を解釈し、型検査して実行します。
単純なラムダ計算にはラムダ抽象と関数適用ぐらいしかありませんが、それだとサンプルが書きにくいので自然数とBoolとif式を実装しています。

```
stack run < samples/sample1.txt
```

## 文法

### 型

この言語に存在する型は以下の３つのみです。

```
Int       -- 整数
Bool      -- Boolean
T1 -> T2  -- T1からT2への関数
```

### リテラル

自然数リテラルとBoolリテラルがあります。

```
0
100
true
false
```

### ラムダ式

ラムダ式は以下のような構文です。
引数は必ず一つだけです。

```
-- identity function
lambda x. x
```

引数を複数個取るような関数が欲しいときはラムダをネストさせてください。

```
lambda f.
    lambda n.
        f n
```

### 関数適用

項を並べれば関数適用になります。関数適用は左結合です。

```
(lambda x. x * x) 100
```

### if式

以下のように `if ... then ... else ...` という構文です。

```
if true then 10 else 20
```

### let式

`let name = expr in ...` という構文で式を変数に束縛できます。

```
let x = 10 in x * x
```

ラムダを束縛すると名前付き関数の宣言として使えます。

```
let f = lambda x.
    x * x
in
f 100
```

let多相も実装されています。

```
let identity = lambda x. x in
let n = identity 100 in
let b = identity true in
if b then n else n+1
```

let多相はその名の通りletで束縛したときに起こります。
以下のように `identity` を lambda の引数として渡すと `identity` は単相な型として型付けされるので、TypeError になります。

```
(lambda identity.
    let n = identity 100 in
    let n = identity 100 in
    let b = identity true in
    if b then n else n+1
) (lambda x. x)
```

### 二項演算子

| シンボル | 意味 | 型 | 結合
|---------|-----|----|-----
| `+` | 足し算 | `Int -> Int -> Int` | 左
| `-` | 引き算 | `Int -> Int -> Int` | 左
| `*` | 掛け算 | `Int -> Int -> Int` | 左
| `/` | 割り算(小数点以下切り捨て) | `Int -> Int -> Int` | 左
| `&&` | 論理積 | `Bool -> Bool -> Bool` | 左
| `｜｜` | 論理和 | `Bool -> Bool -> Bool` | 左
| `==` | 等しい | `∀a. a -> a -> Bool` | 無

※ 論理和を全角文字で表記していますが、正しくは半角です。
表の中に `|` を書くと GitHub の Markdown のパースが狂うので全角で書いています。

### コメント

`--` による行コメントと `{- ... -}` という範囲コメントが使えます。
