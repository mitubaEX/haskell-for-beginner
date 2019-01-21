# Haskellをざっくり始めたい人へ

こんにちは．
Haskellはすごく良い言語であり，他の言語にないような様々な概念を有しています．
楽にそれらの知識を得ようとしてもいまいち理解できなかったりと，結構大変な言語だと思います．
なので，本エントリでは，よりざっくりと概念を説明していきなんとなく理解できるような文章を目指します．

## 実行環境

`Haskell`の実行には，[Stack](https://docs.haskellstack.org/en/stable/README/)というツールを利用します．
以下のコマンドでセットアップします．(Mac)

```
brew install stack
stack setup
```

## Hello World

何はともあれ`Hello world`

```haskell
main = putStrLn "Hello world!"
-- output: Hello world!
```

実行は以下のコマンドで

```
stack runghc -- helloworld.hs
```

`Haskell`では，まず`main`関数があり，そしてその定義を右辺に書きます．
ここでは`putStrLn`関数を利用して，文字列を出力しています．

## 関数定義

関数は`main`関数のように定義します．
関数`f`は，ただ文字列を返すだけの関数です．
`Haskell`に`return`文は必要ではないので，より簡潔に関数を定義できます．

```haskell
f = "Hello world!" -- 関数定義

main = putStrLn f
-- output: Hello world!
```

## パターンマッチ

`Haskell`では，よく処理を再帰的に書くことがあります．
なので再帰を簡潔に書けるように，パターンマッチというものがあります．
パターンマッチは，関数の定義を複数列挙し，マッチした関数の引数のパターンの右辺を実行するという動作をします．
以下に階乗計算するコードを載せます．

```
factorial 0 = 1 -- 引数が0の時
factorial n = n * factorial (n - 1); -- 引数が0以外の時

main = print (factorial 10)
-- output: 3628800
```

`factorial`関数が2行分定義されていることがわかると思います．
これは引数が`0`，`n`のときの2つの分岐を書いていることになります．
なので，普通の言語だと`if (n == 0)`と書くことが多いですが，
`Haskell`ではどういう値が来た時にどういった値を返すかを直感的に書け，より簡潔に関数を定義できます．

同じ動きの`Java`のコードを載せておくので比較してみると面白いかもです．

```java
import java.io.*;
import java.util.*;
class Main {
 static int factorial (int n) {
   if (n == 0) {
     return 1;
   }
   return n * factorial (n - 1);
 }

 public static void main(String[] args) {
   System.out.println(factorial(10));
 }
}
```

## Listのパターンマッチ

パターンマッチを利用して，Listの状態を判断することができます．
以下にただListの要素を足し続けるだけの関数を定義してみます．

```haskell
f :: [Integer] -> Integer
f (x:xs) = x + f xs -- 先頭の要素がある時 (リストの長さが1以上)
f []     = 0 -- リストが空

main = print $ f [1..5]
-- output: 15
```

関数`f`では，`(x:xs)`というパターンと`[]`というパターンを判断しています．
`(x:xs)`ですが，これは先頭の要素があるかどうかを判断します．
そして`[]`は，見た通り空のリストにマッチします．
もう一つ，関数定義の1行目のところに`f :: [Integer] -> Integer`という記述があります．
これは関数の型を定義しており，`[Integer](IntegerのList)`を受け取り，`Integer`を返す関数ということを示しています．
`Haskell`は，`->`の最後の型が戻り値でそれ以外が引数という形で定義します．(一部例外あり)

余談ですが，上のコードは，`sum [1..5]`でも一緒の動きをします．
普通はこちらを使うと思います．

## foldlとfoldr

`Haskell`には，`foldl`と`foldr`という関数があります．
これは両方とも畳み込む関数ですが，方向が違います．

`foldl`は，左から畳み込みます．
そして`foldr`は，右から畳み込みます．
用途的には結合則を満たさない演算(左から計算しても右から計算しても同じ値)で利用できたり，
`foldr`はリストの生成順序と同一であり，リストを連結していく時の助けになります．
以下に例を示します．

```haskell
main = print $ foldr (:) [] [1, 2, 3]
-- output: [1,2,3]
```

これを`foldl`でやろうとすると以下のようなエラーが出ます．

```sh
 • Occurs check: cannot construct the infinite type: a ~ [a]
   Expected type: [a] -> [a] -> [a]
     Actual type: a -> [a] -> [a]
```

この理由として，リストに要素を追加する`:`は，`a : [b]`のように利用します．
しかし`foldl`では`[b] : a`のように利用しようとするのでエラーを吐きます．

## 代数的データ型 (Listを自分で実装してみる)

ここで`Haskell`の代数的データ型を用いて，Listの動きを実装してみましょう．
以下のコードは，Listというデータ型を定義しListの中身を合計する関数を定義したものです．

```haskell
data List a = Nil | Cons a (List a) -- Listを定義

initList :: List Integer -- 初期リストを定義
initList = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))

sumList :: List Integer -> Integer -- リストの中身の合計を計算する関数
sumList (Cons a b) = a + sumList b
sumList Nil        = 0

main = print $ sumList initList
-- output: 15
```

ここで，`List`は**`Nil`**もしくは**`Cons a (List a)`**という型の両方を表現しています．
そして`a`は，型パラメータといい何型が入って来ても良いということを表現しています．
`sumList`では，`List`型を受け取りパターンマッチをしています．
やっていることは，`Cons a b`にマッチしたら`a`，`b`を取り出し，右辺の処理を実行します．
そして`Nil`にマッチしたら`0`を返します．

このようにデータ型のパターンマッチも簡単に実装できます．

## 型クラス

`Haskell`には型クラスという概念があります．
型クラスで定義されている関数をデータ型に実装することで，データ型がその型クラスのインスタンスになります．
そしてその型クラスの制約のもとそのデータ型を取り扱えます．
さっそく先ほどのList型をインスタンス化しましょう．
今回は`foldr`を実行するために`Foldable class`の関数を実装し，インスタンス化します．

なぜインスタンス化しないといけないか？なのですが，まずは`foldr`の定義を見てみましょう．

```sh
class Foldable (t :: * -> *) where # 型パラメータ t
  Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b # 3つ目の引数にt型を要求している
  ...
```

ここでは，Foldable型の変数を`t`と置いており，foldrでは3つ目の引数に`t`型を要求しています．
このことから，今回作成した`List`が`Foldable`のように振舞ってくれないと`foldr`を実行できないことになります．
こういった制約のことを型クラス制約と呼び，これにより既存の関数に自身の型を適応させていくことが比較的容易にできます．

以下が完成形のコードです．

```haskell
data List a = Nil | Cons a (List a) deriving (Show) -- Show classのインスタンスを作成する

initList :: List Integer -- 初期リスト
initList = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))

instance Foldable List where -- Foldable classのインスタンスを作成する
  foldr f z (Cons a b) = f a (foldr f z b)
  foldr f z Nil        = z

main = print $ foldr Cons Nil initList
-- output: Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))
```

ここでは，`instance`キーワードを利用し`Foldable class`の関数を実装します．
そして先ほどと違い，`deriving`キーワードを利用し`Show class`を実装します．
`deriving`は実装が自明の場合に利用できます．
今回はただ`List`をそのまま出力したいがために実装しています．

ちなみに`Foldable class`のインスタンスを作成しないと以下のようなエラーが出ます．
なのでエラーが出たら大人しくそれ通りに実装しましょう．

```sh
• No instance for (Foldable List) arising from a use of ‘foldr’ # Foldable classのインスタンスではない！？
```


