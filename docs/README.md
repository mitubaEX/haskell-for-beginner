# Haskellにおける周辺概念をざっくり説明する

`Haskell`はすごく良い言語であり，他の言語にないような様々な概念を有しています．
しかし学習コストが高く，結構大変な言語だと思います．
なので，本エントリでは，よりざっくりと`Haskell`の概念と小さなコードを紹介していきなんとなく理解できるような文章を目指します．

そしてこの文章を読んで，`Haskell`すごい！と思って入門する人が増えればいいと思います．

## 想定読者

- CやJavaの手続き型言語を軽くやったことがある人
- 再帰的にいろいろあれこれする考え方のある人

## 実行環境

`Haskell`の実行には，[Stack](https://docs.haskellstack.org/en/stable/README/)というツールを利用します．
以下のコマンドでセットアップします．(Mac)

```
brew install stack
stack setup
```

すこし時間がかかるので，待ちます．

## Hello World

何はともあれ`Hello world`です．

```haskell
main = putStrLn "Hello world!"
-- output: Hello world!
```

実行は以下のコマンドでします．

```sh
stack runghc -- helloworld.hs
```

`Haskell`では，まず`main`関数があり，そしてその処理内容を右辺に書きます．
ここでは`putStrLn`関数を利用して，文字列を出力しています．(printlnのようなもの)

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
以下に階乗計算をするコードを載せます．

```haskell

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

## foldlとfoldrという関数

`Haskell`には，`foldl`と`foldr`という関数があります．
これは両方とも畳み込む関数ですが，方向が違います．
ここでの畳み込みという表現ですが，初期値とリストを与えリストの要素をどんどん初期値に足し合わせていくイメージのものを指します．

`foldl`は，左から畳み込みます．
そして`foldr`は，右から畳み込みます．
用途的には結合則を満たさない演算(左から計算した結果と右から計算した結果が同じ結果にならないもの)で使い分けたり，
`foldr`はリストの生成順序と同一なので，リストを連結していく時に使ったりします．(多分)
以下に例を示します．

```haskell
main = print $ foldr (:) [] [1, 2, 3] -- 引数は，結合する関数, 初期値, 与えるリスト
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
これは`foldl`が左から結合していくからです．
なので使い分けが大事になってきます．

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
感覚としては，インターフェースが近い気がします．
型クラスで定義されている関数をデータ型に実装することで，データ型がその型クラスのインスタンスになります．
そしてその型クラスの制約のもとで，そのデータ型を取り扱えます．
さっそく先ほどのList型をインスタンス化しましょう．
今回は`foldr`を実行するために`Foldable class`の関数を実装し，インスタンス化します．

なぜインスタンス化しないといけないか？なのですが，まずは`Foldable`の定義を見てみましょう．

```sh
class Foldable (t :: * -> *) where # 型パラメータ t
  Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b # 3つ目の引数にt型を要求している
  ...
```

ここでは，`class Foldable`と書かれており，その次に`(t :: * -> *)`と書かれています．
この`t`というのは，型を見ると何かを受け取り，何かを返す関数ですね．
ここで関数と言うと若干わかりづらいのですが，データ型も関数として機能します．
`data List a`は定義としては，`a -> List a`になるので`* -> *`と同一になりますね．（おそらく）
そして`List`を`t`として受け取り，foldrでは3つ目の引数に`t`型すなわち`List`型に包まれた引数を要求します．
このように型クラスは，ユーザが定義したデータ型を受け取り，それに対応した`foldr`を実行できます．
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
なのでエラーが出たらインスタンス化しないといけないんだなぁと思っていく感じが良いです．

```sh
• No instance for (Foldable List) arising from a use of ‘foldr’ # Foldable classのインスタンスではない！？
```

## モナド

`Haskell`では，モナドという概念が存在しています．
そしてモナドは`Haskell`の中でだいぶ重要な概念ではあるものの，
少しわかりづらいもので，`Haskell`の学習コストをある程度あげている気がします．
モナドを箱として説明している[箱で考えるFunctor、ApplicativeそしてMonad
](https://qiita.com/suin/items/0255f0637921dcdfe83b)というすごく良い記事があるので，オススメです．
そしてモナドには様々な種類が存在しており，今回はそれを少し紹介します．

早速ですが，`main`関数は実は`IO`モナドを返す関数でした．
詳しく`main`の定義を書くと以下のようになります．

```haskell
main :: IO () -- 返り値がIO
main = print "hello"
```

そして`print`関数の定義は以下のようになります．

```sh
Prelude> :t print
print :: Show a => a -> IO () # print関数の定義
```

`print`関数が`IO`モナドを返していることがわかります．

では`main`関数が`IO`モナドを返さなければどうなるでしょうか．
実際に実行してみると以下のようになります．

```haskell
main = "hello"

-- エラー内容
-- • Couldn't match expected type ‘IO t0’ with actual type ‘[Char]’
-- • In the expression: main
-- When checking the type of the IO action ‘main’
```

当然`IO`モナドを返してもらいたいのに，`[Char]`型が返ってきていると怒られています．

ここでモナドを返り値に持つ関数というのは，そのモナドに包まれた関数であると思ってもらえればいいと思います．
ここでの`main`関数は，`IO`モナドに包まれた関数であるということです．

次に`print`以外の関数で，標準入力から1行取得する関数`getLine`があります．
これの定義は以下のようになっています．

```
Prelude> :t getLine
getLine :: IO String
```

こちらは`print`関数と違い，`IO String`型を返します．
そしてこの`IO`に包まれた`String`型をどうやって取得するかというと，`>>=`関数を利用します．
では`getLine`で取得した値を`print`関数に渡して出力してみましょう．
コードは以下のようになります．

```Haskell
main = getLine >>= print
-- input:
-- hello
-- output:
-- "hello"
```

`>>=`関数の定義は以下のようなもので，モナドの中身の値を取り出して次の関数に適応します．

```Haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

`>>=`を無限に横に繋げていくことで処理を連続させることができますが，`Haskell`にはもっと良いやり方で同じことが書けます．
それが以下です．

```haskell
main = do
  s <- getLine
  print s
-- input:
-- hello
-- output:
-- "hello"
```

`do`キーワードを利用します．
そして`s <- getLine`は，`getLine`の結果から値を取り出し，それを`s`という変数に束縛します．
**変数定義**のような感覚で書けるので，より手続き型風に書けます．
モナドが利用できるケースでは，`do`キーワードをよく利用すると思います．

ここで`IO`モナドの動きが気になった方は，[Haskell IOモナド 超入門](https://qiita.com/7shi/items/d3d3492ddd90d47160f2)という記事が非常にわかりやすいので，読んでみることをオススメします．

### Stateモナド

`Haskell`では，関数内で利用する状態を表現する`State`モナドが存在します．
簡単な例を示してみます．

```haskell
import           Control.Monad.State

count :: State Integer ()
count = do
  s <- get -- 値を取得する
  put (s + 1) -- 1を足して，Stateを更新する
  -- modify (+1)でもオッケー

count10 :: State Integer String
count10 = do
  count -- countを呼ぶ
  s <- get -- 値を取得する
  case s of -- 分岐
    10 -> return $ show s -- 10の時，数値を文字列にしてモナドに包んで返す
    _  -> count10 -- 再帰

main = print $ runState count10 0
-- output: ("10",10)
```

ここで`count10`関数での`State`モナドの定義は，`State Integer String`となっています．
これは状態として`Integer`型を持っていて，結果として`String`型を返すということを示しています．
次の`count`関数を見ると，`State Integer ()`となっており，これは何も返さないことを意味します．

では動きを少しずつ見ていきましょう．
まず`main`関数が`runState`という関数を呼びます．
この関数の定義は以下のようになっています．

```haskell
runState :: State s a -> s -> (a, s)
```

第一引数には，`State`モナドの関数，第二引数に初期状態を入れることにより，最終結果をタプルとして得る関数です．

では次に`count10`を見てみましょう．
`count10`では，`count`関数を呼んでますが，`count`関数では`get`関数で状態を取り出し，それに1足した値を`put`関数で再代入を行っています．
`State`モナドの関数なので，関数内で状態をこねくり回すことができちゃいます．
まるで**グローバル変数**のような振る舞いをしてくれます．

そして`count`を呼び出したあとでは，`count10`関数は`get`関数で値を取り出しています．
最後に`State`モナドの中身が`10`になったら，`show`関数で数値を文字列化し`return`関数でモナドの形にし返しています．
ここでやっといろんな言語で利用されている`return`関数が登場しましたが，ここでの意味合いはだいぶ違います．
`Haskell`での`return`関数の定義は以下です．

```haskell
return :: Monad m => a -> m a
```

`a`を受け取って，モナドに包んで返していますね．
このように`Haskell`では，値をモナドに包む時に`return`を利用します．
ここで値をモナドで包む理由としては，`count10`がモナドを返す関数だからです．

その後，返ってきた返り値を出力してプログラムは終了します．

`Haskell`にはグローバル変数がないらしいので(それらしいものはある)，状態を扱う際はStateモナドなんかを利用する方向でプログラムを組んだらいいと思います．

## まとめ

本エントリでは，パターンマッチ，データ型，型クラス，モナドと`Haskell`の概念を紹介してきました．
しかしながら本エントリで，`Haskell`の全てが理解できる人は当然いないと思うので，だいたい感覚で理解してもらえればと思います．
そして少しでも`Haskell`に興味をもったら何かしら実装してみるのもいいかもしれません．

## おまけ：Haskellを楽しく学べそうなサイト

- [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
    - 日本語
    - [48時間でSchemeを書こう](https://ja.wikibooks.org/wiki/48%E6%99%82%E9%96%93%E3%81%A7Scheme%E3%82%92%E6%9B%B8%E3%81%93%E3%81%86)
- [Project Euler](https://projecteuler.net/archives)
- [H-99: Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)
