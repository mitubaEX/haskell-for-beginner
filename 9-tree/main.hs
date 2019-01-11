data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

initTree :: Tree Char
initTree =
  Node 'd'
  (Node 'c' (Node 'a' Empty Empty) (Node 'b' Empty Empty))
  (Node 'e' (Node 'f' Empty Empty) (Node 'g' Empty Empty))

find :: Tree Char -> Char -> Tree Char
find (Node a (Node b c d) (Node e f g)) ch
  | a == ch = Node a (Node b c d) (Node e f g)
  | ch < a = find (Node b c d) ch
  | ch > a = find (Node e f g) ch
  | otherwise = Node a (Node b c d) (Node e f g)
find (Node a Empty Empty) ch
  | a == ch = Node a Empty Empty
  | otherwise = Node a Empty Empty
find Empty _ = Empty

main = print $ find initTree 'a'
