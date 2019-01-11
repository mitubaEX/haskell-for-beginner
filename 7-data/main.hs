import           Data.Char

data Value = Number Integer | Str String | Empty deriving (Show)

f :: String -> Value
f a | all isDigit a = Number (read a :: Integer)
  | otherwise = Str a

add :: Value -> Value -> Value
add (Number a) (Number b) = Number (a + b)
add (Str a) (Str b)       = Str (a ++ b)
add _ _                   = Empty

main = do
  let numA = f "1"
  let numB = f "2"
  let numC = f "3"
  let strA = f "a"
  let strB = f "b"
  let strC = f "c"

  print $ add numA numB
  print $ add numB numC

  print $ add strA strB
  print $ add strB strC

  print $ add numA (add numB numC)
  print $ add strA (add strB strC)
