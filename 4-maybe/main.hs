f :: Maybe Integer -> Maybe Integer
f (Just a) = return a
f Nothing  = Nothing

main :: IO ()
main = do
  print (f $ Just 1)
  print (f $ Just 2)
  print (f Nothing)
