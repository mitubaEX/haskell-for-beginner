import           Control.Monad.State

count :: State Integer ()
count = do
  s <- get
  put (s + 1)
  -- modify (+1)でもおっけー

count10 :: State Integer String
count10 = do
  count
  s <- get
  case s of
    10 -> return $ show s
    _  -> count10

main = print $ runState count10 0
