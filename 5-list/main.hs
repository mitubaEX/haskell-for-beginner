{-
  -- Java code
  static int f (int n) {
    Integer[] a = {1,2,3,4,5};
    Integer ans = 0;
    for (Integer b: a) {
      ans += b;
    }
    return ans;
  }
-}

f :: [Integer] -> Integer
f (x:xs) = x + f xs
f []     = 0

main = print $ f [1..5]
