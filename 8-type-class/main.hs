-- Prelude> :i Num
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--         -- Defined in ‘GHC.Num’
-- instance Num Word -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Double -- Defined in ‘GHC.Float’

data Value = Number Integer | Str String | Real Float deriving (Eq, Show)

f :: Eq a => a -> a -> Bool
f a b = a == b

instance Num Value where
  Number a + Number b = Number (a+b)
  Number a * Number b = Number (a*b)
  Number a - Number b = Number (a-b)
  abs (Number a) = Number (abs a)
  signum (Number a) = Number (signum a)
  fromInteger a = Number (fromInteger a)

add :: Num a => a -> a -> a
add a b = a + b

-- Prelude> :i Fractional
-- class Num a => Fractional a where
--   (/) :: a -> a -> a
--   recip :: a -> a
--   fromRational :: Rational -> a
--   {-# MINIMAL fromRational, (recip | (/)) #-}
--         -- Defined in ‘GHC.Real’
-- instance Fractional Float -- Defined in ‘GHC.Float’
-- instance Fractional Double -- Defined in ‘GHC.Float’
--
instance Fractional Value where
  Real a / Real b = Real (a / b)
  recip a = a
  fromRational a = Real (fromRational a)

myDiv :: Fractional a => a -> a -> a
myDiv a b = a / b

main = do
  print $ f (Number 1) (Number 1)
  print $ add (Number 1) (Number 1)
  print $ myDiv (Real 1.1) (Real 1.2)
