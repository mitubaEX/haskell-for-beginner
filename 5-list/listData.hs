data List a = Nil | Cons a (List a) deriving (Show)

initList :: List Integer
initList = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))

sumList :: List Integer -> Integer
sumList (Cons a b) = a + sumList b
sumList Nil        = 0

cons :: a -> List a -> List a
cons = Cons

myConcat (Cons x xs) ys = x `cons` myConcat xs ys
myConcat Nil ys         = ys

instance Semigroup (List n) where
  Cons x xs <> Cons y ys = myConcat (Cons x xs) (Cons y ys)

-- class Semigroup a where
--   (<>) :: a -> a -> a
--   GHC.Base.sconcat :: GHC.Base.NonEmpty a -> a
--   GHC.Base.stimes :: Integral b => b -> a -> a
--   {-# MINIMAL (<>) #-}
--         -- Defined in ‘GHC.Base’
-- instance Semigroup (Either a b) -- Defined in ‘Data.Either’
-- instance Semigroup [a] -- Defined in ‘GHC.Base’
-- instance Semigroup Ordering -- Defined in ‘GHC.Base’
-- instance Semigroup a => Semigroup (Maybe a)
--   -- Defined in ‘GHC.Base’
-- instance Semigroup a => Semigroup (IO a) -- Defined in ‘GHC.Base’
-- instance Semigroup b => Semigroup (a -> b) -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d,
--           Semigroup e) =>
--          Semigroup (a, b, c, d, e)
--   -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
--          Semigroup (a, b, c, d)
--   -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b, Semigroup c) =>
--          Semigroup (a, b, c)
--   -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b) => Semigroup (a, b)
--   -- Defined in ‘GHC.Base’
-- instance Semigroup () -- Defined in ‘GHC.Base’

instance Monoid (List n) where
  mempty = Nil
  mappend = myConcat

-- class Semigroup a => Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a
--   {-# MINIMAL mempty #-}
--         -- Defined in ‘GHC.Base’
-- instance Monoid [a] -- Defined in ‘GHC.Base’
-- instance Monoid Ordering -- Defined in ‘GHC.Base’
-- instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
-- instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
-- instance Monoid b => Monoid (a -> b) -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
--          Monoid (a, b, c, d, e)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
--          Monoid (a, b, c, d)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Monoid (a, b)
--   -- Defined in ‘GHC.Base’
-- instance Monoid () -- Defined in ‘GHC.Base’
--

instance Foldable List where
  foldr f z (Cons a b) = f a (foldr f z b)
  foldr f z Nil        = z

-- Prelude> :i Foldable
-- class Foldable (t :: * -> *) where
--   Data.Foldable.fold :: Monoid m => t m -> m
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--   foldr :: (a -> b -> b) -> b -> t a -> b
--   Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
--   foldl :: (b -> a -> b) -> b -> t a -> b
--   Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
--   foldr1 :: (a -> a -> a) -> t a -> a
--   foldl1 :: (a -> a -> a) -> t a -> a
--   Data.Foldable.toList :: t a -> [a]
--   null :: t a -> Bool
--   length :: t a -> Int
--   elem :: Eq a => a -> t a -> Bool
--   maximum :: Ord a => t a -> a
--   minimum :: Ord a => t a -> a
--   sum :: Num a => t a -> a
--   product :: Num a => t a -> a
--     {-# MINIMAL foldMap | foldr #-}
--         -- Defined in ‘Data.Foldable’
-- instance Foldable [] -- Defined in ‘Data.Foldable’
-- instance Foldable Maybe -- Defined in ‘Data.Foldable’
-- instance Foldable (Either a) -- Defined in ‘Data.Foldable’
-- instance Foldable ((,) a) -- Defined in ‘Data.Foldable’
--
main = print $ foldr Cons Nil initList
-- main = print $ myConcat initList initList
