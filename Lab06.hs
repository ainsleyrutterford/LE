import Data.Monoid
import Control.Applicative

-- 1 Parsing

-- 1.1 Applicative for Maybe

data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  fmap f (Just' a) = Just' (f a)
  fmap f Nothing' = Nothing'

-- 1. Applicative instance for Maybe data type:

instance Applicative Maybe' where
  -- pure :: a -> Maybe a
  pure x = Just' x
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Just' f <*> Just' x = Just' (f x)
  _ <*> Nothing' = Nothing'
  Nothing' <*> _ = Nothing'

-- 2 (a).
-- pure id <*> v = Just id <*> v
--               = id <$> v
--               = fmap id v
--               = v

-- (b).
-- pure f <*> pure x = Just f <*> pure x
--                   = Just f <*> pure x
--                   = f <$> pure x
--                   = fmap f (pure x)
--                   = pure (f x)

-- (c).
-- Case 1: u = Just f
-- u <*> pure y = Just f <*> pure y
--              = Just f <*> Just y
--              = f <$> y

-- (d).
-- (pure .) <*> u <*> v <*> w = (Just .) <*> u <*> v <*> w
--                            = ((Just .) <*> u) <*> v <*> w
--                            = (. <$> u) <*> v <*> w

-- 1.2 Parser Combinators for Lists

data List a = Empty
            | Cons a (List a)
  deriving Show

instance Monoid (List a) where
  mempty = Empty
  Empty `mappend` y = y
  Cons x xs `mappend` ys = Cons x (xs <> ys)

-- 1.
instance Functor List where
  fmap f Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 2.

(<$) :: a -> List b -> List a
(<$) = fmap . const

-- 3.
instance Applicative List where
  -- pure :: a -> List a
  pure x = Cons x Empty
  -- (<*>) :: List (a -> b) -> List a -> List b
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  Cons f fs <*> xs = (fmap f xs) <> (fs <*> xs)

-- 4.

(<*) :: List a -> List b -> List a
la <* lb = const <$> la <*> lb

-- 5.

(*>) :: List a -> List b -> List b
la *> lb = flip const <$> la <*> lb

-- 6.
instance Alternative List where
  -- empty :: List a
  empty = Empty
  -- (<|>) :: List a -> List a -> List a
  Empty <|> l = l
  x <|> y = x <> y

-- 7 (a). Cons :: a -> List a -> List a

-- (b). Cons <$> :: Functor f => f a -> f (list a -> List a)

-- (c). <$ :: Functor f => a -> f b -> f a

-- (d). Cons <$ :: Functor f => f b -> f (a -> List a -> List a)

-- (e). Cons <$> Just 3 <*> Just (Cons 2 Empty) :: Maybe (List Int)

-- (f). Cons <$> Just 3 :: Maybe (List Int -> List Int)

-- (g). Cons <$> (Cons 5 Empty) :: List (List Int -> List Int)

-- (h). Cons 3 <$> :: Functor f => f (List Int) -> f (List Int)

-- 8. (a). Just (Cons 3 (Cons 2 Empty))

-- (b). Just (Cons 3 (Cons 1 Empty))

-- (c). Just 2

-- (d). Cons (Just 5) (Cons (Just 2) Empty)

-- 9.
liftATwo :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftATwo f x y = f <$> x <*> y
