import Prelude hiding (Maybe, Just, Nothing)
import Control.Applicative

-- 1.2 Monad

-- 1. Monad laws:
-- Left identity:     return a >>= f = f a
-- Right identity:    m >>= return = m
-- Associativity:     (m >>= f) >>= g = m >>= (\x -> f x >>= g)

data Maybe a = Just a
             | Nothing

instance Monad Maybe where
  return x = Just x
  (Just x)  >>= f = f x
  (Nothing) >>= f = Nothing

instance Functor Maybe where
  fmap f (Nothing) = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure x = Just x
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (Just f) <*> (Just x) = Just (f x)
  m <*> Nothing = Nothing
  Nothing <*> m = Nothing

-- 2.
-- Left identity:
-- return a >>= f = Just a >>= f
--                = f a
-- Right identity:
-- Case 1: m = Nothing
-- m >>= return = Nothing >>= return
--              = Nothing
--              = m
-- Case 2: m = Just a
-- m >>= return = Just a >>= return
--              = return a
--              = Just a
--              = m
-- Associativity:
-- Case 1: m = Just a
-- (m >>= f) >>= g =  (Just a >>= f) >>= g
--                 = (f a) >>= g
-- m >>= (\x -> f x >>= g) = Just a >>= (\x -> f x >>= g)
--                         = (f a) >>= g
-- Case 2: m = Nothing
-- (m >>= f) >>= g = (Nothing >>= f) >>= g
--                 = Nothing >>= g
--                 = Nothing
-- m >>= (\x -> f x >>= g) = Nothing >>= (\x -> f x >>= g)
--                         = Nothing

-- 3.
-- fmap f x = x >>= return . f

data Exception e a = Throw e
                   | Continue a

instance Applicative (Exception e) where
  -- pure :: a -> Exception e a
  pure x = Continue x
  -- (<*>) :: Exception e (a -> b) -> Exception e a -> Exception e b
  (Continue f) <*> (Continue x) = Continue (f x)
  m <*> Throw e = Throw e
  Throw e <*> m = Throw e

instance Functor (Exception e) where
  fmap f (Throw e) = Throw e
  fmap f (Continue a) = Continue (f a)

-- 4.
instance Monad (Exception e) where
  return x = Continue x
  (Throw e)    >>= f = Throw e
  (Continue a) >>= f = f a

-- 5. When declaring a Monad instance, it must be provided with an argument
--    of kind * -> *. Exception is of kind * -> * -> *. The correct argument
--    is (Exception e) which has kind * -> *.

-- 1.3 Tree Monad

data Tree a = Leaf a
            | Fork (Tree a) (Tree a)
  deriving Show

-- 1.
instance Monad Tree where
  return x = Leaf x
  -- (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  Leaf x >>= f = f x
  Fork l r >>= f = Fork (l >>= f) (r >>= f)

instance Applicative Tree where
  pure x = Leaf x
  -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f <*> Leaf x = Leaf (f x)
  Fork l r <*> t = Fork (l <*> t) (r <*> t)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

-- 2.
-- Left identity:
-- return a >>= f = Leaf a >>= f
--                = f a
-- Right identity:
-- Case 1: m = Leaf x
-- m >>= return = Leaf x >>= return
--              = return x
--              = Leaf x
--              = m
-- Case 2: m = Fork l r
-- m >>= return = Fork l r >>= return
--              = Fork (l >>= return) (r >>= return)
-- Assume that trees l and r both follow the Right identity too.
-- Associativity:
-- Case 1: m = Leaf x
-- (m >>= f) >>= g = (Leaf x >>= f) >>= g
--                 = (f x) >>= g
-- m >>= (\x -> f x >>= g) = Leaf x >>= (\x -> f x >>= g)
--                         = (f x) >>= g
-- Case 2: m = Fork l r
-- (m >>= f) >>= g = (Fork l r >>= f) >>= g
--                 = Fork (l >>= f) (r >>= f) >>= g
-- m >>= (\x -> f x >>= g) = Fork l r >>= (\x -> f x >>= g)
--                         = Fork (l >>= ) >>= g

-- 3.
intToDoubleTree :: Int -> Tree Int
intToDoubleTree x = return (2 * x)

-- 4.
addToTree :: Int -> Int -> Tree Int
addToTree x y = return (x + y)

-- 5.
manipTree :: Tree Int -> Tree Int
manipTree t = t >>= (addToTree 3) >>= intToDoubleTree >>= (addToTree 2)

-- 6.
dupToTree :: a -> Tree a
dupToTree a = Fork (return a) (return a)

-- 7.
duplicateTree :: Tree Int -> Tree Int
duplicateTree t = t >>= dupToTree

-- 8.
aWholeNewTree :: Tree Int -> Tree Int
aWholeNewTree t = t >>= dupToTree >>= intToDoubleTree
