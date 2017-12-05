-- Functors
-- 1.

data MaybeError a = Error String | Just' a

-- fmap :: Functor f => (a -> b) -> (f a) -> (f b)

instance Functor MaybeError where
  fmap g (Error s) = Error s
  fmap g (Just' a) = Just' (g a)

-- LAW 1
-- Case one:
-- x = (Error s)
-- fmap id x = fmap id (Error s)
--           = Error s
--           = id (Error s)
--           = id x
-- Case two:
-- x = (Just a)
-- fmap id x = fmap id (Just a)
--           = Just (id a)
--           = Just a
--           = id (Just a)
--           = id x
-- Therefore fmap id x = id x for all cases.

-- LAW 2
-- Case one:
-- x = (Error s)
-- (fmap f . fmap g) x = (fmap f . fmap g) (Error s)
--                     = fmap (f (fmap g (Error s)))
--                     = fmap (f (Error s))
--                     = Error s
--                     = x
-- fmap (f . g) x = fmap (f . g) (Error s)
--                = Error s
--                = x
-- Case two:
-- x = (Just a)
-- (fmap f . fmap g) x = (fmap f . fmap g) (Just a)
--                     = fmap f (fmap g (Just a))
--                     = fmap f (Just (g a))
--                     = Just (f (g (a)))
--                     = Just ((f . g) a)
-- fmap (f . g) x = fmap (f . g) (Just a)
--                = Just ((f . g) a)
-- Therefore (fmap f . fmap g) x = fmap (f . g) x

-- 2.

data Tree a = Leaf | Branch a (Tree a) (Tree a)

instance Functor Tree where
  fmap g (Leaf) = Leaf
  fmap g (Branch a t1 t2) = Branch (g a) (fmap g t1) (fmap g t2)

-- 3.

-- LAW 1
-- Case one:
-- x = Leaf
-- fmap id x = fmap id Leaf
--           = Leaf
--           = id Leaf
--           = id x
-- Case two:
-- Assume that fmap id x = id x for all trees smaller than x.
-- x = (Branch a l r)
-- fmap id x = fmap id (Branch a l r)
--           = Branch (id a) (fmap id l) (fmap id r)
--           = Branch (id a) (id l) (id r)
--           = Branch a l r
--           = x
--           = id x
-- Therefore fmap id x = id x for all cases.

-- LAW 2
-- Case one:
-- x = Leaf
-- (fmap f . fmap g) x = (fmap f . fmap g) Leaf
--                     = fmap (f (fmap g Leaf))
--                     = fmap f Leaf
--                     = Leaf
--                     = x
-- fmap (f . g) x = fmap (f . g) Leaf
--                = Leaf
--                = x
-- (fmap f . fmap g) Leaf = fmap (f . g) Leaf
-- Case two:
-- x = (Branch a l r)
-- (fmap f . fmap g) x = (fmap f . fmap g) (Branch a l r)
--                     = fmap (f (fmap g (Branch a l r)))
--                     = fmap (f (Branch (g a) (fmap g l) (fmap g r)))
--                     = Branch ((f . g) a) ((fmap f . fmap g) l) ((fmap f . fmap g) r)
--                     = Branch ((f . g) a) (fmap (f . g) l) (fmap (f . g) r)
-- fmap (f . g) x = fmap (f . g) (Branch a l r)
--                = Branch ((f . g) a) (fmap (f . g) l) (fmap (f . g) r)
-- Therefore fmap (f . g) x = (fmap f . fmap g) x for all cases.

-- 4. Tree has kind * -> *

-- 5. Exception has kind * -> * -> *

-- 6. * -> *

-- 7. Exception is not a functor because it does not have kind * -> *

data Exception e a = Except e | Result a

instance Functor (Exception e) where
  fmap g (Except e) = Except e
  fmap g (Result a) = Result (g a)

-- Fixpoints
-- 1.

data Fix f = In (f (Fix f))

data TreeF a k = Leaf' | Branch' a k k

-- 2.

instance Functor (TreeF a) where
  fmap g (Leaf') = Leaf'
  fmap g (Branch' a l r) = Branch' (a) (g l) (g r) -- why don't we apply f to a?

-- 3.

sumTree :: Tree Int -> Int
sumTree (Leaf) = 0
sumTree (Branch a l r) = a + (sumTree l) + (sumTree r)

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg = alg . fmap (cata alg) . in0

in0 :: Fix f -> f (Fix f)
in0 (In x) = x

sumTree' :: Fix (TreeF Int) -> Int
sumTree' = cata alg where
  alg :: TreeF Int Int -> Int
  alg (Leaf') = 0
  alg (Branch' n k1 k2) = n + k1 + k2

-- 4.

leaves :: Tree Int -> Int
leaves (Leaf) = 1
leaves (Branch a l r) = (leaves l) + (leaves r)

leaves' :: Fix (TreeF Int) -> Int
leaves' = cata alg where
  alg :: TreeF Int Int -> Int
  alg (Leaf') = 1
  alg (Branch' a k1 k2) = k1 + k2
