{-# LANGUAGE RankNTypes #-}

-- Fix Questions
-- 1.

data Fix f = In (f (Fix f))

-- 2.

in0 :: Fix f -> f (Fix f)
in0 (In x) = x

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg = alg . fmap (cata alg) . in0

-- Robot Questions
-- 1.

data Robot' k = Stop
              | Forward Int k
              | Left' k
              | Right' k

-- 2.

instance Functor Robot' where
  fmap g (Stop) = Stop
  fmap g (Forward d k) = Forward d (g k)
  fmap g (Left' k) = Left' (g k)
  fmap g (Right' k) = Right' (g k)

-- 3.

-- LAW 1
-- Case one:
-- x = Stop
-- fmap id x = fmap id Stop
--           = Stop
--           = id Stop
--           = id x
-- Case two:
-- x = (Forward d k)
-- fmap id x = fmap id (Forward d k)
--           = Forward d (id k)
--           = Forward d k
--           = id (Forward d k)
--           = id x
-- Case three:
-- x = (Left' k)
-- fmap id x = fmap id (Left' k)
--           = Left' (id k)
--           = Left' k
--           = id (Left' k)
--           = id x
-- Case four:
-- x = (Right' k)
-- fmap id x = fmap id (Right' k)
--           = Right' (id k)
--           = Right' k
--           = id (Right' k)
--           = id x
-- Therefore fmap id x = id x for all cases.

-- LAW 2
-- Case one:
-- x = Stop
-- (fmap f . fmap g) x = (fmap f . fmap g) Stop
--                     = fmap f (fmap g Stop)
--                     = fmap f Stop
--                     = Stop
--                     = x
-- fmap (f . g) x = fmap (f . g) Stop
--                = Stop
--                = x
-- Case two:
-- x = (Forward d k)
-- (fmap f . fmap g) x = (fmap f . fmap g) (Forward d k)
--                     = fmap f (fmap g (Forward d k))
--                     = fmap f (Forward d (g k))
--                     = Forward d (f (g (k)))
--                     = Forward d ((f . g) k)
-- fmap (f . g) x = fmap (f . g) (Forward d k)
--                = Forward d ((f . g) k)
-- Case three:
-- x = (Left' k)
-- (fmap f . fmap g) x = (fmap f . fmap g) (Left' k)
--                     = fmap f (fmap g (Left' k))
--                     = fmap f (Left' (g k))
--                     = Left' (f (g (k)))
--                     = Left' ((f . g) k)
-- fmap (f . g) x = fmap (f . g) (Left' k)
--                = Left' ((f . g) k)
-- Case four:
-- x = (Right' k)
-- (fmap f . fmap g) x = (fmap f . fmap g) (Right' k)
--                     = fmap f (fmap g (Right' k))
--                     = fmap f (Right' (g k))
--                     = Right' (f (g (k)))
--                     = Right' ((f . g) k)
-- fmap (f . g) x = fmap (f . g) (Right' k)
--                = Right' ((f . g) k)
-- Therefore (fmap f . fmap g) x = fmap (f . g) x for all cases.

-- 4.

distance :: Fix (Robot') -> Int
distance = cata alg where
  alg :: Robot' Int -> Int
  alg (Stop) = 0
  alg (Forward d k) = d + k
  alg (Left' k) = k
  alg (Right' k) = k

-- 5.

distanceDir :: Fix (Robot') -> Int
distanceDir r = snd (cata alg r) where
  alg :: Robot' (Int,Int) -> (Int,Int)
  alg (Stop) = ((dir r),0)
  alg (Forward n (d,k))
                    | d `mod` 4 == 0   = (d,n+k)
                    | otherwise        = (d,k)
  alg (Left' (d,k)) = (d+1,k)
  alg (Right' (d,k)) = (d-1,k)
  dir :: Fix (Robot') -> Int
  dir = cata alg2 where
    alg2 :: Robot' Int -> Int
    alg2 (Stop) = 0
    alg2 (Forward n k) = k
    alg2 (Left' k) = k+1
    alg2 (Right' k) = k-1

-- 6.



-- 7.
-- h . (cata alg)
-- h . alg . fmap (cata alg) . in0
-- b . fmap h . fmap (cata alg) . in0
-- cata b

-- MCata
-- 1.

mcata :: (forall x. (x -> a) -> (f x -> a)) -> Fix f -> a
mcata alpha = alpha (mcata alpha) . in0

-- 2.

data List a k = Empty | Cons a k

len_alg :: (x -> Int) -> (List b x -> Int)
len_alg f Empty = 0
len_alg f (Cons a k) = 1 + (f k)

-- 3.

sum_alg :: (x -> Int) -> (List Int x -> Int)
sum_alg f Empty = 0
sum_alg f (Cons n k) = n + (f k)

-- 4.

-- Let x = (In Cons 12 (In ....))
-- mcata len_alg x
-- mcata sum_alg x
