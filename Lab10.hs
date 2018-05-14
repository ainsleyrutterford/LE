-- 1 Effect Handlers

-- 1.

data Nondet cnt = Fail
                | Split cnt cnt

instance Functor Nondet where
  fmap f Fail = Fail
  fmap f (Split x y) = Split (f x) (f y)

-- 2.

data Exception e cnt = Throw e
                     | Continue cnt

instance Functor (Exception e) where
  fmap f (Throw e) = Throw e
  fmap f (Continue c) = Continue (f c)

-- 3.

data STATE s cnt = Get (s -> cnt)
                 | Put s cnt

instance Functor (STATE s) where
  fmap f (Get k) = Get (f . k)
  fmap f (Put s c) = Put s (f c)
