-- 1 Effect Handlers

-- 1.1 Free Monads

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

-- 1.2 Effect Handlers

data Free f a = Var a
              | Con (f (Free f a))

eval :: Functor f => (f a -> a) -> Free f -> a
eval alg (Var v) = v
eval alg (Con op) = alg (fmap (eval alg) op)

handle :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
handle gen alg = eval alg . fmap gen

-- 1.

handleNondet :: Free (Nondet) a -> [a]
handleNondet = handle gen alg where
  -- gen :: a -> [a]
  gen x = [x]
  -- alg :: Nondet [a] -> [a]
  alg Fail = []
  alg (Split xs ys) = Split (xs ++ ys)

-- 2.

handleExec :: Free (Exception e) a -> Either e a
handleExec = handle gen alg where
  -- gen :: a -> Either e a
  gen x = Right x
  -- alg :: Exception e (Either e a) -> Either e a
  alg (Throw e) = Left e
  alf (Continue c) = c

-- 3.

handleState :: Free (STATE s) a -> (s -> (s,a))
handleState = handle gen alg where
  -- gen :: a -> (s -> (s,a))
  gen x = (\s -> (s,x))
  -- alg :: (STATE s) (s -> (s,a)) -> (s -> (s,a))
  alg (Put s k) = (\s' -> k s)
  alg (Get f) = (\s -> f s s)
