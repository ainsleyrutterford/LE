import Prelude hiding (Num)
import qualified Prelude (Num)
import While
import Yoda

n_val :: Num -> Z
n_val x = x

s :: State
s "x" = 1
s "y" = 2
s "z" = 3
s  _  = 0

a :: Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

a_val :: Aexp -> State -> Z
a_val (N n) s = n
a_val (V v) s = s v
a_val (Mult x y) s = (a_val x s) * (a_val y s)
a_val (Add x y) s = (a_val x s) + (a_val y s)
a_val (Sub x y) s = (a_val x s) - (a_val y s)

b :: Bexp
b = Neg (Eq (Add (V "x") (V "y")) (N 4))

b_val :: Bexp -> State -> T
b_val (TRUE) s = True
b_val (FALSE) s = False
b_val (Neg b) s = not (b_val b s)
b_val (And x y) s = (b_val x s) && (b_val y s)
b_val (Eq x y) s = (a_val x s) == (a_val y s)
b_val (Le x y) s = (a_val x s) <= (a_val y s)

p :: Stm
p = fromJust (parseMaybe while "y:=1; while !(x=1) do (y:=y*x; x:=x-1)")

update :: State -> Z -> Var -> State
update s i nv v
  | nv == v   = i
  | otherwise = s v

s' :: State
s' = update s 5 "x"

cond :: (a -> T, a -> a, a -> a) -> (a -> a)
cond (b, s1, s2) x = if b x then s1 x else s2 x

fix :: ((State -> State) -> (State -> State)) -> (State -> State)
fix ff = ff (fix ff)

s_ds :: Stm -> State -> State
s_ds (Ass v a) s = update s (a_val a s) v
s_ds (Skip) s = id s
s_ds (Comp s1 s2) s = s_ds s2 (s_ds s1 s)
s_ds (If b s1 s2) s = cond (b_val b, s_ds s1, s_ds s2) s
s_ds (While b s1) s =
  (fix f) s where f g = cond (b_val b, g . (s_ds s1), id)

result :: State
result = s_ds p s'
