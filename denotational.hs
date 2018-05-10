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
p = fromJust (parseMaybe while "y:=1; while !(x=1) do y:=y*x; x:=x-1")
