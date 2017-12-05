data Animal = Lion
            | Tiger
            | Gazelle
            | Ant
  deriving (Eq, Show)

-- 1. Shallow embedding

type Safari' = (Int, Int, [Animal])
-- (Territories, Maximum number of animals in a territory, List of all animals)

territory :: [Animal] -> Safari'
territory xs = (1, length xs, xs)

quadrant :: Safari' -> Safari' -> Safari' -> Safari' -> Safari'
quadrant (t1,a1,x1s) (t2,a2,x2s) (t3,a3,x3s) (t4,a4,x4s) =
  (sum [t1, t2, t3, t4], maximum [a1, a2, a3, a4], concat [x1s, x2s, x3s, x4s])

trine :: Safari' -> Safari' -> Safari' -> Safari'
trine (t1,a1,x1s) (t2,a2,x2s) (t3,a3,x3s) =
  (sum [t1, t2, t3], maximum [a1, a2, a3], concat [x1s, x2s, x3s])

core :: [Safari'] -> Safari'
core [] = (0,0,[])
core (x:xs) = combine x (core xs) where
  combine (a1,b1,c1) (a2,b2,c2) = (a1+a2, max b1 b2, c1 ++ c2)

-- 2. Deep embedding

data Safari = Territory [Animal]
            | Trine Safari Safari Safari
            | Quadrant Safari Safari Safari Safari

territories :: Safari -> Int
territories (Territory xs) = 1
territories (Trine a b c) = sum [territories a, territories b, territories c]
territories (Quadrant a b c d) = sum [territories a, territories b, territories c, territories d]

maxAnimals :: Safari -> Int
maxAnimals (Territory xs) = length xs
maxAnimals (Trine a b c) = maximum [maxAnimals a, maxAnimals b, maxAnimals c]
maxAnimals (Quadrant a b c d) = maximum [maxAnimals a, maxAnimals b, maxAnimals c, maxAnimals d]

listAnimals :: Safari -> [Animal]
listAnimals (Territory xs) = xs
listAnimals (Trine a b c) = concat [listAnimals a, listAnimals b, listAnimals c]
listAnimals (Quadrant a b c d) = concat [listAnimals a, listAnimals b, listAnimals c, listAnimals d]

-- 3. This can be done by adding a function to the shallow embedding, and a new data constructor to the deep embedding
--    and adding in pattern matching for the new data constructor in the functions.

-- 4. This can be done without changing any code in the deep embedding, but
--    code must be changed in the shallow embedding.

-- 5.

-- 6. This can be done with the data constructor defined below.

data Core = Territory' [Animal]
          | Split [Core]

-- 7. Functions mapping the Core data type to the previos semantic values are defined below.

territories' :: Core -> Int
territories' (Territory' xs) = 1
territories' (Split xs) = sum (fmap territories' xs)

maxAnimals' :: Core -> Int
maxAnimals' (Territory' xs) = length xs
maxAnimals' (Split xs) = maximum (fmap maxAnimals' xs) -- Can't maximum an empty list.

listAnimals' :: Core -> [Animal]
listAnimals' (Territory' xs) = xs
listAnimals' (Split xs) = concat (fmap listAnimals' xs)

-- 8. quad is defined below.

quad :: Core -> Core -> Core -> Core -> Core
quad a b c d = Split [a,b,c,d]

-- 9. threeSplit is defined below.

threeSplit :: Core -> Core
threeSplit x = Split [x, Split [], Split []]

-- 1.3
-- 1. An advantage of shallow embedding over deep embedding is that you don't have to explicitly deal with the binding of variables, which takes time.

-- 2. An advantage of deep embedding over shallow embedding is that to change the semantics
--    of the model, the data structure does not need to be touched, only the functions that
--    define the semantics.
