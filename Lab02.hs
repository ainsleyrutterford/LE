data Animal = Lion
            | Tiger
            | Gazelle
            | Ant
  deriving (Eq, Show)

-- 1. Shallow embedding

type Safari = (Int, Int, [Animal])
-- (Territories, Maximum number of animals in a territory, List of all animals)

territory :: [Animal] -> Safari
territory xs = (1, length xs, xs)

quadrant :: Safari -> Safari -> Safari -> Safari -> Safari
quadrant (t1, a1, as1) (t2, a2, as2) (t3, a3, as3) (t4, a4, as4) =
  (sum [t1, t2, t3, t4], maximum [a1, a2, a3, a4], concat [as1, as2, as3, as4])

triple :: Safari -> Safari -> Safari -> Safari
triple (t1, a1, as1) (t2, a2, as2) (t3, a3, as3) =
  (sum [t1, t2, t3], maximum [a1, a2, a3], concat [as1, as2, as3])

core :: [Safari] -> Safari
core [] = (0,0,[])
core (x:xs) = combine x (core xs) where
  combine (a1,b1,c1) (a2,b2,c2) = (a1+a2, max b1 b2, c1 ++ c2)

-- 2. Deep embedding

data Plot = Quadrant Plot Plot Plot Plot
          | Triple Plot Plot Plot
          | Territory [Animal]

tories :: Plot -> Int
tories (Territory as) = 1
tories (Triple a b c) = sum [tories a, tories b, tories c]
tories (Quadrant a b c d) = sum [tories a, tories b, tories c, tories d]

maxanimals :: Plot -> Int
maxanimals (Territory as) = length(as)
maxanimals (Triple a b c) = maximum [maxanimals a, maxanimals b, maxanimals c]
maxanimals (Quadrant a b c d) = maximum [maxanimals a, maxanimals b, maxanimals c, maxanimals d]

listanimals :: Plot -> [Animal]
listanimals (Territory as) = as
listanimals (Triple a b c) = concat [listanimals a, listanimals b, listanimals c]
listanimals (Quadrant a b c d) = concat [listanimals a, listanimals b, listanimals c, listanimals d]

-- 3. This can be done by adding a function to the shallow embedding, and a new data
--    constructor to the deep embedding and adding in pattern matching for the new
--    data constructor in the functions.

-- 4. This can be done without changing any code in the deep embedding, but
--    code must be changed in the shallow embedding.

-- 5. There is junk in the model of String which means that you can represent
--    objects that aren't part of the wanted model, for example "abc" could be
--    represented but it isn't an animal

-- 6. This can be done with the data constructor defined below.

data Core = Split [(Core, Float)]
          | Terry [Animal]

-- 7. Functions mapping the Core data type to the previous semantic values are
--    defined below.

coretories :: Core -> Int
coretories (Terry as) = 1
coretories (Split cs) = sum (fmap (coretories . fst) cs)

coreanimals :: Core -> Int
coreanimals (Terry as) = length(as)
coreanimals (Split cs) = maximum (fmap (coreanimals . fst) cs)

corelist :: Core -> [Animal]
corelist (Terry as) = as
corelist (Split cs) = concat (fmap (corelist . fst) cs)

-- 8. quad is defined below.

quad :: Core -> Core -> Core -> Core -> Core
quad a b c d = Split [(a,1/4), (b,1/4), (c,1/4), (d,1/4)]

-- 9. threeSplit is defined below.

trip :: Core -> Core -> Core -> Core
trip a b c = Split [(a,1/3), (b,1/3), (c,1/3)]

-- 1.3
-- 1. A shallow embedding allows for the extension of a new data construct in
--    an embedding without changing any existing code.

-- 2. An advantage of deep embedding over shallow embedding is that to change
--    the semantics of the model, the data structure does not need to be
--    touched, only the functions that define the semantics.

-- 3. Using a deep 'core' language and a group of shallow 'everyday'
--    conversions, and smart constructors, allows for the advantages of both
--    deep and shallow embeddings. This is because new semantic functions can
--    be created easily using the core data type, and new data structures can
--    be added easily using smart constructors that use the core data type.
