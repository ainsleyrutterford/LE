module Lab1 where

data Robot = Forward Int Robot
           | Right' Robot
           | Left' Robot
           | Stop

distTrav :: Robot -> Int
distTrav Stop = 0
distTrav (Forward n robot) = n + distTrav robot
distTrav (Left' robot) = distTrav robot
distTrav (Right' robot) = distTrav robot

distTravDir :: Robot -> Int -> Int
distTravDir Stop x = 0
distTravDir (Forward n robot) x
                        | x `mod` 4 == 0    = n + distTravDir robot x
                        | otherwise         = distTravDir robot x
distTravDir (Left' robot) x = distTravDir robot (x-1)
distTravDir (Right' robot) x = distTravDir robot (x+1)

displacement :: Robot -> Int -> Int -> Int -> Float
displacement Stop x y d = sqrt ((fromIntegral x)^2 + (fromIntegral y)^2)
displacement (Forward n robot) x y d
                          | d `mod` 4 == 0    = displacement robot (x+n) y d
                          | d `mod` 4 == 1    = displacement robot x (y+n) d
                          | d `mod` 4 == 2    = displacement robot (x-n) y d
                          | d `mod` 4 == 3    = displacement robot x (y-n) d
displacement (Left' robot) x y d = displacement robot x y (d-1)
displacement (Right' robot) x y d = displacement robot x y (d+1)

potato :: (Int, Int, Bool, String)
potato = (0, 0, False, "potato")

peel :: Int -> (Int, Int, Bool, String) -> (Int, Int, Bool, String)
peel n (t,w,c,d) = (t+(2*n), w, c, "peeled " ++ d)

roast :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
roast (t,w,c,d) = (t+70, w, c, "roasted " ++ d)

boil :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
boil (t,w,c,d) = (t+25, w, True, "boiled " ++ d)

mash :: Int -> (Int, Int, Bool, String) -> (Int, Int, Bool, String)
mash n (t,w,c,d) = (t+(n), w, c, "mashed " ++ d)

stick :: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
stick (t,w,c,d) = (t+120, w, True, "stewed " ++ d)

mix :: (Int, Int, Bool, String) -> (Int, Int, Bool, String) -> (Int, Int, Bool, String)
mix (a,b,c,d) (w,x,y,z) = (a+w, b+x, c && y, d ++ (" and " ++ z))


-- What is the difference between a GPL and a DSL?
--
-- The difference between a GPL and a DSL is that a General Purpose Language is
-- Turing complete, whereas a Domain Specific Language may not be.


-- What programming methods should be used in shallow embeddings and deep
-- embeddings when dealing with dependent interpretation?
--
-- Shallow: You output a tuple for the semantic output that evaluates all the
-- functions that need to be evaluated and then use these lues as you increment
-- through.
-- Deep: You simply create a new semantic function and call it as needed.
