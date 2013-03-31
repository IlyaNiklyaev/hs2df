module B where

class TestClass m where
        toB :: m -> Bool

data TestData = One | Two

instance TestClass TestData where
        toB One = False
        toB Two = True

main :: Maybe Int -> Int
main Nothing = 0
main (Just 1) = 100
main (Just 2) = 200
main (Just y) = y + 300

--main 0 = 0
--main 1 = 1
--main y 
--        | x <= 5 = 2
--        | x <= 10 = 3
--        | otherwise = 4
--        where x = y + 1

--main :: (Int, Int) -> Int
--main (1,2) = 0
--main (2,y) = 1 + y
--main (3,y) = 2 + y
--main (_,_) = 3 + 4

--main 0 = 100
--main 1 = 200
--main _ = 300