module B where

class TestClass m where
        toB :: m -> Bool

data TestData = One | Two

instance TestClass TestData where
        toB One = False
        toB Two = True

main :: (Int, Int) -> Int
--main 0 = 0
--main 1 = 1
--main y 
--        | x <= 5 = 2
--        | x <= 10 = 3
--        | otherwise = 4
--        where x = y + 1

main (1,2) = 0
main (_,_) = 3 + 4