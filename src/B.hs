module B where

main :: Float -> Float
main x = if x * 2 > 5 then x + 3 else x

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