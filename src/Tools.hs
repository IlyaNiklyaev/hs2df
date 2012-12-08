module Tools where

safeInit :: [a] -> [a]
safeInit [] = []
safeInit l = init l