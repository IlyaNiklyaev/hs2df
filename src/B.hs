module Main where

data X = Xc | Yc

fact 0 x = 1
fact n x = n*fact(n-1) x

main = print 3