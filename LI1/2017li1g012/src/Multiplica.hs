module Multiplica where

multiplica:: Int -> Int -> Int
multiplica a 0 = 0
multiplica a 1 = a
multiplica a b = a + ( multiplica a (b-1) )


elevado:: Int -> Int -> Int
elevado a 0 = 1
elevado a b = a * (elevado a (b-1) )
