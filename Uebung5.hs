module Uebungen.Uebung5 where

import Data.Char

char2Int :: Char -> Int
char2Int c = ord c - (ord 'A')

int2Char :: Int -> Char
int2Char n = chr $ ord 'A' + n


uppers :: String -> Int
uppers xs = length $ [x | x <- xs, isAsciiUpper x]

shift :: Int -> Char -> Char
shift n c = int2Char ((char2Int c + n) `mod` 26)  

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

code :: String
code = encode 3 "AUDLECTURE"


table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2,
         0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.33, 9.0,
         2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


countChar :: Char -> String -> Int
countChar c [] = 0
countChar c (x:xs) = if c == x then 1 + (countChar c xs) else (countChar c xs)


percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100


frequencies :: String -> [Float]
frequencies xs = [percent (countChar x xs ) (length xs) | x <- xs]



chiSqrt :: [Float] -> [Float] -> Float
chiSqrt ofs efs = sum [((o-e)^2)/e | (o,e) <- zip ofs efs]

decypher :: String -> String
decypher xs = "solve" --TODO
   -- where
      --TODO
rotate n xs = [] --TODO

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [0] --TODO
