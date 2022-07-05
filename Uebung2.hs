
module Uebungen.Uebung2 where



toggle:: Int -> [(Int,Bool)] -> [Bool]
toggle n xs = [ if mod (fst x) n == 0 then (not (snd x)) else (snd x)| x <- xs ]


whistle:: Int -> Int -> Int
whistle 0 _ =  0
whistle _ 0 = 0
whistle n studierende =  length (toggle n (schrankNummerMitZustand studierende)) + whistle (n-1) studierende 

schranksZustand ::Int -> [Bool] 
schranksZustand 0 = []
schranksZustand n = False : schranksZustand (n-1) 

schranksNummer :: Int -> [Int] 
schranksNummer 0 = []
schranksNummer n = schranksNummer (n-1) ++ [n]

schrankNummerMitZustand :: Int -> [(Int,Bool)]
schrankNummerMitZustand n = zip (schranksNummer n) (schranksZustand n)

amountGongs:: Int
amountGongs = 0  -- TODO: An gefundenen Wert anpassen

amountStudents:: Int
amountStudents = 0  -- TODO: An gefundenen Wert anpassen

openClosets:: Int
openClosets = whistle amountGongs amountStudents
