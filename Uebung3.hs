
module Uebungen.Uebung3 where


divisors:: Integral a => a -> [a]
divisors x = [a | a <- [1..x],mod x a == 0]


divisors':: Integral a => a -> [a]
divisors' x = [a | a <- [1..(div x 2)], mod x a == 0]



epsilon:: Float
epsilon = 0.0000001

heron:: Float -> Float
heron r = help 0
  where
    -- Hilfsfunktion:
    help x
      -- Abbruchbedingung
      | abs (heron' x - heron' (x + 1)) < epsilon = heron' (x + 1)
      -- Hilfswert inkrementieren
      | otherwise                                 = help (x + 1)

    -- Heron Verfahren Rekursionsvorschrift
    heron' 0 = 1
    heron' x = (1 / 2) * (heron' (x-1) + (r / heron' (x-1)))



-- mit divisors 
eratosthenes:: Integral a => a -> [a]
eratosthenes 0 = []
eratosthenes 1 = []
eratosthenes x =  map fst (filter (listenMitLenge2) (zip [2..x] (map divisors [2..x])))
listenMitLenge2:: (Integral a) => (a,[a]) -> Bool
listenMitLenge2 x = if (length (snd x)) == 2 then True else False

-- mit divisors' 
eratosthenes':: Integral a => a -> [a]
eratosthenes' 0 = []
eratosthenes' 1 = []
eratosthenes' x =  map fst (filter (listenMitLenge1) (zip [2..x] (map divisors' [2..x])))
listenMitLenge1:: (Integral a) => (a,[a]) -> Bool
listenMitLenge1 x = if (length (snd x)) == 1 then True else False
-- ## SPIELINTERFACE ##

divisorsInterface :: [Int]
divisorsInterface = divisors 42

eratosthenesInterface :: [Int]
eratosthenesInterface = eratosthenes 50