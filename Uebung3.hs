
module Uebungen.Uebung3 where

{-

    ## AUFGABE 1 ##  -- 2 Punkte

    Schreiben Sie eine Funktion:

    divisors:: Int -> [Int] ,

    welche für eine gegebene Zahl alle Teiler findet.

-}

{- wir haben die Frage nicht ganz gut verstanden also, 
   ob wir alle Teiler mit sich selbst oder ohne sich selbst schreiben 
   deshalb haben wir beide Varianten gemacht 
   -}

-- mit sich selbst acuh teilen  
divisors:: Integral a => a -> [a]
divisors x = [a | a <- [1..x],mod x a == 0]

-- ohne sich selbst teilen
divisors':: Integral a => a -> [a]
divisors' x = [a | a <- [1..(div x 2)], mod x a == 0]


{-

    ## AUFGABE 2 ##  -- 4 Punkte

    Die unten stehende Funktion 'heron' wirft bei jeder Ausführung eine
    Exception (D.h. sie funktioniert nicht).

    Sie können die Funktion reparieren, indem Sie:

    a) Den Funktionstypen von ihr und den Datentyp des Epsilons anpassen
    b) An zwei (!) Stellen in der Funktion passendere Funktionen anwenden
       als aktuell.
    
    Finden und korrigieren Sie die Problemauslöser.

-}

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

{-

    ## AUFGABE 3 ##  -- 4 Punkte

    In dieser Aufgabe implementieren Sie den Sieb des Eratosthenes.

    Der 'Sieb des Eratosthenes' bestimmt für Zahlen zwischen 2 und 'n',
    wobei 'n' eine natürliche Zahl ist, alle Primzahlen. Er funktioniert
    wie folgt:

    i)   Die kleinste unmarkierte Zahl ist immer eine Primzahl.

    ii)  Markiere (bzw. entferne) alle Vielfachen jener Primzahl aus der
         Liste [2..'n']
    
    iii) Wiederhole das Verfahren, für die nächst-kleinste, übergebliebenen
         Zahl.

    iv)  Terminiere, wenn das Verfahren für die letzte Zahl wiederholt wurde.

-}

-- mit divisors 
eratosthenes:: Integral a => a -> [a]
eratosthenes 0 = []
eratosthenes 1 = []
eratosthenes x =  map fst (filter (listenMitLenge2) (zip [2..x] (map divisors [2..x])))-- wir haben erstmal alle zahlen mit eine liste von ihren Teiler in eine Teupel gezipt
                                                                                         -- danach filtern wir alle tupeln, die ihren zweiten komponent (liste) die länge 1 hat.
                                                                                         -- am ende geben wir nur die zaheln aus diese Tuplen aus.

-- hier haben wir ein Hilfsfunktion geschrieben um danach in die filter-Funktion zu nutzen.
-- diese funktion hilft alle listen der länge 2 zu finden.
listenMitLenge2:: (Integral a) => (a,[a]) -> Bool
listenMitLenge2 x = if (length (snd x)) == 2 then True else False

-- mit divisors' 
eratosthenes':: Integral a => a -> [a]
eratosthenes' 0 = []
eratosthenes' 1 = []
eratosthenes' x =  map fst (filter (listenMitLenge1) (zip [2..x] (map divisors' [2..x])))-- wir haben erstmal alle zahlen mit eine liste von ihren Teiler in eine Teupel gezipt
                                                                                         -- danach filtern wir alle tupeln, die ihren zweiten komponent (liste) die länge 1 hat.
                                                                                         -- am ende geben wir nur die zaheln aus diese Tuplen aus.

-- hier haben wir ein Hilfsfunktion geschrieben um danach in die filter-Funktion zu nutzen.
-- diese funktion hilft alle liste der länge 1 zu finden
listenMitLenge1:: (Integral a) => (a,[a]) -> Bool
listenMitLenge1 x = if (length (snd x)) == 1 then True else False
-- ## SPIELINTERFACE ##

divisorsInterface :: [Int]
divisorsInterface = divisors 42

eratosthenesInterface :: [Int]
eratosthenesInterface = eratosthenes 50