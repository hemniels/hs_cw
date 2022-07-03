
module Uebungen.Uebung4 where

import Data.Array ( accumArray,  Ix, (!), Array )

{-

    In den dieswöchigen Praxisaufgaben geht es um das 'traveling salesperson
    problem'. Dazu steht Ihnen ein Array
    
    karte:: Array (Stadtteil, Stadtteil) Int

    zur Verfügung, welches die Distanzen zwischen den gegebenen Orten speichert.
    Die Distanz zwischen zwei Orten kann über die Funktion

    distanz:: Stadtteil -> Stadtteil -> Int

    abgerufen werden.

    Die Orte, welche besucht werden müssen, haben den Datentypen 'Stadtteil'
    (siehe Anhang unten). Rundtouren im Sinne des TSPs werden als Listen der Form
    '[Stadtteil]' gespeichert. In einer gültigen Rundtour muss jeder vorhandene
    Ort genau einmal vorkommen.



    ## AUFGABE 1 ##

    a)  -- 2 Punkte

    Implementieren Sie eine Funktion

    routenLaenge:: [Stadtteil] -> Int,

    welche für eine gegebene Liste von Stadtteilen, die eine ausgewählte Route
    darstellt, die Routenlänge ausgibt.

    Um die Distanz zwischen zwei Stadtteilen zu bestimmen, können Sie die
    Funktion 'distanz' aus dem Anhang verwenden.

-}

routenLaenge:: [Stadtteil] -> Int
routenLaenge [] = 0
routenLaenge (erstestadt :xs) = help (erstestadt :xs) 
   where help [letztestadt] = distanz letztestadt erstestadt 
         help (erstestadt' : zweitestatd : zs) = distanz erstestadt' zweitestatd + help (zweitestatd:zs) 
{-

    ## AUFGABE 1 ##

    b)  -- 2 Punkte

    Implementieren Sie eine Funktion

    naechsterOrt:: Stadtteil -> [Stadtteil] -> Stadtteil,

    welche mittels:

    i)  dem aktuellen Standort (1. Parameter)
    ii) Auswahlmöglichkeiten weiterer (noch nicht besuchter) Standorte (2. Parameter)

    den Stadtteil mit der kürzesten Distanz zum aktuellen Standort bestimmt.

-}


naechsterOrt:: Stadtteil -> [Stadtteil] -> Stadtteil
naechsterOrt stadtteil [] = stadtteil
naechsterOrt stadtteil [x] = x 
naechsterOrt stadtteil (x:y:ys) = if distanz stadtteil x <= distanz stadtteil y then x
                                                                                else naechsterOrt stadtteil (ys)

{-

    ## AUFGABE 1 ##

    c)  -- 3 Punkte

    Implementieren Sie eine rekursive Funktion

    opportunistischeSuche:: [Stadtteil] -> [Stadtteil], 

    welche für eine gegebene Liste von Stadtteilen, die eine beliebige
    TSP-Instanz repräsentiert, eine möglichst gute Lösung mittels der
    opportunistischen Suche für das TSP-Problem bestimmt.

-}

opportunistischeSuche:: [Stadtteil] -> [Stadtteil]
opportunistischeSuche [] = []
opportunistischeSuche [x] = [x]
opportunistischeSuche (x:y:ys) = x:(naechsterOrt x (y:ys)) : opportunistischeSuche (loesche (naechsterOrt x (y:ys) ) (y:ys))
      where
         loesche :: Eq a => a -> [a] -> [a]
         loesche _ [] = []
         loesche x (y:ys)  
                        | x == y  = ys
                        | otherwise = y : loesche x ys
{-

    d)  -- 2 Bonuspunkte

    Implementieren Sie eine Funktion

    permutationen:: Eq a => [a] -> [[a]],

    welche alle möglichen Permutationen für eine gegebene Liste Elementen
    (polymorpher Datentyp) erzeugt.

-}

permutationen:: Eq a => [a] -> [[a]]
permutationen [] = [[]]
permutationen (x:xs) = [ps++[x]++qs | rs <- permutationen xs ,(ps,qs) <- help' rs ]
            where
                help' :: [a]->[([a],[a])]
                help' []     = [ ([],[]) ]
                help' (y:ys) = ([],y:ys) : [ (y:ps,qs) | (ps,qs) <- help' ys]
                



{-

    e)  -- 3 Punkte

    Implementieren Sie eine rekursive Funktion

    erschoepfendeSuche:: [Stadtteil] -> [Stadtteil],

    welche für eine gegebene Liste von Stadtteilen, die eine beliebige
    TSP-Instanz repräsentiert, die optimale Lösung mittels der erschöpfenden
    Suche bestimmt.


    * HINWEIS:

    Hilfreich hierfür könnte die Funktion 'permutationen' aus Bonusaufgabenteil
    (d) sein. Sollten Sie (d) nicht bearbeitet haben, so können Sie die Funktion
    'permutations' aus dem Modul 'Data.List' verwenden.

-}

erschoepfendeSuche:: [Stadtteil] -> [Stadtteil]
erschoepfendeSuche xs = [] --minimum zip $ permutationen xs $ map (\x -> routenLaenge x ) permutationen xs  -- TODO

-- ## ANHANG ##

data Stadtteil = AntiByte
               | Bytetown
               | Bytefeld
               | PicoByte
               | FemtoByte
               | MegaByte
               | KiloByte
               | LambdaByte
               | MuByte
               deriving (Show, Eq, Ord, Ix, Enum,Bounded)

karte:: Array (Stadtteil, Stadtteil) Int
karte = accumArray (+) 
                   0 
                   ((AntiByte, AntiByte), (MuByte, MuByte)) 
                   (distances++map (\((a,b),d) -> ((b,a),d)) distances)
    where
        distances = [ ((AntiByte, AntiByte), 0),
                      ((AntiByte, Bytetown), 636),
                      ((AntiByte, Bytefeld), 256),
                      ((AntiByte, PicoByte), 377),
                      ((AntiByte, FemtoByte), 482),
                      ((AntiByte, MegaByte), 488),
                      ((AntiByte, KiloByte), 73),
                      ((AntiByte, LambdaByte), 560),
                      ((AntiByte, MuByte), 644),

                      ((Bytetown, Bytetown), 0),
                      ((Bytetown, Bytefeld), 392),
                      ((Bytetown, PicoByte), 394),
                      ((Bytetown, FemtoByte), 810),
                      ((Bytetown, MegaByte), 289),
                      ((Bytetown, KiloByte), 573),
                      ((Bytetown, LambdaByte), 190),
                      ((Bytetown, MuByte), 585),

                      ((Bytefeld, Bytefeld), 0),
                      ((Bytefeld, PicoByte), 183),
                      ((Bytefeld, FemtoByte), 578),
                      ((Bytefeld, MegaByte), 253),
                      ((Bytefeld, KiloByte), 193),
                      ((Bytefeld, LambdaByte), 371),
                      ((Bytefeld, MuByte), 598),

                      ((PicoByte, PicoByte), 0),
                      ((PicoByte, FemtoByte), 704),
                      ((PicoByte, MegaByte), 128),
                      ((PicoByte, KiloByte), 314),
                      ((PicoByte, LambdaByte), 373),
                      ((PicoByte, MuByte), 753),

                      ((FemtoByte, FemtoByte), 0),
                      ((FemtoByte, MegaByte), 756),
                      ((FemtoByte, KiloByte), 432),
                      ((FemtoByte, LambdaByte), 649),
                      ((FemtoByte, MuByte), 350),

                      ((MegaByte, MegaByte), 0),
                      ((MegaByte, KiloByte), 425),
                      ((MegaByte, LambdaByte), 396),
                      ((MegaByte, MuByte), 775),

                      ((KiloByte, KiloByte), 0),
                      ((KiloByte, LambdaByte), 500),
                      ((KiloByte, MuByte), 576),

                      ((LambdaByte, LambdaByte), 0),
                      ((LambdaByte, MuByte), 430),

                      ((MuByte, MuByte), 0) ]

-- | Gibt die Distanz zwischen zwei Stadtteilen wieder.
distanz:: Stadtteil -> Stadtteil -> Int
distanz stadtteil1 stadtteil2 = karte ! (stadtteil1, stadtteil2)

-- | Liste aller Stadtteile, beginnend in ByteCenter
alleStadtteile:: [Stadtteil]
alleStadtteile = [AntiByte .. MuByte]

-- ## SPIELINTERFACE ##

postRouteGS:: [Stadtteil]
postRouteGS = opportunistischeSuche alleStadtteile

postRouteBF:: [Stadtteil]
postRouteBF = erschoepfendeSuche alleStadtteile
