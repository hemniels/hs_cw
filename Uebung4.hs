
module Uebungen.Uebung4 where

import Data.Array ( accumArray,  Ix, (!), Array )


routenLaenge:: [Stadtteil] -> Int
routenLaenge [] = 0
routenLaenge (erstestadt :xs) = help (erstestadt :xs) 
   where help [letztestadt] = distanz letztestadt erstestadt 
         help (erstestadt' : zweitestatd : zs) = distanz erstestadt' zweitestatd + help (zweitestatd:zs) 


naechsterOrt:: Stadtteil -> [Stadtteil] -> Stadtteil
naechsterOrt stadtteil [] = stadtteil
naechsterOrt stadtteil [x] = x 
naechsterOrt stadtteil (x:y:ys) = if distanz stadtteil x <= distanz stadtteil y then x
                                                                                else naechsterOrt stadtteil (ys)

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

permutationen:: Eq a => [a] -> [[a]]
permutationen [] = [[]]
permutationen (x:xs) = [ps++[x]++qs | rs <- permutationen xs ,(ps,qs) <- help' rs ]
            where
                help' :: [a]->[([a],[a])]
                help' []     = [ ([],[]) ]
                help' (y:ys) = ([],y:ys) : [ (y:ps,qs) | (ps,qs) <- help' ys]
                




erschoepfendeSuche:: [Stadtteil] -> [Stadtteil]
erschoepfendeSuche xs = [] --minimum zip $ permutationen xs $ map (\x -> routenLaenge x ) permutationen xs  -- TODO


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

distanz:: Stadtteil -> Stadtteil -> Int
distanz stadtteil1 stadtteil2 = karte ! (stadtteil1, stadtteil2)

alleStadtteile:: [Stadtteil]
alleStadtteile = [AntiByte .. MuByte]


postRouteGS:: [Stadtteil]
postRouteGS = opportunistischeSuche alleStadtteile

postRouteBF:: [Stadtteil]
postRouteBF = erschoepfendeSuche alleStadtteile
