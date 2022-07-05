
module Uebungen.Uebung1 where


import Data.List ((\\))
schachfigurPositionenS:: [(Int, Char)]
schachfigurPositionenS = [(8,y) | (_,y) <-schachfigurPositionenW]  



schachfigurenMitPosW:: [(Schachfigur, (Int, Char))]
schachfigurenMitPosW = zip schachfiguren schachfigurPositionenW 

schachfigurenMitPosS:: [(Schachfigur, (Int, Char))]
schachfigurenMitPosS = zip schachfiguren schachfigurPositionenS

bauernWeiß:: [(Schachfigur, (Int, Char))]
bauernWeiß = zip (cycle [bauer]) [(2,y)| y <- ['a'..'h'] ]

bauernSchwarz:: [(Schachfigur, (Int, Char))]
bauernSchwarz = zip (cycle [bauer]) [(7,y)| y <- ['a'..'h'] ]



schachbrettMitFiguren:: [(Schachfigur, (Int, Char))]
schachbrettMitFiguren = schachfigurenMitPosW ++ bauernWeiß ++
                        (zip [leeresFeldW | _ <- [1..]] 
                             [ (reihe, spalte) | reihe <- [3..6]
                                               , spalte <- ['a'..'h'] ]) ++ bauernSchwarz ++ schachfigurenMitPosS




type Schachfigur = String

-- | Leeres Schachbrett
leeresSchachbrett:: [(Schachfigur, (Int, Char))]
leeresSchachbrett = zip [leeresFeldW | _ <- [1..]] 
                        [ (reihe, spalte) | reihe <- [1..8]
                                          , spalte <- ['a'..'h'] ]

-- | Leeres Schachfeld
leeresFeldW:: Schachfigur
leeresFeldW = "Leerstehend"

-- | Bauer Spielfigur
bauer:: Schachfigur
bauer = "Bauer"

-- | Schachfiguren
schachfiguren:: [Schachfigur]
schachfiguren = [ "Turm", "Katze", "Laeufer"
                , "Dame", "Koenig", "Laeufer"
                , "Katze", "Turm" ]

-- | Schachfigurpositionen für Weiß
schachfigurPositionenW:: [(Int, Char)]
schachfigurPositionenW = [ (1, 'a'), (1, 'b'), (1, 'c')
                         , (1, 'd'), (1, 'e'), (1, 'f')
                         , (1, 'g'), (1, 'h') ]

-- ## SPIELINTERFACE ##

-- | Wähle dir einen passenden Gegenstand zum Öffnen deines Spindes!
chosenTool:: Tool
chosenTool = LeereHaende


-- | Auswählbare Gegenstände
data Tool = Nichts
          | Besen
          | Studierendenausweis
          | LeereHaende
          deriving Eq


-- | Kontrollwert unverändert lassen und nicht weiter benutzen.
schachbrettAufgeräumt:: Bool
schachbrettAufgeräumt
    = [ ("Turm",(1,'a'))
      , ("Katze",(1,'b'))
      , ("Laeufer",(1,'c'))
      , ("Dame",(1,'d'))
      , ("Koenig",(1,'e'))
      , ("Laeufer",(1,'f'))
      , ("Katze",(1,'g'))
      , ("Turm",(1,'h'))
      , ("Turm",(8,'a'))
      , ("Katze",(8,'b'))
      , ("Laeufer",(8,'c'))
      , ("Dame",(8,'d'))
      , ("Koenig",(8,'e'))
      , ("Laeufer",(8,'f'))
      , ("Katze",(8,'g'))
      , ("Turm",(8,'h'))
      , ("Bauer",(2,'a'))
      , ("Bauer",(2,'b'))
      , ("Bauer",(2,'c'))
      , ("Bauer",(2,'d'))
      , ("Bauer",(2,'e'))
      , ("Bauer",(2,'f'))
      , ("Bauer",(2,'g'))
      , ("Bauer",(2,'h'))
      , ("Bauer",(7,'a'))
      , ("Bauer",(7,'b'))
      , ("Bauer",(7,'c'))
      , ("Bauer",(7,'d'))
      , ("Bauer",(7,'e'))
      , ("Bauer",(7,'f'))
      , ("Bauer",(7,'g'))
      , ("Bauer",(7,'h'))
      , ("Leerstehend",(3,'a'))
      , ("Leerstehend",(3,'b'))
      , ("Leerstehend",(3,'c'))
      , ("Leerstehend",(3,'d'))
      , ("Leerstehend",(3,'e'))
      , ("Leerstehend",(3,'f'))
      , ("Leerstehend",(3,'g'))
      , ("Leerstehend",(3,'h'))
      , ("Leerstehend",(4,'a'))
      , ("Leerstehend",(4,'b'))
      , ("Leerstehend",(4,'c'))
      , ("Leerstehend",(4,'d'))
      , ("Leerstehend",(4,'e'))
      , ("Leerstehend",(4,'f'))
      , ("Leerstehend",(4,'g'))
      , ("Leerstehend",(4,'h'))
      , ("Leerstehend",(5,'a'))
      , ("Leerstehend",(5,'b'))
      , ("Leerstehend",(5,'c'))
      , ("Leerstehend",(5,'d'))
      , ("Leerstehend",(5,'e'))
      , ("Leerstehend",(5,'f'))
      , ("Leerstehend",(5,'g'))
      , ("Leerstehend",(5,'h'))
      , ("Leerstehend",(6,'a'))
      , ("Leerstehend",(6,'b'))
      , ("Leerstehend",(6,'c'))
      , ("Leerstehend",(6,'d'))
      , ("Leerstehend",(6,'e'))
      , ("Leerstehend",(6,'f'))
      , ("Leerstehend",(6,'g'))
      , ("Leerstehend",(6,'h')) ] \\ schachbrettMitFiguren == []
