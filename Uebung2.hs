
module Uebungen.Uebung2 where

{-

    Erinnern Sie sich an die dritte Aufgabe des ersten Theorie-Übungsblattes.

    In der dieswöchigen Praxisübung, werden Sie einen Algorithmus
    implementieren, welcher nahezu die gleiche Idee umsetzt.

    ## AUFGABE 1 ##

    a)  -- 5 Punkte

    Schreiben Sie eine Funktion in Haskell:

    toggle:: (..) -> [Bool] ,

    welche das Auf- und Zumachen der Schulschränke simuliert. D.h. diese
    Funktion soll den aktuellen gegebenen Wert (oder auch Zustand) ALLER
    gegebene Schulschränke negieren.

    Dabei beschreibt ein 'Bool' einen Schulschrank wie folgt:

        'False' : Der Schulschrank ist geschlossen
        'True'  : Der Schulschrank ist geöffnet

    Beispiel: (hier sind die Zustaende aller Spinde als einer Liste von
              boolschen Werten festgehalten)
    toggle [True, False, False] = [False, True, True]

    HINWEIS:
    Das '(..)' in der Aufgabenbeschreibung bedeutet, dass es Ihnen
    offen steht an dieser Stelle beliebig viele weitere Datentypen
    einzusetzen. Sie muessen sich also zuerst die Typsignatur der
    Funktion ueberlegen.

-}

toggle:: Int -> [(Int,Bool)] -> [Bool]
toggle n xs = [ if mod (fst x) n == 0 then (not (snd x)) else (snd x)| x <- xs ]


{-  ## AUFGABE 1 ##

    b)  -- 5 Punkte

    Schreiben Sie eine Funktion:

    whistle:: Int -> Int -> Int

    die ausgibt wie viele Schulschränke nach einer vorgegebenen Anzahl an Gongs
    noch OFFEN sind. Dabei haben die Parameter der Funktion folgende
    Bedeutungen:

        1. Parameter 'Int' : Die Anzahl der Gongs
        2. Parameter 'Int' : Die Anzahl der Studierenden
    
    Gehen Sie davon aus, dass jeder Studierende genau einen Schulschrank
    besitzt!

    ! Im Unterschied zur Theorieaufgaben können hier jedoch die Anzahl der
    Gongs von der Anzahl der Studierenden abweichen !

-}

whistle:: Int -> Int -> Int
whistle 0 _ =  0
whistle _ 0 = 0
whistle n studierende =  length (toggle n (schrankNummerMitZustand studierende)) + whistle (n-1) studierende -- ich weiss nicht wie ich das rikusiv machen kann

schranksZustand ::Int -> [Bool] 
schranksZustand 0 = []
schranksZustand n = False : schranksZustand (n-1) 

schranksNummer :: Int -> [Int] 
schranksNummer 0 = []
schranksNummer n = schranksNummer (n-1) ++ [n]

schrankNummerMitZustand :: Int -> [(Int,Bool)]
schrankNummerMitZustand n = zip (schranksNummer n) (schranksZustand n)

-- ## SPIELINTERFACE ##

-- Um im Spiel weiterzukommen, korrigiere die zwei folgenden Werte,
-- nach erfolgreicher Umsetzung von toggle und whistle!

-- 1) Wie häufig wird der Schulgong laut dem Hausmeister läuten?
amountGongs:: Int
amountGongs = 0  -- TODO: An gefundenen Wert anpassen

-- 2) Wie viele Schulschränke sind im Schulgebäudeabschnitt, den
-- du begehen konntest, vorzufinden?
amountStudents:: Int
amountStudents = 0  -- TODO: An gefundenen Wert anpassen

-- | Kontrollwert unverändert lassen und nicht weiter benutzen.
openClosets:: Int
openClosets = whistle amountGongs amountStudents
