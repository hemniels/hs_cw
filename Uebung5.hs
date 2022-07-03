module Uebungen.Uebung5 where

import Data.Char

{--
    ### AUFGABE 1 ###
   Wir nutzen Funktionen die nicht in Prelude enthalten sind und importieren Sie
   dazu aus einem gegebenen Modul.
   (Was machen die Funktionen? Fragen Sie hoogle.haskell.org indem Sie die Funk-
   tionsnamen 'hoogeln'.)

   Die Haskell-Funktion 'ord', aus dem Modul Data.Char konvertiert einen Buchstaben
   in seine zugehoerige Unicode Nummer.

   (a) Schreiben Sie eine Haskell-Funktion die einen gegeben Buchstaben im Bereich
   'A' bis  'Z' in einen Int-Wert zwischen 0 und 25 (Position im Alphabet) trans-
   formiert, mithilfe der Funktion 'ord' aus dem Modul Data.Char.
   Sie koennen auch das Modul in ghci laden, mittels
   :m Data.Char
   und sich dann mittels
   :bro
   alle Funktionen des Moduls anzeigen lassen.
--}

char2Int :: Char -> Int
char2Int c = ord c - (ord 'A')

{- (b) Schreiben Sie eine Funktion die einen gegeben Int-Wert im Bereich 0 bis 25
   zurueck in den entsprechenden Buchstaben transformiert
 -}

int2Char :: Int -> Char
int2Char n = chr $ ord 'A' + n

{- (c) Schreiben Sie eine Funktion die ALLE upper-Case Buchstaben in einem
   gegebenen String xs zaehlt
   -}
uppers :: String -> Int
uppers xs = length $ [x | x <- xs, isAsciiUpper x]

{-
   Irgendwie sah der Code in der Zeitung nach einer simplen Rotation der Buchstaben
   im Alphabets aus ... sowas wie fuer 'A' schreibe 'C' fuer 'B' schreibe 'D' ...,
   ob das bei der Dekodierung weiterhilft? Vielleicht ueberlegen wir uns als erstes
   wie ein String mit dieser Methode kodiert werden kann.

###### CODIEREN ######


    (d) Nutzen Sie Ihre Funktionen int2Char und char2Int um eine Haskell-Funktion
    'shift' zu schreiben, welche einen Buchstaben c um eine Faktor n (Int) im Al-
    phabet verschiebt:
    1. Der Buchstabe soll dabei zunaechst in einen Int transformiert werden.
    2. auf den Int soll der Shift-Faktor n addiert werden,
    3. aus dem neuen Wert soll darauffolgende der Restwert bei Division durch die
    Groesse des Alphabets (26) bestimmt werden (so werden Werte welche die Groesse
    des Alphabets ueberschreiten an den Anfang des Alphabet *gewrapped*) und
    4. der neue Int-Wert soll zurueck in einen Buchstaben konvertiert werden.

    WICHTIG: Das Alphabet in der Zeitung besteht nur aus Grossbuchstaben.

   Beispiel-Aufruf der Funktion:

    ghci> shift 3 'A'
    'D'
    ghci> shift 3 'Y'
    'B'

 -}
shift :: Int -> Char -> Char
shift n c = int2Char ((char2Int c + n) `mod` 26)  

{- Hurra, wir koennen jetzt Buchstaben um einen Faktor im Alphabet verschieben. Diese
   Methode nutzte schon CAESAR ...


   Also dann, einen ganzen String codieren...

   e) Schreiben Sie eine Funktion 'encode', welche eine List comprehension und Ihre
   Funktion 'shift' benutzt um alle Buchstaben eines gegebenen Strings xs um einen Faktor
   n im Alphabet zu verschieben und die den so transformierten String zurueckgibt.

   Testen Sie Ihre Funktion auf beliebigen Strings:

   ghci> encode 3 "AUDLECTURE"
   "DXGOHFWXUH"
-}

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

code :: String
code = encode 3 "AUDLECTURE"

{-   #### AUFGABE 2 ####
   Super wir koennen jetzt Strings codieren, aber wie hilft uns das beim
   Entschluesseln der mysterioes kodierten Zeitung?

###### DECODIEREN ######

   Vielleicht laesst sich von Frequenzen bestimmter Buchstaben in (englischen) Texten
   etwas ableiten... 'E' tritt am haeufigsten auf, mit einer Frequenz von 12.7%, 'Z'
   dagegen eher seltener. Das scheint zu passen...
-}

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2,
         0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.33, 9.0,
         2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

{-- Idee: Buchstaben treten in unterschiedlicher Frequenz in Texten auf.
   Kennen wir zudem die Frequenz der kodierten Buchstaben in einem kodierten String, so
   koennen wir vielleicht rueckschliessen welche Buchstaben Sie im urspruenglichen Text
   gewesen sind... vielleicht laesst sich so der Code knacken?


   a) Schreiben Sie eine Haskell-Funktion 'countChar' welche die Vorkommen eines bestimmten
   Buchstaben c in einem gegeben String xs zaehlt.

   Beispiel-Aufruf der Funktion:

   ghci> countChar 'E' "ALGORITHMEN UND DATENSTRUKTUREN"
   3
 -}

countChar :: Char -> String -> Int
countChar c [] = 0
countChar c (x:xs) = if c == x then 1 + (countChar c xs) else (countChar c xs)
{- Funktion, welche die Frequenz, d.h. das Vorkommen eines Buchstaben bezogen auf die
   Gesamtanzahl an Buchstaben berechnet:
   (Anzahl vorkommen Char c / Anzahl Chars gesamt) * 100
-}

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

{- Idee: vielleicht lassen sich die erwarteten Frequenzen aus einer gegebenen Tabelle im
   Vergleich mit den im String auftretenden Frequenzen nutzen um die 'wahrscheinlichste'
   Rotation unter allen moeglichen Rotationen eines kodierten Textes zu bestimmen

   Idee: Als erstes bestimmen wir die Frequenzen aller Buchstaben im kodierten String


   b) Nutzen Sie die Funktion 'percent' um eine Funktion 'frequencies' zu schreiben, welche
   mithilfe einer List comprehension und der Funktion 'countChar' eine Frequenztabelle, in
   Form einer Haskell-Liste fuer einen gegebenen String erzeugt. Die Vorkommen eines Buch-
   staben sollen als Frequenz enthalten sein.
   Die Position des Buchstaben im Alphabet [0..25] bestimmt dabei die Position des Eintrags
   in der die Tabelle darstellenden Frequenz-Liste vom Typ Float.

   Beispiel-Aufruf der Funktion:

   ghci> frequencies "ABBCCCDDDDEEEEE"
   [6.666667, 13.333334, 20.0, 26.666668, ... , 0.0]

   WICHTIG: Pruefen Sie ob
   ghci>  length meineTabelle == length table
-}

frequencies :: String -> [Float]
frequencies xs = [percent (countChar x xs ) (length xs) | x <- xs]


{- Wir nutzen nun den Chi-Quadrat Test, hier mittels List comprehension umgesetzt, um eine Liste
   von beobachteten Frequenzen (ofs) mit einer Liste von erwarteten Frequenzen (efs) zu vergleichen.
   Details dieses Tests interessieren uns hier nicht, wichtig ist lediglich, dass je kleiner der
   berechnete Wert umso besser stimmen die beiden Frequenzlisten ueberein.
-}

chiSqrt :: [Float] -> [Float] -> Float
chiSqrt ofs efs = sum [((o-e)^2)/e | (o,e) <- zip ofs efs]

{- Nun wollen wir uns an die finale Decodierung machen...

  c) Schreiben Sie eine Funktion 'decypher', welche das Folgende umsetzt und zum Decodieren benutzt
   werden kann:
   1. Berechnet die Frequenztabelle des eingegebene kodierten Strings und speichert sie in einer
   Haskell-Liste table' mittels Ihrer Funktion 'frequencies'
   2. Rotiert die Liste table' und vergleicht jede Rotation [0..25] mittels der Chi-Quadrat Statistik
   (chiSqrt) mit der Liste table und speichert jedes der Chi-Quadrat Statistik Ergebnisse in eine
   Haskell-Liste chitab. Die Rotation, bestimmt dabei den Index des Eintrags in die Haskell-Liste
   chitab,
   3. Nimmt den Index des minimalsten Eintrags in der Haskell Liste chitab und nutzt ihn als Rotations-
   faktor (factor) zur Decodierung des Strings xs, d.h. um den String xs um einen gegebenen Faktor nach
   links zu verschieben.
-}

decypher :: String -> String
decypher xs = "solve" --TODO
   -- where
      --TODO
-- Definieren Sie ZUERST folgenden Hilfs-Funktionen zur Nutzung in Ihrer 'decypher'-Funktion

{-
   d) Schreiben Sie eine polymorphe Funktion rotate, welche eine Liste von Elementen xs um n Plaetze nach
   LINKS rotiert, wobei Elemente die aus dem Start der Liste fallen am Ende wieder angefuegt werden sollen.
   Tipp: Hier lassen sich einfache Listenfunktionen nutzen.

   Beispiel-Aufruf der Funktion:

   ghci> rotate 3 [0.2, 0.3, 0.4, 0.5]
   [0.5,0.2,0.3,0.4]
 -}
rotate :: Int -> [a] -> [a]
rotate n xs = [] --TODO

{-
   e) Schreiben Sie eine Haskell-Funktion 'positions' die eine Liste aller Positionen zurueckgibt an denen
   ein Element x in einer ihr uebergebenen Liste xs vorkommt. Nutzen Sie dazu nach Moeglichkeit eine List
   comprehension. Testen Sie Ihre Funktion auf verschiedenen Strings und einzelnen Buchstaben.

   Beispiel-Aufruf der Funktion:

   ghci> positions 'E' "ALGORITHMEN UND DATENSTRUKTUREN"
   [9,19,29]

-}
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [0] --TODO

{-
  Anwendung Ihrer 'decypher' Haskell-Funktion in einer Haskell-Definition 'rfunction' auf den folgenden String:
   "BRXU PLQG LV WKH NHB WR HYHUB GRRU"
-}

-- ## SPIELINTERFACE ##
rfunction :: String
rfunction = decypher "BRXU PLQG LV WKH NHB WR HYHUB GRRU"
