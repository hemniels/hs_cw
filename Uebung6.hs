module Uebungen.Uebung6 where

{- 

    In den dieswöchigen Praxisaufgaben werden Sie sich mit der Registermaschine
    auseinandersetzen. Ihre Aufgabe wird es sein, eine Registermaschine mittels
    Haskell zu emulieren.
    
    Dazu müssen Sie neben einer Reihe von Hilfsfunktionen einen Datentyp
    vervollständigen, welcher die mögliche Instruktionen für eine 
    Registermaschine repräsentiert.

    Mithilfe der Hilfsfunktionen und des Datentyps werden Sie anschließend
    eine Funktion schreiben, welche eine Registermaschinenkonfiguration
    gemäß einer vorliegenden Instruktion in eine neue Konfiguration überführt.

    Zu guter Letzt schreiben Sie mithilfe der vorherigen Aufgabenteile
    den finalen 'Registermaschinenemulator'.


    * HINWEIS:

    Um das Verhalten Ihres Emulators zu testen steht Ihnen eine Hilfunktion

    rmWrapper:: Program -> Config -> IO ()

    zur Verfügung, welche Ihnen die Konfigurationsübergänge Ihrer
    Registermaschine im Terminal ausgibt.

    Sie können diesen über den Aufruf 'testRm' ausprobieren.


    ##### AUFGABE 1 #####

    a)  -- 1 Punkte

    Ergänzen Sie den Datentyp 'Instruction' um alle in der Vorlesung
    vorgestellten direkten Instruktionen der Registermaschine.

    * HINWEIS:

    In dieser Aufgabe bleibt das Ein- und Ausgabeband unberücksichtigt.
    Daher brauchen Sie nicht die 'Read' und 'Write' Instruktionen zu
    berücksichtigen.

-}
data Instruction = Load Int
                 | CLoad Int
                 | Store Int
                 | Add Int
                 | CAdd Int
                 | Sub Int
                 | CSub Int
                 | Mult Int
                 | CMult Int
                 | Div Int
                 | CDiv Int
                 | Goto Int
                 | If Int
                 | End
    deriving (Show, Eq)


{-

    Eine Konfiguration (b, C_0, C_1, ..) soll durch eine Liste der Form
    [b, C_0, C_1, ..] dargestellt werden.

    b)  -- 2.5 Punkte
   
    Vervollständigen Sie die unten stehenden Hilfsfunktionen, welche Operationen
    einer Registermaschine ausführen.

-}

type Config = [Int]



{-| 
    Lade den Wert an der Stelle 'entry' aus einer Registermaschinenkonfiguration
    'conf'.

    (nicht zu vervollständigen)
-}
getEntry:: Config -> Int -> Int
getEntry conf entry = conf !! entry



{-|
    Speichere einen Wert 'value' in einer Registermaschinenkonfiguration 'conf'
    an der Stelle 'pos'.

    (nicht zu vervollständigen)
-}
setEntry:: Config -> Int -> Int -> Config
setEntry conf pos value = help 0 conf
  where
    help _ [] = error "setEntry: empty list"
    help p (x : xs)
      | p == pos  = value : xs
      | otherwise = x : help (p + 1) xs



{-
    Lade den Wert gespeichert im Register 'entry' der
    Registermaschinenkonfiguration 'conf'.

    (nicht zu vervollständigen)
-}
getRegister:: Config -> Int -> Int
getRegister conf entry = getEntry conf (entry + 1)



{-
    Speichere einen Wert 'value' im Arbeitsspeicher im Register 'entry' der
    Registermaschinenkonfiguration 'conf'.

    (nicht zu vervollständigen)
-}
setRegister:: Config -> Int -> Int -> Config
setRegister conf entry value = setEntry conf (entry + 1) value


{-
    Lese den Befehlszähler aus einer Registermaschinenkonfiguration 'conf'.

    (nicht zu vervollständigen)
-}
getCounter:: Config -> Int
getCounter conf = getEntry conf 0



{-
    Setze den Befehlszähler in einer Registermaschinenkonfiguration 'conf'
    auf den Wert 'value'.
-}
setCounter:: Config -> Int -> Config
setCounter conf value = value :(tail conf)



{-
    Inkrementiere den Wert des Befehlszählers der
    Registermaschinenkonfiguration 'conf'.
-}
incCounter:: Config -> Config
incCounter conf = setCounter conf ((getCounter conf) +1 )



{-
    Lade den aktuellen Wert des Akkumulators der 
    Registermaschinenkonfiguration 'conf'.
-}
getAccum:: Config -> Int
getAccum conf = conf !! 1 



{-
    Setze den aktuellen Wert des Akkumulators einer Registermaschine auf den
    Wert 'value'
-}
setAccum:: Config -> Int -> Config
setAccum conf value = head conf : value : tail conf 



{-
    Lade die aktuell zu verarbeitende Instruktion eines Programms 'program' 
    mithilfe einer Registermaschinenkonfiguration 'conf'.
-}
getInstr:: Program -> Config -> Instruction
getInstr program conf = program !! (getCounter conf)



{-

    Implementieren Sie eine Funktion:

    rm:: Program -> Config -> Config,

    die ein gegebenes Programm ausgehend von einer Startkonfiguration
    verarbeitet und (falls das Programm terminiert) das Ergebnis in Form der
    Endkonfiguration der Maschine liefert.

    c)  -- 3 Punkte

    Überlegen Sie sich zuerst, wie man eine einzige Instruktion ausführen kann. 
    Schreiben Sie dafür eine Funktion:

    transfer:: Program -> Config -> Config
    
    Ein Ansatz ist unten angeführt.
-}

transfer:: Program -> Config -> Config
transfer program conf
  = case getInstr program conf of
      Load i  -> incCounter (setAccum conf (getRegister conf i))
      CLoad i -> setAccum conf i
      Add i   -> setAccum conf ((getAccum conf) + (getRegister conf i))
      CAdd i  -> setAccum conf ((getAccum conf)+i)
      Sub i   -> setAccum conf ((getAccum conf) - (getRegister conf i))
      CSub i  -> setAccum conf ((getAccum conf)-i)
      Mult i  -> setAccum conf ((getAccum conf) * (getRegister conf i))
      CMult i -> setAccum conf ((getAccum conf)*i)
      Div i   -> setAccum conf (div (getAccum conf) (getRegister conf i))
      CDiv i  -> setAccum conf (div (getAccum conf) i)
      End     -> setCounter conf ((getCounter conf ) - 1)
      Store i -> setRegister conf i (getAccum conf)



{-

    d)  -- 1 Punkt
    
    Nun muss eine Registermaschine eine Folge von Instruktionen ausführen
    können.

    Wir sagen, sie soll terminieren, wenn gilt:

    Zustand(i - 1) == Zustand(i)

    Schreiben Sie nun dem finale Registermaschinenemulator
    
    rm:: Program -> Config -> Config,
    
    welcher ganze Programme ausführen kann. Benutzen Sie dafür die
    'transfer'-Funktion.

-}

rm:: Program -> Config -> Config
rm program conf = []  -- TODO

{- 

    Ein Programm für eine Registermaschine lässt sich als eine Liste von
    Instruktionen '[Instruction]' darstellen.

    e)  -- 1 Punkt

    Schreiben Sie ein Programm für die Registermaschine, welche die Funktion

    n^n

    implementiert.

    Dabei soll das Ergebnis zum Schluss der Berechnung in C_1 
    (Das erste Register im Arbeitsspeicher) gespeichert werden.

-}

type Program = [Instruction]

prog:: Program
prog = []  -- TODO


-- ## ANHANG ##

rmWrapper:: Program -> Config -> IO ()
rmWrapper prog conf = do print $ show conf ++ " - next instruction: "
                                           ++ show (getInstr prog conf)
                         let conf' = transfer prog conf
                         if conf' == conf
                           then print "Halt."
                           else rmWrapper prog conf'

programInit:: (Program, Config)
programInit = (prog, [1, 0, 3, 0, 0])

testRm:: IO ()
testRm = uncurry rmWrapper programInit

-- ## SPIELINTERFACE ##

rmIsRunning:: Config
rmIsRunning = uncurry rm (prog, [1, 0, 3, 0, 0])