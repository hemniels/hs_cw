module Uebungen.Uebung6 where

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

type Config = [Int]


getEntry:: Config -> Int -> Int
getEntry conf entry = conf !! entry
setEntry:: Config -> Int -> Int -> Config
setEntry conf pos value = help 0 conf
  where
    help _ [] = error "setEntry: empty list"
    help p (x : xs)
      | p == pos  = value : xs
      | otherwise = x : help (p + 1) xs



getRegister:: Config -> Int -> Int
getRegister conf entry = getEntry conf (entry + 1)
setRegister:: Config -> Int -> Int -> Config
setRegister conf entry value = setEntry conf (entry + 1) value



getCounter:: Config -> Int
getCounter conf = getEntry conf 0
setCounter:: Config -> Int -> Config
setCounter conf value = value :(tail conf)



incCounter:: Config -> Config
incCounter conf = setCounter conf ((getCounter conf) +1 )



getAccum:: Config -> Int
getAccum conf = conf !! 1 
setAccum:: Config -> Int -> Config
setAccum conf value = head conf : value : tail conf 




getInstr:: Program -> Config -> Instruction
getInstr program conf = program !! (getCounter conf)




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




rm:: Program -> Config -> Config
rm program conf = []  -- TODO


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