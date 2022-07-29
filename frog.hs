-- frog.hs - FROG interpreter
-- Copyright (C) 2022 Robert Coffey
-- Released under the MIT license.

import Text.Read

-- fexp ------------------------------------------------------------------------

data FExp = FPrim Prim | FInt Int | FVoid | FError

instance Show FExp where
  show (FPrim _) = "prim"
  show (FInt n) = show n
  show (FVoid) = "void"
  show (FError) = "error"

-- stack -----------------------------------------------------------------------

type Stack = [FExp]

emptyStack :: Stack
emptyStack = []

stackSize :: Stack -> Int
stackSize stk = length stk

stackPush :: FExp -> Stack -> Stack
stackPush e stk = e:stk

stackPop :: Stack -> (FExp,Stack)
stackPop (e:stk) = (e,stk)

printStack :: Stack -> IO ()
printStack stk =
  do putChar '<'
     putStr . show $ length stk
     putStr "> "
     printStack' $ reverse stk

printStack' :: Stack -> IO ()
printStack' [] = return ()
printStack' (e:es) =
  do putStr . show $ e
     putChar ' '
     printStack' es

-- dictionary ------------------------------------------------------------------

type Dict = [(String,FExp)]

dictLookup :: String -> Dict -> Maybe (String,FExp)
dictLookup w [] = Nothing
dictLookup w (p:ps)
  | w == fst p = Just p
  | otherwise  = dictLookup w ps

-- eval ------------------------------------------------------------------------

eval :: String -> Stack -> Dict -> (Stack,Dict)
eval w stk dict =
  case exp of
    Just (FPrim p) -> (p stk,dict)
    Nothing ->
      case num of
        Just n  -> (stackPush (FInt n) stk,dict)
        Nothing -> (stk,dict)
  where exp = case dictLookup w dict of
                Just (_,e) -> Just e
                Nothing    -> Nothing
        num = readMaybe w :: Maybe Int

-- primitive -------------------------------------------------------------------

type Prim = Stack -> Stack

primDrop :: Prim
primDrop [] = [FError]
primDrop (e:es) = es

primDup :: Prim
primDup [] = [FError]
primDup (e:es) = e:e:es

primSwap :: Prim
primSwap (e1:e2:es) = e2:e1:es
primSwap _ = [FError]

primAdd :: Prim
primAdd ((FInt n1):(FInt n2):es) = (FInt $ n1 + n2):es
primAdd _ = [FError]

primSub :: Prim
primSub ((FInt n1):(FInt n2):es) = (FInt $ n1 - n2):es
primSub _ = [FError]

primMul :: Prim
primMul ((FInt n1):(FInt n2):es) = (FInt $ n1 * n2):es
primMul _ = [FError]

-- main ------------------------------------------------------------------------

toplevelDict :: Dict
toplevelDict =
  [ ("drop", FPrim primDrop)
  , ("dup",  FPrim primDup)
  , ("swap", FPrim primSwap)
  , ("+", FPrim primAdd)
  , ("-", FPrim primSub)
  , ("*", FPrim primMul)
  ]

printBanner :: IO ()
printBanner = mapM_ putStrLn ["FROG v0.0.0", "(C) 2022 Robert Coffey"]

printPrompt :: IO ()
printPrompt = putStr "> "

main :: IO ()
main =
  do printBanner
     main' [] emptyStack toplevelDict

main' :: [String] -> Stack -> Dict -> IO ()
main' [] stk dict =
  do printStack stk
     printPrompt
     line <- getLine
     main' (words line) stk dict
main' (w:ws) stk dict = main' ws stk' dict'
  where (stk',dict') = eval w stk dict
