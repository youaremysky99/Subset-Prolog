module Main where

import System.IO
import System.Console.Haskeline
import System.Environment
import qualified Data.Map.Strict as M
import Parser
import AST
import Engine

data State = Idle | Search Rel Tree Int

showTruth :: Int -> IO ()
showTruth nSols = if nSols == 0 then putStrLn "false" else putStrLn "true"

printSolution :: Rel -> Subs -> IO ()
printSolution query table = traverse printVar (variables query) >> return ()
  where
    printVar v@(Var x) = putStrLn $ x ++ " = " ++ show (resolve table v)

interpret :: Program -> Rel -> [Subs]
interpret prog rel = searchAll prog (initTree rel) 0

lazy_show :: Rel -> [Subs] -> Bool -> IO() 
lazy_show rel [] _ = showTruth 0
lazy_show rel [x] _ = (printSolution rel x) 
lazy_show rel here@(x:xs) firstTime =  
  do 
    _ <- if (firstTime) then (printSolution rel x) else (return ())
    chr <- getChar
    hFlush stdout
    case chr of 
        '.' -> (putStrLn "") >> return ()
        ';' -> (putStrLn "") >> (lazy_show rel xs True)
        _ -> (lazy_show rel here False)

{- allow a goal to be typed -}
run_prog :: Program -> IO ()
run_prog program = do
  putStrLn "type a goal: "
  command <- getLine
  if (command == "quit")
    then return ()
    else
      case parseRel command of
        Right rel -> 
          do
            let answers = interpret program rel
            (lazy_show rel answers True) >> (run_prog program)

        Left err  -> print err >> run_prog program

{- reading prolog file as argument -}
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

  args <- getArgs
  case args of
    [filename] -> do
      source <- readFile (head args)
      case parseProgram source of
        Right program -> putStrLn "Loaded successfully" >> run_prog program
        Left err -> print err
    _ -> putStrLn "Usage: purelog <filename>"
  return ()
