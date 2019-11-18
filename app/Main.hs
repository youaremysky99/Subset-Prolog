module Main where

import System.IO
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
lazy_show rel [x] _ = if (length (variables rel)) == 0 then showTruth 1 else (printSolution rel x) >> (lazy_show rel [] True) 
lazy_show rel here@(x:xs) firstTime = if (length (variables rel)) == 0 then showTruth 1 else
  do 
    _ <- if (firstTime) then (printSolution rel x) else (printSolution rel x) >> (lazy_show rel [] True)
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
      case parseQuery command of
        Right (Multiple relations) ->
          do
            let rels = relations >>= (\lst -> lst >>= (\x -> return [x]))
            let all = unique $ variablesOfQuery(rels)
            let new_predicate = Rel "mr_phan_duc_nhat_minh_pro_vip_98" all 
            let new_rule = (Rule new_predicate rels) 
            let new_program = append program new_rule
            let answers = interpret new_program new_predicate 
            (lazy_show new_predicate answers True) >> (run_prog program)
        Right (Single rel) -> 
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