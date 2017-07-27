module Main where

import Log
import LogAnalysis
import Language.Haskell.Interpreter


demort :: String -> String -> IO ()
demort cmd ('(':stresult) = demort cmd (init stresult)
demort cmd stresult = putStrLn (cmd ++ " = " ++ stresult)

demor :: String -> String -> IO ()
demor cmd sresult = demort cmd (drop 6 sresult) 

demoLogm :: String -> IO ()
demoLogm cmd = do
  result <- runInterpreter $ setImports ["Prelude","Log","LogAnalysis"] >> interpret cmd (as :: LogMessage)
  demor cmd (show result)

demoTree :: String -> IO ()
demoTree cmd = do
  result <- runInterpreter $ setImports ["Prelude","Log","LogAnalysis"] >> interpret cmd (as :: MessageTree)
  demor cmd (show result)

main :: IO ()
main = do
  putStrLn "CIS194 Homework 2"

  putStrLn "\nExercise 1"
  demoLogm "parseMessage \"E 2 562 help help\"               "
  demoLogm "parseMessage \"I 29 la la la\"                   "
  demoLogm "parseMessage \"This is not in the right format\" "

  putStrLn "\nExercise 2"
  demoTree "insert (parseMessage \"I 29 la la la\") Leaf     "
  demoTree "insert (parseMessage \"unkown message\") Leaf    "
  demoTree "insert (parseMessage \"I 1 one\") (insert (parseMessage \"I 2 two\") Leaf)\n"
  demoTree "insert (parseMessage \"I 2 two\") (insert (parseMessage \"I 1 one\") Leaf)\n"
    
