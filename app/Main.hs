module Main where

import Log
import LogAnalysis
import Language.Haskell.Interpreter

demor :: String -> String -> IO ()
demor cmd sresult =
  putStrLn (cmd ++ " = " ++ (drop 6 (sresult)))

demoInts :: String -> IO ()
demoInts cmd = do
  result <- runInterpreter $ setImports ["Prelude","Log","LogAnalysis"] >> interpret cmd (as :: [Integer])
  demor cmd (show result)

main :: IO ()
main = do
  putStrLn "CIS194 Homework 2"

  putStrLn "\nExercise 1"
  demoInts "toDigits 1234              "
