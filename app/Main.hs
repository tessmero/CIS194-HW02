module Main where

import Log
import LogAnalysis
import Language.Haskell.Interpreter

demor :: String -> String -> IO ()
demor cmd sresult = do
  let trim = reverse (drop 1 (reverse (drop 7 sresult)))
  putStrLn (cmd ++ " = " ++ trim)

demoLogm :: String -> IO ()
demoLogm cmd = do
  result <- runInterpreter $ setImports ["Prelude","Log","LogAnalysis"] >> interpret cmd (as :: LogMessage)
  demor cmd (show result)

demoLogs :: String -> IO ()
demoLogs cmd = do
  result <- runInterpreter $ setImports ["Prelude","Log","LogAnalysis"] >> interpret cmd (as :: (IO [LogMessage]))
  demor cmd (show result)

main :: IO ()
main = do
  putStrLn "CIS194 Homework 2"

  putStrLn "\nExercise 1"
  demoLogm "parseMessage \"E 2 562 help help\"               "
  demoLogm "parseMessage \"I 29 la la la\"                   "
  demoLogm "parseMessage \"This is not in the right format\" "
  demoLogs "testParse parse 10 \"../input/error.log\"        "
