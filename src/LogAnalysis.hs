module LogAnalysis (
  parseMessage, parse
  ) where

import Log

-- Exercise 1 
-- parse an individual message, then a whole log file

parseMessage :: String -> LogMessage

parseMessage ('I':' ':message) = do
  let parts = words message
  let timeStamp = read (parts!!0) :: Int
  let infoMsg = unwords (drop 1 parts)
  LogMessage Info timeStamp infoMsg

parseMessage ('W':' ':message) = do
  let parts = words message
  let timeStamp = read (parts!!0) :: Int
  let warnMsg = unwords (drop 1 parts)
  LogMessage Warning timeStamp warnMsg

parseMessage ('E':' ':message) = do
  let parts = words message
  let errNum = read (parts!!0) :: Int
  let timeStamp = read (parts!!1) :: Int
  let errMsg = unwords (drop 2 parts)
  LogMessage (Error errNum) timeStamp errMsg

parseMessage message = Unknown message

parse :: String -> [LogMessage]
parse log = map parseMessage (lines log)


