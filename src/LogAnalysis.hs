module LogAnalysis (
  parseMessage, parse
  ) where

import Log

-- Exercise 1 
-- parse an individual message, then a whole log file

parseMessageWithType :: MessageType -> [String] -> LogMessage
parseMessageWithType flavor parts = do
  let timeStamp = read (parts!!0) :: Int
  let message = unwords (drop 1 parts)
  LogMessage flavor timeStamp message

parseMessage :: String -> LogMessage

parseMessage ('I':' ':message) = do
  parseMessageWithType Info (words message)

parseMessage ('W':' ':message) = do
  parseMessageWithType  Warning (words message)

parseMessage ('E':' ':message) = do
  let parts = words message
  let errNum = read (parts!!0) :: Int
  parseMessageWithType (Error errNum) (drop 1 parts)

parseMessage message = Unknown message

parse :: String -> [LogMessage]
parse log = map parseMessage (lines log)


