module LogAnalysis (
  parseMessage, parse, insert
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


-- Exercise 2
-- Define a function that inserts a new 
-- LogMessage into an existing MessageTree

instance Ord LogMessage where
  (LogMessage _ t1 _) `compare` (LogMessage _ t2 _) = t1 `compare` t2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown newmsg) tree = tree
insert newmsg Leaf = Node Leaf newmsg Leaf
insert newmsg (Node left msg right) 
  | newmsg < msg  = insert newmsg left
  | otherwise       = insert newmsg right
