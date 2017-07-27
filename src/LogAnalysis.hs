module LogAnalysis (
  parseMessage, parse, insert, build, inOrder, whatWentWrong
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
  | newmsg < msg   = Node (insert newmsg left) msg right
  | otherwise      = Node left msg (insert newmsg right)


-- Exercise 3
-- Define a functions which builds up a MessageTree
-- containing the messages in the given list

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg:others) = insert msg (build others)


-- Exercise 4
-- Define a function which takes a sorted MessageTree
-- and produces a list of all the LogMessages it contains,
-- sorted by timestamp (ascending)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ msg : (inOrder right)


-- Exercise 5
-- Define a function which takes an unsorted list of LogMessages,
-- and returns a sorted list of any errores with serverity >= 50

filterBadErrs :: [LogMessage] -> [LogMessage]
filterBadErrs [] = []
filterBadErrs ( (LogMessage (Error severity) time msg) : [])
  | severity >= 50 = [LogMessage (Error severity) time msg]
  | otherwise      = []
filterBadErrs ( _:[] ) = []
filterBadErrs ( msg : others ) = (filterBadErrs [msg]) ++ (filterBadErrs others)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (messages) = map show (filterBadErrs (inOrder (build messages)))
