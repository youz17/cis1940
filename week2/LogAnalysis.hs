{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.Function ((&))
import Log (LogMessage (..), MessageTree (..), MessageType (..))

splitByFirst :: Char -> String -> (String, String)
splitByFirst _ [] = ([], [])
splitByFirst c (cc : s) =
  if c == cc
    then ([], s)
    else
      let (f, b) = splitByFirst c s
       in (cc : f, b)

parseMessageType :: String -> Maybe (MessageType, String)
parseMessageType msg =
  let (msgType, msg1) = splitByFirst ' ' msg
   in case msgType of
        "E" ->
          let (errorLevel, msg2) = splitByFirst ' ' msg1
           in Just (Error (read errorLevel), msg2)
        "W" -> Just (Warning, msg1)
        "I" -> Just (Info, msg1)
        _ -> Nothing

parseMessage :: String -> LogMessage
parseMessage msg =
  case parseMessageType msg of
    Just (msgType, msg1) ->
      let (time, trueMsg) = splitByFirst ' ' msg1
       in LogMessage msgType (read time) trueMsg
    Nothing -> Unknown msg

parse :: String -> [LogMessage]
parse logs = case splitByFirst '\n' logs of
  ([], _) -> []
  (l, rest) -> parseMessage l : parse rest

parse' :: String -> [LogMessage]
parse' logs = logs & lines & map parseMessage

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage {}) Leaf = Node Leaf msg Leaf
insert (Unknown {}) node = node
insert msg@(LogMessage _ msgTime _) (Node l nodeMsg@(LogMessage _ nodeTime _) r) =
  if msgTime > nodeTime
    then Node l nodeMsg (insert msg r)
    else Node (insert msg l) nodeMsg r
insert _ (Node _ (Unknown _) _) = error "Unknow node should not in MessageTree"

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs =
  logs
    & filter isRelevant
    & build
    & inOrder
    & map getMessage
  where
    isRelevant =
      \case
        (LogMessage (Error n) _ _) -> n > 50
        _ -> False
    getMessage =
      \case
        (LogMessage _ _ s) -> s
        _ -> error ""
