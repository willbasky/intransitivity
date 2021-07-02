{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Intransitivity
    ( intransitive
    ) where

import System.Random
import Control.Concurrent.Async (replicateConcurrently_)
import Data.List (sortOn, uncons)
import Control.Concurrent.STM
import System.Environment (getArgs)

data Element
  = Rock
  | Scizzers
  | Paper
  deriving stock (Eq, Show, Enum)

data Statistics = Statistics
  { rock :: !Int
  , scizzers :: !Int
  , paper :: !Int
  }
  deriving stock (Eq, Show)

defaultStatistics :: Statistics
defaultStatistics = Statistics 0 0 0

fight :: Element -> Element -> Maybe Element
fight e1 e2 = case (e1, e2) of
  (Rock, Scizzers) -> Just Rock
  (Scizzers, Rock) -> Just Rock
  (Scizzers, Paper) -> Just Scizzers
  (Paper, Scizzers) -> Just Scizzers
  (Paper, Rock) -> Just Paper
  (Rock, Paper) -> Just Paper
  _ -> Nothing

countStatistic :: Statistics -> Maybe Element -> Statistics
countStatistic stat Nothing = stat
countStatistic stat (Just el) = case el of
  Rock -> stat{rock + 1}
  Scizzers -> stat{scizzers + 1}
  Paper -> stat{paper + 1}

randomizer :: IO (Element, Element)
randomizer = do
 r1 <- toEnum <$> getStdRandom (randomR (0,2))
 r2 <- toEnum <$> getStdRandom (randomR (0,2))
 pure (r1, r2)

handleStatistics :: TMVar Statistics -> IO ()
handleStatistics mvStat = do
  (r1, r2) <- randomizer
  atomically $ do
    stat <- takeTMVar mvStat
    let statNew = countStatistic stat $ fight r1 r2
    putTMVar mvStat statNew

supreme :: Statistics -> Element
supreme stat = fst $ last $ sortOn snd
  [(Rock, stat.rock), (Scizzers, stat.scizzers), (Paper, stat.paper)]

intransitive :: IO ()
intransitive = do
  arg <- getArgs
  let num = maybe 10000 (read . fst) $ uncons arg
  putStrLn $ "Thread number: " <> show num
  mvStat <- newTMVarIO defaultStatistics
  replicateConcurrently_ num $ handleStatistics mvStat
  res <- atomically $ readTMVar mvStat
  print $ supreme res
  print res

