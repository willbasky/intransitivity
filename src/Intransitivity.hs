{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Intransitivity
    ( intransitive
    ) where

import System.Random
import Control.Concurrent.Async (replicateConcurrently_)
import Data.List (sortOn, uncons)
import Control.Concurrent.STM
import System.Environment (getArgs)

infix  4 .<, .<=, .>=, .>
class (Eq a, Enum a, Bounded a) => IntransitiveDominance a where
    superCompare             :: a -> a -> Ordering
    (.<), (.<=), (.>), (.>=) :: a -> a -> Bool
    superMax, superMin       :: a -> a -> a

    superCompare x y = case (x == minBound, x == maxBound, y == minBound, y == maxBound) of
      (True,_,_,True) -> GT
      (_,True,True,_) -> LT
      _ ->  if x == y then EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else if x .<= y then LT else GT


    x .<  y = case superCompare x y of { LT -> True;  _ -> False }
    x .<= y = case superCompare x y of { GT -> False; _ -> True }
    x .>  y = case superCompare x y of { GT -> True;  _ -> False }
    x .>= y = case superCompare x y of { LT -> False; _ -> True }

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    superMax x y = if x .<= y then y else x
    superMin x y = if x .<= y then x else y
    {-# MINIMAL superCompare | (.<=) #-}

data Element
  = Paper
  | Scizzers
  | Rock
  deriving stock (Eq, Show, Enum, Bounded)

instance IntransitiveDominance Element where
  superCompare e1 e2 =
    case (e1 == minBound, e1 == maxBound, e2 == minBound, e2 == maxBound) of
      (True,_,_,True) -> GT
      (_,True,True,_) -> LT
      _ -> compare (fromEnum e1) (fromEnum e2)
  (.<=) e1 e2 =
    case (e1 == minBound, e1 == maxBound, e2 == minBound, e2 == maxBound) of
      (True,_,_,True) -> False
      (_,True,True,_) -> True
      _ -> (fromEnum e1) <= (fromEnum e2)

data Statistics = Statistics
  { rock :: !Int
  , scizzers :: !Int
  , paper :: !Int
  }
  deriving stock (Eq, Show)

defaultStatistics :: Statistics
defaultStatistics = Statistics 0 0 0

fight :: Element -> Element -> Maybe Element
fight e1 e2
  | e1 == e2 = Nothing
  | e1 .< e2 = Just e2
  | otherwise = Just e1

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
  let numT = show num
  putStrLn $ "Thread number: " <> numT
  mvStat <- newTMVarIO defaultStatistics
  putStrLn $ "First running of " <> numT <> " fights."
  replicateConcurrently_ num $ handleStatistics mvStat
  putStrLn $ "Second running of " <> numT <> " fights."
  replicateConcurrently_ num $ handleStatistics mvStat
  putStrLn $ "Third running of " <> numT <> " fights."
  replicateConcurrently_ num $ handleStatistics mvStat
  res <- atomically $ readTMVar mvStat
  print $ supreme res
  print res

