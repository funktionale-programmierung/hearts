{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Tournament where

import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import qualified Data.List as List
import Data.List.Split (chunksOf)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import qualified Cards
import qualified Gameplay as G
import qualified Game
import qualified Shuffle

type ScoreInterface m = (MonadIO m, MonadState ScoreMap m)
type ScoreMap = Map Game.PlayerName Int

start :: [G.Player] -> Int -> IO [[(Game.PlayerName, Int)]]
start players nRounds =
  let n = length players
      nPadding | n `mod` 4 == 0 = 0
               | otherwise      = 4 - n `mod` 4
      padding = map (\i -> G.makePlayer ("Robo-" ++ show i) G.playAlongStrategy) [1..nPadding]
      nGroups = (n + 3) `div` 4
      groups = take nGroups $ chunksOf 4 (interleave 3 players padding) -- (players ++ padding) -- bad distribution!
      emptyScore = Map.fromList [(G.playerName player, 0) | player <- take (4*nGroups) (players ++ padding) ]
  in do
    allScores <- State.execStateT (allRounds groups nRounds) emptyScore
    let groupScores = map (List.sortBy (\ (_,x) (_,y) -> compare y x) .
                           map (\player -> let n = G.playerName player in (n, allScores ! n))) groups
    return groupScores

allRounds :: ScoreInterface m => [[G.Player]] -> Int -> m ()
allRounds groups nRounds =
  if nRounds == 0 then
    return ()
  else do
    shuffledCards <- liftIO (Shuffle.shuffleRounds 10 Cards.deck)
    flip mapM_ groups $ \ group -> do
               let perms = List.permutations group in
                 flip mapM_ perms $ \ group_perm -> do
                   score <- liftIO $ G.runGame (return shuffledCards) group_perm
                   State.modify (Map.unionWith (+) score)
    allRounds groups (nRounds - 1)

full :: [G.Player] -> Int -> IO [[(Game.PlayerName, Int)]]
full players nRounds = do
  groupScores <- start players nRounds
  if length groupScores == 1
    then return groupScores
    else do
      putStrLn "Tournament: INTERMEDIATE RESULTS"
      putStrLn "Tournament: ===================="
      mapM_ (putStrLn . ("Tournament: "++) . show) groupScores
      let promotedPlayers = map fst $ filter (not . List.isPrefixOf "Robo-" . fst) $ concatMap (take 2) groupScores
      putStrLn ("Tournament: Promoted players: " ++ show promotedPlayers)
      full (filter (\p -> G.playerName p `elem` promotedPlayers) players) nRounds

interleave :: Int -> [a] -> [a] -> [a]
interleave n xs (y:ys) = let (xn,xs') =  List.splitAt n xs in y: xn ++ interleave n xs' ys
interleave n xs [] = xs
