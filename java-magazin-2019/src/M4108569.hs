module M4108569 where

import Control.Conditional (ifM)
import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import Control.Monad as Monad

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Function (on)

import Debug.Trace (trace, traceShowId, traceIO, traceM)

import Cards
import Game hiding (processGameEvent)
import qualified Shuffle

import Gameplay


shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      legalHand = Set.filter (\card -> legalCard hand trick card) hand
      trickContainsHeart = length (filter (==Hearts) (map (suit.snd) trick)) /= 0
      trickContainsQoS = length (filter (==Card Spades Queen) (map snd trick)) /= 0
      handContainsHeart = length (Set.filter (==Hearts) (Set.map suit legalHand)) /= 0
      handContainsQoS = length (Set.filter (==Card Spades Queen) legalHand) /= 0
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      heartCardsOnHand = Set.filter ((== Hearts) . suit) hand
      otherCardsOnHand = Set.filter ((/= Hearts) . suit) hand
      otherLegalCardsOnHand = Set.filter ((/= Hearts) . suit) legalHand
      myhand = Set.elems legalHand
      ncards = Set.size legalHand
      totalHand = Set.elems hand
  -- liftIO $ putStrLn "Mike is trying to shoot the moon"
  -- liftIO $ putStrLn ("Mikes Cards: " ++ (show (Set.toList hand)))
  -- liftIO $ putStrLn ("Hearts in trick: " ++ (show trickContainsHeart))
  -- liftIO $ putStrLn ("QoS in trick: " ++ (show trickContainsQoS))
  if trickEmpty trick
    then
      if handContainsQoS then return (Card Spades Queen) else
          if length otherCardsOnHand /= 0 then return (Set.findMin otherCardsOnHand) else return (Set.findMax heartCardsOnHand)
    else
      if (trickContainsHeart || trickContainsQoS)
          then 
              if length followingCardsOnHand == 0 then return (Set.findMin legalHand) else return (Set.findMax followingCardsOnHand) 
          else
              if length otherLegalCardsOnHand /= 0 then return (Set.findMin otherLegalCardsOnHand) else return (Set.findMin legalHand)


penaltyHistory :: PlayerHistory -> [(PlayerName, Int)]
penaltyHistory [] = []
penaltyHistory (x:xs) = ((fst x), penalty (map snd (snd x))):penaltyHistory xs

-- Returns all previously played cards
allHistoryCards :: PlayerHistory -> [Card]
allHistoryCards [] = []
allHistoryCards (x:xs) = (map snd (snd x)) ++ allHistoryCards xs

-- Tallies up all penalty scores as of now
tally :: PlayerHistory -> Map PlayerName Int
tally history = tallyI (penaltyHistory history) Map.empty
    where
        tallyI :: [(PlayerName, Int)] -> Map PlayerName Int -> Map PlayerName Int
        tallyI [] tmap = tmap
        tallyI (x:xs) tmap = tallyI xs (Map.insertWith (+) (fst x) (snd x) tmap)

data Mood = Normal | Shooting | OtherShooting
    deriving (Read, Show, Eq)

{-
playerMood :: [Card] -> PlayerHistory -> Map PlayerName Int -> Mood
playerMood stack hist hmap | length (Map.filter (/=0) hmap) >= 2 = Normal
                           | length (Map.filter (>=13) hmap) == 1 && length (allHistoryCards hist) == length stack = Shooting
                           | length (Map.filter (>=20) hmap) == 1 && length (allHistoryCards hist) /= length stack = OtherShooting
                           | otherwise = Normal
-}

-- Calculates the players mood depending on the game history
playerMood :: [Card] -> PlayerHistory -> Map PlayerName Int -> Mood
playerMood stack hist hmap | length (Map.filter (/=0) hmap) >= 2 = Normal
                           | length (Map.filter (/=0) hmap) == 1 && length (Map.filter (>=12) hmap) == 1 && length (allHistoryCards hist) == length stack = Shooting
                           | length (Map.filter (/=0) hmap) == 1 && length (Map.filter (>=14) hmap) == 1 && length (allHistoryCards hist) /= length stack = OtherShooting
                           | otherwise = Normal

-- Calculates the cards that are still in the game
remainingPlayableCards :: [Card] -> [Card] -> [Card]
remainingPlayableCards ownHand cardHistory = filter (\x -> (notElem x ownHand) && (notElem x cardHistory)) deck

-- Checks whether a card is beatable for going out
cardIsBeatable :: Card -> [Card] -> Bool
cardIsBeatable card remaining = length (filter (\x -> cardBeats x card) remaining) /= 0

-- If we are playing normally we always wand to throw out the highest card which is still a loss
highestLoosingCard :: Card -> [Card] -> Maybe Card
highestLoosingCard card cards = highestLoosingCardI card (List.sort cards)
    where
        highestLoosingCardI card [] = Nothing
        highestLoosingCardI card (x:xs) | not (cardBeats x card) = Just x
                                        | otherwise = highestLoosingCardI card xs

rarestSuitI :: [Card] -> Map Suit Int -> Map Suit Int
rarestSuitI [] smap = smap
rarestSuitI (x:xs) smap = rarestSuitI xs (Map.insertWith (+) (suit x) 1 smap)

-- rarestExistingSuit :: [Card] -> Suit
-- rarestExistingSuit cards = fst (head(List.sortBy (compare `on` snd) Map.toList((rarestSuitI cards Map.empty))))

rarestExistingSuit :: [Card] -> Suit
rarestExistingSuit cards = fst (head (List.sortBy (compare `on` snd) (Map.toList((rarestSuitI cards Map.empty)))))

-- Gives the current hand, with only the rarest Suit remaining
handOfRarestExistingSuit :: [Card] -> Set.Set Card -> Set.Set Card
handOfRarestExistingSuit hand cards = Set.filter (\x -> suit x == (rarestExistingSuit hand)) cards



strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      stack = playerStack playerState
      history = playerHistory playerState
      -- Get preliminary score of all players
      record = tally history
      mood = playerMood stack history record -- Try to derive a appropriate attack plan
      remaining = remainingPlayableCards (Set.elems hand) (allHistoryCards history)  -- Remaining cards in the game
      legalHand = Set.filter (\card -> legalCard hand trick card) hand
      trickContainsHeart = length (filter (==Hearts) (map (suit.snd) trick)) /= 0
      highestHeartInTrick = Set.findMax (Set.fromList (filter (\x -> suit x == Hearts) (map snd trick)))
      trickContainsQoS = length (filter (==Card Spades Queen) (map snd trick)) /= 0
      legalHandContainsHeart = length (Set.filter (==Hearts) (Set.map suit legalHand)) /= 0
      legalHandContainsQoS = length (Set.filter (==Card Spades Queen) legalHand) /= 0
      legalHandContainsOtherCards = length (Set.filter (/=Hearts) (Set.map suit legalHand)) /= 0
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      heartCardsOnHand = Set.filter ((== Hearts) . suit) hand
      legalHeartCardsOnHand = Set.filter ((== Hearts) . suit) legalHand
      otherCardsOnHand = Set.filter ((/= Hearts) . suit) hand
      otherLegalCardsOnHand = Set.filter ((/= Hearts) . suit) legalHand
      myhand = Set.elems legalHand
      ncards = Set.size legalHand
      totalHand = Set.elems hand
  -- liftIO $ putStrLn "Mike is trying to shoot the moon"
  -- liftIO $ putStrLn ("Strategy Cards: " ++ (show (Set.toList hand)))
  -- liftIO $ putStrLn ("Hearts in trick: " ++ (show trickContainsHeart))
  -- liftIO $ putStrLn ("QoS in trick: " ++ (show trickContainsQoS))
  -- liftIO $ putStrLn ("Mood: " ++ (show mood))

  if mood == Normal 
      then
          if trickEmpty trick  -- New Trick
              then
                  if legalHandContainsHeart  -- If There are heats on hand
                      then
                          if cardIsBeatable (Set.findMin legalHeartCardsOnHand) remaining  -- Try to play save heart card or lowest card
                              then
                                  return (Set.findMin legalHeartCardsOnHand)
                              else
                                  return (Set.findMin legalHand)
                      else
                          return (Set.findMin (handOfRarestExistingSuit myhand legalHand))  -- If heart isnt playable play generic low card, prefer rarest available suit
              else
                  if firstSuit == Hearts
                      then
                          if legalHandContainsHeart 
                              then
                                  case highestLoosingCard highestHeartInTrick (Set.elems legalHeartCardsOnHand) of  -- Play highest losing heart if there is such thing on hand
                                      Just card -> return card
                                      Nothing -> if legalHandContainsOtherCards then return (Set.findMax (handOfRarestExistingSuit (Set.elems otherLegalCardsOnHand) otherLegalCardsOnHand)) else return (Set.findMax (handOfRarestExistingSuit myhand legalHand))
                              else
                                  if legalHandContainsQoS
                                      then
                                          return (Card Spades Queen)  -- Try to play QoS if player has no heart
                                      else
                                          return (Set.findMax (handOfRarestExistingSuit myhand legalHand)) -- Prefer rarest available suit
                      else
                          if legalHandContainsQoS && firstSuit /= Spades
                              then
                                  return (Card Spades Queen)  -- Try to play QoS if Spades isnt first suit
                              else 
                                  if legalHandContainsHeart
                                      then
                                          return (Set.findMax legalHeartCardsOnHand)  -- Otherwise get rid if heart 
                                      else
                                          if trickContainsHeart
                                              then
                                                  return (Set.findMin (handOfRarestExistingSuit myhand legalHand)) -- Prefering rarest available suit
                                              else
                                                  return (Set.findMax (handOfRarestExistingSuit myhand legalHand))   -- Or get rid of other things
      else
          if trickEmpty trick
              then
                  if legalHandContainsQoS
                      then
                          return (Card Spades Queen)  -- When trying to shoot for the moon, I simply try to get the QoS through by bute force
                      else
                          if length otherCardsOnHand /= 0  -- Otherwise go out with the card which is most likely to win
                              then
                                  return (Set.findMax otherCardsOnHand)
                              else 
                                  return (Set.findMax heartCardsOnHand) -- Try to preserve hearts in case someone plays a heart later
          else
              if (trickContainsHeart || trickContainsQoS)
                  then                                                --  \/ if thats the case we sadly lost
                      if length followingCardsOnHand == 0 then return (Set.findMin legalHand) else return (Set.findMax followingCardsOnHand)  -- Try winning Hearts and QoS
                  else
                      if length otherLegalCardsOnHand /= 0 then return (Set.findMin otherLegalCardsOnHand) else return (Set.findMin legalHand)  -- Avoid winning other things

