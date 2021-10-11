module M5154605 where

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
import Data.List
import Data.Maybe

import Gameplay
import Cards
import Game

-- a simple robo player trying to get tricks (and thereby points)
shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick 
    then
      return (Set.findMax hand)
    else
      case Set.lookupMax followingCardsOnHand of
        Nothing ->
          return (Set.findMin hand)
        Just card ->
          return card


-- a strategy that isn't completely braindead
strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick 
    then
      return (bestLeadingCard playerState)
    else
      do if Set.null followingCardsOnHand then
           return (fromJust $ highest hand) -- don't follow suite, throw
         else
           do case highestNonBeating (takingTrick trick) followingCardsOnHand of
                Nothing ->
                  return (bestBeatingCard playerState)
                Just card ->
                  return card
          
queenOfSpades = Card Spades Queen
kingOfSpades  = Card Spades King
aceOfSpades   = Card Spades Ace
          
-- cards left on the hands of other players
cardsLeft :: PlayerState -> [Card]
cardsLeft state = deck \\ (stackCards ++ handCards ++ trickCards)
  where stackCards = concatMap (cardsOfTrick . snd) (playerHistory state)
        trickCards = cardsOfTrick $ playerTrick state
        handCards  = Set.elems $ playerHand state
        
-- True if the queenOfSpades is on enemy hand  
queenOfSpadesLeft :: PlayerState -> Bool
queenOfSpadesLeft state = queenOfSpades `elem` (cardsLeft state)
        
-- given a non-empty trick, finds the card that will currently take it        
takingTrick :: Trick -> Card
takingTrick tr = foldr (\a b -> if cardBeats a b then a else b) x xs
  where (x:xs) = cardsOfTrick tr
  
-- returns the highest Card that won't beat the given card  
highestNonBeating :: (Foldable t) => Card -> t Card -> Maybe Card
highestNonBeating card = foldl helper Nothing
  where helper x y | cardBeats y card    = x
        helper Nothing y                 = Just y
        helper (Just x) y | isHigher x y = Just x
                          | otherwise    = Just y
                 
-- returns the highest Card
highest :: (Foldable t) => t Card -> Maybe Card
highest = foldl helper Nothing
  where helper Nothing y                 = Just y
        helper (Just x) y | isHigher x y = Just x
                          | otherwise    = Just y

-- returns the lowest Card
lowest :: (Foldable t) => t Card -> Maybe Card
lowest = foldl helper Nothing
  where helper Nothing y                 = Just y
        helper (Just x) y | isHigher x y = Just y
                          | otherwise    = Just x
                          
-- If we can not go lower but must follow, what is the best choice?
-- If there is a penalty, go low to try to avoid.
-- Otherwise, we can go high and just take an empty trick.
-- If we are in last position, go highest (but not queenOfSpades...)                          
bestBeatingCard :: PlayerState -> Card
bestBeatingCard state = if length followingCardsOnHand == 1 then head followingCardsOnHand else best
  where hand = playerHand state
        trick = playerTrick state
        firstCard = leadingCardOfTrick trick
        trickCards = cardsOfTrick trick
        firstSuit = suit firstCard
        followingCardsOnHand = Set.elems $ Set.filter ((== firstSuit) . suit) hand
        followingNoQueen = followingCardsOnHand \\ [queenOfSpades] -- never beat with queenOfSpades, we're not shooting for the moon
        followingAvoiding | queenOfSpadesLeft state = followingNoQueen \\ [kingOfSpades, aceOfSpades]
                          | otherwise = followingNoQueen
        best | trickSize trick == 3 = fromJust $ highest followingNoQueen
             | (penalty trickCards > 0) && (length followingAvoiding > 0) = fromJust $ lowest followingAvoiding
             | penalty trickCards > 0 = fromJust $ lowest followingNoQueen
             | length followingAvoiding > 0 = fromJust $ highest followingAvoiding
             | otherwise = fromJust $ highest followingNoQueen
        
-- The best card to lead with
-- Try to have as few as possible cards of the same suite which are lower (but at least one that is higher),
-- if undecided choose the one with more higher cards of the same suite.
-- If there is no card on our hand for which there is a higher card left of the same suite, we'll take the rest either way.
bestLeadingCard :: PlayerState -> Card
bestLeadingCard state = fst best
  where (c:cs) = Set.elems $ playerHand state
        left = cardsLeft state
        best = foldl helper (c, cardsAB c left) cs
        helper (bst, (a, b)) card = let (a', b') = cardsAB card left in if (a' > 0) && ((b' < b) || ((b' == b) && (a' > a))) then (card, (a', b')) else (bst, (a, b))

-- how many cards of the same suite which are higher?        
cardsAbove :: Card -> [Card] -> Int
cardsAbove card = length . (filter (\x -> cardBeats x card))

-- how many cards of the same suite which are lower?        
cardsBelow :: Card -> [Card] -> Int
cardsBelow card = length . (filter (\x -> cardBeats card x))

-- how many cards of the same suite which are higher / lower?    
cardsAB :: Card -> [Card] -> (Int, Int)
cardsAB c cs = (cardsAbove c cs, cardsBelow c cs)
  
-- a custom ordering of cards
isHigher :: Card -> Card -> Bool
isHigher (Card Spades Queen) _ = True
isHigher _ (Card Spades Queen) = False
isHigher (Card Hearts x) (Card Hearts y) = x > y
isHigher (Card Hearts x) _ = True
isHigher _ (Card Hearts x) = False
isHigher (Card Spades Ace) _ = True
isHigher _ (Card Spades Ace) = False
isHigher (Card Spades King) _ = True
isHigher _ (Card Spades King) = False
isHigher (Card _ x) (Card _ y) = x > y
