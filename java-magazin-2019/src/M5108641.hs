module M5108641 where

import Control.Monad.State.Lazy as State
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (group,sort)

--import qualified Data.List as List

import Gameplay
import Cards
import Game

--does a list of cards contain a high value card?
cardsContainsValueCard :: [Card] -> Bool
cardsContainsValueCard = foldr ((||) . isHighValueCard) False

-- is a card of high value (i. e. being hearts or Queen of Spades)
isHighValueCard :: Card -> Bool
isHighValueCard card =
    case card  of
            (Card Hearts _)     -> True
            (Card Spades Queen) -> True
            (Card _ _ ) -> False


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
        return (Set.findMax hand) -- if we lead we play our highest card.
    else
      case null followingCardsOnHand of
        False -> if cardsContainsValueCard $ map snd trick
                    then return $ Set.findMax followingCardsOnHand -- we want to get tricks with value cards
                 else return $ Set.findMin followingCardsOnHand
        True -> case Set.lookupMin (Set.filter (not .isHighValueCard) hand) of
                 -- if we cannot follow suit we want to discard the lowest non value card, if we can. 
                  Just card -> return card
                  Nothing -> return $ head $ Set.toList hand -- if we cannot do that, we have to discard a value card (meaning that we lost)

strategy  :: PlayerStrategy
strategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      history = playerHistory playerState
  if trickEmpty trick
    then
      return (Set.findMin hand)
    else
      case Set.lookupMin followingCardsOnHand of
        Nothing -> -- find a good card to discard here. 
          return $ findCardToDiscard (Set.toList hand) history
        Just card ->
          return card   -- otherwise use the minimal following card 

findCardToDiscard :: [Card] -> PlayerHistory -> Card
findCardToDiscard cards history =
    if Card Spades Queen `elem` cards -- the ace of spaces should be discarded with highest priority.
        then Card Spades Queen
    else -- otherwise we should discard a card of a suit, that has been played a lot. 
        let playedCards = concatMap (map snd . snd) history
            suitDistribution = getSuitDistribution playedCards
            mostPlayedSuits = maxOfList suitDistribution
            cardsOfMostPlayedSuits =
                filter (\card -> suit card `elem` mostPlayedSuits) cards
        in case cardsOfMostPlayedSuits of
            []     ->  head  cards--just return a random card
            (x:xs) ->  x -- return a card of the suit thats valid to discard

-- get a map from suit to Int telling how many times each suit appeared on the game. 
getSuitDistribution :: [Card] -> [(Suit,Int)]
getSuitDistribution [] = []
getSuitDistribution xs = map (\x -> (head x, length x)) . group . sort $ map suit xs
--inspired by following ressource: https://www.oreilly.com/library/view/haskell-data-analysis/9781783286331/ch02s09.html


-- returns the keys of a map/(tupel list)  that have maximum value!
maxOfList :: Ord b => [(a,b)] -> [a]
maxOfList [] = []
maxOfList xs = map fst $ filter ((== (maximum $ map snd xs)) . snd) xs

