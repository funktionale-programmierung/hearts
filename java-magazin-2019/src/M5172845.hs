module M5172845 where

import Cards
import Control.Monad
import Control.Monad.State.Lazy as State
import qualified Data.Set as Set
import Game
import Gameplay

contains :: Card -> [Card] -> Bool
contains card (x : xs) =
  if x == card
    then True
    else contains card xs
contains _ [] = False

containsSpadesQueen xs = contains (Card Spades Queen) xs

getHighestCard :: [Card] -> Card
getHighestCard xs = foldr beats (xs !! 0) xs

beats :: Card -> Card -> Card
beats c1 c2 = if cardBeats c1 c2 then c1 else c2

filterBeats :: [Card] -> Card -> [Card]
filterBeats hand card = filter (\x -> cardBeats x card) hand

isHearts :: Card -> Bool
isHearts (Card Hearts _) = True
isHearts _ = False

isClubs :: Card -> Bool
isClubs (Card Clubs _) = True
isClubs _ = False

isDiamonds :: Card -> Bool
isDiamonds (Card Diamonds _) = True
isDiamonds _ = False

isSpades :: Card -> Bool
isSpades (Card Spades _) = True
isSpades _ = False

getCards :: (Card -> Bool) -> (Set.Set Card) -> (Set.Set Card)
getCards pred cards = Set.filter pred cards

isVoidPred :: (Card -> Bool) -> (Set.Set Card) -> PlayerHistory -> Bool
isVoidPred pred hand history =
  let historyCards = Set.map snd $ Set.fromList (concat (map snd history))
      -- cards = Set.union (getCards pred hand) (getCards pred historyCards)
      -- in length cards == 13
      cardsInHand = (getCards pred hand)
      cardsInHistory = (getCards pred historyCards)
   in length cardsInHand + length cardsInHistory == 13

isVoid :: Card -> (Set.Set Card) -> PlayerHistory -> Bool
isVoid (Card Clubs _) hand history = isVoidPred isClubs hand history
isVoid (Card Hearts _) hand history = isVoidPred isHearts hand history
isVoid (Card Spades _) hand history = isVoidPred isSpades hand history
isVoid (Card Diamonds _) hand history = isVoidPred isDiamonds hand history

getLegalCards :: (Set.Set Card) -> Trick -> (Set.Set Card)
getLegalCards hand trick = Set.filter (\x -> legalCard hand trick x) hand

shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
    playerState <- State.get
    let trick = playerTrick playerState
        hand = playerHand playerState
        stack = playerHand playerState
        legalCards = Set.filter (\x -> legalCard hand trick x) hand
        legalCardsBeats =
          -- legal cards that beat cards in the current trick
          if length trick == 0
            then Set.empty
            else
              Set.fromList $
                filterBeats
                  (Set.toList legalCards)
                  ( getHighestCard $
                      cardsOfTrick trick
                  )
    if (penalty $ cardsOfTrick trick) > 0 -- If there is score
      then
        if length legalCardsBeats > 0 -- Check if we can beat the current trick
          then return (Set.findMin legalCardsBeats) -- Try to shoot the moon
          else return (Set.findMax legalCards)
      else return (Set.findMin legalCards)

{-
Tried to implement some of the strategies at
https://www.thesprucecrafts.com/hearts-card-game-strategy-and-tips-411726
https://mobilityware.helpshift.com/a/hearts-card-game/?s=how-to-play-settings&f=advanced-strategy
-}
strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
    playerState <- State.get
    let trick = playerTrick playerState
        hand = playerHand playerState
        stack = playerHand playerState
        history = playerHistory playerState
        legalCards = getLegalCards hand trick
        -- Check history to determine non void cards so
        -- that other players can't dump their poor cards
        nonVoidLegalCards =
          Set.filter
            (not . (\x -> isVoid x hand history))
            legalCards
        canQueen = containsSpadesQueen $ Set.toList legalCards
        hasAceofClubs = contains (Card Clubs Ace) $ Set.toList hand
        trickPenalty = penalty $ cardsOfTrick trick
        legalCardsWithoutQueen =
          Set.filter
            (\x -> x /= Card Spades Queen)
            legalCards
        zeroCards = getCards (not . isHearts) legalCardsWithoutQueen
        hearts = getCards isHearts legalCards
        clubsAndDiamonds =
          Set.intersection
            ( getCards
                (\x -> isClubs x || isDiamonds x)
                legalCards
            )
            nonVoidLegalCards

    if (length legalCards == 1) -- Automatically play only legal card
      then return (Set.elemAt 0 legalCards)
      else
        if length trick == 3 -- Last Player
          then
            if hasAceofClubs -- Win first round if total penalty is 0
              && length history == 0 -- First round
              && trickPenalty == 0 -- Zero penalty
              then return (Card Clubs Ace)
              else
                if canQueen
                  && cardBeats -- Queen of Spades is not the highest
                    (getHighestCard $ cardsOfTrick trick)
                    (Card Spades Queen)
                  then -- Get rid of Queen of Spades
                    return (Card Spades Queen)
                  else
                    if length zeroCards > 0
                      then return (Set.findMin zeroCards)
                      else
                        if length nonVoidLegalCards > 0
                          then return (Set.findMin nonVoidLegalCards)
                          else return (Set.findMin legalCards)
          else -- Not the last player

            if length clubsAndDiamonds > 0
              then -- Get rid of clubs and diamonds
                return (Set.findMin clubsAndDiamonds)
              else
                if length nonVoidLegalCards > 0
                  then return (Set.findMin nonVoidLegalCards)
                  else return (Set.findMin legalCards)
