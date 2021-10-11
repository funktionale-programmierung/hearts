module M5164530 where

import Control.Monad.State.Lazy as State
import qualified Data.Set as Set

import Gameplay
import Cards
import Game hiding (processGameCommandM, processGameEvent, playerProcessGameEvent)




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
          return (Set.findMin hand) -- any card is fine, so try to get rid of high hearts
        Just card ->
          return card           -- otherwise use the minimal following card

strategy :: PlayerStrategy
strategy =
    PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      history = playerHistory playerState
      filtered = filterHistory history
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick 
    then
      return (Set.findMin hand)
    else
        if all ((/= firstSuit) . suit) hand && firstSuit /= Spades && Set.member queenOfSpades hand
             || firstSuit == Spades && Set.member queenOfSpades hand && (kingOfSpades `elem` cardsOfTrick trick || aceOfSpades `elem` cardsOfTrick trick) -- getting rid of the queen of spades if ypu have it
            then return queenOfSpades
            else
                 case Set.lookupMin followingCardsOnHand of
                Nothing ->
                    return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
                Just card ->
                    return card           -- otherwise use the minimal following card

filterHistory [] = []
filterHistory (x:xs) = cardsOfTrick (snd x) : filterHistory xs

queenOfSpades = Card Spades Queen
kingOfSpades = Card Spades King
aceOfSpades = Card Spades Ace
