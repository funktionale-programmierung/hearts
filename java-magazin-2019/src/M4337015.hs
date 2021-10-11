{-# LANGUAGE RankNTypes #-}
module M4337015 where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Control.Monad.State.Lazy as State

import Game
import Cards
import Gameplay


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
      if (penalty (cardsOfTrick trick) > 0)
        && (suit (Set.findMax hand) == firstSuit) then
        return (Set.findMax hand)
      else
        case Set.lookupMax followingCardsOnHand of
        Nothing ->
          return (Set.findMin hand)
        Just card ->
          return card


strategy :: PlayerStrategy
strategy = M4337015.shootTheMoonStrategy
