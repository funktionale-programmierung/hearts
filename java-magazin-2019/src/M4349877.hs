module M4349877 where

import Gameplay
import Game
import Cards

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

shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      noHeartCards = Set.filter ((== Hearts) . suit) hand
  if trickEmpty trick 
    then -- We have to play the first card
        -- Select maximum card
      return (Set.findMax hand)
    else
        -- Search for maximum cards
      case Set.lookupMax followingCardsOnHand of
        Nothing ->
            case Set.lookupMin noHeartCards of
                Nothing -> return (Set.findMin hand) -- unfortunately we have to play a heart card
                Just card -> return card -- Play a low non-heart card

        Just card ->
          return card           -- otherwise use the maximum following card
          
strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      stack = playerStack playerState
      history = playerHistory playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      noHeartCards = Set.filter ((== Hearts) . suit) hand
      noHeartAndQueenSpadesCards = Set.filter (\ c -> suit c == Hearts || (suit c == Spades && rank c == Queen)) hand
      heartAndQueenSpadesCards = Set.filter (\ c -> not (suit c == Hearts || (suit c == Spades && rank c == Queen))) hand
      ownPenalty = penalty stack -- Our own current penalty
      totalPenalty = sum(map penalty (map snd (concat (map snd history)))) -- Combined penalty so far for all players
      playedCards = map snd (concat (map snd history)) -- Cards that have already been played in the game (not this trick)
      deckLeft = filter (\c -> length (filter (\c2 -> c2 == c) playedCards) == 0) deck -- Cards that are left in the game
      shooting = (ownPenalty) > 0 && totalPenalty == ownPenalty
      queenSpadesPlayed = length (filter (\c -> rank c == Queen && suit c == Spades) playedCards) == 1 -- If queen spades has already been played in a past trick

  -- Update shooting flag
  -- modifyShoot is not really necessary as it is always calculated based on the rest of the state
  if shooting
    then modifyShoots (const True) -- We currently own all penalty so try to shoot for moon
    else modifyShoots (const False) -- We have no penalty or others also have penalty so don't shoot for the moon

  --liftIO $ putStrLn (show deckLeft)

  if shooting
    then
      if trickEmpty trick 
        then -- We have to start the trick
          return (Set.findMax hand) -- Play our highest card
        else
          if length followingCardsOnHand > 0
            then -- We have to follow suit
              return (Set.findMax followingCardsOnHand) -- Use our highest card
            else
            case Set.lookupMin noHeartAndQueenSpadesCards of
                Nothing -> return (Set.findMin hand)
                Just card -> return card
    else
      if trickEmpty trick 
        then -- We have to start the trick
          return (Set.findMin hand) -- Play our lowest card.
        else
          if length followingCardsOnHand > 0
            then -- We have to follow suit.
              if firstSuit /= Hearts && (queenSpadesPlayed || firstSuit /= Spades)
                then return (Set.findMax followingCardsOnHand) -- This likely does not result in points so we can use something high
                else return (Set.findMin followingCardsOnHand) -- This likely (or surely) results in points so we use something low
            else -- We don't have to follow suit.
              case Set.lookupMax heartAndQueenSpadesCards of -- Try to get rid of the highest cards which would give points
                Nothing -> return (Set.findMax hand)
                Just card -> return card
   