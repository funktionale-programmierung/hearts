module M4307546 where

import Gameplay
import Control.Monad.State.Lazy as State
import Data.Set as Set
import Cards
import Game hiding (processGameCommandM, processGameEvent, playerProcessGameEvent)

-- this is a naive shoot the moon strategy, it is not very sophisticated yet
shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick                = playerTrick playerState
      hand                 = playerHand playerState
      firstCard            = leadingCardOfTrick trick
      firstSuit            = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick
    then
      if Set.member (Card Spades Queen) hand -- always tries to get the Queen of Spades by opening a trick with her, if he holds her
        then
          return $ Card Spades Queen
        else
          return (Set.findMax hand) -- changed findMin to findMax to try to get tricks
    else
      case Set.lookupMax followingCardsOnHand of -- changed lookupMin to lookupMax to try to get tricks
        Nothing ->
          return (Set.findMin hand) -- changed findMax to findMin to get rid of low cards
        Just card ->
          return card -- otherwise use the maximal following card

-- exercise 9: it does not CONSISTENTLY win against playAlongStrategy yet, still needs some tweaking...
strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
  playerState <- State.get
  let name                 = self playerState -- extracts his own name in order to compute his own penalty points of his current stack!
      trick                = playerTrick playerState
      hand                 = playerHand playerState
      stack                = playerStack playerState
      history              = playerHistory playerState -- exercise 9: ok these next lines are stupid because we already have the stack, but nevertheless it is a working example:
      ownHistory           = Prelude.filter (\p -> fst p == name) history -- extract all cards that went to this player's stack
      ownStack             = Prelude.map snd ownHistory -- this line and the next two lines just ignore where the cards came from
      ownStack'            = concat ownStack
      ownStack''           = Prelude.map snd ownStack' -- we now have the player's stack reconstructed directly from the history!
      ownPoints            = sum $ Prelude.map penalty ownStack'' -- compute the player's penalty points in his stack
      firstCard            = leadingCardOfTrick trick
      firstSuit            = suit firstCard
      highestFollowingCard = maximum $ Prelude.map snd (Prelude.filter ((== firstSuit) . suit . snd) trick) -- IMPORTANT: highest following card currently in the stack!
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick
    then
      if (Set.member (Card Spades Queen) hand) && (ownPoints > 8)
        then
          return $ Card Spades Queen -- tries to get the Queen of Spades by opening a trick with her, if he holds her and has accumulated at least 9 penalty points already
        else
          return (Set.findMin hand) -- open tricks with low cards
    else
      case Set.lookupMax followingCardsOnHand of
        Nothing ->
          if (Set.member (Card Spades Queen) hand) && (ownPoints <= 8)
            then
              return $ Card Spades Queen -- always tries to get rid of the Queen of Spades, if he holds her and has not accumulated more than 8 penalty points already
            else
              return (Set.findMax hand) -- get rid of high cards when not having to follow suit!
        Just card -> do
          -- IMPORTANT: "Set.lookupLT: Find largest element smaller than the given one." - This is perfect here! Try to play a card as high as possible without getting the trick!
          case Set.lookupLT highestFollowingCard followingCardsOnHand of
            Nothing ->
              return card -- player will likely get this trick anyway, so he plays the highest card
            Just card' -> do
              return card' -- player will not get this trick, so he plays a card that is the highest of the cards that are lower than the currently highest following card!
