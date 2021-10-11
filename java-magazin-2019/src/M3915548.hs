module M3915548 where

import Cards
import Game hiding (processGameCommandM, processGameEvent)
import Gameplay
import qualified Data.Set as Set
import Control.Monad.State.Lazy as State
import Debug.Trace (trace, traceShowId, traceIO, traceM)

-- shoot the moon strategy
-- the strategy is kept very simple like instructed
-- strategy summarised: Try to play biggest card of leading suit in tricks
-- -> get more hearts and the spade queen on average
shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick -- if the trick is empty, play the maximum ranked card of one of his suits:
                      -- If I'm not mistaken it doesn't matter if he plays Queen of Diamonds
                      -- or 2 of Hearts if they're both maximum of the corresponding suit
    then
      return (Set.findMax hand)
    else
      case Set.lookupMax followingCardsOnHand of
        Nothing ->    -- if we don't have a card matching the leading-suit we discard a minimum ranked card
                      -- since we can't win the trick anymore.
          return (Set.findMin hand)
        Just card -> -- the below if - expression checks if our best card beats every right-suited card in the trick
            if (and ( map (cardBeats card) [x | x <- (cardsOfTrick trick), suit x == firstSuit]))
              then -- card beats the trick
                  return card
              else -- card can't win the trick -> discard minimum card instead
                  case Set.lookupMin followingCardsOnHand of
                    Nothing ->  -- dead code but needed to extract Just value below
                        return (Set.findMin hand)
                    Just minCard ->
                        return minCard  -- if our card can't win the trick we discard the minimum instead 


-- strategy using history and better baseline play
strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      history = playerHistory playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      myCurrentPenalties = sum ([ (penalty (cardsOfTrick (snd x))) | x <- history, fst x == "myself"])
      othersCurrentPenalties = sum ([ (penalty (cardsOfTrick (snd x))) | x <- history, fst x /= "myself"])
      -- cards in hand which match leading-card suit
      handList = Set.toList followingCardsOnHand
      -- cards in stack which match leading-card suit
      relevantCardsOfTrick = [x | x <- (cardsOfTrick trick), suit x == firstSuit]
      -- all cards in playerhand (with leadingcard-suit) which lose to highest card (with leadingcard-suit) in stack
      cardsLosingToStack = [ c | c <- handList, not (and ( map (cardBeats c) relevantCardsOfTrick))]

  if (not( myCurrentPenalties >= 14 && othersCurrentPenalties == 0))  -- use normal strategy
    then
      if trickEmpty trick 
        then
          return (Set.findMin hand)
        else
          -- ((length handList) == 0) -> if True there are no suit-matching cards in hand
          case ((length handList) == 0) of
            True ->
              return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
            False ->
              if ((length cardsLosingToStack) == 0)
                  -- every suit-matching card wins the stack -> pick the highest one so it's gone
                  then return (maximum handList)
                  else
                  -- play the highest card which still is lower than the highest in the stack
                      return (maximum cardsLosingToStack)

    -- the logic below is copy-pasted from shootTheMoonStrategy
    -- there it is also commented and explained
    else                                                             -- use to the moon strategy
      if trickEmpty trick
      then
        return (Set.findMax hand)
      else
        case Set.lookupMax followingCardsOnHand of
          Nothing ->
            return (Set.findMin hand)
          Just card ->
              if (and ( map (cardBeats card) [x | x <- (cardsOfTrick trick), suit x == firstSuit]))
                then
                    return card
                else
                    case Set.lookupMin followingCardsOnHand of
                      Nothing ->
                          return (Set.findMin hand)
                      Just minCard ->
                          return minCard