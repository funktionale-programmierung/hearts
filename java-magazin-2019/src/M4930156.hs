module M4930156 where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Gameplay
import Cards
import Control.Monad.State.Lazy as State
import Game hiding (processGameCommandM, processGameEvent, playerProcessGameEvent)

shootTheMoonStrategy :: Gameplay.PlayerStrategy
shootTheMoonStrategy =
   Gameplay.PlayerStrategy $ do
   playerState <- State.get
   let trick = playerTrick playerState
       hand = playerHand playerState
       firstCard = leadingCardOfTrick trick
       firstSuit = suit firstCard
       followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
       heartCards = List.filter ((== Hearts) . suit . snd) trick
       queenOfSpades = List.filter ((== Card Spades Queen) . snd ) trick
   -- check for empty trick otherwise firstCard and all derived value will not be defined
   if trickEmpty trick
      then
         return (Set.findMin hand)
      else
         if (((List.length heartCards/=0) || (List.length queenOfSpades/=0))&& (not (trickEmpty trick)))
            then
               case Set.lookupMax followingCardsOnHand of
                  Nothing -> return (Set.findMin hand) -- cant win - no need to waste high cards
                  Just card -> return card -- reply with highest card possible
            else
               -- TODO: only play non point cards
               case Set.lookupMin followingCardsOnHand of -- if no points are in the trick play a low card
                  Nothing -> return (Set.findMin hand)
                  Just card -> return card

-- TODO: try to play something that can be followd if spade queen is still in the game
strategy :: Gameplay.PlayerStrategy
strategy =
   Gameplay.PlayerStrategy $ do
   playerState <- State.get
   let trick = playerTrick playerState
       hand = playerHand playerState
       history = playerHistory playerState
       firstCard = leadingCardOfTrick trick
       firstSuit = suit firstCard
       trickCards = cardsOfTrick trick
       followingCardsTrick = List.filter ((== firstSuit) . suit) trickCards
       highestCard = maximum(followingCardsTrick)
       followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
       heartOnHand = Set.filter ((==Hearts) .suit) hand
       heartCards = List.filter ((== Hearts) . suit) trickCards
       queenOfSpades = List.filter (== Card Spades Queen) trickCards
   if trickEmpty trick 
     then
       return (Set.findMin hand)
     else
       if (List.elem (Card Spades Queen) hand) && (firstSuit == Spades) && (highestCard > (Card Spades King)) then 
          return (Card Spades Queen)
       else if (List.elem (Card Spades Queen) hand) && (List.length followingCardsOnHand == 0) then 
          return (Card Spades Queen)
       else if (List.length followingCardsOnHand > 0) then
          return (Set.findMin followingCardsOnHand)
       else if (List.length followingCardsOnHand == 0) && (Set.size heartOnHand > 0) then
          return (Set.findMax heartOnHand)
       else
          return (Set.findMax hand)

