{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module M4719482 where


import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer  
import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set

import Gameplay
import Cards
import Game

------------------------------ for shoot the moon

-- checks if Rank of a hearts card is high enough to be reasonably played
highEnoughRank :: Card -> Bool 
highEnoughRank (Card _ (Numeric x)) = if (x >= 5) then True else False
highEnoughRank _ = True  -- must have Rank higher than Numeric, therefore always okay

-- returns highest card, preferring Hearts. If the heart would be very small, don't play it
highestHeartOrAny :: Hand -> Card
highestHeartOrAny h = let possibleHearts = (Set.filter ((== Hearts) . suit) h) in
                          case Set.lookupMax possibleHearts of
                            Just c -> if (highEnoughRank c) then c else (Set.findMax h)
                            Nothing -> (Set.findMax h)

smallestNonHeart :: Hand -> Card
smallestNonHeart h = let nonHearts = (Set.filter ((/= Hearts) . suit) h) in
                         case Set.lookupMin nonHearts of
                           Just c -> c  -- smallest non-heart
                           Nothing -> (Set.elemAt 0 h)  -- random other card. In this case, shooting moon is now impossible

-- Basic shoot the moon: Doesn't try to void others, and mostly just tries to win
-- the suit in all situations (prefers to play high hearts/QueenOfSpades if leading the trick).
-- If the suit can't be followed, it plays a small non-heart card to get rid of them.  
-- Doesn't change strategy even when shooting the moon is no longer possible
shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
  playerState' <- State.get
  let playerState = playerState' { playerShoots = True } 
      trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      matchingCards = (Set.filter ((== firstSuit) . suit) hand)
      possibleCards = (Set.filter (\x -> legalCard hand trick x) hand)
      canQueenOfSpades = Set.member (Card Spades Queen) possibleCards
  if trickEmpty trick 
    then 
      if canQueenOfSpades  -- highest priority: Play Queen of Spades to easily win it
         then 
          return (Card Spades Queen)
         else 
          return (highestHeartOrAny possibleCards)  -- highest heart if possible, else highest normal card
    else
      case Set.lookupMax matchingCards of  -- try to follow the suit
        Just c -> return c
        Nothing -> return $ smallestNonHeart possibleCards -- get rid of small non-heart card



------------------------------ for smarter strategy

-- smarter Strategy for Player. Differentiates between early and regular game and
-- adjusts accordingly.
-- also recognizes if this player is in danger of getting a "monopoly" of cards and tries
-- to avoid being the only one with cards of some suit left to avoid winning tricks (especially at the end)
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
      withoutHearts = Set.filter ((/= Hearts) . suit) hand
      withoutHeartsAndQOS = Set.filter (/= (Card Spades Queen)) withoutHearts
      roundsPlayed = length $ history
      haveQueenOfSpades = Set.member (Card Spades Queen) hand

      earlyGame =  -- in early game, try to get rid of high cards.
        if trickEmpty trick
           then 
            case Set.lookupMax withoutHeartsAndQOS of
              Nothing -> Set.findMin hand
              Just a -> a
           else 
            if (safeToTake trick)
              then case Set.lookupMax followingCardsOnHand of
                  Nothing -> if haveQueenOfSpades then (Card Spades Queen) 
                                                  else (Set.findMax hand)
                  Just a -> a
              else
                case biggestNonWinning trick firstSuit followingCardsOnHand of
                  Nothing -> if haveQueenOfSpades then (Card Spades Queen) 
                                                  else (Set.findMax hand)
                  Just card -> card

      regularGame = 
        if trickEmpty trick
          then 
            case (Set.lookupMin withoutHeartsAndQOS) of
              Just tryCard ->
                if (((suitHistory (suit tryCard) history) + (ownCards tryCard hand) == 13) ||
                    highestLeft tryCard history)
                  then case Set.lookupMin (Set.filter ((/= (suit tryCard)) . suit ) hand) of
                        Nothing -> tryCard
                        Just a -> a
                  else tryCard
              Nothing -> Set.findMin hand
          else 
            if (safeToTake trick)
              then case Set.lookupMax followingCardsOnHand of
                      Nothing -> Set.findMax hand
                      Just a -> a
              else
                case biggestNonWinning trick firstSuit followingCardsOnHand of
                  Nothing -> if haveQueenOfSpades then (Card Spades Queen) 
                                                  else (Set.findMax hand)
                  Just card -> card
            
  case roundsPlayed of
    x | x <= 3 -> return earlyGame 
    _ -> return regularGame

-- return how many cards of the same suit are on the hand
ownCards :: Card -> Hand -> Int
ownCards c h = length $ Set.filter ((== (suit c)) . suit) h

-- try to return the biggest card in hand that is still smaller than the matching in suit
-- if you can't avoid winning the trick, pick the biggest card that matches
-- if none even match the trick, then return Nothing
biggestNonWinning :: Trick -> Suit -> Hand -> Maybe Card
biggestNonWinning t s h = 
  let maxInTrick = maximum (filter ((== s) . suit) $ cardsOfTrick t)
      smallerInHand = (Set.filter ((<= rank maxInTrick) . rank ) h)
  in case (Set.lookupMax smallerInHand) of
       Just a -> Just a
       Nothing -> (Set.lookupMax h)

-- if last player and no hearts have been played yet
safeToTake :: Trick -> Bool
safeToTake t = 
  if (length t == 3)
     then if (sum (map (\x -> if (suit x == Hearts) then 0 else 1) $ cardsOfTrick t) == 3)
          then True
          else False
     else  False

-- return list of all cards played from history
cardList :: PlayerHistory -> [Card]
cardList history = concatMap ((map snd) . snd) history

-- checks whether a card would be the highest left.
-- only supports comparisons for cards with rank >= Numeric 10; for smaller
-- cards, it just returns false.
highestLeft :: Card -> PlayerHistory -> Bool
highestLeft c history = 
  let tricks = cardList history
      onlyMatching = filter ((== (suit c)) . suit) tricks
      contains rank = elem (Card (suit c) rank) onlyMatching
   in case (rank c) of
        Ace -> True
        King -> if (contains Ace) then True else False
        Queen -> if (contains Ace && contains King) then True else False
        Jack -> if (contains Ace && contains King && contains Queen) then True else False
        (Numeric 10) -> if (contains Ace && contains King && contains Queen && contains Jack) then True else False
        _ -> False

-- counts occurances of suit in history
suitHistory :: Suit -> PlayerHistory -> Int
suitHistory s history = 
  let tricks = cardList history
      playedCards = map (\x -> if suit x == s then 1 else 0) tricks
  in sum playedCards 
