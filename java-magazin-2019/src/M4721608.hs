module M4721608 where

import Gameplay 
import Game 
import Cards
import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import Control.Monad as Monad
import qualified Data.Set as Set


strategy :: PlayerStrategy
strategy =  PlayerStrategy $ do
    playerState <- State.get
    let history = playerHistory playerState
        trick = playerTrick playerState
        hand = playerHand playerState
        myLegalHand = Set.fromList $ filter (\card -> legalCard hand trick card) (Set.elems hand)
        firstCard = leadingCardOfTrick trick
        firstSuit = suit firstCard
        followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
        numHearts = countPlayedSuits Hearts history 
        numSpades = countPlayedSuits Spades history 
        numClubs = countPlayedSuits Clubs history 
        numDiamonds = countPlayedSuits Diamonds history 

        allDiamonds = allSuitsOnHand Diamonds hand
        allHearts = allSuitsOnHand Hearts hand
        allSpades = allSuitsOnHand Spades hand
        allClubs = allSuitsOnHand Clubs hand

        handOwnsSpadesQueen = null $ Set.filter (== Card Spades Queen) hand

    if trickEmpty trick
        then 
            if numDiamonds == 0 && not (Set.null allDiamonds)
                then return $ Set.findMax allDiamonds
                else if numSpades == 0 && not (Set.null allSpades)
                        then return $ Set.findMax allSpades
                        else if numClubs == 0 && not  (Set.null allClubs)
                                then return $ Set.findMax allClubs
                                else 
                                    return (Set.findMin myLegalHand)
        else          
            if firstSuit == Spades 
                then 
                    case Set.lookupMin followingCardsOnHand of
                        Just card -> return card 
                        Nothing -> return (Set.findMin hand)
                else 
                    if firstSuit == Hearts
                        then case Set.lookupMin followingCardsOnHand of
                                Just card -> return card
                                Nothing -> if handOwnsSpadesQueen
                                                then return (Card Spades Queen)
                                                else return (Set.findMin myLegalHand)                
                        else  -- other suits
                            if numHearts > 9
                                then case Set.lookupMin followingCardsOnHand of
                                    Just card -> return card 
                                    Nothing -> return (Set.findMin hand)
                                else case Set.lookupMax followingCardsOnHand of
                                    Just card -> return card 
                                    Nothing -> return (Set.findMax myLegalHand)


cardInHand :: Card -> Hand -> Bool
cardInHand card hand = not (Set.null $ Set.filter (== card) hand)

allSuitsOnHand :: Suit -> Hand -> Hand 
allSuitsOnHand s hand = Set.filter ((== s) . suit) hand

alreadyPlayed :: Card -> PlayerHistory -> Bool
alreadyPlayed card history = let allTricks =  fmap snd history in
                                null $ fmap (filter ((== card) . snd)) allTricks

countPlayedSuits :: Suit -> PlayerHistory -> Int
countPlayedSuits s history = let allTricks =  fmap snd history in
                                length $ fmap (filter ((== s) . suit. snd)) allTricks 

-- goal: get all tricks with haeatrs
-- if the trick is empty, play either high heart, clubs Queen or a low card
-- if the trick is not empty and includes no hearts and not Clubs Queen, then choose the
-- lowest card of the suitable cards. If you don't have any suitable card choose any other
-- but try to avoid hearts and Clubs Queen. If the trick doesn't contain any heart or Clubs 
-- Queen yet, then choose the lowest of the suitable cards. If no suitable card exists
-- then try to find the lowest card which is not a heart or spades queen. If it's not possible,
-- choose the lowest of all other cards in the hand.

shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy = 
    PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      handHearts = Set.filter ((== Hearts) . suit) hand
      handOwnsSpadesQueen = null $ Set.filter (== Card Spades Queen) hand
      trickOwnsSpadesQueen = null $ filter ((== Card Spades Queen) . snd) trick
      otherHandCards = Set.difference hand (Set.insert (Card Spades Queen) handHearts )
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick 
    then -- play first card
        if not (null handHearts)
            then return (Set.findMax handHearts) 
        else 
            if handOwnsSpadesQueen 
                then return (Card Spades Queen)
                else return (Set.findMin hand)
    else
        if trickEmpty (filter ((== Hearts) . suit . snd) trick) && not trickOwnsSpadesQueen
            then case Set.lookupMin followingCardsOnHand of
                Just card -> return card
                Nothing -> case Set.lookupMin otherHandCards of
                               Nothing -> return (Set.findMin hand)
                               Just card' -> return card'
            else case Set.lookupMax followingCardsOnHand of
                Just card -> return card
                Nothing -> return (Set.findMin hand)
