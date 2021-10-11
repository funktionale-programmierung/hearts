module M5006291 where

import Gameplay
import Game
import Cards
import qualified Data.Set as Set
import Control.Monad.State.Lazy as State

shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = Game.playerTrick playerState
      hand = Game.playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = Cards.suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . Cards.suit) hand
  if trickEmpty trick 
    then
      return (Set.findMax hand)
    else
      case Set.lookupMax followingCardsOnHand of
        Nothing ->
          return (Set.findMin hand) -- any card is fine, so try to get rid of low cards
        Just card ->
          return card           -- otherwise use the maximal following card

cardInHistory :: Card -> PlayerHistory -> Bool 
cardInHistory card = any (cardInTrick card . snd)

cardInTrick card trick = card `elem` cardsOfTrick trick

safeToStartWith card remainingCardsInGame = let cards_of_same_suit = filter (\card' -> suit card' == suit card) remainingCardsInGame
                                                smaler_cards = filter (\card' -> cardBeats card card') cards_of_same_suit
                                                higher_cards = filter (\card' -> cardBeats card' card) cards_of_same_suit
                                            in null smaler_cards && not (null higher_cards)

cardLoosesTrick card trick = suit card /= suit (leadingCardOfTrick  trick) ||
  any (\card_in_trick -> cardBeats card_in_trick card) (cardsOfTrick trick)

findMin set = foldr (\card1 card2 -> if rank card1 < rank card2 then card1 else card2) (Set.findMin set) set
findMax set = foldr (\card1 card2 -> if rank card1 > rank card2 then card1 else card2) (Set.findMin set) set

strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = Game.playerTrick playerState
      hand = Game.playerHand playerState
      firstCard = leadingCardOfTrick trick
      history = playerHistory playerState
      firstSuit = Cards.suit firstCard
      --followingCardsOnHand = Set.filter ((== firstSuit) . Cards.suit) hand
      remainingCardInGame = filter (\card -> notElem card hand && not (cardInHistory card history)) Cards.deck
      cards_safe_to_start_with = Set.filter (`safeToStartWith` remainingCardInGame) hand
      legal_hand = Set.filter (\card -> legalCard hand trick card) hand
      card_safe_to_play_in_trick = Set.filter (`cardLoosesTrick` trick) legal_hand
  if trickEmpty trick 
    then
      if null cards_safe_to_start_with then
        return (findMin hand)
      else
        return $ findMax cards_safe_to_start_with
    else
      if Set.size card_safe_to_play_in_trick > 0 then
        return $ findMax card_safe_to_play_in_trick
      else
          return (findMin legal_hand) -- any card is fine, so try to get rid of low cards
