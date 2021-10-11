module M4724561 where

import qualified Data.Set as Set
import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)
import Data.Maybe (fromJust)
import Data.List

import Gameplay
import Game
import Cards


-- shooting-for-the-moon bot
-- Implement a simple robo player shootTheMoonStrategy that attempts to collect all scoring cards.
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
      return (Set.findMax hand)  -- maximum card to have high chance of Trick
    else
      case Set.lookupMax followingCardsOnHand of
        Nothing ->
          case Set.lookupMax $ Set.filter ((/= Hearts) . suit) hand of  -- do not throw Hearts in lost Trick
            Nothing -> return (Set.findMin hand) -- not following suit -> get rid of low cards that could not win a Trick easily
            Just c -> return c
        Just card ->
          -- only interested in winnable Tricks, and only on those that are hearts or contain Queen of Spades
          if cardBeats card (highestCardOfTrick trick) && ( Hearts `elem` map suit (cardsOfTrick trick) || Card Spades Queen `elem` map snd trick)
          then return card  -- use the maximal following card to ensure the Trick
          -- Trick can't be won, get rid of low card if possible
          else return $ fromJust $ Set.lookupMin followingCardsOnHand


-- strategy bot
-- Devise a strategy (based on the extended PlayerState) that examines the history to determine the next move
-- It should win consistently against playAlongStrategy and there will be bonus points for strategies that win against other students’ strategies
strategy :: PlayerStrategy
strategy = 
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      history = playerHistory playerState  -- :: [(PlayerName, Trick)]
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick
    then
      -- leading trick. play out card with best odds of winning
      return $ Set.findMin hand      -- default: minimum card to have high chance of losing Trick -> better: play medium card, in order to be safe for later in game??
    else
      -- not leading trick.
      case Set.lookupMin followingCardsOnHand of
        -- no fitting cards. maximize future profit -> play out worst cards
        Nothing ->
          if Set.member (Card Spades Queen) hand  -- try to get rid of Queen of Spades if suit cannot be followed
          then return (Card Spades Queen)
          else 
            case Set.lookupMax $ Set.filter ((== Hearts) . suit) hand of  -- else: try to get rid of high hearts
              Just c -> return c
              Nothing -> return (Set.findMax hand) -- no hearts on hand -> simply get rid of high cards  : CHANGE
                -- TODO: try to minimize number of cards of suit -> more options if player can't follow suit
        -- fitting cards. minimize loss, try to avoid taking trick if dangerous, take it if no loss possible
        Just card ->
          -- beware of tricks with hearts or Queen of Spades inside!
          if Card Spades Queen `elem` cardsOfTrick trick || Hearts `elem` map suit (cardsOfTrick trick)
          then return card  -- use the minimal following card to try to lose Trick
          else
            if fromJust (Set.lookupMax followingCardsOnHand) == highestCardInGame trick history -- highest card that can take this trick is own
            then return $ fromJust $ Set.lookupMax followingCardsOnHand  -- trick is harmless as of now. other players all played? TODO
            else return $ fromJust $ Set.lookupMax followingCardsOnHand  -- assume someone else is taking that card  : CHANGE


cardsInGame :: PlayerHistory -> [Card]
cardsInGame history =
    let cardsInHistory = concatMap cardsOfTrick [trick | trick <- [snd h | h <- history]]
    in deck \\ cardsInHistory

-- numOfBetterCards :: Card -> [Card] -> Hand -> Int
-- numOfBetterCards c cs h = 0

-- highest Card in the game (hands) that still can win trick (including own!!)
highestCardInGame :: Trick -> PlayerHistory -> Card
highestCardInGame trick history = 
    let leadingSuit = suit $ leadingCardOfTrick trick
        cardsOfSuitLeft = filter ((== leadingSuit) . suit) (cardsInGame history)
    in highestCardOfList cardsOfSuitLeft

