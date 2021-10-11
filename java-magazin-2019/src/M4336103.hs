module M4336103 where

import Control.Monad.State.Lazy as State
  ( MonadState(put, get), modify, evalStateT, execStateT )

import qualified Data.Set as Set

import Game hiding (processGameCommandM, processGameEvent, playerProcessGameEvent)
import Gameplay
import Cards

-- task 7
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
      return (Set.findMax hand) -- start with highest card to win that trick
    else
      if penalty (cardsOfTrick trick) > 0 -- check if it is worth to take this trick
        then
          case Set.lookupMax followingCardsOnHand of
              Nothing -> return (Set.findMin hand) -- if you cant take this trick, get rid of a bad card
              Just c  -> if cardWinsTrick c (map snd trick) -- if you can win this trick
                then
                  return c -- take it!
                else
                  return (Set.findMin followingCardsOnHand) -- otherwise get rid of a bad card
        else
          case Set.lookupMax followingCardsOnHand of
            Nothing -> return (Set.findMin hand) -- get rid of a bad card
            Just c  -> return (Set.findMin followingCardsOnHand) -- get rid of a bad card

cardWinsTrick :: Card -> [Card] -> Bool
cardWinsTrick card = foldr ((&&) . cardBeats card) True

-- task 9

strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
    playerState <- State.get
    let trick                = playerTrick playerState
        hand                 = playerHand playerState
        history              = playerHistory playerState
        shooting             = playerShoots playerState
        firstCard            = leadingCardOfTrick trick
        firstSuit            = suit firstCard
        followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
        trickcards           = cardsOfTrick trick
    if shooting
      then -- try to get all points (not yet implemented)
        return (Set.findMax followingCardsOnHand)
      else -- try not to get points
        if trickEmpty trick
          then
            case getBestStart (Set.toList  hand) history of
              Just card -> return card -- play a card with which you want get the Trick
              Nothing   -> case getOkStart (Set.toList (Set.filter ((/= Hearts) . suit) hand)) history of
                Just card -> return card
                Nothing   -> return (Set.findMin hand) -- there is no good start
          else
            case Set.size followingCardsOnHand of
              0 -> if Set.member (Card Spades Queen) hand -- I can pas whatever I want
                    then
                      return (Card Spades Queen)  -- Play Spades Queen if I have it
                    else
                      case Set.lookupMax (Set.filter (cardIsOfSuit Hearts) hand) of
                        Just card -> return card -- play highest heart available
                        Nothing   -> let wsq = Set.filter (cardLosesTrick trickcards) hand
                                         ls  = lowestNumberOfCardsOfSuit Spades [Diamonds, Clubs] wsq
                                         set = Set.filter (cardIsOfSuit ls) hand in
                                      case Set.lookupMax set of
                                        Just card -> return card -- play highest card of a suit I have less
                                        Nothing   -> return (Set.findMax hand)
              1 -> return (Set.findMax followingCardsOnHand)
              _ -> if cardLosesTrick trickcards (Card Spades Queen) && Set.member (Card Spades Queen) followingCardsOnHand
                      then
                        return (Card Spades Queen)
                      else 
                        if length trickcards == 3
                          then
                            if penalty (cardsOfTrick trick) > 0
                              then
                                let lowestCard = Set.findMin followingCardsOnHand in
                                  if cardWinsTrick lowestCard trickcards
                                    then
                                      let wsq = Set.filter (\c -> c /= Card Spades Queen) followingCardsOnHand in
                                        case Set.lookupMax wsq of
                                          Just card -> return card  -- play highest card that is not spades Queen
                                          Nothing   -> return (Set.findMax followingCardsOnHand)
                                    else
                                      let hntt = Set.filter (cardLosesTrick trickcards) followingCardsOnHand in
                                        case Set.lookupMax hntt of
                                          Just card -> return card  -- play highest card that does not take the trick
                                          Nothing   -> return (Set.findMax followingCardsOnHand)
                              else -- penalty is 0
                                let wsq = Set.filter (\c -> c /= Card Spades Queen) followingCardsOnHand in
                                  case Set.lookupMax wsq of
                                    Just card -> return card -- play highest card that is not spades Queen
                                    Nothing   -> return (Set.findMax followingCardsOnHand)
                          else
                            let hntt = Set.filter (\c -> not (cardWinsTrick c (cardsOfTrick trick))) followingCardsOnHand in
                              case Set.lookupMax hntt of
                                Just card -> return card
                                Nothing   -> return (Set.findMax followingCardsOnHand) -- play highest card that does not take the trick

lowestNumberOfCardsOfSuit :: Suit -> [Suit] -> Hand -> Suit
lowestNumberOfCardsOfSuit suit [] _        = suit
lowestNumberOfCardsOfSuit suit (x:xs) hand = if Set.size (Set.filter (cardIsOfSuit x) hand) > Set.size (Set.filter (cardIsOfSuit suit) hand)
  then lowestNumberOfCardsOfSuit suit xs hand
  else lowestNumberOfCardsOfSuit x xs hand

cardLosesTrick :: [Card] -> Card -> Bool
cardLosesTrick [] _        = False
cardLosesTrick (x:xs) card = cardBeats x card || cardLosesTrick xs card

--
getBestStart :: [Card] -> PlayerHistory -> Maybe Card
getBestStart [] _           = Nothing
getBestStart (x:xs) history = if cardIsLowestOfSuit x history then Just x else getBestStart xs history

--
getOkStart :: [Card] -> PlayerHistory -> Maybe Card
getOkStart [] _           = Nothing
getOkStart (x:xs) history = if cardIsHighestOfSuit x history then getOkStart xs history else Just x

--
cardIsHighestOfSuit :: Card -> PlayerHistory -> Bool
cardIsHighestOfSuit (Card s r) history = highestCard (Card s r) (cardsOfSuitLeft s history) == Card s r

cardIsLowestOfSuit :: Card -> PlayerHistory -> Bool
cardIsLowestOfSuit (Card s r) history = lowestCard (Card s r) (cardsOfSuitLeft s history) == Card s r

--
cardsOfSuitLeft :: Suit -> PlayerHistory -> [Card]
cardsOfSuitLeft suit history = filter (\card -> card `notElem` playedCards history) (allOfSuit suit)

--
allOfSuit :: Suit -> [Card]
allOfSuit suit = filter (cardIsOfSuit suit) deck

--
cardIsOfSuit :: Suit -> Card -> Bool
cardIsOfSuit suit (Card s _) = suit == s

--
playedCards :: PlayerHistory -> [Card]
playedCards = concatMap (cardsOfTrick . snd)

--
highestCard :: Card -> [Card] -> Card
highestCard card []     = card
highestCard card (x:xs) = if cardBeats card x then highestCard card xs else highestCard x xs

--
lowestCard :: Card -> [Card] -> Card
lowestCard card []     = card
lowestCard card (x:xs) = if cardBeats card x then lowestCard x xs else lowestCard card xs
