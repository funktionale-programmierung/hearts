module M5252008 where
import Gameplay
import qualified Data.Set as Set
import Game
import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)
import Cards
import Data.List

queen = Card Spades Queen

strategy :: PlayerStrategy
strategy = PlayerStrategy $ do
    playerState <- State.get
    let trick = playerTrick playerState
        hand = playerHand playerState
        stack = playerStack playerState
        myLegalHand = filter (\c -> legalCard hand trick c) $ Set.elems hand
        firstCard = leadingCardOfTrick trick
        firstSuit = suit firstCard
        history = playerHistory playerState
        starting = if trickEmpty trick then True else False
        iHaveSameSuit = if (not starting) && all (\c -> suit c == firstSuit) myLegalHand then True else False  
    if starting 
      then 
        if historyContain queen history 
          then 
            if elem queen stack 
              then 
                return (maximum myLegalHand)
              else
                return (lowestHeartWRTHistory' myLegalHand history)
          else
            return (lowestHeartWRTHistory' myLegalHand history)
      else
        if iHaveSameSuit
          then
            if firstSuit == Hearts then return $ minimum myLegalHand else if firstSuit == Spades then return $ minimum myLegalHand else return $ maximum myLegalHand
          else
            if elem queen myLegalHand then return queen else return $ playHighestHeart myLegalHand

historyContain :: Card -> PlayerHistory -> Bool
historyContain c h = elem c $ map snd (concat $ map snd h)

historyCards :: PlayerHistory -> [Card]
historyCards h = map snd (concat $ map snd h)

listInsideList l1 l2 = length (l1 `intersect` l2) == length l1

lowestHeartWRTHistory :: Card -> PlayerHistory -> Maybe Card
lowestHeartWRTHistory c h = let cards = historyCards h in
                            let allHeartsLessThanC = filter (\cc -> suit cc == Hearts && cc < c) deck in
                            if listInsideList allHeartsLessThanC cards then Just c else Nothing
                            
lowestHeartWRTHistory' :: [Card] -> PlayerHistory -> Card
lowestHeartWRTHistory' hand h = let hearts = filter (\c -> suit c == Hearts) hand in
                                let c = if length hearts > 0 then lowestHeartWRTHistory (minimum hearts) h else Nothing in
                                case c of 
                                    Nothing -> playHighestCardLessThanQueen hand
                                    Just card -> card

playHighestHeart :: [Card] -> Card
playHighestHeart hand = let hearts = filter (\c -> suit c == Hearts) hand in
                        if length hearts > 0 then maximum hearts else foldr (\c cs -> if rank c > rank cs then c else cs) (Card Diamonds (Numeric 2)) hand

playHighestCardLessThanQueen :: [Card] -> Card
playHighestCardLessThanQueen hand = let cardsLessThanQueen = filter (\c -> c < queen ) hand in
                                if length cardsLessThanQueen > 0 then maximum cardsLessThanQueen else minimum hand



shootTheMoonStrategy :: Gameplay.PlayerStrategy
shootTheMoonStrategy = PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      myLegalHand = filter (\c -> legalCard hand trick c) $ Set.elems hand 
  if trickEmpty trick then return (minimum myLegalHand) 
    else
        let tryToWin = trickHasPointsCard trick in
        if tryToWin then return (maximum myLegalHand) else return (minimum myLegalHand)

trickHasPointsCard :: Game.Trick -> Bool
trickHasPointsCard trick = any (\c -> (suit c == Hearts || (c == Card Spades Queen))) (map snd trick)
