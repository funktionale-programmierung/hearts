{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module M4925953 where
import Gameplay
import Game
import Cards
import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)
import qualified Data.Set as Set
--import Safe.Foldable
-- module M4925953 where

-- --import Control.Conditional (ifM)
-- import qualified Control.Monad.Writer (WriterT)
-- import Control.Monad.Writer as Writer

-- import Control.Monad.State.Lazy as State
-- import Control.Monad.State.Lazy (State, StateT)

-- import Control.Monad as Monad

-- import qualified Data.Foldable as Foldable
-- import qualified Data.Map.Strict as Map
-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Set as Set

-- --import Debug.Trace (trace, traceShowId, traceIO, traceM)

-- import Cards
-- import Game hiding (processGameCommandM, processGameEvent, playerProcessGameEvent)
-- --import qualified Shuffle
-- type HasPlayerState m = MonadState PlayerState m

-- -- different
-- type StrategyInterface m = (HasPlayerState m, MonadIO m)

-- data PlayerStrategy
--   = PlayerStrategy { chooseCard :: forall m . StrategyInterface m => m Card }

--type PlayerInterface m = (MonadIO m, MonadWriter [GameCommand] m)

-- data PlayerEventProcessor =
--   PlayerEventProcessor (forall m . PlayerInterface m =>
--                          GameEvent -> m PlayerEventProcessor)

-- data Player = 
--   Player
--   { playerName :: PlayerName
--   , eventProcessor :: forall m . PlayerInterface m => GameEvent -> m Player
--   }

shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
    PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      followingCardsOnTrack = filter ((== firstSuit) . suit) $ map snd trick
      heighestCardOnTrack = safemaximum followingCardsOnTrack
      biggerthighest = heighestCardOnTrack >>= (\x -> return (Set.filter (`cardBeats` x) followingCardsOnHand))
      snmallest_bigger = biggerthighest >>= safeMinSet
  if trickEmpty trick
    then
      return (Set.findMax hand)
    else case snmallest_bigger of
        Nothing -> case Set.lookupMax followingCardsOnHand of
                      Nothing ->
                              return (Set.findMin hand) -- any card is fine, so try to get rid of low cards
                      Just card ->
                        return card           -- otherwise use the minimal following card
        Just bigCard -> if length trick == 3 then return bigCard else return (Set.findMin hand)
  -- PlayerStrategy $ do
  -- playerState <- State.get
  -- let trick = playerTrick playerState
  --     hand = playerHand playerState
  --     firstCard = leadingCardOfTrick trick
  --     firstSuit = suit firstCard
  --     followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  -- if trickEmpty trick
  --   then
  --     return (Set.findMax hand)
  --   else
  --     case Set.lookupMax followingCardsOnHand of
  --       Nothing ->
  --         return (Set.findMin hand) -- any card is fine, so try to get rid of high hearts
  --       Just card ->
  --         return card           -- otherwise use the maximal following card


playAlongStrategy' :: Bool -> PlayerStrategy
playAlongStrategy' True = playToMoon''
playAlongStrategy' False = playAlongStrategy''

trickNotZero :: Trick -> Bool
trickNotZero trick = penalty (map snd trick) /= 0

decidIfToShoot ::  HasPlayerState  m =>  m ()
decidIfToShoot = do playerState <- State.get
                    let trick = playerTrick playerState
                        hand = playerHand playerState
                        toShoot  = playerShoots playerState
                        --firstCard = leadingCardOfTrick trick
                        --firstSuit = suit firstCard
                        --followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
                        history = playerHistory playerState
                        countPlayerShooting = Set.size $ Set.fromList  $  map fst $ filter (trickNotZero . snd) history
                        ownstack = playerStack playerState
                        ownpoints = sum $ map penalty ownstack
                        newToShoot = ((ownpoints /= 0 && countPlayerShooting == 1) || ( countPlayerShooting == 0) ||  length hand == 13 && goodCards  (Set.toList   hand))  && toShoot  in
                        --chooseCard (playAlongStrategy' toShoot)
                        --put $  playerState {playerShoots = newToShoot}
                        modifyShoots (const newToShoot) -- PJT changed from: setShootB newToShoot

                        --return ()




strategy :: PlayerStrategy
strategy = PlayerStrategy $ do decidIfToShoot
                               s <- get

                               chooseCard (playAlongStrategy' (playerShoots s))


--strategy :: PlayerStrategy
-- strategy :: PlayerStrategy
-- strategy =  PlayerStrategy $
--                        do playerState <- State.get
--                           let trick = playerTrick playerState
--                               hand = playerHand playerState
--                               firstCard = leadingCardOfTrick trick
--                               firstSuit = suit firstCard
--                               followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
--                               history = playerHistory playerState
--                               countPlayerShooting = Set.size $ Set.fromList  $  map fst $ filter (trickNotZero . snd) history
--                               ownstack = playerStack playerState
--                               ownpoints = sum $ map penalty ownstack
--                               toShoot = ((ownpoints /= 0 && countPlayerShooting == 1) || history ==  []) && goodCards  (Set.toList   hand) in
--                               chooseCard (playAlongStrategy' toShoot)



highHearts :: [Card]
highHearts = map (Card Hearts ) [Numeric 10, Jack, Queen, King, Ace]

containsHighHearts :: [Card] -> Bool
containsHighHearts xs = length (filter (`elem` highHearts)  xs) >= 4

--goodCardsHelper :: [Card] -> Bool
sameColor :: (Foldable t, Num a, Num b, Num c, Num d) => t Card -> (a, b, c, d)
sameColor = foldr f (0,0,0,0) where
  f (Card Hearts _)  (a,b,c,d) = (a+1,b,c,d)
  f (Card Spades _ ) (a,b,c,d) = (a,b+1,c,d)
  f (Card Diamonds  _ ) (a,b,c,d) = (a,b,c+1,d)
  f (Card Clubs  _ ) (a,b,c,d) = (a,b,c,d+1)

sameColor2 :: [Card] -> Bool
sameColor2 xs = case sameColor xs of (a,b,c,d) -> a >= 10 || b >= 10 || c >= 10 || d >= 10

highCards :: [Card] -> [Card]
highCards = filter p where
  p (Card _ r) = r `elem` [Numeric 10, Jack, Queen, King, Ace]

highCards2 :: [Card] -> Bool
highCards2 xs = length (highCards xs) >= 5

goodCards :: [Card] -> Bool
goodCards xs = containsHighHearts xs || (sameColor2 xs && highCards2 xs)

  -- if trickEmpty trick 
  --   then
  --     return (Set.findMax hand)
  --   else
  --     case Set.lookupMax followingCardsOnHand of
  --       Nothing ->
  --         return (Set.findMin hand) -- any card is fine, so try to get rid of high hearts
  --       Just card ->
  --         return card           -- otherwise use the maximal following card 

safemaximum :: Ord a => [a] -> Maybe a
safemaximum [] =Nothing
safemaximum xs = Just $ maximum xs

safemin :: Ord a => [a] -> Maybe a
safemin [] =Nothing
safemin xs = Just $ maximum xs

safeMaximumSet :: Set.Set a -> Maybe a
safeMaximumSet xs = if null xs then Nothing  else Just $ Set.findMax xs


safeMinSet :: Set.Set a -> Maybe a
safeMinSet xs = if null xs then Nothing  else Just $ Set.findMax xs

playAlongStrategy'' :: PlayerStrategy
playAlongStrategy'' =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      followingCardsOnTrack = filter ((== firstSuit) . suit) $ map snd trick
      heighestCardOnTrack = safemaximum followingCardsOnTrack
      smallerThanHeighestOnTrack = heighestCardOnTrack >>= (\x -> return (Set.filter (cardBeats x) followingCardsOnHand))
      biggest_smaller = smallerThanHeighestOnTrack >>= safeMaximumSet
  if trickEmpty trick
    then
      return (Set.findMin hand)
    else case biggest_smaller of
        Nothing -> case Set.lookupMin followingCardsOnHand of
                      Nothing ->
                              return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
                      Just card ->
                        return card           -- otherwise use the minimal following card
        Just bigCard -> return bigCard


playToMoon'' :: PlayerStrategy
playToMoon'' =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      followingCardsOnTrack = filter ((== firstSuit) . suit) $ map snd trick
      heighestCardOnTrack = safemaximum followingCardsOnTrack
      biggerthighest = heighestCardOnTrack >>= (\x -> return (Set.filter (`cardBeats` x) followingCardsOnHand))
      snmallest_bigger = biggerthighest >>= safeMinSet
  if trickEmpty trick
    then
      return (Set.findMax hand)
    else case snmallest_bigger of
        Nothing -> case Set.lookupMin followingCardsOnHand of
                      Nothing ->
                              return (Set.findMin hand) -- any card is fine, so try to get rid of high hearts
                      Just card ->
                        return card           -- otherwise use the minimal following card
        Just bigCard -> return bigCard
