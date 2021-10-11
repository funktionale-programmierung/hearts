module M4951475 where

import Gameplay
import Cards
import Game

import qualified Data.Set as Set
import Control.Monad.State.Lazy as State

-- hopefully a good strategy
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
    -- first case, beginning of game and we have Spades 2
    if null history && trickEmpty trick then
        return (Set.findMin hand)
    -- if is the fist trick and we did not start, just play highest Club and try to win trick 
    -- or we cannot follow the trick, then just
    else if null history then
      case Set.lookupMax followingCardsOnHand of
        Nothing -> do
            -- try to get rid of Queen of Spades
            let handfiltered = Set.filter (\x -> (x == Card Spades Queen)) hand
            if Set.size handfiltered == 0 then do
                -- alternatively get rid of higher Spades
                let handfiltered = Set.filter (\x -> (x == Card Spades King || x == Card Spades Ace)) hand
                if Set.size handfiltered == 0 then
                    -- get rid of any high Card
                    return (Set.findMax hand)
                else
                    return (Set.findMax handfiltered)
            else
                return (Card Spades Queen)
        Just card ->
          return card
    -- case we won the last trick, try to "lure out" Queen of Spades, or "lure out" Hearts, else any small card
    else if trickEmpty trick then do
        -- if we do not have Queen of Spades on out hand, try to lure it out
        let queenofspades = Set.filter (\x -> (x == Card Spades Queen)) hand
        if Set.size queenofspades == 0 then do
            let handfiltered = Set.filter (\x -> (suit x == Spades && x /= Card Spades King && x /= Card Spades Ace)) hand
            if Set.size handfiltered == 0 then do
                let handfiltered = Set.filter (\x -> x > Card Hearts (Numeric 10)) hand
                if Set.size handfiltered == 0 || 8 - historyNumUnplayedSuit Hearts history < Set.size handfiltered then do
                    -- get rid of any high Card
                    let handfiltered = Set.filter (\x -> (x <= Card Clubs Ace && x > Card Clubs (Numeric 10)) || (x <= Card Diamonds Ace && x > Card Diamonds (Numeric 10))) hand
                    if Set.size handfiltered == 0 then
                        return (Set.findMax hand)
                    else
                        return (Set.findMax handfiltered)
                else
                    return (Set.findMax handfiltered)
            else
                return (Set.findMax handfiltered) -- play the biggest Spade smaller than the Queen
        -- else try to lure out Hearts
        else do
            let handfiltered = Set.filter (\x -> x > Card Hearts (Numeric 10)) hand
            if Set.size handfiltered == 0 || 8 - historyNumUnplayedSuit Hearts history < Set.size handfiltered then do
                -- get rid of any high Card
                let handfiltered = Set.filter (\x -> (x <= Card Clubs Ace && x > Card Clubs (Numeric 10)) || (x <= Card Diamonds Ace && x > Card Diamonds (Numeric 10))) hand
                if Set.size handfiltered == 0 then
                    return (Set.findMax hand)
                else
                    return (Set.findMax handfiltered)
            else
                return (Set.findMax handfiltered)
    -- case it is not the first trick, but we also cannot follow, try to dump Queen of Spades, alternatively high card
    else if Set.size followingCardsOnHand == 0 then
        -- if the Queen of Spades has been played then just try to remove any high card
        if historyContains (Card Spades Queen) history then
                return (Set.findMax hand)
        else do
            -- try to get rid of Queen of Spades
            let handfiltered = Set.filter (\x -> (x == Card Spades Queen)) hand
            if Set.size handfiltered == 0 then do
                -- alternatively get rid of higher Spades
                let handfiltered = Set.filter (\x -> (x == Card Spades King || x == Card Spades Ace)) hand
                if Set.size handfiltered == 0 then do
                    -- try to get rid of higher Heart
                    let handfiltered = Set.filter (\x -> x > Card Hearts (Numeric 10)) hand
                    if Set.size handfiltered == 0 || 8 - historyNumUnplayedSuit Hearts history < Set.size handfiltered then do
                        -- get rid of any high Card
                        let handfiltered = Set.filter (\x -> (x <= Card Clubs Ace && x > Card Clubs (Numeric 10)) || (x <= Card Diamonds Ace && x > Card Diamonds (Numeric 10))) hand
                        if Set.size handfiltered == 0 then
                            return (Set.findMax hand)
                        else
                            return (Set.findMax handfiltered)
                    else
                        return (Set.findMax handfiltered)
                else
                    return (Set.findMax handfiltered)
            else
                return (Card Spades Queen)
    -- last case, just try to follow up with smallest possible card, that is smaller than the biggest played
    else do
        let highestCard = (maximum (filter (\x -> suit x == suit (leadingCardOfTrick trick)) (cardsOfTrick trick)))
            handfiltered = Set.filter (\x -> x < highestCard) followingCardsOnHand
        if Set.size handfiltered == 0 then
            return (Set.findMin followingCardsOnHand)
        else
            return (Set.findMax handfiltered)

historyNumUnplayedSuit :: Suit -> PlayerHistory -> Int
historyNumUnplayedSuit _ [] = 0
historyNumUnplayedSuit s ((x, y):xs) = historyNumUnplayedSuit s xs + length (filter (\x -> suit x == s) (cardsOfTrick y))

historyContains :: Card -> PlayerHistory -> Bool
historyContains _ [] = False
historyContains c ((x, y):xs) = elem c (cardsOfTrick y) || historyContains c xs

-- stupid robo play but aims for shooting the moon :$
shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
    PlayerStrategy $ do
    playerState <- State.get
    let trick = playerTrick playerState
        hand = playerHand playerState
        firstCard = leadingCardOfTrick trick
        firstSuit = suit firstCard
        followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
    if trickEmpty trick then do
        -- If it is our turn, just play any lowest non Point card.
        let handfiltered = Set.filter (\x -> (x /= Card Spades Queen && suit x /= Hearts)) hand
            numberofcards = Set.size handfiltered
        if numberofcards == 0 then -- If we only have Point cards left, then play the highest.
            return (Set.findMax hand)
        else
            return (Set.findMin handfiltered)
    else
        -- check if there are any Point card in the trick
        if any (\x -> (x == Card Spades Queen || suit x == Hearts )) (cardsOfTrick trick) then
            -- if there are then try to get the trick
            case Set.lookupMax followingCardsOnHand of
                Nothing -> return (Set.findMin hand) -- mission failed :/
                Just card -> return card
        else do
            -- no, then just get rid of minimal card, 
            let handfiltered = Set.filter (\x -> (x /= Card Spades Queen && suit x /= Hearts)) followingCardsOnHand
                numberofcards = Set.size handfiltered
            if numberofcards == 0 then do -- If we only have Point cards left, then play any lowest card.
                case Set.lookupMin followingCardsOnHand of
                    Nothing -> return (Set.findMin hand) -- else any other small card
                    Just card -> return card
            else
                return (Set.findMin handfiltered)