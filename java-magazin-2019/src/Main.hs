module Main where

import qualified Gameplay as G
import qualified Shuffle
import qualified Cards
import qualified MyStrategy as S

main :: IO ()
main = start

playerMike = G.makePlayer "Mike" S.strategy
playerPeter = G.makePlayer "Peter" G.playInteractive
playerAnnette = G.makePlayer "Annette" G.playAlongStrategy
playerNicole = G.makePlayer "Nicole" G.shootTheMoonStrategy

start :: IO ()
start =
  G.runGame (Shuffle.shuffleRounds 10 Cards.deck)
            [playerNicole, playerAnnette, playerPeter, playerMike]

