module Main where

-- command line parsing
import Options.Applicative
import Data.Semigroup ((<>))

import qualified Gameplay as G
import qualified Shuffle
import qualified Cards
import qualified MyStrategy as S
import qualified AllStrategies as A

import qualified Tournament as T


data CmdLineArgs = CmdLineArgs
  { tournament :: Bool,
    automatic :: Bool,
    rounds :: Int,
    strategicPlayers :: [StrategicPlayers]
  }

data StrategicPlayers = PlayAlong String | ShootTheMoon String | MyStrategy String

playStrategy = PlayAlong <$> strOption
  (  long "playAlong"
  <> short 'p'
  <> metavar "PLAYER"
  <> help "Player name" )

shootStrategy = ShootTheMoon <$> strOption
  (  long "shootTheMoon"
  <> short 's'
  <> metavar "PLAYER"
  <> help "Select ShootTheMoon strategy" )

myStrategy = MyStrategy <$> strOption
  (  long "myStrategy"
  <> short 'm'
  <> metavar "PLAYER"
  <> help "Select my strategy" )


cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs = CmdLineArgs
  <$> switch (long "tournament"
              <> short 't'
              <> help "Switch to tournament mode")
  <*> switch (long "auto"
             <> short 'a'
             <> help "Switch to automatic tournament mode")
  <*> option auto
          ( long "rounds"
         <> short 'n'
         <> help "# rounds for tournament"
         <> showDefault
         <> value 1
         <> metavar "ROUNDS" )
  <*> many (playStrategy <|> shootStrategy <|> myStrategy)
  --   <*> many (argument str (metavar "NAMES..."))

main :: IO ()
main = go =<< execParser opts
  where
    opts = info (cmdLineArgs <**> helper)
      ( fullDesc
     <> progDesc "Run the game of Hearts interactively or in tournament mode"
     <> header "hearts - run the game of Hearts" )

go :: CmdLineArgs -> IO ()
go args =
  if not (tournament args) then
    start
  else do
    putStrLn "Tournament: Tournament mode"
    putStrLn "Tournament: ==============="
    let createPlayer sp = case sp of
          PlayAlong n -> G.makePlayer n G.playAlongStrategy
          ShootTheMoon n -> G.makePlayer n G.shootTheMoonStrategy
          MyStrategy n -> G.makePlayer n S.strategy
        getPlayer (n, st) = G.makePlayer n st
        players | automatic args = map getPlayer A.realStrategies
                | otherwise = map createPlayer (strategicPlayers args)
    scores <- (if automatic args then T.full else T.start) players (rounds args)
    putStrLn "Tournament: FINAL RESULTS"
    putStrLn "Tournament: ============="
    mapM_ (putStrLn . ("Tournament: "++) . show) scores
    return ()

--------------------------------------------------------------------------------
-- interactive play

playerMike = G.makePlayer "Mike" S.strategy
playerPeter = G.makePlayer "Peter" G.playInteractive
playerAnnette = G.makePlayer "Annette" G.playAlongStrategy
playerNicole = G.makePlayer "Nicole" G.shootTheMoonStrategy

start :: IO ()
start = do
  G.runGame (Shuffle.shuffleRounds 10 Cards.deck)
            [playerNicole, playerAnnette, playerPeter, playerMike]
  return ()

