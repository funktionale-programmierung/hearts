module AllStrategies where

import qualified M5108641
import qualified M5164530
import qualified M5006291
import qualified M4336103
import qualified M4721608
import qualified M4349877
import qualified M4925953
import qualified M4719482
import qualified M5172845
import qualified M3915548
import qualified M4108569
import qualified M4337015
import qualified M5252008
import qualified M4930156
import qualified M5154605
import qualified M4724561
import qualified M4307546
import qualified M4951475

import qualified MyStrategy
import qualified Gameplay

strategies :: [(String, Gameplay.PlayerStrategy)]
strategies =
  [
    ("s5108641", M5108641.strategy),
    ("m5108641", M5108641.shootTheMoonStrategy),
    ("s5164530", M5164530.strategy),
    ("m5164530", M5164530.shootTheMoonStrategy),
    ("s5006291", M5006291.strategy),
    ("m5006291", M5006291.shootTheMoonStrategy),
    ("s4336103", M4336103.strategy),
    ("m4336103", M4336103.shootTheMoonStrategy),
    -- ("s4721608", M4721608.strategy), -- attempts illegal moves
    ("m4721608", M4721608.shootTheMoonStrategy),
    ("s4349877", M4349877.strategy),
    ("m4349877", M4349877.shootTheMoonStrategy),
    ("s4925953", M4925953.strategy),
    ("m4925953", M4925953.shootTheMoonStrategy),
    ("s4719482", M4719482.strategy),
    ("m4719482", M4719482.shootTheMoonStrategy),
    ("s5172845", M5172845.strategy),
    ("m5172845", M5172845.shootTheMoonStrategy),
    ("s3915548", M3915548.strategy),
    ("m3915548", M3915548.shootTheMoonStrategy),
    ("s4108569", M4108569.strategy),
    ("m4108569", M4108569.shootTheMoonStrategy),
    ("s4337015", M4337015.strategy),
    ("m4337015", M4337015.shootTheMoonStrategy),
    -- ("s5252008", M5252008.strategy), -- attempts illegal moves
    ("m5252008", M5252008.shootTheMoonStrategy),
    ("s4930156", M4930156.strategy),
    ("m4930156", M4930156.shootTheMoonStrategy),
    ("s5154605", M5154605.strategy),
    ("m5154605", M5154605.shootTheMoonStrategy),
    ("s4724561", M4724561.strategy),
    ("m4724561", M4724561.shootTheMoonStrategy),
    ("s4307546", M4307546.strategy),
    ("m4307546", M4307546.shootTheMoonStrategy),
    ("s4951475", M4951475.strategy),
    ("m4951475", M4951475.shootTheMoonStrategy),
    ("s0000000", MyStrategy.strategy),
    ("m0000000", Gameplay.shootTheMoonStrategy)
  ]

realStrategies :: [(String, Gameplay.PlayerStrategy)]
realStrategies = filter ((>"s"). fst) strategies

moonStrategies :: [(String, Gameplay.PlayerStrategy)]
moonStrategies = filter ((<"s"). fst) strategies
