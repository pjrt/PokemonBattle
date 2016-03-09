{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module BattleStatus where

import Control.Lens
import Data.Text (Text)

-- | BattleStatus represents the current state of the battle.
data BattleStatus = BS { _bsHP :: Int
                       , _bsTurn :: Int
                       , _bsMessages :: [Text]
                       }

makeLenses ''BattleStatus

-- | Damage the current HP from the DMG.
--
-- NOTE: The damage is a positive number that is substracted from the current
-- HP. A negative damage would mean healing.
damageHP :: Int -> BattleStatus -> BattleStatus
damageHP dmg = over bsHP (\h -> h - dmg)

-- | Increment the turn count by one.
incTurn :: BattleStatus -> BattleStatus
incTurn = over bsTurn (+ 1)

-- | Add a message to the current battle
addMsg :: Text -> BattleStatus -> BattleStatus
addMsg msg = over bsMessages (++ [msg])

-- | The initial state of the battle.
initialBattleStatus :: Int -> BattleStatus
initialBattleStatus initHP = BS initHP 0 []
