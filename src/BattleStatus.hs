{-# LANGUAGE FlexibleContexts #-}
module BattleStatus where

import Control.Lens
import Data.Text (Text)

-- | BattleStatus represents the current state of the battle.
--
-- (Current HP, Current turn, Current set of messages)
newtype BattleStatus = BS { runBS :: (Int, Int, [Text]) }

-- | Damage the current HP from the DMG.
--
-- NOTE: The damage is a positive number that is substracted from the current
-- HP. A negative damage would mean healing.
damageHP :: Int -> BattleStatus -> BattleStatus
damageHP dmg = BS . over _1 (\h -> h - dmg) . runBS

-- | Increment the turn count by one.
incTurn :: BattleStatus -> BattleStatus
incTurn = BS . over _2 (+ 1) . runBS

-- | Add a message to the current battle
addMsg :: Text -> BattleStatus -> BattleStatus
addMsg msg = BS . over _3 (++ [msg]) . runBS

-- | The initial state of the battle.
initialBattleStatus :: Int -> BattleStatus
initialBattleStatus initHP = BS (initHP, 0, [])
