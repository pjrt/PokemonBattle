{-# LANGUAGE FlexibleContexts #-}
module BattleStatus where

import Control.Lens
import Control.Monad.State
import Data.Text (Text)

-- | BattleStatus represents the current state of the battle.
--
-- (Current HP, Current turn, Current set of messages)
newtype BattleStatus = BS { runBS :: (Int, Int, [Text]) }

damageHP :: Int -> BattleStatus -> BattleStatus
damageHP dmg = BS . over _1 (\h -> h - dmg) . runBS

incTurn :: BattleStatus -> BattleStatus
incTurn = BS . over _2 (+ 1) . runBS

addMsg :: Text -> BattleStatus -> BattleStatus
addMsg msg = BS . over _3 (++ [msg]) . runBS

initialBattleStatus :: Int -> BattleStatus
initialBattleStatus initHP = BS (initHP, 0, [])

type BattleStateT m a = StateT BattleStatus m a

damageHPS :: MonadState BattleStatus m => Int -> m ()
damageHPS dmg = modify' (damageHP dmg)

incTurnS :: MonadState BattleStatus m => m ()
incTurnS = modify' incTurn

addMsgS :: MonadState BattleStatus m => Text -> m ()
addMsgS msg = modify' (addMsg msg)
