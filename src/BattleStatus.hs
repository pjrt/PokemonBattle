{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module BattleStatus where

import Control.Lens
import Data.Text (Text)

-- | BattleStatus represents the current state of the battle.
data BattleStatus = BS { _bsHP           :: Int
                       , _bsTurn         :: Word
                       , _bsMessages     :: [Text]
                       , _bsPrevHitCount :: Word
                       }

makeLenses ''BattleStatus

-- | The initial state of the battle.
initialBattleStatus :: Int -> BattleStatus
initialBattleStatus initHP = BS initHP 0 [] 0
