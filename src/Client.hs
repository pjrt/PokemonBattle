module Client(getPokemonById,
              getAbilityById,
              getMoveById
) where

import Types
import Resource(getResource)
import Data.Word
import Data.Aeson(FromJSON)

type DBID = Word

getPokemonById :: DBID -> IO Pokemon
getPokemonById = getResourceById "pokemon"

getAbilityById :: DBID -> IO Ability
getAbilityById = getResourceById "ability"

getMoveById :: DBID -> IO Move
getMoveById = getResourceById "move"



-- Utility wrapper for `ById` functions
getResourceById :: FromJSON a => String -> DBID -> IO a
getResourceById res dbId = getResource $ "api/v1/" ++ res ++ "/" ++ show dbId ++ "/"

