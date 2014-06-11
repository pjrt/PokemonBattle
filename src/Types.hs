{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Word
import Data.Vector(toList)
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Format
import Data.Time.Clock

import System.Locale

import Control.Applicative
import Control.Monad(mzero)

import Resource(getResource)

newtype Meta a = Meta { runMeta :: (MetaData, a)}

data MetaData = MetaData {
              resourceUri :: String,
              created :: UTCTime, -- Figure out what a good date time lib for haskell is
              modified :: UTCTime -- Same
              }

-- Don't repeat yourself
getMetadata :: Object -> Parser MetaData
getMetadata v = MetaData <$>
                    v .: "resource_uri" <*>
                    convert (v .: "created") <*>
                    convert (v .: "modified")
              where convert :: Parser String -> Parser UTCTime
                    convert ps = readTime defaultTimeLocale formatStr <$> ps
                    formatStr = "%FT%H:%M:%Q"

buildPotentialList :: FromJSON a => Array -> Parser [IO a]
buildPotentialList a = map getResource <$> mapM uri (toList a)
        where uri (Object o) = o .: "resource_uri"
              uri _ = mzero

-- Pokemon Data and FromJson instance
data Pokemon = Pokemon {
             pokemonName :: String,
             nationalId :: Int,
             abilities :: AbilityList,
             moves :: MoveList,
             types :: TypeList,
             catchRate :: Int,
             hp :: Int,
             attack :: Int,
             defense :: Int,
             spAtk :: Int,
             spDef :: Int,
             speed :: Int,
             metadata :: MetaData
             }

instance FromJSON Pokemon where
        parseJSON (Object v) = Pokemon <$>
                                v .: "name" <*>
                                v .: "national_id" <*>
                                v .: "abilities" <*>
                                v .: "moves" <*>
                                v .: "types" <*>
                                v .: "catch_rate" <*>
                                v .: "hp" <*>
                                v .: "attack" <*>
                                v .: "defense" <*>
                                v .: "sp_atk" <*>
                                v .: "sp_def" <*>
                                v .: "speed" <*>
                                getMetadata v
        parseJSON _ = mzero


-- Ability Data and FromJSON
data AbilityList = Abilities [Ability] | PotentialAbilities [IO Ability]
data Ability = Ability {
             abilityName :: String,
             abilityDescription :: String,
             abilityMetadata :: MetaData
             }

instance FromJSON Ability where
        parseJSON (Object v) = Ability <$>
                                v .: "name" <*>
                                v .: "description" <*>
                                getMetadata v

instance FromJSON AbilityList where
        parseJSON (Array a) = PotentialAbilities <$> buildPotentialList a
        parseJSON _ = mzero

data TypeList = Types [Type] | PotentialTypes [IO Type]
data Type = Type {
          typeName :: String,
          ineffective :: [Type],
          noEffect :: [Type],
          resistance :: [Type],
          superEffective :: [Type],
          weakness :: [Type],
          typeMetadata :: MetaData
          }

instance FromJSON Type where
        parseJSON (Object o) = Type <$>
                                o .: "name" <*>
                                o .: "ineffective" <*>
                                o .: "no_effect" <*>
                                o .: "resistance" <*>
                                o .: "super_effective" <*>
                                o .: "weakness" <*>
                                getMetadata o
        parseJSON _ = mzero

instance FromJSON TypeList where
        parseJSON (Array a) = PotentialTypes <$> buildPotentialList a
        parseJSON _ = mzero

data MoveList = Moves [Move] | PotentialMoves [IO Move]
data Move = Move {
          moveName :: String,
          power :: Int,
          pp :: Int,
          accuracy :: Int,
          moveMetadata :: MetaData
          }

instance FromJSON Move where
        parseJSON (Object o) = Move <$>
                                o .: "name" <*>
                                o .: "power" <*>
                                o .: "pp" <*>
                                o .: "accuracy" <*>
                                getMetadata o

instance FromJSON MoveList where
        parseJSON (Array a) = PotentialMoves <$> buildPotentialList a
