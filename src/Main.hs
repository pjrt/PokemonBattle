{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, MultiWayIf #-}
module Main where

import BattleStatus

import Haskmon
import System.Random.Shuffle (shuffleM)
import Control.Lens (ASetter, view, over)
import Control.Monad.State
import Control.Monad.Random

import Data.Monoid ((<>))
import qualified Data.Text as T

-- | App type. A composition of Random and StateT
newtype PB m a = PB { runPB :: RandT StdGen (StateT BattleStatus m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadState BattleStatus)

-- Get a random pokemon from the original 152
getRandomPkmn :: MonadIO m => PB m Pokemon
getRandomPkmn = do
  randomInt <- getRandomR (1, 152)
  liftIO $ getPokemonById randomInt

-- Given a pokemon, get 4 random moves from it
downloadMoves :: MonadIO m => Pokemon -> PB m [Move]
downloadMoves pkm = do
  let metaMoves = pokemonMoves pkm
  randMoves <- shuffleM metaMoves -- Randomize the getters
  liftIO (mapM getMove $ take 4 randMoves) -- Pick 4 from the random getters and download them

data DamageResult = Damage Int | Missed
instance Show DamageResult where
  show (Damage amt) = "dealing " ++ show amt ++ " damage!"
  show Missed = "but it missed!"

-- | Given a pokemon and a move, calculate, randomly, how much damage that
-- move will do.
--
-- Sadly, pokeapi.co doesn't seem to have move categories for any of the
-- moves, so we can't properly calculate the damage. We will just add the
-- pokemon Attack and Special Attack stats to any move.
--
-- Additionally, accuracy is a plain check with a growing probability for
-- each miss. This is different than the actual pokemon damage equation since
-- you don't have any stats and pure randomness is boring.
--
-- Another difference is the damage scale. Here were are picking the damage
-- to be between 0 and some top value. In reality, damage in pokemon games
-- are based on the defense of the defendes (your stats that you don't have),
-- the type of attack vs the type of the defender, the base and the level.
-- We don't have that yet, so for now it is that simple, kind of arbitrary
-- equation.
calculateDamage :: Monad m => Pokemon -> Move -> PB m DamageResult
calculateDamage pkmn move = do
  let pkAtk  = (+) <$> pokemonAttack <*> pokemonSpAtk $ pkmn
      top = floor (fromIntegral (pkAtk * movePower move) / 300 :: Double)
  pkmAtkDmg <- getRandomR (0, top)
  prevHitC <- view bsPrevHitCount <$> get
  let modAcc = prevHitC * 5
  didItHit <- (moveAccuracy move + modAcc >=) <$> getRandomR (0, 100)
  if didItHit then resetHitCount >> return (Damage pkmAtkDmg)
              else incHitCount   >> return Missed
  where
    resetHitCount = modBS bsPrevHitCount (const 0)
    incHitCount   = modBS bsPrevHitCount (+ 1)

main :: IO ()
main = do
  gen <- newStdGen
  battleReport <- view bsMessages <$> execStateT (evalRandT play gen) (initialBattleStatus 100)
  mapM_ (putStrLn . T.unpack) battleReport
    where play = runPB $ do pk <- getRandomPkmn
                            moves <- downloadMoves pk
                            battle pk moves

type HP = Int
type MoveCount = Int
type MoveSet = [Move]

-- | A battle consists of an initial HP (your HP), a count, a pokemon and
-- a moveset from said pokemon. The pokemon will continusly attack you with
-- one of the moves from the moveset until you die, counting how many moves
-- it took.
battle :: Monad m => Pokemon -> MoveSet -> PB m ()
battle pkm moves = do
  addMsgS $ "A wild " <> showT (pokemonName pkm) <> " has appeared!"
  battle'
  where
    addMsgS msg = modBS bsMessages (++ [msg])
    battle' = do
      (BS hp turn _ _) <- get
      if | hp <= 0 ->
             addMsgS $ "You were defeated in " <> showT turn <> " moves!"
         | turn > 50 ->
             addMsgS "You survived over 50 turns. You win!"
         | otherwise -> do
            move <- choose moves
            dmg <- calculateDamage pkm move
            addMsgS $ printResult (pokemonName pkm) (moveName move) dmg
            updateHP dmg
            incTurnS
            battle'
      where printResult pkName mvName dmg =
              T.pack pkName <> " attacked you with " <> T.pack mvName <> " " <> showT dmg
            updateHP Missed = return () -- do nothing
            updateHP (Damage dmg) = damageHPS dmg

            damageHPS dmg = modBS bsHP (\h -> h - dmg)
            incTurnS = modBS bsTurn (+ 1)

-- Utility for choosing a move from a MoveSet
choose :: MonadRandom m => [a] -> m a
choose xs = head <$> shuffleM xs

showT :: Show a => a -> T.Text
showT = T.pack . show

-- | Helper function for modifying the battle status inside a PB using
-- lenses
modBS :: Monad m => ASetter BattleStatus BattleStatus a b -> (a -> b) -> PB m ()
modBS l f = modify' (over l f)
