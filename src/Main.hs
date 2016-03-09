{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Main where

import Haskmon
import System.Random.Shuffle (shuffleM)

import Control.Monad.Random
import Control.Monad.Writer

-- | App type. A composition of Random, Writer and some other monad (IO)
newtype PB m a = PB { runPB :: RandT StdGen (WriterT [String] m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadWriter [String])

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

-- | Given a pokemin and a move, calculate, randomly, how much damage that
-- move will do.
calculateDamage :: (RandomGen g, Monad m) => Pokemon -> Move -> RandT g m DamageResult
calculateDamage pkmn move = do
            let pkAtk  = (+) <$> pokemonAttack <*> pokemonSpAtk $ pkmn
                top = floor (fromIntegral (pkAtk * movePower move) / 300 :: Double)
            pkmAtkDmg <- getRandomR (0, top)
            -- TODO: We are comparing it from 0 to 200 in order to make it
            -- more likely to hit. Though what we should do is implement
            -- a pseudo-random system instead of purely random. Purely random
            -- is too boring.
            didItHit <- (>= moveAccuracy move) <$> getRandomR (0, 200)
            return $ if didItHit then Damage pkmAtkDmg else Missed

main :: IO ()
main = do
  gen <- newStdGen
  battleReport <- execWriterT (evalRandT (runPB go) gen)
  mapM_ putStrLn battleReport
    where go = do pk <- getRandomPkmn
                  moves <- downloadMoves pk
                  playGame pk moves

type HP = Int
type MoveCount = Int
type MoveSet = [Move]

playGame :: Monad m => Pokemon -> MoveSet -> PB m ()
playGame pk moves = do
  tell ["A wild " ++ pokemonName pk ++ " has appeared!"]
  battle 100 0 pk moves

-- | A battle consists of an initial HP (your HP), a count, a pokemon and
-- a moveset from said pokemon. The pokemon will continusly attack you with
-- one of the moves from the moveset until you die, counting how many moves
-- it took.
battle :: Monad m => HP -> MoveCount -> Pokemon -> MoveSet -> PB m ()
battle h c pkm moves = PB $ battle' h c
  where
    battle' hp count =
                    if hp <= 0 then tell ["You were defeated in " ++ show count ++ " moves!"]
                    else do
                      move <- choose moves
                      dmg <- calculateDamage pkm move
                      tell [printResult (pokemonName pkm) (moveName move) dmg]
                      battle' (calculateHp hp dmg) (count + 1)
                          where calculateHp hp' Missed = hp'
                                calculateHp hp' (Damage dmg) = hp' - dmg
                                printResult pkName mvName dmg =
                                    pkName ++ " attacked you with " ++ mvName ++ " " ++ show dmg

-- Utility for choosing a move from a MoveSet
choose :: (RandomGen g, Monad m) => [a] -> RandT g m a
choose xs = head <$> shuffleM xs

