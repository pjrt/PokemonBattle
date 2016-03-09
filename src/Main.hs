{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Main where

import Haskmon
import System.Random.Shuffle (shuffleM)

import Control.Monad.Random
import Control.Monad.Writer

-- | App type. A composition of Random, Writer and some other monad (IO)
newtype PB g m a = PB { runPB :: RandT g (WriterT [String] m) a }
                   deriving (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadWriter [String])

-- Get a random pokemon from the original 152
getRandomPkmn :: (RandomGen g, MonadIO m, Functor m) => PB g m Pokemon
getRandomPkmn = do
            randomInt <- getRandomR (1, 152)
            liftIO $ getPokemonById randomInt

-- Given a pokemon, get 4 random moves from it
downloadMoves :: (RandomGen g, MonadIO m) => Pokemon -> PB g m [Move]
downloadMoves pkm = do
            let metaMoves = pokemonMoves pkm
            randMoves <- shuffleM metaMoves -- Randomize the getters
            liftIO (mapM getMove $ take 4 randMoves) -- Pick 4 from the random getters and download them

data DamageResult = Damage Int | Missed
instance Show DamageResult where
        show (Damage amt) = "dealing " ++ show amt ++ " damage!"
        show (Missed) = "but it missed!"

calculateDamage :: (RandomGen g, Monad m, Functor m) => Pokemon -> Move -> RandT g m DamageResult
calculateDamage  pkmn move = do
            let top = floor $ fromIntegral ((pokemonAttack pkmn + pokemonSpAtk pkmn) * movePower move) / 300
            pkmAtkDmg <- getRandomR (0, top)
            didItHit <- (>= moveAccuracy move) <$> getRandomR (0, 200)
            return $ if didItHit then Damage pkmAtkDmg else Missed

main = do
  gen <- newStdGen
  battleReport <- execWriterT (evalRandT (runPB go) gen)
  mapM_ putStrLn battleReport
    where go :: (RandomGen g, Monad m, Functor m, MonadIO m) => PB g m ()
          go = do
              pk <- getRandomPkmn
              moves <- downloadMoves pk
              playGame pk moves

type HP = Int
type MoveCount = Int
type MoveSet = [Move]

playGame :: (RandomGen g, Monad m, Functor m) => Pokemon -> MoveSet -> PB g m ()
playGame pk moves = do
  tell ["A wild " ++ pokemonName pk ++ " has appeared!"]
  battle 100 0 pk moves

battle :: (RandomGen g, Monad m, Functor m) => HP -> MoveCount -> Pokemon -> MoveSet -> PB g m ()
battle hp count pkm moves = PB (battle' hp count)
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

