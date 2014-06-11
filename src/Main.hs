{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Types
import Client
import System.Random (newStdGen, randomRIO)
import System.Random.Shuffle (shuffle', shuffleM)

import Data.Word

import Control.Applicative((<$>))
import Control.Monad.Random
import Control.Monad.Writer
import Control.Applicative

choose :: (RandomGen g, Monad m, Functor m) => [a] -> RandT g m a
choose xs = head <$> shuffleM xs

-- Get a random pokemon from the original 152
getRandomPkmn :: RandomGen g => Rand g (IO Pokemon)
getRandomPkmn = do
            randomInt <- getRandomR(1, 152)
            return $ getPokemonById randomInt

-- Given a pokemon, return it with 4 randomly downloaded moves
downloadMoves :: Pokemon -> IO Pokemon
downloadMoves pkm @ (Pokemon { moves = PotentialMoves funcGetters }) = do
            let randMoves = shuffleM funcGetters -- Randomize the getters
            moves <- sequence =<< take 4 <$> randMoves -- Pick 4 from the random getters and download them
            return $ pkm { moves = Moves moves } -- Return the same pokemon with the downloaded moves
downloadMoves pkm = return pkm -- If moves already downloaded, then just get them

getReadyRandomPkm :: RandomGen g => g -> IO Pokemon
getReadyRandomPkm gen = (evalRand getRandomPkmn gen) >>= downloadMoves

data DamageResult = Damage Int | Missed
instance Show DamageResult where
        show (Damage amt) = "dealing " ++ show amt ++ " damage!"
        show (Missed) = "but it missed!"

calculateDamage :: (RandomGen g, Monad m, Functor m) => Pokemon -> Move -> RandT g m DamageResult
calculateDamage  pkmn move = do
            let top = floor $ fromIntegral ((attack pkmn + spAtk pkmn) * power move) / 300
            pkmAtkDmg <- getRandomR (0, top)
            didItHit <- (>= accuracy move) <$> getRandomR (0, 200)
            if didItHit then return $ Damage pkmAtkDmg
                        else return Missed

main = do
  gen <- newStdGen
  pk <- getReadyRandomPkm gen
  putStrLn $ "A wild " ++ pokemonName pk ++ " has appeared!"
  let ms = case moves pk of { Moves m -> m }
      bRes = battle 100 0 pk ms
      reports = execWriter $ evalRandT bRes gen
  mapM_ putStrLn reports

type HP = Int
type MoveCount = Int
type MoveSet = [Move]
type WriterRand g a = RandT g (Writer [String]) a

battle :: RandomGen g => HP -> MoveCount -> Pokemon -> MoveSet -> WriterRand g ()
battle hp count pkm moves = battle' hp count
  where
    battle' hp count =
                    if hp <= 0 then tell ["You were defeated in " ++ (show count) ++ " moves!"]
                    else do
                      move <- choose moves
                      dmg <- calculateDamage pkm move
                      tell [printResult (pokemonName pkm) (moveName move) dmg]
                      battle' (calculateHp hp dmg) (count + 1)
                          where calculateHp hp' (Missed) = hp'
                                calculateHp hp' (Damage dmg) = hp' - dmg
                                printResult pkName mvName dmg =
                                    pkName ++ " attacked you with " ++ mvName ++ " " ++ show dmg
