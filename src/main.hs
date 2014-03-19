module Main where

import Data.Aeson
import qualified System.IO.Streams as Streams
import Network.Http.Client
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Internal (c2w)
import Control.Applicative((<$>))
import Control.Applicative((<*>))
import Control.Monad(mzero)
import qualified Data.Text as T
import Data.Vector(toList)

import System.Random (newStdGen, randomRIO)
import System.Random.Shuffle (shuffle')

-- http://www.reddit.com/r/haskell/comments/1zy7ut/how_would_this_program_be_implemented_in_haskell/

shuffleIO :: [a] -> IO [a]
shuffleIO xs = shuffle' xs (length xs) <$> newStdGen

chooseIO xs = head <$> shuffleIO xs

data Pokemon = Pokemon {
             name :: String,
             moves :: MoveList,
             atk :: Integer,
             spAtk :: Integer
             }

data MoveList = Moves [Move] | PotentialMoves [IO Move]

data Move = Move { accuracy :: Integer, power :: Integer, moveName :: String }

instance FromJSON Move where
        parseJSON (Object v) = Move <$> v .: T.pack "accuracy" <*> v .: T.pack "power" <*> v .: T.pack "name"
        parseJSON _ = mzero

instance FromJSON MoveList where
        parseJSON (Array v) = PotentialMoves <$> getMovesFunction
            where getMovesFunction = map getMove <$> mapM uri (toList v)
                  uri (Object o) = o .: T.pack "resource_uri"
                  uri _ = mzero
        parseJSON _ = mzero

instance FromJSON Pokemon where
        parseJSON (Object v) = Pokemon <$>
                                v .: T.pack "name" <*>
                                v .: T.pack "moves" <*>
                                v .: T.pack "attack" <*>
                                v .: T.pack "sp_atk"
        parseJSON _ = mzero


getRandomPkmn :: IO Pokemon
getRandomPkmn = do
            rInt <- randomRIO (1, 152) :: IO Int
            json <- apiRequest $ "/api/v1/pokemon/" ++ show rInt ++ "/"
            return $ maybe (error "har") id $ decode json

getMove :: String -> IO Move
getMove uri = do
        json <- apiRequest uri
        return $ maybe (error "hor") id $ decode json

apiRequest :: String -> IO BL.ByteString
apiRequest uri =
        let uri' = B.pack $ "http://viclib.com/pokemon?query=" ++ uri
        in convertToLazyB <$> get uri' concatHandler

convertToLazyB :: B.ByteString -> BL.ByteString
convertToLazyB strict = BL.fromChunks [strict]

downloadMoves :: Pokemon -> IO Pokemon
downloadMoves pkmn @ (Pokemon { moves = PotentialMoves getFuncs }) = do
        let randomedMoves = shuffleIO getFuncs
        moves <- sequence =<< take 4 <$> randomedMoves
        return $ pkmn { moves = Moves moves}

downloadMoves pkm = return pkm


data DamageResult = Damage Integer | Missed
instance Show DamageResult where
        show (Damage amt) = "dealing " ++ show amt ++ " damage!"
        show (Missed) = "but it missed!"

calculateDamage :: Pokemon -> Move -> IO DamageResult
calculateDamage  pkmn move =
        let pkmAtkDmg = randomRIO (0,  floor $ fromIntegral ((atk pkmn + spAtk pkmn) * (power move)) / 300)
            didItHit = (>= accuracy move) <$> randomRIO (0, 200) 
        in do
            hit <- didItHit
            if hit then Damage <$> pkmAtkDmg
                   else return Missed

printResult :: Pokemon -> Move -> DamageResult -> String
printResult pkm move dmg =
        name pkm ++ " attacked you with " ++ moveName move ++ " " ++ show dmg

main = do
    pkmn <- getRandomPkmn
    pkWithMoves <- downloadMoves pkmn
    putStrLn $ "Wild " ++ name pkmn ++ " appeared!"
    battle pkWithMoves 200 0

battle :: Pokemon -> Integer -> Integer -> IO ()
battle pk hp moveCount = do
        if (hp <= 0)
            then putStrLn $ name pk ++ " defeated you in " ++ show moveCount ++ " moves!"
            else do
                move <- case moves pk of Moves has -> chooseIO has
                dmg <- calculateDamage pk move
                putStrLn $ printResult pk move dmg
                battle pk (remaningHP hp dmg) (moveCount + 1)
        where remaningHP hp' (Damage amt) = hp' - amt
              remaningHP hp' (Missed) = hp'

