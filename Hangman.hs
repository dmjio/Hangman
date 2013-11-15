{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.IO 
import Data.List
import Data.Char
import Control.Monad.RWS
import qualified Data.Set as S
import Control.Lens
import Data.Default (Default(..))
import Text.Printf (printf)
import Control.Applicative 
import System.Time (getClockTime)

-- Type Synonyms
type HiddenWord = String -- To represent 
type Log        = String -- To log guesses as they happen
type Hangman    = RWST HiddenWord String GameState IO Result -- Our 'global' state

-- Data Declarations
data Result     = Win | Lose 
data GameState  = GameState { _guesses      :: S.Set Char -- * Only correct guesses
                            , _guessesLeft  :: Int
                            } deriving (Show)
-- Lensification
$(makeLenses ''GameState)

-- Instances
instance Default GameState where def = GameState S.empty 6

-- original sig = RWST r w s m a -> r -> s -> m (a, s, w), do you see how these fit?
playHangman :: Hangman -> HiddenWord -> GameState -> IO (Result, GameState, Log)
playHangman = runRWST

-- | Helper functions
io :: MonadIO m => IO a -> m a
io = liftIO

check :: Bool -> Hangman -> Hangman -> Hangman
check p x y = if p then x else y -- can't use when since it returns m ()

-- | Main game loop
gameLoop :: Hangman -- We must return a 'Result' type here since it is our 'a'
gameLoop = do game <- get
              check (game ^. guessesLeft == 0) (return Lose) $ handleGame game
    where handleGame game = do
            hiddenWord <- ask
            check (game ^. guesses == S.fromList hiddenWord) (return Win) $ handleGuess game
          handleGuess game = do 
            hiddenWord <- ask
            (time,guess) <- io $ do
                      printf "You have %d guesses left\n" $ game ^. guessesLeft
                      printf "%s\n" $ intersperse ' ' $ 
                             map (\letter -> if S.member letter $ game ^. guesses
                                             then letter
                                             else '_') hiddenWord
                      putStr "Guess a letter: " >> hFlush stdout
                      time <- io $ (head . reverse . drop 2 . reverse . drop 3 . words . show) <$> getClockTime
                      guess <- head <$> getLine
                      (,) <$> pure time <*> pure guess
            if S.member guess $ S.fromList hiddenWord
            then do put $ game & guesses %~ S.insert guess
                    tell $ printf "%c - Correct - %s\n" guess $ show time
            else do put $ game & guessesLeft -~ 1
                    tell $ printf "%c - InCorrect - at %s\n" guess $ show time
            gameLoop

main :: IO ()              
main = do
  putStrLn "Welcome to Haskell Hangman!\n\ 
           \Enter a word to guess"        -- Multi-Line strings
  word <- fmap toLower <$> getLine        -- (fmap . fmap) toLower getLine or (fmap fmap fmap) toLower getLine
  case word of
    [] -> putStrLn "You didn't enter anything" -- cheap validation
    otherwise -> do 
         (res, _, log) <- playHangman gameLoop word def  
         case res of
           Win ->  putStrLn "Congratulations You Won!"
           Lose -> putStrLn "Too bad you lost :( "
         putStrLn "Game log:"
         putStrLn log
         putStrLn "Play again? (y/n)"
         getLine >>= \case -- lambda-case usage
                 "y" -> hFlush stdout >> main
                 otherwise -> putStrLn "Thank you for playing!"
         
