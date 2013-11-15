{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where -- Only export what we need to

import System.IO (hFlush, stdout)
import Data.List (intersperse)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Control.Monad.RWS (RWST(..), 
                              put,  -- * State  Monad
                              get,  -- * State  Monad
                              ask,  -- * Reader Monad
                              tell, -- * Writer Monad
                              MonadIO(..))
import qualified Data.Set as S
import Control.Lens (makeLenses, (^.), (-~), (&), (%~))
import Data.Default (Default(def))
import Text.Printf (printf)
import Control.Applicative ((<*>),(<$>), pure)
import System.Time (getClockTime)

-- Type Synonyms
type HiddenWord = String -- To represent the word needed for guessing.
type Log        = String -- To log guesses as they happen
type Hangman    = RWST HiddenWord Log GameState IO Result -- Our 'global' state

-- Data Declarations
data Result     = Win | Lose 
data GameState  = GameState { _guesses      :: S.Set Char -- * Only correct guesses
                            , _guessesLeft  :: Int
                            } deriving (Show)
-- Lensification
$(makeLenses ''GameState)

-- Instances
instance Default GameState where def = GameState S.empty 6

-- Original sig = RWST r w s m a -> r -> s -> m (a, s, w), do you see how these fit?
-- | Game execution method
playHangman :: Hangman -> HiddenWord -> GameState -> IO (Result, GameState, Log)
playHangman = runRWST

-- | Helper function
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Main game loop
gameLoop :: Hangman -- We must return a 'Result' type here since it is our 'a'
gameLoop = do (game, word) <- (,) <$> get <*> ask
              if | game ^. guessesLeft == 0               -> return Lose    -- Losing base case
                 | game ^. guesses     == S.fromList word -> return Win     -- Winning base case
                 | otherwise -> 
                     do (time, guess) <- io $ getGuess game word -- Loop base case
                        if | S.member guess $ S.fromList word -> -- If guess is correct
                               do put $ game & guesses %~ S.insert guess  -- add to correct guess Set
                                  tell $ printf "%c - Correct - at %s\n" guess $ show time -- log guess
                           | otherwise -> --o/w == True
                               do put $ game & guessesLeft -~ 1 -- if incorrect, subtract from rem. guesses
                                  tell $ printf "%c - InCorrect - at %s\n" guess $ show time -- log guess
                        gameLoop 

-- | Guess sub-method
getGuess :: GameState -> String -> IO (String, Char)
getGuess game word = do 
  printf "You have %d guesses left\n%s\n" (game ^. guessesLeft) (display game word)
  putStr "Guess a letter: " >> hFlush stdout
  guess <- getLine
  let g | guess == [] = ' ' | True = head guess -- cheap safe head
  (,) <$> time <*> pure g -- applicative
      where 
        time         = head . reverse . drop 2 . reverse . drop 3 . words . show <$> getClockTime 
        display game = intersperse ' ' . map (\letter -> if S.member letter $ game ^. guesses then letter else '_')

main :: IO ()              
main = do
  putStrLn "Welcome to Haskell Hangman!\n\ 
           \Enter a word to guess"        -- Multi-Line strings
  word <- fmap toLower <$> getLine        -- (fmap . fmap) toLower getLine or (fmap fmap fmap) toLower getLine
  case word of
    [] -> putStrLn "You didn't enter anything" -- cheap validation
    otherwise -> do 
         (result, _, log) <- playHangman gameLoop word def  -- Where the men are hung
         case result of
           Win  -> putStrLn "Congratulations You Won!"
           Lose -> putStrLn "Too bad you lost :( "
         putStrLn $ "Word was: " <> word
         putStrLn "Game log:"
         putStrLn log
         putStrLn "Play again? (y/n)"
         getLine >>= \case -- lambda-case usage
                 "y" -> hFlush stdout >> main
                 otherwise -> putStrLn "Thank you for playing!"
