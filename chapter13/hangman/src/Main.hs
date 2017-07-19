module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- most of this code is given in Chapter 13 of Haskell Programming from First Principles
-- except for a few simple functions and the condition in gameOver

-- type WordList = [String]
newtype WordList = WordList [String]
  deriving (Eq,Show)


allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length w
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0,length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w d g) c | elem c w  = True
                            | otherwise = False

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle w d g) c | elem c g  = True
                                | otherwise = False

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c
  = Puzzle word newFilledInSoFar (c:s)
  where zipper guessed wordChar guessChar | guessed == wordChar = Just wordChar
                                          | otherwise           = guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_,True) -> do
      putStrLn "Already guessed - pick something else"
      return puzzle
    (True,_) -> do
      putStrLn "Character in word - updating puzzle"
      return (fillInCharacter puzzle guess)
    (False,_) -> do
      putStrLn "Not in word - try again"
      return (fillInCharacter puzzle guess)

-- the condition calculate the number of incorrect guesses by subtracting the number
-- of filled-in letters from the total number of guesses 
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess discovered guessed)
  = if (length guessed - (length . filter isJust $ discovered)) > 7 then
      do
        putStrLn "You lose!"
        putStrLn $ "The word was " ++ wordToGuess
        exitSuccess
    else
      return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _)
  = if all isJust filledInSoFar then
      do putStrLn "You win!"
         exitSuccess
    else
      return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
  
