module Palindrome where

import Data.Char (toLower,isAlphaNum)
import Control.Monad

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  -- filter out any non alphanumeric characters and translate all characters to lower-case
  -- (allows "Madam I'm Adam" to be considered a palindrome)
  let line = map toLower . filter isAlphaNum $ line1
  case (line == reverse line) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"

