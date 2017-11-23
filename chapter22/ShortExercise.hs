module ShortExercise where

import Data.Char
import Control.Applicative (liftA2)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char],[Char])
tupled = (,) <$> rev <*> cap

tupledLift :: [Char] -> ([Char],[Char])
tupledLift = liftA2 (,) rev cap

tupledM :: [Char] -> ([Char],[Char])
tupledM = do
  a <- rev
  b <- cap
  return (a,b)
  
-- this is just a translation of do-syntax into >>=
tupledM' :: [Char] -> ([Char],[Char])
tupledM' = rev >>= (\a -> cap >>= \b -> return (a,b))
