module ChapterExercises where

{--
  Exercises at end of Chapter 22 of Haskell Programming from First Principles (v.0.12).
--}

import Control.Applicative
import Data.Maybe

-- this first part is thr 'warming up' section

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer,Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer,Maybe Integer)
x3 n = (z' n,z' n)

summed :: Num c => (c,c) -> c
summed = uncurry (+)

-- lift boolean function over two partially-applied functions
bolt :: Integer -> Bool
-- exercise comment: "use &&, >3, <8"
bolt = liftA2 (&&) (>3) (<8)

-- from the exercise description
main :: IO ()
main = do
  print $ sequenceA [Just 3,Just 2, Just 1]
  print $ sequenceA [x,y]
  print $ sequenceA [xs,ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3),(<8),even] 7
  -- here are the actual exercises
  -- exercise 1
  print $ foldr (&&) True $ sequA 6
  -- exercise 2
  print $ sequA . fromMaybe 0 $ s'
  -- exercise 3
  print $ bolt . fromMaybe 0 $ ys

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3),(<8),even] m

s' = summed <$> ((,) <$> xs <*> ys)

