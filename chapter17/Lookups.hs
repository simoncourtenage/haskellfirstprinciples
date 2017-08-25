module Lookups where

import Data.List (elemIndex)

{--
   Lookup exercises from Chapter 17, page 684 of Haskell Programming from
   First Principles (version 0.12.0)
--}

-- Exercise 1

{--
  Zip the two lists together to get [(1,4),(2,5),(3,6)]; then lookup 3 to get "Just 6" - then
  use infix fmap to lift (+3) over the Maybe structure and apply it to the contained value.
--}

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- Exercise 2

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer,Integer)
tupled = (,) <$> y <*> z

-- Exercise 3

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

-- renamed to avoid clash with 'y' in exercise 2
y_ :: Maybe Int
y_ = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

-- this is slightly artificial - I originally had
--    max' <$> x <*> y_
-- but this version demonstrates a use of 'pure'
maxed :: Maybe Int
maxed =  pure max' <*> x <*> y_

-- Exercise 4

xs = [1,2,3]
ys = [4,5,6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys


y' :: Maybe Integer
y' = lookup 2 $ zip xs ys

-- this only returns the second element of the tuple
-- which is the behaviour of sum over tuples
summed :: Maybe Integer
summed = fmap sum  $ (,) <$> x' <*> y'

