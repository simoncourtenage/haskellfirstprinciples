module LibraryFunctions where

import Data.Monoid

{--
  Exercises to implement library functions in terms of Foldable's foldr and foldMap.
--}

-- Exercise 1

{--
  We map the Sum data constructor over the structure and fold using the default monoid for Sum values
  (which is (+)).  When the final folded value is available, we use getSum to extract the result.
--}

sum :: (Foldable t,Num a) => t a -> a
sum as = getSum $ foldMap Sum as

-- Exercise 2

product :: (Foldable t, Num a) => t a -> a
product as = getProduct $ foldMap Product as

-- Exercise 3

elem :: (Foldable t,Eq a) => a -> t a -> Bool
elem a as = foldr (\x b -> b || x == a) False as

-- Exercise 4

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum as = foldr f Nothing as
  where
    f x Nothing              = Just x
    f x (Just y) | x < y     = Just x
                 | otherwise = Just y

-- Exercise 5

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum as = foldr f Nothing as
  where
    f x Nothing              = Just x
    f x (Just y) | x > y     = Just x
                 | otherwise = Just y

-- Exercise 6

{--
  Turn the structure into a list and ask if the length of the list is 0.
  Not terribly smart, particularly if the argument is actually a list, but
  given that all you know about the argument is that it's Foldable, I couldn't
  think of an alternative.
--}

null :: (Foldable t) => t a -> Bool
null a = length (foldr (\h t -> h:t) [] a) == 0

-- Exercise 7

length :: (Foldable t) => t a -> Int
length as = foldr (\x y -> y + 1) 0 as

-- Exercise 8

toList :: (Foldable t) => t a -> [a]
toList as = foldr (\h t -> h:t) [] as

-- Exercise 9

{--
  This also works:
     foldr (\x y -> x <> y) mempty as
--}

fold :: (Foldable t, Monoid m) => t m -> m
fold as = foldMap (<> mempty) as

-- Exercise 10

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f as = foldr (\x y -> f x <> y) mempty as



