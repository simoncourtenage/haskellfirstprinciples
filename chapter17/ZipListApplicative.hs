module ZipListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Data.Monoid

data List a = Nil | Cons a (List a) deriving (Eq,Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' n Nil = Nil
take' n (Cons h t) = Cons h (take' (n-1) t)


append :: List a -> List a -> List a
append Nil l = l
append l Nil = l
append (Cons a as) l = Cons a $ as `append` l

{--
  From the book - a version of left fold over the List type
--}

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs as = flatMap (<$> as) fs

newtype ZipList' a = ZipList' (List a) deriving (Eq,Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

{--
  One of the key things to remember here is the behaviour of the 'zip' function - which zipList' is meant
  to replicate.  'zip' only zips up to the length of the shortest list.  So if you have
     Cons (+1) (Cons (*2) Nil) <*> Cons 1 (Cons 2 (Cons 3 Nil))
  - a list of 2 functions but 3 values - then you get
     Cons 2 (Cons 4 Nil)
  This fact is important to bear in mind for the identity law.  This says that
     pure id <*> v = v
  If we define for the Applicative
     pure a = ZipList (Cons a Nil)
  (as I first did), then we end up with
     Cons id Nil) <*> Cons 1 (Cons 2 (Cons 3 Nil))
  which gives us
     Cons 1 Nil
  as the result.  This fails the identity law because
     pure id <*> Cons 1 (Cons 2 (Cons 3 Nil)) != Cons 1 (Cons 2 (Cons 3 Nil))
--}
instance Applicative ZipList' where
  pure a = ZipList' (repeat' a)
  (<*>) (ZipList' fs) (ZipList' as) = ZipList' $ zipApp fs as

{--
  zipApp - which borrows heavily from the zip functions in the Prelude, apropos
  the hint in the exercise, runs down each list, applying a function from the
  first list to a value from the second list, each in the same position in their
  respective lists, to produce a value in that position in the output list.
--}
zipApp :: List (b -> c) -> List b -> List c
zipApp Nil _ = Nil
zipApp _ Nil = Nil
zipApp (Cons f fs) (Cons b bs) = Cons (f b) (zipApp fs bs)

{--
  repeat' simply produces infinite Lists - just as repeat does for [..] lists.
--}
repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

-- Sample values from exercise used as first test of code.  The exercise in the book
-- uses [..] syntax, but really they should be List values.
z = ZipList' (Cons (+9) (Cons (*2) (Cons (+8) Nil)))
z' = ZipList' (Cons 1 (Cons 2 (Cons 3 Nil)))
z'' = ZipList' (repeat' 1)

-- From the List Applicative example.  Needed so that the
-- Arbitrary ZipList' instance works.
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1,return Nil),(3, return $ Cons a b)]

instance Arbitrary a => Arbitrary (ZipList' a)  where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a

check :: IO ()
check = quickBatch $ applicative (undefined :: ZipList' (Int,Int,Int))

