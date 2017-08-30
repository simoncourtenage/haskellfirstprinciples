module ChapterExercises where

import Data.Monoid
import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{--
  Answers to Chapter 17 exercises of Haskell Programming from First Principles.
--}

-- Type specializations

-- Exercise 1

-- pure :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]

-- Exercise 2

-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- Exercise 3

-- pure :: Monoid a => b -> ((,) a) b
-- (<*>) :: Monoid a => ((,) a) (b -> c) -> ((,) a) b -> ((,) a) c

-- Exercise 4

-- pure :: a -> ((->) e) a
-- (<*>) :: ((->) e) (a -> b) -> ((->) e) a -> ((->) e) b

-- Applicative instances

-- Exercise 1

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

{--
  The definition of EqProp for Pair says that two pairs are equal if their
  respective pairs of values are equal.  The (.&&.) operator combines, using
  conjunction, the two Property values produced by eq.
--}
instance Eq a => EqProp (Pair a) where
  (=-=) (Pair a a') (Pair b b') = a `eq` b .&&. a' `eq` b'

checkPair :: IO ()
checkPair = quickBatch $ applicative (undefined :: Pair (Int,Int,Int))


-- Exercise 2

data Two a b = Two a b deriving Show

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

{--
  This briefly gave me a headache, until I remembered the discussion of applicative
  for two-tuple on page 677.  Because the first type variable, a, is bound in the instance
  header, it can't change on the RHS of the <*> definition.  So my firt attempt was
     (<*>) (Two a g) (Two a' b) = Two a' (g b)
  but this failed the interchange law check.  But the discussion on page 677 reminded me
  that function application in applicative only relates to the unbound type variable, so
  the first argument to the first Two is a value you need to treat as a function to
  be applied.  Instead, because the type context says it must be a monoid, just combine
  the two first Two arguments using monoidal <>.
--}
instance Monoid a =>  Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a g) (Two a' b) = Two (a <> a') (g b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a,Eq b) => EqProp (Two a b) where
  (=-=) (Two a a') (Two b b') = a `eq` b .&&. a' `eq` b'

checkTwo :: IO ()
checkTwo = quickBatch $ applicative (undefined :: Two String (Int,Int,Int))

-- Exercise 3

data Three a b c = Three a b c deriving Show

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a,Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a,Eq b,Eq c) => EqProp (Three a b c) where
  (=-=) (Three a b c) (Three a' b' c') = a `eq` a' .&&. b `eq` b' .&&. c `eq` c'

checkThree :: IO ()
checkThree = quickBatch $ applicative (undefined :: Three String String (Int,Int,Int))

-- Exercise 4

data Three' a b = Three' a b b deriving Show

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  (<*>) (Three' a f g) (Three' a' b c) = Three' (a <> a') (f b) (g c)

instance (Arbitrary a,Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) (Three' a b c) (Three' a' b' c') = a `eq` a' .&&. b `eq` b' .&&. c `eq` c'

checkThree' :: IO ()
checkThree' = quickBatch $ applicative (undefined :: Three' String (Int,Int,Int))

-- Exercises 5 and 6 are similar to 3 and 4

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

{--
  Uses the three tuple operator as the function to apply over elements from the three lists.
  To run, type
     combos stops vowels stops
  The result looks exactly like the results of list comprehension, i.e., the result is produced
  as [('p','a','p'),('p','a','b'),('p','a','t'),...] etc.

--}
combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (,,)

