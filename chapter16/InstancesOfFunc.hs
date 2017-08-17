module InstancesOfFunc where

{--

These exercises are found in Chapter 16 on page 678 of
Haskell Programming from First Principles.  Each exercise
asks you to define Functor instances for each type and
use QuickCheck to validate the identity and composability
properties of the Functor instances.

--}

import Test.QuickCheck
import Test.QuickCheck.Function

-- First, from the book, the functions to perform checking of id and comp laws

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap (g . f) x) == (fmap g (fmap f x))

functorCompose' :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g)
   = (fmap g . fmap f $ x) == fmap (g . f) x



-- Exercise 1

newtype Identity a = Identity a deriving (Eq,Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

type IntToInt = Fun Int Int
type IdFC = Identity Int -> IntToInt -> IntToInt -> Bool

-- Exercise 2

data Pair a = Pair a a deriving (Eq,Show)

{--
   Although the data constructor takes two arguments, we can use fmap over each (if we so choose)
   - the restriction to not touch the first parameter (as mentioned in Section 16.8 on page 673)
   only applies to the type constructor (which, in this case, has only one parameter).
--}
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

-- Exercise 3

data Two a b = Two a b deriving (Eq,Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a,Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoFC = Two String Int -> IntToInt -> IntToInt -> Bool

-- Exercise 4

data Three a b c = Three a b c deriving (Eq,Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeFC = Three String Bool Int -> IntToInt -> IntToInt -> Bool

-- Exercise 5

data Three' a b = Three' a b b deriving (Eq,Show)

{--
  We can apply fmap to the 2nd, as well as the third parameter of the data constructor Three',
  because the type of both parameters is the second type parameter to the type constructor.
--}
instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

type ThreeFC' = Three' String Int -> IntToInt -> IntToInt -> Bool

-- Exercises 6 and 7 are essentially variations on 4 and 5.  In Exercise 7, you can only
-- apply fmap to the last parameter to the data constructor because it is the only parameter
-- typed by the second type parameter.
-- Exercise 8 - no you can't because the kindedness of Trivial is *, not * -> * (as required by Functor).

-- main function to perform tests

main :: IO ()
main = do
  putStrLn "Exercise 1"
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: IdFC)
  putStrLn "Exercise 2"
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose' :: PairFC)
  putStrLn "Exercise 3"
  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck (functorCompose' :: TwoFC)
  putStrLn "Exercise 4"
  quickCheck (functorIdentity :: Three String String Int -> Bool)
  quickCheck (functorCompose' :: ThreeFC)
  putStrLn "Exercise 5"
  quickCheck (functorIdentity :: Three' String Int -> Bool)
  quickCheck (functorCompose' :: ThreeFC')
 
