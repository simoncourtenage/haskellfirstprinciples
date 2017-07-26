module OptionalMonoid where

import Data.Monoid
import Test.QuickCheck
import Optional

-- properties (from Haskell book)

monoidAssoc :: (Eq m,Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m,Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m,Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- code from haskell book
-- objective is to write a Monoid instance for Optional type
-- that doesn't require a Monoid instance for the contents of
-- the type

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq,Show)

-- add new code for definitions of 'mempty' and 'mappend'
-- 'mappend' defined to return first 'success' value unless both are Nada
instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) y = y
  mappend x _ = x

-- new code to allow us to use quickCheck on First'
genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  a <- arbitrary
  return (First' a)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
