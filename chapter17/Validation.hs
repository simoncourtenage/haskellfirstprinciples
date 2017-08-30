module Validation where

import Data.Monoid
-- need to qualify Test.QuickCheck because it imports Failure
-- into the namespace
import qualified Test.QuickCheck as TQ
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Failure e | Success a deriving (Eq,Show)

instance Functor (Validation e) where
  fmap f (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

{--
  This is fairly straightforward.  We use pattern matching on the Validation values
  for (<*>) to decide how to combine the Failure and Sucess arguments.  The only
  interesting case is for two Failures, where we use monoid <> to 'mappend' them
  depending on their actual types.
--}

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (<*>) (Failure f) (Success s) = Failure f
  (<*>) (Success s) (Failure f) = Failure f
  (<*>) (Success s) (Success s') = Success $ s s'
  (<*>) (Failure f) (Failure f') = Failure $ f <> f'

instance (TQ.Arbitrary e,TQ.Arbitrary a) => TQ.Arbitrary (Validation e a) where
  arbitrary = do
    e <- TQ.arbitrary
    a <- TQ.arbitrary
    TQ.oneof [return $ Failure e,return $ Success a]

instance (Eq e,Eq a) => EqProp (Validation e a) where
  (=-=) = eq

check :: IO ()
check = quickBatch $ applicative (undefined :: Validation String (Int,Int,Int))
    
