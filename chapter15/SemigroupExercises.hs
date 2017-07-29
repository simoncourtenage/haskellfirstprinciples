module SemigroupExercises where

-- code for Semigroup exercises in Chapter 15 of
-- Haskell Programming from First Principles

-- aim of these exercises is to implement the Semigroup instance and validate
-- using QuickCheck

import Data.Semigroup
import Test.QuickCheck (quickCheck,oneof,frequency,arbitrary,Arbitrary,Gen,sample')

-- exercise 1
data Trivial = Trivial deriving (Eq,Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semiGroupAssoc :: (Eq m,Semigroup m) => m -> m -> m -> Bool
semiGroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- exercise 2
newtype Identity a = Identity a deriving (Eq,Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

-- using other types, such as Bool or Int, results in an error that there exists no
-- semigroup instance for, e.g, Bool - this is because of the constraint we put on the
-- Semigroup instance
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- exercise 3

data Two a b = Two a b deriving (Eq,Show)

instance (Semigroup a,Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a,Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

genTwo :: (Arbitrary a,Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- exercises 4 and 5 are the same as for exercise 3, but with more type arguments

-- exercise 6

newtype BoolConj = BoolConj Bool deriving (Eq,Show)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _                = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = genBoolConj

genBoolConj :: Gen BoolConj
genBoolConj = do
  oneof [return $ BoolConj True,return $ BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- exercise 7

newtype BoolDisj = BoolDisj Bool deriving (Eq,Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _               = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = genBoolDisj

genBoolDisj :: Gen BoolDisj
genBoolDisj = do
  -- use frequency to return True twice as often as False (just to try it out)
  frequency [(1,return $ BoolDisj False),(2,return $ BoolDisj True)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- exercise 8

data Or a b = Fst a | Snd b deriving (Eq,Show)

{--
  the behaviour of <> over Or values, as explained in the Haskell book, is that we keep the
  first Snd value (it is 'sticky') encountered.  If no Snd value (they are both Fst), then return
  the second Fst value (!)
--}
instance Semigroup (Or a b) where
  a@(Snd _) <> _ = a
  _ <> b         = b -- if first value is not Snd, must be Fst, in which case, always return second

instance (Arbitrary a,Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

genOr :: (Arbitrary a,Arbitrary b) => Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Fst a,return $ Snd b]

type OrAssoc = Or Int Char -> Or Int Char -> Or Int Char -> Bool

-- exercise 9

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

  
{--
   the <> operation over Combine values applies the two functions separately to the argument
   and then applies <> to their results (this explains the expected behaviour described in the
   book) - which means we need 'Semigroup b' as context.
--}
instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\a -> (f a) <> (g a))


{--
   TO DO - figure out how to use coarbitrary to test exercise 9!  The explanations in the Testing
   chapter on how to use CoArbitrary are weak, too weak to enable you to do this part of the exercise.
   I tried to read some online content, outside the book, on how to use CoArbitrary, but there is
   nothing easily digestible.  There is, by the way, a subreddit, /r/haskellbook, where one of the
   authors tackles a complaint about just this point.  His reply was, "The CoArbitrary exercise is
   really mostly for intermediates using the book to refresh and deepen what they know. If you
   can't figure it out, move on." - which, to me, is a total cop-out for a book called
   "Haskell Programming from First Principles."
--}

-- exercise 10

newtype Comp a = Comp { unComp :: (a -> a)}

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (\a -> f . g $ a)

-- exercise 11

data Validation a b =
  Failure a | Success b
  deriving (Eq,Show)

{--
   What I've chosen to do here is accumulate the failures (earlier in the book, there is
   an example of just that), and make the success sticky - in other words, stick with the first
   encountered success.  (Other answers to this question that can be found online provide
   slightly different versions - one, for example, accumulates successes and is sticky in the
   first failure.)
   Accumulating the failures means using '<>' only on the content when both Validation values
   are Failures - hence the type context for the instances etc. only needs to be 'Semigroup a'
   (where 'a' is the first type variable of Validation and applies to the Failure constructor).
--}

instance Semigroup a => Semigroup (Validation a b) where
  (Failure f) <> (Failure f') = Failure $ f <> f'   -- combine the failures
  f@(Failure _) <> _ = f                            -- propagate the failure 
  _ <> f@(Failure _) = f
  s@(Success _) <> _ = s                            -- stick with first success

instance (Arbitrary a,Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = genValidation

genValidation :: (Arbitrary a,Arbitrary b) => Gen (Validation a b)
genValidation = do
  a <- arbitrary
  b <- arbitrary
  frequency [(2,return $ Failure a),
             (3,return $ Success b)
            ]

type ValStringInt = Validation String Int
type ValidAssoc = ValStringInt -> ValStringInt -> ValStringInt -> Bool

-- exercise 12

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq,Show)

-- in this exercise, the type context is 'Semigroup b' - which ends up as the
-- second type parameter (the 'Success' type parameter) of Validation
instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success s)) <> (AccumulateRight (Success s'))
    = AccumulateRight $ Success $ s <> s'
  l@(AccumulateRight (Failure _)) <> _ = l  -- if Failure-Success or Failure-Failure, then return left
  _ <> r                               = r  -- Success-Failure, so return Failure

instance (Arbitrary a,Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = genAccumulateRight

genAccumulateRight :: (Arbitrary a,Arbitrary b) => Gen (AccumulateRight a b)
genAccumulateRight = do
  a <- arbitrary
  return $ AccumulateRight a

type ARBoolString = AccumulateRight Bool String
type AccumAssoc = ARBoolString -> ARBoolString -> ARBoolString -> Bool

-- exercise 13

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq,Show)

instance (Semigroup a,Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Failure f)) <> (AccumulateBoth (Failure f')) = AccumulateBoth (Failure $ f <> f')
  f@(AccumulateBoth (Failure _)) <> _ = f
  _ <> f@(AccumulateBoth (Failure _)) = f
  (AccumulateBoth (Success s)) <> (AccumulateBoth (Success s')) = AccumulateBoth . Success $ s <> s'

instance (Arbitrary a,Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = genAccumulateBoth

genAccumulateBoth :: (Arbitrary a,Arbitrary b) => Gen (AccumulateBoth a b)
genAccumulateBoth = do
  a <- arbitrary
  return $ AccumulateBoth a

type ABothAccum = AccumulateBoth String String
type AccumBothAssoc = ABothAccum -> ABothAccum -> ABothAccum -> Bool

-- do the associativity tests

main :: IO ()
main = do
  quickCheck (semiGroupAssoc :: TrivialAssoc)
  quickCheck (semiGroupAssoc :: IdentityAssoc)
  quickCheck (semiGroupAssoc :: TwoAssoc)
  quickCheck (semiGroupAssoc :: BoolConjAssoc)
  quickCheck (semiGroupAssoc :: BoolDisjAssoc)
  quickCheck (semiGroupAssoc :: OrAssoc)
  quickCheck (semiGroupAssoc :: ValidAssoc)
  quickCheck (semiGroupAssoc :: AccumAssoc)
  quickCheck (semiGroupAssoc :: AccumBothAssoc)
