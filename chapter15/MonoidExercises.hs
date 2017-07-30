module MonoidExercises where

{--
  Answers to Monoid exercises in Chapter 15 of Haskell Programming from First Principles
--}

import Test.QuickCheck (sample',quickCheck,Gen,Arbitrary,arbitrary,oneof,frequency)
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup,(<>))

-- checking for associativity and left and right identity

semiGroupAssoc :: (Eq m,Semigroup m) => m -> m -> m -> Bool
semiGroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- it wasn't clear to me at first, but 'Semigroup m' needs to be added to the
-- type context because we are using '<>' from Semigroup - hence the argument (of type 'm')
-- needs to be an instance of Semigroup
monoidLeftIdentity :: (Eq m,Monoid m,Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m,Monoid m,Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- main function to carry out checking

main :: IO ()
main = do
  putStrLn "Checking Trivial"
  quickCheck (semiGroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  putStrLn "Checking BoolConj"
  quickCheck (semiGroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

-- Exercises

-- Exercise 1

data Trivial = Trivial deriving (Eq,Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- exercises 2 and 3 not terribly interesting
-- exercise 4

newtype BoolConj = BoolConj Bool
  deriving (Eq,Show)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _                = BoolConj True

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = genBoolConj

genBoolConj :: Gen BoolConj
genBoolConj = do
  oneof [return $ BoolConj True,return $ BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- exercise 5 really a repeat of 4

-- exercise 6

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\a -> (f a) <> (g a))

{--
  We need both Semigroup and Monoid type contexts for this to work.  'Semigroup b' is
  needed because it is required by the use of '<>' from the Semigroup instance - since
  the Semigroup instance is constrained by 'Semigroup b', we also need it here.  The
  'Monoid b' type constraint has to do with the identity -
  in this case, the identity function has to accept an argument of type 'a', but return the
  'mempty' value (or identity) of type 'b' - which it only has if it is constrained to be a Monoid
  instance.  Originally, I had
       mempty = Combine $ \a -> (mempty :: (Monoid b => b))
  to represent this, but the current code is simpler.  It relies on the type of mempty being inferred as
  mempty :: Monoid b => a -> b, allowing the right (function) value of mempty to be found.  Try this at the
  REPL prompt:
       (mempty :: Bool -> Sum Int) True
       (mempty :: Bool -> Sum Int) False

  In both cases, you will get Sum { getSUm = 0 } as the answer.  By typing mempty as a function for a return
  type that is an instance of Monoid, we get the right function for mempty.
--}
instance (Semigroup b,Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- exercise 7

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp f') = Comp $ f . f'

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

-- exercise 8

newtype Mem s a =
  Mem {
        runMem :: s -> (a,s)
      }

{--
  Below is one solution to the problem.  A simpler solution would be to define mappend in the Monoid instance
  using the value given for (<>) in the Semigroup instance - as shown here
  https://github.com/lukleh/haskell-book-exercises/blob/gh-pages/ch15/ch15_15.14_20.hs, which uses (<>) imported
  from Data.Monoid, so Semigroup isn't needed.

  The key elements of the solution, though, are the chaining of the 's' values and the (<>) of the 'a' values.  This
  gives you the required behaviour.
--}

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem f') =
    Mem $ \s -> let (a1,s1) = f' s
                    (a2,s2) = f s1
                in (a1 <> a2,s2)
                   

instance (Semigroup a,Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty,s)
  mappend = (<>)

f' = Mem $ \s -> ("hi",s+1)

check :: IO ()
check = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String,Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
  
