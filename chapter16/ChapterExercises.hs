{-# LANGUAGE FlexibleInstances #-}
module ChapterExercises where

{--
   These are the end of chapter exercises for Chapter 16 of
   Haskell Programming from First Principles.
--}

import Test.QuickCheck
import Test.QuickCheck.Function

import GHC.Arr -- required for Exercise 5

-- Exercise 1

-- renamed to avoid clash with Prelude.Bool
data Bool_ = True_ | False_ -- no functor because kind of Bool is *

-- Exercise 2

data BoolAndSomethingElse a = False' a | True' a

-- yes we can have a valid Functor because BoolAndSomethingElse has kind * -> *
instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a)  = True' (f a)

-- Exercise 3

data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- again, yes, because BoolAndMaybeSomethingElse has kind * -> *.  In the definition of
-- fmap, when we encounter Falsish, we just leave it alone.  Similar to the Maybe functor
-- example presented earlier in the chapter.

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)

-- Exercise 4

-- I think the 'f' here stands for 'fiendish'.  This has stumped me, and the only reason I think the
-- answer is no is that the kind of Mu is (* -> *) -> *.
newtype Mu f = InF { outF :: f (Mu f) }

-- Exercise 5

data D = D (Array Word Word) Int Int -- no, even though D has structure, because kind of D is *

-- Re-arranginf arguments to type constructor

-- The solutions all require that the type variable of the data constructor that is transformed
-- by the 'f argument to 'fmap' is the LAST type variable of the type constructor

-- Exercise 1

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a) -- type variable for First needs to be second for valid Functor
  fmap f (Second b) = Second b

-- Exercise 2

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- Exercise 3

data More b a = L a b a | R b a b deriving (Eq,Show)

-- note the 'f' argument is only applied to arguments of L and R that are of type 'a'
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Writing functor instances

-- Exercise 1

data Quant a b = Finance | Desk a | Bloor b

{--
  I found it interesting that this didn't type-check:
   instance Functor (Quant a) where
      fmap f (Bloor b) = Bloor (f b)
      fmap _ x         = x

  The code seems fine - for anything other than Bloor, we want to keep things unchanged.
  However, by using 'x' on both sides, we bind the type variable of the RHS of = to the
  type variable of the LHS - in other words, it's a rigid type variable.  So we end up
  with a type for fmap of fmap :: forall a1 b. (a1 -> b) -> Quant a a1 -> Quant a b
  but the type checker being unable (or perhaps unwilling) to unify 'a1' and 'b' - despite
  the fact that the 'x' only refers to the 'a' type variable in the type signature!
--}

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a

-- Exercise 2

data K a b = K a

-- 'b' is a phantom type variable but is also the type of the content we wish
-- to alter.  Ergo, there's nothing for fmap to do.
instance Functor (K a) where
  fmap _ (K a) = K a

-- Exercise 3

{--
  Slightly more complicated - partly by the profusion of different type variables all with the
  name 'a'.  In the definition of fmap, we apply the function 'f' to the argument to K' because Flip
  flips the type variables.  In the functor instance, (Flip K' a), the 'a' refers to the second (phantom)
  type variable of the K' type constructor, so that the type variable the functor applies to (the 'b' in
  Flip f a b) is actually the 'a' of 'K' a b'.  I hope that's clear :)
--}
newtype Flip f a b = Flip (f b a) deriving (Eq,Show)

-- renamed K' from K to avoid clash with exercise 3
newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))

-- I decided to test this answer, just to be safe...

instance (Arbitrary a,Arbitrary b) => Arbitrary (Flip K' a b) where
  arbitrary = do
    a <- arbitrary
    return $ Flip (K' a)

type FKInt = Flip K' Int Int
type I2I = Fun Int Int
type FKComp = FKInt -> I2I -> I2I -> Bool

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap (g . f) x) == (fmap g (fmap f x))

functorCompose' :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g)
   = (fmap g . fmap f $ x) == fmap (g . f) x

check :: IO ()
check = do
  putStrLn "Checking Exercise 3"
  quickCheck $ \x -> functorIdentity (x :: FKInt)
  quickCheck (functorCompose' :: FKComp)

-- yep, and that was all okay...

-- Exercise 4

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- Exercise 5

data LiftItOut f a = LiftItOut (f a)

{--
  This is the same as the Wrap example in Section 16.13.  The 'f' in the type of LiftItOut
  has kind * -> * and forms part of the structure defined in the Functor instance.  Hence, we
  need to lift over LiftItOut and whatever 'f' is.  That's why we need the context 'Functor f'
  and why we need to re-apply fmap over the argument to the LiftItOut data constructor.
--}

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

-- Exercise 6

data Parappa f g a = DaWrappa (f a) (g a)

{--
  Just a slightly more complicated version of Exercise 5 and the Wrap example
--}

instance (Functor f,Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- Exercise 7

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- Exercise 8

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- Exercise 9

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- Exercise 10

-- what's the deal with the goats?
data GoatLord a =
     NoGoat
   | OneGoat a
   | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap f NoGoat               = NoGoat
  fmap f (OneGoat a)          = OneGoat $ f a
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- Exercise 11

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)


{--
  The tricky case is the Read, where the argument is a function.  You could write
   fmap f (Read r) = Read (f . r)
  which I think is what the authors mean by "your solution might do it monomorphically
  without using fmap".  However, I've chosen to stick with the fmap-based solution - it
  resembles the code in Section 16.7 where fmap is used with negate.  This actually works
  as composition of functions (try the use of negate in the example code in 16.7 with
  '(const 1)' to see what I mean).
--}
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read r)    = Read (fmap f r)
  


