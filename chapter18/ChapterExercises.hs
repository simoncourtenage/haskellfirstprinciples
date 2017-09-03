module ChapterExercises where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{--
  Answers to exercises in Chapter 18 of Haskell Programming from First Principles
--}

-- Exercise 1

data Nope a = NopeDotJpg deriving (Eq,Show)

instance Functor Nope where
  fmap f _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) NopeDotJpg _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = do
    return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

testNope :: IO ()
testNope = do
  quickBatch $ monad (undefined :: Nope (Int,Int,Int))

-- Exercise 2

-- slight change to type constructor name to make it more convenient to type!  Also,
-- data constructors renamed to avoid clashes with Either type in Prelude.
data PhbtEither b a = Left_ a | Right_ b deriving (Eq,Show)

instance Functor (PhbtEither b) where
  fmap f (Right_ b) = Right_ b
  fmap f (Left_ a)  = Left_ (f a)

instance Applicative (PhbtEither b) where
  pure a = Left_ a
  (<*>) (Right_ b) _ = Right_ b
  (<*>) _ (Right_ b) = Right_ b
  (<*>) (Left_ f) (Left_ a) = Left_ (f a)

instance Monad (PhbtEither b) where
  return = pure
  (>>=) (Right_ b) f = Right_ b
  (>>=) (Left_ a)  f = f a

instance (Arbitrary a,Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Right_ b, return $ Left_ a]

instance (Eq a,Eq b) => EqProp (PhbtEither b a) where
  (=-=) = eq

testPhbtEither :: IO ()
testPhbtEither = quickBatch $ monad (undefined :: PhbtEither String (Int,Int,Int))

-- Exercise 3

newtype Identity a = Identity a deriving (Eq,Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

{--
  I like this style of defining arbitrary for a data type.  In
     Identity <$> arbitrary
  the arbitrary has type 'Gen a'.  So the infix fmap <$> is mapping the
  data constructor function Identity over the value typed 'a' to produce a
  value of type Gen (Identity a).
--}
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

testIdentity :: IO ()
testIdentity = quickBatch $ monad (undefined :: Identity (Int,Int,Int))

-- Exercise 4

data List a = Nil | Cons a (List a) deriving (Eq,Show)

-- functions from List Applicative exercise in Chapter 17

append :: List a -> List a -> List a
append Nil l = l
append l Nil = l
append (Cons a as) l = Cons a $ as `append` l

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as


instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons h t) = Cons (f h) (f <$> t)

-- from List Applicative exercise
instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs as = flatMap (<$> as) fs

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) a f = flatMap f a

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1,return Nil),(3,return $ Cons a b)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testList :: IO ()
testList = quickBatch $ monad (undefined :: List (Int,Int,Int))

-- Solving problems

-- Exercise 1

j :: Monad m => m (m a) -> m a
j = join

-- Exercise 2

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = f <$> a

-- Exercise 3

-- this is monad's version of liftA2
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- Exercise 4

a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

-- Exercise 5

{--
  This exercise, for me, was particularly fiendish.  The hint about recursion was useful, partly because
  it meant any attempt solution that didn't use it was probably going in the wrong direction.  Early on,
  I realised that the f::a -> m b had to be on the left of >>=, applied to the head of the list, to create
  a value of type 'm b'.  That meant that, across the >>=, we unbound a value of type 'b' to be added to
  the list embedded in the result of type m [b].  But I couldn't see how to do this, until, after waking
  up from a nap (I had been working in the garden all morning), the answer (in draft form) just came to me.

  So what we do is (looking at it imperatively)
     i. create a value of type 'm b'
    ii. unbind this across the first >>= and pass it as argument to a function \x -> ...
   iii. in the function \x -> ..., we call meh t f to get the remanining 'm [b]' value
   iv.  we pass the m [b] value across >>= t unbind the list [b]
    v.  we cons the first value to the remaining list
   vi.  we inject the list back into the monad using 'return' to end up with a value of type m [b].

  Simples!
--}

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (h:t) f = f h >>= \x -> meh t f >>= \y -> return (x:y)

mehtest = meh [1,2,3] Just -- try this to see how it works

-- Exercise 6

{--
  The type of the function argument to meh is a -> m b.  This solution depends on the 'a' type
  variable being specialized to 'm b', i.e, a valid instantiation of a -> m b is m b -> m b.  So
  meh :: [a] -> (a -> m b) -> m [b] is specialised to [m b] -> (m b -> m b) -> m [b].
--}

flipType :: Monad m => [m a] -> m [a]
flipType ms = meh ms id
