module ChapterExercises where

import Data.Monoid

{--
  Answers to exercises in Chapter 19 of Haskell Programming from First Principles.
--}

-- Exercise 1

data Constant a b = Constant a

{--
  Folding over this data type has to return the initial value 'z'.  The type of the
  value held in the Constant structure is actually part of the structure itself (see
  the instance header, where we need a type - 'Constant a' - that has kind * -> *). The
  type variable 'b' represents the values that we would fold over, but is a
  'phantom' type, so there is actually nothing to fold over.
--}

instance Foldable (Constant a) where
  foldr f z (Constant a) = z
  

-- Exercise 2

data Two a b = Two a b

{--
  Unlike the answer to exercise 1, the value we fold over is present in the data constructor.
--}

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z

-- Exercise 3

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

-- Exercise 4

data Three' a b = Three' a b b

{--
  Since there are two values in the data constructor of type 'b' - the type variable which
  represents values to fold over, I decided to code it as follows - apply the fold function
  to the first 'b' value with the initial value, then use that result as the initial value
  to apply the fold function to the second 'b' variable.
--}

instance Foldable (Three' a) where
  foldr f z (Three' a b b') = f b' $ f b z

-- Exercise 5

data Four' a b = Four' a b b b

{--
  Using same approach as for Exercise 4
--}

instance Foldable (Four' a) where
  foldr f z (Four' a b1 b2 b3) = f b3 $ f b2 $ f b1 z

-- filter function exercise

{--
  Sometimes, these questions feel like detective stories.  Why does the result type 'f a' have to be a monoid,
  and why the 'f' has to be an applicative?  The answer is in the functions we need from the interfaces of
  these type classes.

  This answer doesn't seem to work like traditional filter, because you can't see it dropping elements that don't
  satisfy the predicate.  However, that does happen in a way that depends on the 'mappend' implemented for whatever
  monoid 'f a' happens to be (used by the fold part of foldMap), and on the 'mempty' value - which, remember, is
  the identity for mappend.  Finally, we need to inject the value tested against the predicate into the structure
  used for the output (the 'f' type).  To do this, we need 'pure' from Applicative.

  So, we need 'mappend' (used by foldMap) and 'mempty' from the Monoid typeclass for the output type ('f a'),
  and we need 'pure' from Applicative for the structure of the output type (the 'f' type). And now the crime is
  solved...
--}

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f as = foldMap (\x -> if f x then pure x else mempty) as
  
