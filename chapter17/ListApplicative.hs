module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq,Show)

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

{--
  The code for <*> took a little thinking, and I've left in my second solution, because it
  helped me see how to follow the instruction in the book to use flatMap and fmap.  In the
  second solution, I knew that each function from the first list had to be applied to the
  whole list of values and then the list of resulting lists concatenated.  But I couldn't
  work out how to take each function and then fmap it over the list of values - at least
  without extra code.  It was only when I inverted my idea and thought of fmap'ing over the
  list of functions with the entire list of values that I realised how to solve the puzzle.
  The function to fmap over the list of functions is the partially applied infix version of
  fmap - so it's a kind of fmap of fmap.  The first fmap is over functions, taking each
  function and fmap'ing it over the list of values. Finally, we concat all the resulting lists
  together.

  Of course, the first part of this definition is the same as the definition of flatMap, which
  leads to the final version of <*>.
--}

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  -- (<*>) (Cons f fs) as = (f <$> as) `append` (fs <*> as) --| first solution
  -- (<*>) fs as = concat' $ fmap (<$> as) fs  -- | second solution
  (<*>) fs as = flatMap (<$> as) fs


functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1,return Nil),(3, return $ Cons a b)]

instance Eq a => EqProp (List a) where (=-=) = eq

check :: IO ()
check = quickBatch $ applicative (Nil :: List (Int, Int,Int))

