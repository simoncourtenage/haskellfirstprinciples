module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid((<>))

-- Traversable Instances

-- Identity

newtype Identity a = Identity a deriving (Eq,Ord,Show)

-- required superclass
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

-- required superclass
instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Constant

newtype Constant a b = Constant { getConstant :: a} deriving (Eq,Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty

-- since we can't apply f to c (because it's treated as constant), we need to inject
-- c into the structure using pure
instance Traversable (Constant a) where
  traverse f (Constant c) = Constant <$> pure c

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq
  

-- Maybe

data Optional a = Nada | Yep a deriving (Eq,Show)

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldr f z Nada = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse f Nada = pure Nada
  -- 'f a' puts what will be the outer structure around the 'a' value.  Then use
  -- infix functor to insert the 'Yep' structure inside the outer structure and around
  -- the 'a' value.
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1,return Nada),(3,return $ Yep a)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- List

data List a = Nil | Cons a (List a) deriving (Eq,Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldr f z Nil = z
  foldr f z (Cons a as) = f a (foldr f z as)

instance Traversable List where
  traverse f Nil = pure Nil
  -- interesting one.  The 'Cons <$> f a' creates a value of type
  -- f (List a -> List a), where f is the type of the outer structure.
  -- Then use applicative infix operator to applly the function in the 'f'
  -- structure to a value that has type 'f (List a)'.
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1,return Nil),(3, return $ Cons a b)]

instance Eq a => EqProp (List a) where
  (=-=) = eq
 
-- Three

data Three a b c = Three a b c deriving (Eq,Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a,Arbitrary b,Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a,Eq b,Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Three'

data Three' a b = Three' a b b deriving (Eq,Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'

instance (Arbitrary a,Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a,Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- S

data S n a = S (n a) a deriving (Eq,Show)

{--
  We need to apply f to all values of type 'a' - hence as well as applying f to
  the second argument to S, we also need to apply it to the values of type 'a'
  embedded in the first argument (of type 'n a').  We have to use fmap to do this,
  which means 'n' must be a type instance of functor - hence the constraint.
--}
instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a


{--
  Both for this instance and EqProp, we need the constraint to be on 'n a', not just
  'n' - if we use 'Arbitrary n' as one of the constraints, then a kind error pops up
  to the effect that the constraint implies 'n' has kind '*', but in the type of S,
  'n' has kind '* -> *' because it is used as a data constructor.  To solve this, by
  making the kind of 'n' the same in the constraint as it is in the type of S, we need
  the constraint to be over the application of 'n' to 'a'.
--}
instance (Arbitrary a,Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = do
    na <- arbitrary
    a <- arbitrary
    return $ S na a

instance (Eq (n a),Eq a) => EqProp (S n a) where
  (=-=) = eq

-- Tree

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq,Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t a t') = foldMap f t <> f a <> foldMap f t'
  foldr f z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node t a t') = f a (foldr f (foldr f z t') t)

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    frequency [(1,return Empty), (3,return $ Leaf a),(3,return $ Node b a c)]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

-- quickcheck testing

main = do
  putStr "Testing Identity"
  quickBatch $ traversable (undefined :: Identity (Int,Int,[Int]))
  putStr "Testing Constant"
  quickBatch $ traversable (undefined :: Constant (Int,Int,Int) (Bool,Bool,[Bool]))
  putStr "Testing Optional"
  quickBatch $ traversable (undefined :: Optional (Int,Int,[Int]))
  putStr "Testing List"
  quickBatch $ traversable (undefined :: List (Int,Int,[Int]))
  putStr "Testing Three"
  quickBatch $ traversable (undefined :: Three (Int,Int,Int) (Int,Int,Int) (Int,Int,[Int]))
  putStr "Testing Three'"
  quickBatch $ traversable (undefined :: Three' (Int,Int,Int) (Int,Int,[Int]))
  putStr "Testing S"
  quickBatch $ traversable (undefined :: S Identity (Int,Int,[Int]))
  putStr "Testing Tree"
  quickBatch $ traversable (undefined :: Tree (Int,Int,[Int]))
