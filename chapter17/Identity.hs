module Identity where

newtype Identity a = Identity a
  deriving (Eq,Show,Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)
  
-- just to check
idconst = const <$> Identity [1,2,3] <*> Identity [9,9,9]
