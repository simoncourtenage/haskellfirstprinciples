module SumMonad where

data Sum a b = First a | Second b deriving (Eq,Show)

instance Functor (Sum a) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure a = Second a
  (<*>) (First a) _ = First a
  (<*>) (Second f) (Second b) = Second (f b)
  
instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second a) f = f a

  

  
