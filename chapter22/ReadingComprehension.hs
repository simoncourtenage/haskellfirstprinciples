{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where

{--
   These are answers to the section called 'Reading Comprehension' in
   Chapter 22 on page 848 of 'Haskell Programming from First Principles'
   v. 0.12.0.
--}

-- Exercise 1 - implement your own version of liftA2

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a
         -> f b
         -> f c
myLiftA2 f a b = f <$> a <*> b

-- Exercise 2

newtype Reader r a = Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

-- Exercise 3 - implement the applicative for Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
   pure :: a -> Reader r a
   pure a = Reader $ \r -> a
   
   (<*>) :: Reader r (a -> b)
         -> Reader r a
         -> Reader r b
   (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

-- Reader Monad exercise from page 852

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> (Reader r b)
  (Reader ra) >>= aRb = Reader $ \r -> (runReader $ aRb (ra r)) r

newtype HumanName = HumanName String deriving (Eq,Show)

newtype DogName = DogName String deriving (Eq,Show)

newtype Address = Address String deriving (Eq,Show)

data Person =
  Person {
     humanName :: HumanName
   , dogName :: DogName
   , address :: Address
   } deriving (Eq,Show)

data Dog =
  Dog {
     dogsName :: DogName
   , dogsAddress :: Address
   } deriving (Eq,Show)

pers :: Person
pers =
  Person (HumanName "Simon")
         (DogName "Snoopy")
         (Address "Victoria Road")

getDogRM :: Person -> Dog
getDogRM = dogName >>= \n -> address >>= \a -> return $ Dog n a
  
