module HumanAndDog where

{--
   This is just the example code from the book to demonstrate the
   function Applicative.
--}

import Control.Applicative (liftA2)


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

-- example data

pers :: Person
pers =
  Person (HumanName "Simon")
         (DogName "Snoopy")
         (Address "Victoria Road")

john :: Person
john =
  Person (HumanName "John")
         (DogName "Asprouli")
         (Address "Koropi")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

{--
   Type of liftA2 is
     liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
   In this case, Dog has type (DogName -> Address -> Dog),
   so a = DogName, b = Address, and c = Dog.
   In addition, dogName has type Person -> DogName, and address has type
   Person -> Address, so in the context of the type of liftA2,
   f = (->) Person.  This means that the applied type of liftA2 is
     liftA2 :: (DogName -> Address -> Dog) -> (Person -> DogName)
                 -> (Person -> Address) -> (Person -> Dog)
   So applying liftA2 to Dog, dogName and address produces an expression
   of type Person -> Dog which matches the type pf getDogR'.

   Just to add that it confused me at first why functor of functions was
   composition, while applicative of functions are parallel application. In
   the example below, getDogName and address are being separately applied, as if
   in parallel, to the same argument value.  But my view is that this is dictated
   by the type of applicative (and by extension, liftA2). Consider the applied type
   of liftA2 above:
   liftA2 :: (DogName -> Address -> Dog) -> (Person -> DogName)
                 -> (Person -> Address) -> (Person -> Dog)
   All we know from this type is that the Person value received by
     (Person -> DogName),
     (Person -> Address) and
     (Person -> Dog)
   must be the same Person value.  Hence, dogName and address must be applied to
   the same value received by the resulting function.  I know this is not well explained,
   but the answer is the reason for parallel application lies in the semantics of the
   type signature.
--}

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address


