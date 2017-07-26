module Optional where

import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq,Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada y = y
  mappend x Nada = x
  mappend (Only x) (Only y) = Only (x `mappend` y)

-- new code
instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional

genOptional :: Arbitrary a => Gen (Optional a)
genOptional = do
  a <- arbitrary
  oneof [return Nada,return $ Only a]
  
