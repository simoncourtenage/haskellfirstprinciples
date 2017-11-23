module Bip where

import Control.Applicative

-- boop :: Num a => a -> a
boop = (*2)

-- doop :: Num a => a -> a
doop = (+10)

-- bip :: Num a => a -> a
bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

boopDoop' :: Integer -> Integer
boopDoop' = boop >>= (\a -> doop  >>= (\b -> return (a + b)))
