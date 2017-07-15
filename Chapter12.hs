module Chapter12 where

import Data.List (intersperse,intercalate)

notThe :: String -> Maybe String
notThe s | s == "the" || s == "The" = Nothing
         | otherwise                = Just s

replaceThe :: String -> String
replaceThe = intercalate " " . map (changeThe . notThe) .  words
  where changeThe Nothing = "a"
        changeThe (Just s) = s

countTheBeforeVowel :: String -> Int
countTheBeforeVowel = count 0 . map notThe . words
  where count c [] = c
        count c [w] = c
        count c (Nothing:(Just s):ws) | head s `elem` "aeiou" = count (c+1) ws
                                      | otherwise             = count c ws
        count c (_:ws) = count c ws
        

-- given description of problem, this has much simpler solution
-- which is length . filter isVowel
countVowels :: String -> Int
countVowels = foldr f 0 . map getVowels . words
  where f Nothing a   = a
        f (Just vs) a = a + length vs

getVowels :: String -> Maybe String
getVowels s = case filter isVowel s of
                "" -> Nothing
                vs -> Just vs
                

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

newtype Word' =
  Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = case (length v > length c) of
             True -> Nothing
             False -> Just (Word' w)
  where (v,c) = (filter isVowel w,filter (not . isVowel) w) -- or partition isVowel w
  


data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i | i < 0 = Nothing
               | otherwise = Just (f i)
   where f 0 = Zero
         f n = Succ (f (n-1))
        

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f (Just a) = f a
mayybee x _ _        = x

fromMaybe :: a -> Maybe a -> a
fromMaybe x (Just a) = a
fromMaybe x _        = x

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x = mayybee x id 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])

catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just a) -> a) . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f Nothing _ = Nothing
        f _ Nothing = Nothing
        f (Just x) (Just xs) = Just (x:xs)

-- either library

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) as = a:as
        f _ as        = as
        
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right x) xs = x:xs
        f _         xs = xs

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' es = (lefts' es,rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ (Left _)  = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (Just . f)

-- unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Nothing    -> []
                  Just (a,b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a,f a)) x

-- binary tree exercise for unfold

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
               Nothing -> Leaf
               Just (a,b,c) -> Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (f n) 0
  where f n x | n == x    = Nothing
              | otherwise = Just (x+1,x,x+1)



