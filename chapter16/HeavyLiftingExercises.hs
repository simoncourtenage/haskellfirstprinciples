module HeavyLiftingExercises where

-- solution to exercises in Chapter 16 on page 640 of
-- Haskell Programming from First Principles

-- Exercise 1

a = fmap (+1) $ read "[1]" :: [Int]

-- Exercise 2

b = (fmap . fmap) (++ "lol") (Just ["Hi,","Hello"])

-- Exercise 3

c = fmap (*2) (\x -> x- 2)

-- Exercise 4

d = fmap ((return '1' ++) . show) (\x -> [x,1..3])

-- Exercise 5

{--

This answer takes a little explaining.  The code given in the exercise won't compile
because things are missing.  Specifically, the fmaps are missing to "lift" operations
over the IO structure.  And, unlike the previous exercises, where only one fmap was
needed, this answer requires us to lift over IO more than once.

First, 'ioi' is of type IO Integer.  So, in 'changed', we need (i) to turn the integer
into a string so we can prefix it with "123".  So we fmap '(("123"++) . show)' over IO
so that the integer inside IO is converted to a string and prefixed with "123".  Then we
need to turn the string in IO back into an integer - so we fmap read over IO.

Lastly, we now need to multiply the integer (which is now 1231) by 3 - so we fmap (*3) over
IO so achieve this.

--}

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap (("123"++) . show) ioi)
    in fmap (*3) changed
       
