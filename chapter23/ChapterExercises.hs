module ChapterExercises where 

import Moi

-- Exercise 1

get :: Moi s s
get = Moi $ \s -> (s,s)

-- Exercise 2

put :: s -> Moi s ()
put s = Moi $ \s' -> ((),s)

-- Exercise 3

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s 

-- Exercise 4

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

-- Exercise 5

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((),f s)

