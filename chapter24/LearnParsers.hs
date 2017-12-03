module LearnParsers where 

import Text.Trifecta
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof >> stop

one' = one >> stop

oneTwo = char '1' >> char '2' -- >> eof >> stop

oneTwo' = oneTwo >> stop

{--
   Exercise 2
   Not sure if this is what was asked for.  The question seemed a little unclear.
   Edit: originally, I had them in reverse order, which meant the first one would
   always succeed.  But after reading some other solutions available on guthub, I
   realised the parsers needed to be in this order, so that 'string "1"' only
   succeeds after the others have been tried.  It was early on a Sunday morning
   when I did this...
   The other thing I realised after looking at other collections of solutions,
   is that a lot of people seem to give up after the Reader and State chapters!
--}
oneTwoStr = choice [string "123",string "12",string "1"]

{--
    Exercise 3
    Write parser to do what string does but using char.  This solution
    first turns the string into a list of char parsers, and then replaces
    the ':' operator with (>>) to join the char parsers together.  The
    problem was to find the initial value and understand what that might
    be.  I have to admit this took a little trial and error! :/
--}
mystring s = foldr (>>) (return s) $ map char s

testParse :: String -> Parser Char -> IO ()
testParse s p =
    print $ parseString p mempty s

testParseStr :: String -> Parser String -> IO ()
testParseStr s p =
    print $ parseString p mempty s

pNL s = putStrLn ('\n' : s)

main = do
    s <- (putStrLn "Enter string to be parsed: " >> getLine)
    pNL "stop:"
    testParse s stop
    pNL "one:"
    testParse s one
    pNL "one':"
    testParse s one'
    pNL "oneTwo:"
    testParse s oneTwo
    pNL "oneTwo':"
    testParse s oneTwo'
    pNL "oneTwoStr"
    testParseStr s oneTwoStr
