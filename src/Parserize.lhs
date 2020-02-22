
> module Parserize (parserize) where
> 
> import Parse (parseProgram)
> import Data.List (isInfixOf)
> import Data.Char (isSpace)

The idea with this code is to take some source code which may or may
not be valid, and mess with it, and output the variations and whether
they parse, and if they don't, the error message. This can be used to
generate text file reports, which can be reviewed for the error
message quality, and compared to see how changes to the parsing
affect the error messages.

Things to do, lex the source, then:

remove one token at a time from the end and parse again each time

remove one or more tokens from random places and parse again

add a token in each place and parse again

There's no lexer atm, so start with something that takes a text, then
removes one char at a time from the end and reparses.

To reduce the size of the output, group consecutive strings which all
succeed or all have the same error message together?

TODO: try to skip whitespace
try to skip comments

> parserize :: FilePath -> String -> String
> parserize fn src =
>     let doAnother [] = ""
>         doAnother s = let ast = parseProgram fn s
>                       in s ++ (case ast of
>                                    Right _ -> " OK\n"
>                                    Left e -> "\n" ++ f e ++ "\n")
>                          ++ doAnother (reverse $ drop 1 $ keepDroppingWhitespace $ reverse s)
>     in doAnother $ keepDroppingWhitespace src
>   where
>     keepDroppingWhitespace (x:xs) | isSpace x = keepDroppingWhitespace xs
>                                   | otherwise = (x:xs)
>     keepDroppingWhitespace [] = []
>     f s = if not (isInfixOf "unexpected end of input" s)
>           then s ++ "\n^^^^^^^^^^^^^^^^^^^^^^^"
>           else s
