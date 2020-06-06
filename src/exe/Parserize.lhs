
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

adjust the last token one at a time to remove a char from the end of
it, then remove it and remove a char from the end of the previous
token, etc.

remove one token at a time from the end and parse again each time

remove one or more tokens from random places and parse again

add a token in each place and parse again

swap a token in each place and parse again

There's no lexer atm, so start with something that takes a text, then
removes one char at a time from the end and reparses.

Try to simulate what a user might do:
leaving something out is common
spelling something wrong is also common
  -> replace lexical symbols with something similar but different

To reduce the size of the output, group consecutive strings which all
succeed or all have the same error message together?

The way to use this is to generate a lot of output, then quickly scan
through it looking for long tedious error messages, then try to tweak
them. Also, pick out some at random, and look closely. By commiting
the output to source control, every time you adjust the parser you
can check the diff to see the effect it has on the error
messages. Even if you don't carefully review every error message,
when they change, you can review the changes, and this will gradually
improve the error messages and prevent them from regressing unless
you decide explicitly to let this happen.

It's already been very effective, and I was also able to reverse
engineer some rules of thumb to proactively write parsers which also
produce better error messages from the adjustments done from this
process (techniques in addition to removing the evil 'try')

TODO: write a lexer so can do this better
add more syntax examples to the test - aim for coverage of all the
  grammar -> use a code coverage tool for this

do some example code showing the error messages improving following
these rules

this work should be depriorized as much as possible right now because
it's polish that should come after many of the other things are
much further along


> parserize :: FilePath -> String -> String
> parserize _fn src =
>     let doAnother [] = ""
>         doAnother s = let ast = parseProgram "" s
>                       in s ++ (case ast of
>                                    Right _ -> " OK\n"
>                                    Left e -> "\n" ++ f e ++ "\n")
>                          ++ doAnother (dr s)
>     in doAnother src
>   where
>     dr s = let s' = reverse s
>            in case s' of
>                   (x:_) | isSpace x -> reverse $ dropWhile isSpace $ drop 1 s'
>                   _ -> reverse $ drop 1 s'
>     f s = if not (isInfixOf "unexpected end of input" s)
>           then s ++ "\n^^^^^^^^^^^^^^^^^^^^^^^"
>           else s
