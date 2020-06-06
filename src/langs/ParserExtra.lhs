
> module ParserExtra (parseSet
>                    ) where

> import Text.Megaparsec (Parsec
>                        ,errorBundlePretty
>                        --,many
>                        ,eof
>                        ,takeWhileP
>                        --,takeWhile1P
>                        ,choice
>                        --,notFollowedBy
>                        ,try
>                        ,anySingle
>                        ,(<|>)
>                        --,manyTill
>                        )
> import qualified Text.Megaparsec as Q (parse)

> import Data.Void (Void)
> import Text.Megaparsec.Char (space
>                             ,char
>                             ,string
>                             ,letterChar
>                             )
> import Control.Monad (void)
> import Data.Char (isAlphaNum)


> parseSet :: String -> Either String (String,String)
> parseSet src = either (Left . errorBundlePretty) Right $
>                    Q.parse (whiteSpace *> pSet <* eof) "" src

> type Parser = Parsec Void String

> pSet :: Parser (String,String)
> pSet = lexeme $ do
>     lexeme_ $ string ":set"
>     k <- identifier
>     symbol_ "="
>     v <- identifier
>     pure (k,v)

> identifier :: Parser String
> identifier =
>     lexeme ((:)
>     <$> (letterChar <|> char '_' <|> char '-')
>     <*> takeWhileP Nothing (\a -> (isAlphaNum a || a `elem` "?-+_")))


> symbol_ :: String -> Parser ()
> symbol_ x = lexeme_ (string x)


> lexeme :: Parser a -> Parser a
> lexeme f = f <* whiteSpace

> lexeme_ :: Parser a -> Parser ()
> lexeme_ f = () <$ f <* whiteSpace


> whiteSpace :: Parser ()
> whiteSpace = space *> choice [blockComment *> whiteSpace
>                              ,lineComment *> whiteSpace
>                              ,pure ()]


> lineComment :: Parser ()
> lineComment = () <$ try (string "#") <* takeWhileP Nothing (/='\n')

> blockComment :: Parser ()
> blockComment = startComment *> ctu
>   where
>     startComment = void (try (string "#|"))
>     endComment = void $ try (string "|#")
>     ctu = endComment <|> ((blockComment <|> void anySingle) *> ctu)

