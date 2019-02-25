
> module Parse (parseExpr) where


> import Text.Megaparsec (Parsec
>                        ,many
>                        ,(<|>)
>                        ,parse
>                        ,eof
>                        ,some
>                        ,choice
>                        ,option
>                        ,(<?>)
>                        ,manyTill
>                        ,takeWhileP
>                        ,takeWhile1P
>                        ,try
>                        ,optional
>                        --,eitherP
>                        ,notFollowedBy
>                        )

> --import qualified Text.Megaparsec as Parser
> import Data.Void (Void)
> --import Control.Applicative ((<**>))
> --import Control.Monad (guard)

> import Text.Read (readMaybe)

> --import Text.Megaparsec (satisfy, anySingle)

> import Text.Megaparsec (errorBundlePretty)
> import Text.Megaparsec.Char (space
>                             --,letterChar
>                             ,char
>                             --,digitChar
>                             ,string
>                             --,satisfy
>                             --,anyChar
>                             )


> import Data.Char ({-isAlphaNum,isDigit,-}isSpace)
> import Data.Scientific

> import qualified Text.Megaparsec as Parser
> --import qualified Data.Set as Set

> import Data.Void (Void)
> import Control.Applicative ((<**>))
> import Control.Monad (guard)

> import Text.Megaparsec (satisfy, anySingle)

> import Text.Megaparsec.Char (space
>                             ,letterChar
>                             ,char
>                             --,digitChar
>                             ,string
>                             --,satisfy
>                             --,anyChar
>                             )

> import Data.Char (isAlphaNum,isDigit)

> --import Control.Monad.Combinators.Expr (makeExprParser)

> --import qualified Control.Monad.Combinators.Expr as ME


> import Syntax

> parseExpr :: String -> String -> Either String Expr
> parseExpr fn src = either (Left . errorBundlePretty) Right $
>                    parse (whiteSpace *> expr <* eof) fn src


> type Parser = Parsec Void String

> whiteSpace :: Parser ()
> whiteSpace = space *> choice [blockComment *> whiteSpace
>                              , lineComment *> whiteSpace
>                              ,pure ()] <?> ""

> lineComment :: Parser ()
> lineComment = () <$ try (string "#" <?> "") <* takeWhileP Nothing (/='\n')

todo: remove the try
make it parse nested block comments correctly

> blockComment :: Parser ()
> blockComment = () <$ try (string "#|" <?> "") <* manyTill anySingle (try (string "|#"))


> lexeme :: Parser a -> Parser a
> lexeme f = f <* whiteSpace

> lexeme_ :: Parser a -> Parser ()
> lexeme_ f = () <$ f <* whiteSpace

> char_ :: Char -> Parser ()
> char_ x = () <$ char x

> parens :: Parser a -> Parser a
> parens f = lexeme_ (char_ '(')
>            *> f
>            <* lexeme_ (char_ ')')

> xSep1 :: Char -> Parser f -> Parser [f]
> xSep1 x f = (:) <$> f <*> option [] (lexeme_ (char_ x) *> xSep1 x f)

> xSep :: Char -> Parser f -> Parser [f]
> xSep x f = option [] (xSep1 x f)

> commaSep1 :: Parser f -> Parser [f]
> commaSep1 = xSep1 ','

> commaSep :: Parser f -> Parser [f]
> commaSep = xSep ','

> keyword :: String -> Parser String
> keyword n = lexeme (try $ string n)

> keyword_ :: String -> Parser ()
> keyword_ n = lexeme_ (try $ string n)


todo: make an identifier only end on a alphanum or underscore
add : to the identifierS?

> identifier :: Parser String
> identifier =
>     lexeme ((:)
>     <$> (letterChar <|> char '_' <|> char '-')
>     <*> takeWhileP Nothing (\a -> (isAlphaNum a || a `elem` "?-+_")))
>     <?> "identifier"

> num :: Parser String
> num = lexeme (
>     choice [digits <**> choice [eSuffix,dotSuffixOnly,pure id]
>            ,myChar '.' <**> afterDot
>            ]
>    -- this is for definitely avoiding possibly ambiguous source
>    -- not sure if it is needed
>     <* notFollowedBy (satisfy (`elem` "eE."))) <?> "number"
>   where
>     -- parse one or more 0-9
>     digits = takeWhile1P Nothing isDigit
>     -- parse .[digits][e[+-]digits]
>     dotSuffixOnly = append <$> (myChar '.' <**> choice [afterDot, eSuffix, pure id])
>     -- parse digits[e[+-]digits], used after the .
>     afterDot = append <$> (digits <**> choice [eSuffix, pure id])
>     -- parse e[+-]digits
>     eSuffix = append <$> concatA [myChar 'e', optionalPlusOrMinus,digits]
>     optionalPlusOrMinus = option "" (myChar '+' <|> myChar '-')
>     -- parse a char, return it as a string
>     myChar c = [c] <$ char_ c
>     -- concat in applicative
>     concatA xs = concat <$> sequenceA xs
>     -- not sure if this def pays its way
>     append = flip (++)

> numE :: Parser Expr
> numE = do
>     x <- num
>     maybe (fail $ "parsing number failed: " ++ x)
>           (pure . Num) (readMaybe x)

todo: escape quotes
and make sure it doesn't parse newlines when it shouldn't

> stringE :: Parser Expr
> stringE = Str <$> choice [char_ '\'' *> takeWhileP Nothing (/='\'') <* lexeme_ (char_ '\'')
>                          ,char_ '"' *> takeWhileP Nothing (/='"') <* lexeme_ (char_ '"')]
>             <?> "string literal"


 > startsWithIden :: Parser Expr
 > startsWithIden = identifier <**> option Iden (
 >     choice [appSuffix])

> appSuffix :: Parser (Expr -> Expr)
> appSuffix = flip App <$> parens (commaSep1 expr)

> expr :: Parser Expr
> expr = term <**> option id binOpSuffix

> binOpSuffix :: Parser (Expr -> Expr)
> binOpSuffix = do
>     op <- binOpSym
>     b <- expr
>     pure $ bo op b
>   where
>     bo op b a = BinOp a op b

> symbol :: String -> Parser String
> symbol x = lexeme (string x)

> symbol_ :: String -> Parser ()
> symbol_ x = lexeme_ (string x)


> parensE :: Parser Expr
> parensE = Parens <$> parens expr

> binOpSym :: Parser String
> binOpSym = choice [symbol "+"
>                   ,symbol "*"
>                   ,symbol "=="
>                   ,symbol "<>"
>                   ,symbol "<"
>                   ,symbol "<="
>                   ,symbol ">"
>                   ,symbol ">="
>                   ,symbol "-"
>                   ,symbol "/"
>                   ]
> 

> unaryMinus :: Parser Expr
> unaryMinus = UnaryMinus <$> (symbol "-" *> term)

> lamE :: Parser Expr
> lamE = Lam <$> (keyword_ "lam" *> parens (commaSep identifier) <* symbol_ ":")
>            <*> (expr <* keyword_ "end")
>            

> explicitLet :: Parser Expr
> explicitLet = Let <$> (keyword_ "let" *> commaSep1 bind)
>                   <*> (symbol_ ":" *> expr <* keyword_ "end")
>     where
>         bind = (,) <$> identifier
>                    <*> (symbol_ "=" *> expr)

> ifE :: Parser Expr
> ifE = If <$> (keyword_ "if" *> ((:) <$> cond <*> many elseif))
>          <*> (optional elseE <* keyword_ "end")
>     where
>         cond = (,) <$> expr <*> (symbol_ ":" *> expr)
>         elseif = (try (keyword_ "else" *> keyword_ "if") *> cond)
>         elseE = keyword_ "else" *> symbol ":" *> expr

ask:
  { | expr then: expr } *
  [ | otherwise: expr ]
end

> ask :: Parser Expr
> ask = Ask <$> (keyword_ "ask" *> symbol_ ":" *> many branch)
>           <*> (optional otherwise <* keyword_ "end")
>   where
>       branch = (,) <$> try (symbol_ "|" *> expr <* keyword "then" <* symbol_ ":")
>                    <*> expr
>       otherwise = symbol_ "|" *> keyword_ "otherwise" *> symbol_ ":" *> expr
> 



> term :: Parser Expr
> term = choice [unaryMinus
>               ,lamE
>               ,explicitLet
>               ,ifE
>               ,ask
>               ,Iden <$> identifier
>               ,numE
>               ,stringE
>               ,parensE
>               ] <**> option id appSuffix
>                 -- <**> option id letConversion

