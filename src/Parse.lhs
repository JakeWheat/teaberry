
> module Parse (parseExpr
>              ,parseStmt
>              ,parseStmts) where


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

> import Data.Void (Void)

> import Text.Read (readMaybe)


> import Text.Megaparsec (errorBundlePretty)
> import Text.Megaparsec.Char (space
>                             ,char
>                             ,string
>                             )

> import Control.Applicative ((<**>))
> import Control.Monad (guard)

> import Text.Megaparsec (satisfy, anySingle)

> import Text.Megaparsec.Char (letterChar)

> import Data.Char (isAlphaNum,isDigit)


> import Syntax

------------------------------------------------------------------------------

= api

> parseExpr :: String -> String -> Either String Expr
> parseExpr fn src = either (Left . errorBundlePretty) Right $
>                    parse (whiteSpace *> expr <* eof) fn src

> parseStmt :: String -> String -> Either String Stmt
> parseStmt fn src = either (Left . errorBundlePretty) Right $
>                    parse (whiteSpace *> stmt <* eof) fn src


> parseStmts :: String -> String -> Either String [Stmt]
> parseStmts fn src = either (Left . errorBundlePretty) Right $
>                     parse (whiteSpace *> many stmt <* eof) fn src


------------------------------------------------------------------------------

= boilerplate

> type Parser = Parsec Void String

------------------------------------------------------------------------------

= token parsing

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

> symbol :: String -> Parser String
> symbol x = lexeme (string x)

> symbol_ :: String -> Parser ()
> symbol_ x = lexeme_ (string x)


> parensE :: Parser Expr
> parensE = Parens <$> parens expr


todo: make an identifier only end on a alphanum or underscore
add : to the identifierS?

todo: don't parse reserved keywords with this function

here's an idea: push reserved keywords distinction into the lexer
the can get rid of more trys

> reservedKeywords :: [String]
> reservedKeywords = ["end"]

> identifierX :: Parser String
> identifierX =
>     lexeme ((:)
>     <$> (letterChar <|> char '_' <|> char '-')
>     <*> takeWhileP Nothing (\a -> (isAlphaNum a || a `elem` "?-+_")))
>     <?> "identifier"

> identifier :: Parser String
> identifier = try $ do
>     i <- identifierX
>     guard (i `notElem` reservedKeywords)
>     pure i


TODO: add support for all the pyret numbers
parse them properly

consider what other numbers to support, e.g. integer, positive
 integer, 64 bit integer, different precision roughnums

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

------------------------------------------------------------------------------

= expressions

> numE :: Parser Expr
> numE = do
>     x <- num
>     maybe (fail $ "parsing number failed: " ++ x)
>           (pure . Sel . Num) (readMaybe x)

todo: escape quotes
and make sure it doesn't parse newlines when it shouldn't

> stringE :: Parser Expr
> stringE = (Sel . Str) <$> choice [char_ '\'' *> takeWhileP Nothing (/='\'') <* lexeme_ (char_ '\'')
>                          ,char_ '"' *> takeWhileP Nothing (/='"') <* lexeme_ (char_ '"')]
>             <?> "string literal"

> appSuffix :: Parser (Expr -> Expr)
> appSuffix = flip App <$> parens (commaSep expr)


> expr :: Parser Expr
> expr = term <**> option id binOpSuffix

> binOpSuffix :: Parser (Expr -> Expr)
> binOpSuffix = do
>     op <- binOpSym
>     b <- expr
>     pure $ bo op b
>   where
>     bo op b a = BinOp a op b


todo: remove the trys by implementing a proper lexer or a lexer style
 approach

> binOpSym :: Parser String
> binOpSym = choice [symbol "+"
>                   ,symbol "*"
>                   ,try $ symbol "<="
>                   ,try $ symbol "=="
>                   ,try $ symbol ">="
>                   ,try $ symbol "<>"
>                   ,symbol "<"
>                   ,symbol ">"
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
> explicitLet = keyword_ "let" *> letBody Let
>
> letBody :: ([(String, Expr)] -> Expr -> Expr) -> Parser Expr
> letBody ctor = ctor <$> commaSep1 bind
>                    <*> (symbol_ ":" *> expr <* keyword_ "end")
>     where
>         bind = (,) <$> identifier
>                    <*> (symbol_ "=" *> expr)

> explicitLetRec :: Parser Expr
> explicitLetRec = keyword_ "letrec" *> letBody LetRec


> ifE :: Parser Expr
> ifE = If <$> (keyword_ "if" *> ((:) <$> cond <*> many elseif))
>          <*> (optional elseE <* keyword_ "end")
>     where
>         cond = (,) <$> expr <*> (symbol_ ":" *> expr)
>         elseif = (try (keyword_ "else" *> keyword_ "if") *> cond)
>         elseE = keyword_ "else" *> symbol ":" *> expr

> ask :: Parser Expr
> ask = Ask <$> (keyword_ "ask" *> symbol_ ":" *> many branch)
>           <*> (optional ow <* keyword_ "end")
>   where
>       branch = (,) <$> try (symbol_ "|" *> expr <* keyword "then" <* symbol_ ":")
>                    <*> expr
>       ow = symbol_ "|" *> keyword_ "otherwise" *> symbol_ ":" *> expr
> 

> block :: Parser Expr
> block = Block <$>
>     (keyword_ "block" *> symbol_ ":" *>
>     some stmt
>     <* keyword_ "end")
>     

put all the parsers which start with a keyword first

> term :: Parser Expr
> term = choice [unaryMinus
>               ,lamE
>               ,explicitLetRec
>               ,explicitLet
>               ,ifE
>               ,ask
>               ,block
>               ,Iden <$> identifier
>               ,numE
>               ,stringE
>               ,parensE
>               ] <**> option id appSuffix

------------------------------------------------------------------------------

= statements

> stmt :: Parser Stmt
> stmt = choice
>     [whenStmt
>     ,recDecl
>     ,funDecl
>     ,varDecl
>     ,setVarStmt
>     ,expr <**> option StExpr letDecl
>     ]

> whenStmt :: Parser Stmt
> whenStmt = When <$> (keyword_ "when" *> expr)
>            <*> (symbol_ ":" *> expr <* keyword_ "end")

> varDecl :: Parser Stmt
> varDecl = VarDecl <$> (keyword_ "var" *> identifier)
>            <*> (symbol_ "=" *> expr)

> setVarStmt :: Parser Stmt
> setVarStmt = try (SetVar <$> identifier
>            <*> (symbol_ ":=" *> expr))


> funDecl :: Parser Stmt
> funDecl = FunDecl
>     <$> (keyword "fun" *> identifier)
>     <*> parens (commaSep identifier)
>     <*> (symbol_ ":" *> (unwrapSingle <$>
>          (Block <$> (some stmt <* keyword_ "end"))))
>   where
>       unwrapSingle (Block [StExpr (a)]) = a
>       unwrapSingle x = x

> recDecl :: Parser Stmt
> recDecl = RecDecl
>     <$> (keyword_ "rec" *> identifier)
>     <*> (symbol_ "=" *> expr)


> letDecl :: Parser (Expr -> Stmt)
> letDecl = f <$> (symbol_ "=" *> expr)
>    where
>        f y (Iden x) = LetDecl x y
>        --f _ x = fail ("bad pattern: " ++ show x)
