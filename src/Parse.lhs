
Parsing notes:

there isn't a separate lexer, which isn't great

in some places it lazily uses try, try should not be used for non
single token syntax imo, because it makes the error messages bad, and
also can affect the performance badly in some cases


todo: issues with what is supported already

doesn't deal with optional ; at end of tuple list
let parsing isn't quite right atm

you can put multiple commands on one line

it doesn't require 0 whitespace between an fn and the () app parens
this makes a lot of things which should work fail and is particularly bad

doesn't require ws around binops

doesn't quite support - in identifiers properly

doesn't parse binops properly at the moment. I think it's mostly the
case that it's too permissive compared with pyret. It doesn't use a
fixity parser either


> {-# LANGUAGE TupleSections,ScopedTypeVariables #-}
> module Parse (parseExpr
>              ,parseStmt
>              ,parseProgram) where


> import Text.Megaparsec (Parsec
>                        ,many
>                        ,(<|>)
>                        ,parse
>                        ,eof
>                        ,some
>                        ,choice
>                        ,option
>                        ,(<?>)
>                        --,manyTill
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
> import Control.Monad (guard, void)

> import Text.Megaparsec (satisfy, anySingle)

> import Text.Megaparsec.Char (letterChar)

> import Data.Char (isAlphaNum,isDigit)


> import Syntax (Stmt(..), Expr(..), Selector(..), VariantDecl(..)
>               ,Shadow(..)
>               ,Binding(..)
>               ,Pat(..)
>               ,Program(..)
>               ,Provide(..)
>               ,ProvideTypes(..)
>               ,Import(..)
>               ,ImportSource(..))

------------------------------------------------------------------------------

= api

> parseExpr :: FilePath -> String -> Either String Expr
> parseExpr fn src = either (Left . errorBundlePretty) Right $
>                    parse (whiteSpace *> expr <* eof) fn src

> parseStmt :: FilePath -> String -> Either String Stmt
> parseStmt fn src = either (Left . errorBundlePretty) Right $
>                    parse (whiteSpace *> stmt <* eof) fn src



> parseProgram :: FilePath -> String -> Either String Program
> parseProgram fn src = either (Left . errorBundlePretty) Right $
>                     parse (whiteSpace *> program <* eof) fn src

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

> blockComment :: Parser ()
> blockComment = startComment *> ctu
>   where
>     startComment = void $ try (string "#|")
>     endComment = void $ try (string "|#")
>     ctu = endComment <|> ((blockComment <|> void anySingle) *> ctu)

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
> reservedKeywords =
>     ["end", "lam", "let", "letrec", "if", "else", "ask", "then"
>     ,"otherwise", "block", "cases", "when", "var", "check"
>     ,"where", "fun", "rec", "data"
>     ,"import", "provide", "provide-types"
>     ,"from", "and", "or", "shadow", "as"
>     ]

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

> nonNegativeInteger :: Parser Int
> nonNegativeInteger = lexeme (read <$> takeWhile1P Nothing isDigit)

------------------------------------------------------------------------------

= expressions

> numE :: Parser Expr
> numE = do
>     x <- num
>     maybe (fail $ "parsing number failed: " ++ x)
>           (pure . Sel . Num) (readMaybe x)

todo: escape quotes
and make sure it doesn't parse newlines when it shouldn't

> stringRaw :: Parser String
> stringRaw = choice [char_ '\'' *> takeWhileP Nothing (/='\'') <* lexeme_ (char_ '\'')
>                          ,char_ '"' *> takeWhileP Nothing (/='"') <* lexeme_ (char_ '"')]
>             <?> "string literal"

> stringE :: Parser Expr
> stringE = (Sel . Str) <$> stringRaw
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
> binOpSym = choice ([symbol "+"
>                   ,symbol "*"
>                   ,try $ symbol "<="
>                   ,try $ symbol "=="
>                   ,try $ symbol ">="
>                   ,try $ symbol "<>"
>                   ,symbol "<"
>                   ,symbol ">"
>                   ,symbol "-"
>                   ,symbol "/"
>                   ] ++ map keyword
>                   ["and"
>                   ,"or"
>                   ])
> 

> unaryMinus :: Parser Expr
> unaryMinus = UnaryMinus <$> (symbol "-" *> term)

> lamE :: Parser Expr
> lamE = Lam <$> (keyword_ "lam" *> parens (commaSep pat) <* symbol_ ":")
>            <*> (expr <* keyword_ "end")
>            

> expressionLet :: Parser Expr
> expressionLet = keyword_ "let" *> letBody Let
>
> letBody :: ([Binding] -> Expr -> Expr) -> Parser Expr
> letBody ctor = ctor <$> commaSep1 binding
>                    <*> (symbol_ ":" *> expr <* keyword_ "end")

> binding :: Parser Binding
> binding = Binding <$> pat
>                        <*> (symbol_ "=" *> expr)

> expressionLetRec :: Parser Expr
> expressionLetRec = keyword_ "letrec" *> letBody LetRec


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

> construct :: Parser Expr
> construct = Construct <$> (symbol_ "[" *> (Iden <$> identifier) <* symbol_ ":")
>             <*> (commaSep expr <* symbol_ "]")
>
> -- optional here doesn't work, need to refactor it
> -- also should left factor this, especially the try
> -- as used here will make the error messages worse
> tupleOrRecord :: Parser Expr
> tupleOrRecord =
>     choice [try ((Sel . Tuple) <$> (symbol_ "{" *> xSep ';' expr <* optional (symbol_ ";") <* symbol_ "}"))
>            ,(Sel . Record) <$> (symbol_ "{" *> commaSep fld <* symbol_ "}")]
>   where
>     fld = (,) <$> (identifier <* symbol_ ":") <*> expr


> dotSuffix :: Parser (Expr -> Expr)
> dotSuffix = symbol_ "." *>
>     choice [flip TupleGet <$> (symbol_ "{" *> nonNegativeInteger <* symbol_ "}")
>            ,flip DotExpr <$> identifier]

> cases :: Parser Expr
> cases = Cases <$> (keyword_ "cases" *> parens identifier)
>               <*> (expr <* symbol_ ":")
>               <*> some cas
>               <*> (optional els <* keyword_ "end")
>   where
>      cas = (,) <$> try (symbol_ "|" *> pat)
>                <*> (symbol_ "=>" *> expr)
>      els = symbol_ "|" *> keyword_ "else" *> symbol_ "=>" *> expr

> pat :: Parser Pat
> pat = choice
>       [(IdenP <$> option NoShadow (Shadow <$ keyword_ "shadow")
>               <*> identifier) <**> option id ctorPSuffix
>       ,TupleP <$> (symbol_ "{" *> xSep ';' pat <* symbol_ "}")]
>       <**> option id asPatSuffix

> asPatSuffix :: Parser (Pat -> Pat)
> asPatSuffix = flip AsP <$> (keyword_ "as" *> identifier)

> ctorPSuffix :: Parser (Pat -> Pat)
> ctorPSuffix = do
>     x <- parens (commaSep pat)
>     pure (\(IdenP NoShadow y) -> CtorP y x)


put all the parsers which start with a keyword first

> term :: Parser Expr
> term = choice [unaryMinus
>               ,lamE
>               ,expressionLetRec
>               ,expressionLet
>               ,ifE
>               ,ask
>               ,block
>               ,cases
>               ,Iden <$> identifier
>               ,numE
>               ,stringE
>               ,parensE
>               ,construct
>               ,tupleOrRecord
>               ] <**> option id (appSuffix <|> dotSuffix)


> exprSuffix :: Parser (Expr -> Expr)
> exprSuffix = do
>     x <- option id (appSuffix <|> dotSuffix)
>     y <- option id binOpSuffix
>     pure (y . x)

------------------------------------------------------------------------------

= statements

> stmt :: Parser Stmt
> stmt = choice
>     [whenStmt
>     ,recDecl
>     ,funDecl
>     ,varDecl
>     ,dataDecl
>     ,checkBlock
>     ,setVarStmt

did all this hard work, but still ended up needing the try ...
need to change pat to patOrExpression
  so it can parse stuff that starts like a pattern
  but then can only be an expression after that

>     ,try pat <**> choice [letDecl
>                          ,do
>                           es <- option id exprSuffix
>                           bb <- option StExpr testPost
>                           pure $ bb . es . patternSyntaxToExpr
>                          ]
>     ,expr <**> option StExpr testPost
>     ]

parsing let decls:

a lot of patterns before a =, look just like expressions. we can't
tell which one it is until later. to avoid massive use of try, left
factor this,

so there is a 'pattern or expression' parser which can then become
either depending on what follows. this means having to maintain an
extra data type just for the parsing here that has to be kept in sync
with the syntax (the parser itself has to be kept in sync also)

if there are no patterns that can't be expressions also,
then can just use the pattern syntax here. do that until it no longer
works

> patternSyntaxToExpr :: Pat -> Expr
> patternSyntaxToExpr (IdenP NoShadow s) = Iden s
> patternSyntaxToExpr (CtorP s p) = App (Iden s) $ map patternSyntaxToExpr p
> patternSyntaxToExpr (TupleP es) = Sel $ Tuple $ map patternSyntaxToExpr es
> --patternSyntaxToExpr (AsP {}) = Nothing

> whenStmt :: Parser Stmt
> whenStmt = When <$> (keyword_ "when" *> expr)
>            <*> (symbol_ ":" *> expr <* keyword_ "end")

> varDecl :: Parser Stmt
> varDecl = VarDecl <$> (keyword_ "var" *> binding)

> setVarStmt :: Parser Stmt
> setVarStmt = try (SetVar <$> identifier
>            <*> (symbol_ ":=" *> expr))

> checkBlock :: Parser Stmt
> checkBlock = Check
>      <$> (keyword "check" *> optional checkName <* symbol_ ":")
>      <*> (many stmt <* keyword_ "end")
>   where
>     checkName = stringRaw <?> "check block name"

> whereBlock :: Parser [Stmt]
> whereBlock = keyword_ "where" *> symbol_ ":" *> many stmt

> funDecl :: Parser Stmt
> funDecl = FunDecl
>     <$> (keyword "fun" *> identifier)
>     <*> parens (commaSep pat)
>     <*> (symbol_ ":" *> (unwrapSingle <$>
>          (Block <$> some stmt)))
>     <*> (optional whereBlock <* keyword_ "end")
>     
>   where
>       unwrapSingle (Block [StExpr (a)]) = a
>       unwrapSingle x = x

> recDecl :: Parser Stmt
> recDecl = RecDecl <$> (keyword_ "rec" *> binding)

> dataDecl :: Parser Stmt
> dataDecl = DataDecl
>     <$> (keyword_ "data" *> identifier <* symbol_ ":")
>     <*> (((:[]) <$> singleVariant) <|> some variant)
>     <*> (optional whereBlock <* keyword_ "end")
>   where
>     singleVariant = VariantDecl
>                     <$> identifier <*> option [] (parens (commaSep identifier))
>     variant = VariantDecl
>               <$> (symbol_ "|" *> identifier)
>               <*> option [] (parens (commaSep identifier))

> letDecl :: Parser (Pat -> Stmt)
> letDecl = f <$> (symbol_ "=" *> expr)
>    where
>        f y x = LetDecl (Binding x y)

-----------------------------------------

> testPost :: Parser (Expr -> Stmt)
> testPost = choice [isPred, postOp, inf]
>   where
>       isPred = do
>           t <- keyword "is%" <|> keyword "is-not%"
>           p <- parens expr
>           e2 <- expr
>           pure (\x -> TPred x t p e2)
>       postOp = do
>           keyword_ "does-not-raise"
>           pure (flip TPostfixOp "does-not-raise")
>       inf = do
>           k <- choice $ map keyword tks
>           e <- expr
>           pure (\x -> StExpr $ BinOp x k e)
>       tks = ["is-not", "is"
>             ,"raises-other-than", "raises-satisfies", "raises-violates"
>             ,"satisfies", "violates", "raises"]

> program :: Parser Program
> program = Program <$> optional provide <*> optional provideTypes <*> many importS <*> many stmt

> provide :: Parser Provide
> provide = try (keyword_ "provide" *> choice
>     [Provide <$> (symbol_ "{" *> commaSep p <* symbol_ "}" <* keyword_ "end")
>     ,ProvideAll <$ symbol_ "*"])
>   where
>     p = (,) <$> identifier <*> (symbol_ ":" *> identifier)

> provideTypes :: Parser ProvideTypes
> provideTypes = keyword_ "provide-types" *> choice
>     [ProvideTypes <$> (symbol_ "{" *> commaSep p <* symbol_ "}")
>     ,ProvideTypesAll <$ symbol_ "*"] 
>   where
>     p = (,) <$> identifier <*> (symbol_ "::" *> identifier)

> importS :: Parser Import
> importS = do
>     keyword_ "import"
>     choice [try importFrom
>            ,importAs]
>   where
>     importFrom = ImportFrom <$> commaSep identifier <*> (keyword_ "from" *> importSource)
>     importSource = choice [try (ImportSpecial <$> identifier
>                                 <*> parens (commaSep stringRaw))
>                           ,ImportString <$> stringRaw
>                           ,ImportName <$> identifier]
>     importAs = Import <$> importSource <*> (keyword_ "as" *> identifier)

