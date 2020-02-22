
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
>               ,Ref(..)
>               ,Program(..)
>               ,PreludeItem(..)
>               ,ProvideItem(..)
>               ,ImportSource(..)
>               )

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
>                              ,lineComment *> whiteSpace
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

todo: switch to a separate lexer

> keyword :: String -> Parser String
> keyword n = lexeme (try (string n <* notFollowedBy (satisfy (\a -> isAlphaNum a || a `elem` "?-+_"))))

> keyword_ :: String -> Parser ()
> keyword_ n = void $ keyword n

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
>     ,"ref"
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
>                   ,symbol "^"
>                   ,symbol "|>"
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
> ifE = do
>     keyword_ "if"
>     ife <- cond
>     nextBranch [ife]
>   where
>     cond = (,) <$> expr <*> (symbol_ ":" *> expr)
>     nextBranch bs =
>         choice [do
>                 x <- elsePart
>                 case x of
>                     Right el -> endif bs (Just el)
>                     Left b -> nextBranch (b:bs)
>                ,endif bs Nothing]
>     elsePart :: Parser (Either (Expr,Expr) Expr)
>     elsePart = do
>         keyword_ "else"
>         choice
>             [Right <$> (symbol_ ":" *> expr)
>             ,Left <$> (keyword_ "if" *> cond)
>             ]
>     endif bs el = keyword_ "end" *> pure (If (reverse bs) el)

> ask :: Parser Expr
> ask = do
>     keyword_ "ask"
>     symbol_ ":"
>     nextBranch []
>   where
>     nextBranch bs =
>         choice [do
>                 x <- branchPart
>                 case x of
>                     Right ot -> endask bs (Just ot)
>                     Left b -> nextBranch (b:bs)
>                ,endask bs Nothing]
>     branchPart :: Parser (Either (Expr,Expr) Expr)
>     branchPart = do
>         symbol_ "|"
>         choice
>             [Right <$> (keyword_ "otherwise" *> symbol_ ":" *> expr)
>             ,Left <$> ((,) <$> (expr <* keyword "then" <* symbol_ ":")
>                           <*> expr)
>             ]
>     endask bs ot = keyword_ "end" *> pure (Ask (reverse bs) ot)     
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
> tupleOrRecord :: Parser Expr
> tupleOrRecord = do
>     symbol_ "{"
>     choice [-- {} is an empty record, not an empty tuple
>             symbol_ "}" *> pure (Sel $ Record [])
>            ,eitherElement]
>   where
>     eitherElement = do
>         x <- expr
>         case x of
>             Iden i -> choice
>                 [do
>                  symbol_ ":"
>                  e <- expr
>                  moreRecord [(i,e)]
>                 ,moreTuple [x]]
>             _ -> moreTuple [x]
>     moreTuple ts = choice
>         [symbol_ "}" *> pure (Sel $ Tuple (reverse ts))
>         ,symbol ";" *> choice
>              [symbol_ "}" *> pure (Sel $ Tuple (reverse ts))
>              ,do
>               te <- expr
>               moreTuple (te:ts)]]
>     moreRecord fs = choice
>         [symbol_ "}" *> pure (Sel $ Record (reverse fs))
>         ,symbol "," *> choice
>              [symbol_ "}" *> pure (Sel $ Record (reverse fs))
>              ,do
>               f <- fld
>               moreRecord (f:fs)]]
>     fld = (,) <$> (identifier <* symbol_ ":") <*> expr


> dotSuffix :: Parser (Expr -> Expr)
> dotSuffix = symbol_ "." *>
>     choice [flip TupleGet <$> (symbol_ "{" *> nonNegativeInteger <* symbol_ "}")
>            ,flip DotExpr <$> identifier]

> cases :: Parser Expr
> cases = do
>     ty <- keyword_ "cases" *> parens identifier
>     t <- (expr <* symbol_ ":")
>     nextCase ty t []
>   where
>     nextCase ty t cs =
>         choice [do
>                 x <- casePart
>                 case x of
>                     Right el -> endCase ty t cs (Just el)
>                     Left c -> nextCase ty t (c:cs)
>                ,endCase ty t cs Nothing]
>     casePart :: Parser (Either (Pat,Expr) Expr)
>     casePart = do
>         symbol_ "|"
>         choice
>             [Right <$> (keyword_ "else" *> symbol_ "=>" *> expr)
>             ,Left <$> ((,) <$> pat <*> (symbol_ "=>" *> expr))]
>     endCase ty t cs el = keyword_ "end" *> pure (Cases ty t (reverse cs) el)

> pat :: Parser Pat
> pat = choice
>       [(IdenP <$> option NoShadow (Shadow <$ keyword_ "shadow")
>               <*> identifier) <**> option id vntPSuffix
>       ,TupleP <$> (symbol_ "{" *> xSep ';' pat <* symbol_ "}")]
>       <**> option id asPatSuffix

> asPatSuffix :: Parser (Pat -> Pat)
> asPatSuffix = f <$> (keyword_ "as" *> option NoShadow (Shadow <$ keyword_ "shadow"))
>                 <*> identifier
>    where
>      f a b c = AsP c a b

> vntPSuffix :: Parser (Pat -> Pat)
> vntPSuffix = do
>     x <- parens (commaSep pat)
>     pure (\(IdenP NoShadow y) -> VariantP y x)

todo: try remove this try
can parse a set box also then give a nice error message?
some options:
support statements, expressions and check expressions more flexibly in
the syntax so that can give nicer error messages
try to do the same thing, without putting it in the syntax
giving up on applicative parsing combinators and just doing monadic
stuff will make the second option more feasible
using monadic parsing will allow something like this:
x <- parsepatternorexpression
y <- getexpression x
  this will do a fail "pattern in expression context"
  if it can't be converted to an expression
  and there is a getpattern also

> unboxSuffix :: Parser (Expr -> Expr)
> unboxSuffix = flip Unbox <$> (try (symbol_ "!" *> identifier))


put all the parsers which start with a keyword first

> term :: Parser Expr
> term = do
>     x <- choice [unaryMinus
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
>               ]
>     choice [termSuffixes x, pure x]


> termSuffixes :: Expr -> Parser Expr
> termSuffixes x = option x $ do
>     y <- choice [pure x <**> appSuffix
>                 ,pure x <**> dotSuffix 
>                 ,pure x <**> unboxSuffix]
>     termSuffixes y

> exprSuffix :: Expr -> Parser Expr
> exprSuffix x = do
>     y <- termSuffixes x
>     pure y <**> option id binOpSuffix

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
todo: try must get rid of this try

maybe can allow patterns and expressions in every context for either
in the syntax and then give errors later. this will definitely make
it easy to give better error messages, but will make working with the
syntax more tedious

one option which concentrates the tediousness in one place, is to make
a parse syntax just for the parser, and then this converts to the
xproper ast and produces any errors

>     ,try $ do
>      x <- pat
>      choice [pure x <**> letDecl 
>             ,do
>              es <- exprSuffix (patternSyntaxToExpr x)
>              bb <- choice [testPost
>                           ,setRef
>                           ,pure StExpr]
>              pure (bb es)
>             ]
>      
>     ,expr <**> choice [testPost
>                       ,setRef
>                       ,pure StExpr]
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
> patternSyntaxToExpr (VariantP s p) = App (Iden s) $ map patternSyntaxToExpr p
> patternSyntaxToExpr (TupleP es) = Sel $ Tuple $ map patternSyntaxToExpr es
> --patternSyntaxToExpr (AsP {}) = Nothing

> whenStmt :: Parser Stmt
> whenStmt = When <$> (keyword_ "when" *> expr)
>            <*> (symbol_ ":" *> expr <* keyword_ "end")

> varDecl :: Parser Stmt
> varDecl = VarDecl <$> (keyword_ "var" *> binding)

todo: try can remove this try?
it's not awful atm, but if the try is removed, might still
get slightly better parse error messages

> setVarStmt :: Parser Stmt
> setVarStmt = try (SetVar <$> (identifier <* symbol_ ":=")
>            <*> expr)

> setRef :: Parser (Expr -> Stmt)
> setRef = flip SetRef
>          <$> (symbol_ "!{" *> commaSep1 rf <* symbol "}")
>   where
>     rf = (,) <$> identifier <*> (symbol_ ":" *> expr)
>          

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
>                     <$> identifier <*> option [] (parens (commaSep fld))
>     variant = VariantDecl
>               <$> (symbol_ "|" *> identifier)
>               <*> option [] (parens (commaSep fld))
>     fld = (,) <$> option Con (Ref <$ keyword_ "ref") <*> identifier

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
> program = Program <$> many preludeItem <*> many stmt

> preludeItem :: Parser PreludeItem
> preludeItem = provide <|> include <|> importItem


> provide :: Parser PreludeItem
> provide = Provide <$> (keyword_ "provide"
>                        *> symbol_ ":"
>                        *> commaSep provideItem
>                        <* keyword_ "end")

> provideItem :: Parser ProvideItem
> provideItem = choice
>     [ProvideAll <$ symbol_ "*"
>     ,do
>      a <- identifier
>      choice [ProvideAlias a <$> (keyword_ "as" *> identifier)
>             ,pure $ ProvideName a]
>     ]

> include :: Parser PreludeItem
> include = do
>     keyword_ "include"
>     choice [IncludeFrom
>             <$> (keyword_ "from" *> identifier <* symbol_ ":")
>             <*> (commaSep provideItem <* keyword_ "end")
>            ,Include <$> importSource]

> importSource :: Parser ImportSource
> importSource = do
>     a <- identifier
>     choice [ImportSpecial a <$> parens (commaSep stringRaw)
>            ,pure $ ImportName a]


todo: try remove this try

> importItem :: Parser PreludeItem
> importItem = keyword_ "import" *> (try importFrom <|> importAs)
>   where
>     importFrom = ImportNames
>                 <$> commaSep1 identifier
>                 <*> (keyword_ "from" *> importSource)
>                 
>     importAs = Import <$> importSource
>                       <*> (keyword_ "as" *> identifier)
