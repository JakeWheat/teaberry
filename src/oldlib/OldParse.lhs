
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

doesn't quite support - in identifiers properly (I think, maybe it
does, it's a little hacky with the potential overlap with binary -
operator atm because of the whitespace issue)

doesn't parse binops properly at the moment. I think it's mostly the
case that it's too permissive compared with pyret. It doesn't use a
fixity parser either, so not using parens with multiple binops
produces unexpected asts


imroving the error messages:

these are some rules determined by the parser error testing

use 'eof <?> ""' to avoid '... or end of input' in error messages

whenever there is some sort of optional suffix, use '<?> ""' to hide
   it from error messages
  I prefer to try to use it at the use point, than in the suffix parser
    function itself
same if there is something embedded that's optional
  option NoShadow (Shadow <$ keyword_ "shadow")
  ->
  (option NoShadow (Shadow <$ keyword_ "shadow")) <?> ""
  probably better with a helper function

for things like expressions, patterns, terms, etc.
  use <?> "expression" or whatever, to describe the next expected node,
  and avoid a long list of each possibility
  e.g.
  expecting "ask", "block", "cases", "if", "lam", "let", "letrec", '(', '-', '[', '{', identifier, number, or string literal
  ->
  expecting expression


> {-# LANGUAGE TupleSections,ScopedTypeVariables, MultiWayIf, LambdaCase #-}
> module OldParse (parseExpr
>              ,parseStmt
>              ,parseModule) where

> --import Debug.Trace(trace)
> --import Text.Show.Pretty (ppShow)

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
> import Control.Monad ({-guard-} when, void)

> import Text.Megaparsec (satisfy, anySingle)

> import Text.Megaparsec.Char (letterChar)

> import Data.Char (isAlphaNum,isDigit)


> import OldSyntax (Stmt(..), Expr(..), Selector(..), VariantDecl(..)
>               ,Shadow(..)
>               ,Binding(..)
>               ,Pat(..)
>               ,PatName(..)
>               ,Ref(..)
>               ,Module(..)
>               ,PreludeStmt(..)
>               ,ProvideItem(..)
>               ,ImportSource(..)
>               )

------------------------------------------------------------------------------

= api

> parseExpr :: FilePath -> String -> Either String Expr
> parseExpr fn src = either (Left . errorBundlePretty) Right $
>                    parse (whiteSpace *> expr <* myEof) fn src

> parseStmt :: FilePath -> String -> Either String Stmt
> parseStmt fn src = either (Left . errorBundlePretty) Right $
>                    parse (whiteSpace *> stmt <* myEof) fn src



> parseModule :: FilePath -> String -> Either String Module
> parseModule fn src = either (Left . errorBundlePretty) Right $
>                     parse (whiteSpace *> pmodule <* myEof) fn src

------------------------------------------------------------------------------

= boilerplate

> type Parser = Parsec Void String

------------------------------------------------------------------------------

error message helpers

> myEof :: Parser ()
> myEof = eof <?> ""

> boption :: a -> Parser a -> Parser a
> boption v p = (option v p) <?> ""
>  
> boptional :: Parser a -> Parser (Maybe a)
> boptional p = optional p <?> ""

> bchoice :: [Parser a] -> Parser a
> bchoice cs = choice $ addEmpty cs
>   where
>     addEmpty (x:xs@(_:_)) = (x <?> "") : addEmpty xs
>     addEmpty [x] = [x]
>     addEmpty [] = []

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
>     startComment = void (try (string "#|") <?> "")
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
>     when (i `elem` reservedKeywords)
>         $ fail $ "unexpected keyword: " ++ i
>     --guard (i `notElem` reservedKeywords)
>     pure i


TODO: add support for all the pyret numbers
parse them properly

consider what other numbers to support, e.g. integer, positive
 integer, 64 bit integer, different precision roughnums

> num :: Parser String
> num = lexeme (
>     choice [digits <**> bchoice [eSuffix,dotSuffixOnly,pure id]
>            ,myChar '.' <**> afterDot
>            ]
>    -- this is for definitely avoiding possibly ambiguous source
>    -- not sure if it is needed
>     <* notFollowedBy (satisfy (`elem` "eE."))) <?> "number"
>   where
>     -- parse one or more 0-9
>     digits = takeWhile1P Nothing isDigit
>     -- parse .[digits][e[+-]digits]
>     dotSuffixOnly = append <$> (myChar '.' <**> bchoice [afterDot, eSuffix, pure id])
>     -- parse digits[e[+-]digits], used after the .
>     afterDot = append <$> (digits <**> bchoice [eSuffix, pure id])
>     -- parse e[+-]digits
>     eSuffix = append <$> concatA [myChar 'e', optionalPlusOrMinus,digits]
>     optionalPlusOrMinus = boption "" (myChar '+' <|> myChar '-')
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
>                    ,char_ '"' *> takeWhileP Nothing (/='"') <* lexeme_ (char_ '"')]
>             <?> "string literal"

> stringE :: Parser Expr
> stringE = (Sel . Text) <$> stringRaw
>             <?> "string literal"

> appSuffix :: Parser (Expr -> Expr)
> appSuffix = flip App <$> parens (commaSep expr)


> expr :: Parser Expr
> expr = term <**> boption id binOpSuffix

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
>     many stmt -- todo: an empty block should be an error later, not during parsing
>     <* keyword_ "end")
>     

> construct :: Parser Expr
> construct = Construct <$> (symbol_ "[" *> (Iden <$> identifier) <* symbol_ ":")
>             <*> (commaSep expr <* symbol_ "]")
>

> tupleOrRecord :: Parser Expr
> tupleOrRecord = tupleOrRecord2 (Sel . RecordSel)
>                                (Sel . TupleSel)
>                                expr
>                                (\case
>                                      Iden i -> Just i
>                                      _ -> Nothing)

> tupleOrRecordP :: Parser EPExpr
> tupleOrRecordP = tupleOrRecord2 (EPExpr . Sel . RecordSel)
>                                  (EPSel . EPTupleP)
>                                  epExpr
>                                  (\case
>                                        EPExpr (Iden i) -> Just i
>                                        _ -> Nothing)
>                                

> tupleOrRecord2 :: ([(String, Expr)] -> a)
>                -> ([a] -> a)
>                -> Parser a
>                -> (a -> Maybe String)
>                -> Parser a
> tupleOrRecord2 mkRecSel mkTupSel pTupEl extractIden = do
>     symbol_ "{"
>     choice [-- {} is an empty record, not an empty tuple
>             symbol_ "}" *> pure (mkRecSel [])
>            ,eitherElement]
>   where
>     eitherElement = do
>         x <- pTupEl
>         if | Just i <- extractIden x -> choice
>                 [do
>                  symbol_ ":"
>                  e <- expr
>                  moreRecord [(i,e)]
>                 ,moreTuple [x]]
>            | otherwise -> moreTuple [x]
>     moreTuple ts = choice
>         [symbol_ "}" *> pure (mkTupSel (reverse ts))
>         ,symbol ";" *> choice
>              [symbol_ "}" *> pure (mkTupSel (reverse ts))
>              ,do
>               te <- pTupEl
>               moreTuple (te:ts)]]
>     moreRecord fs = choice
>         [symbol_ "}" *> pure (mkRecSel (reverse fs))
>         ,symbol "," *> choice
>              [symbol_ "}" *> pure (mkRecSel (reverse fs))
>              ,do
>               f <- fld
>               moreRecord (f:fs)]]
>     fld = (,) <$> (identifier <* symbol_ ":") <*> expr
> 

> dotSuffix :: Parser (Expr -> Expr)
> dotSuffix = symbol_ "." *>
>     bchoice [flip TupleGet <$> (symbol_ "{" *> nonNegativeInteger <* symbol_ "}")
>             ,flip DotExpr <$> identifier]

> cases :: Parser Expr
> cases = do
>     ty <- keyword_ "cases" *> parens typeName
>     t <- (expr <* symbol_ ":")
>     nextCase ty t []
>   where
>     typeName = (do
>         i <- identifier
>         -- todo: don't allow whitespace?
>         choice [do
>                 j <- char '.' *> identifier
>                 pure $ i ++ "." ++ j
>                ,pure i]) <?> "type name"
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
>             ,Left <$> ((,) <$> (pat <?> "pattern") <*> (symbol_ "=>" *> expr))]
>     endCase ty t cs el = keyword_ "end" *> pure (Cases ty t (reverse cs) el)

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
> term = (do
>     x <- choice [unaryMinus
>                 ,lamE
>                 ,expressionLetRec
>                 ,expressionLet
>                 ,ifE
>                 ,ask
>                 ,block
>                 ,cases
>                 ,Iden <$> identifier
>                 ,numE
>                 ,stringE
>                 ,parensE
>                 ,construct
>                 ,tupleOrRecord
>                 ]
>     bchoice [termSuffixes x, pure x]) <?> "expression"


> termSuffixes :: Expr -> Parser Expr
> termSuffixes x = boption x $ do
>     y <- choice [pure x <**> appSuffix
>                 ,pure x <**> dotSuffix 
>                 ,pure x <**> unboxSuffix]
>     termSuffixes y

> exprSuffix :: Expr -> Parser Expr
> exprSuffix x = do
>     y <- termSuffixes x
>     pure y <**> boption id binOpSuffix

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
>     ,startsWithExprOrPattern
>     ] <?> "statement"


parsing a statement left factored (for good error messages)

1. parse a pattern or expression - need a new syntax for this

what can start with this:
let decl (incl. pattern binding (if it looks like a pattern)
set var (if it looks like an iden)
setref (if it looks like an expression)
statement expressions:
  iden (if it looks like an iden)
  tupleget (if it looks like an expr)
  dotexpr (if it looks like an expr)
  app (if it looks like an expr)
  binop (if it looks like an expr)
  unbox (if it looks like an expr)

need to run through this path, trying to disambiguate
three classes:
  parse a pattern or expression
  then ask: can it be an iden, can it be an expr, can it be a pattern
    assume it could be any combination of these
let decl -> '=' is next
set var ':=' is next
setref '!{' is next

  tupleget '.{' is next
  unbox '!' is next
  dotexpr '.' is next
  app '(' is next
  binop an operator is next
how can we choose, and hook into stuff
we have a parser which continues an expression
  with app, binop, tupleget, dotexpr, unbox
  all the statemnt expressions
so:
can it be a pattern:
  = -> let decl
can it be an iden -> := -> set var
can it be an expr -> !{ -> setref
can it be an expr ->
  optional exprsuffix

use guard for these 'can it be' sections
a guard won't commit so doesn't need try

the pattern or expr parser has to replace the expr parser
in expr only contexts, create a wrapper which does a fail "pattern in
expr context" or something?
there are plenty of times when we know we only want a expr and then
we can say 'pattern in expr context' in the parser
maybe can do this with patterns as well?
if there's an error, it's more ok to try to restart that part with a
  different parser that is designed to give good error messages,
  once you know more about the context

what do patterns parse that exprs don't:
_ - this already parses as an identifier
as x
shadow

1. create a new data to parse these pattern or expressions
2. adjust the parser to be able to parse these
  is this a big job?
  don't adjust the parser to add 'as x' yet, get the rest working first
  then come back to this when everything else is done
  the new data type is expr with just asP added as a ctor
  which won't be hit until this is added to the parsing
3. create the wrapper to convert to an expression with a fail
4. implement the above branching

> startsWithExprOrPattern :: Parser Stmt
> startsWithExprOrPattern = do
>     ex <- epExpr
>     --trace (ppShow ex) $
>     choice
>         [do
>          EPExpr (Iden i) <- pure ex
>          SetVar i <$> ((symbol_ ":=" <?> "") *> expr)
>         ,do
>          Just p <- pure $ epExprToPat ex
>          e <- ((symbol_ "=" <?> "") *> expr)
>          pure $ LetDecl (Binding p e)
>         ,do
>          Just ex' <- pure $ epExprToExpr ex
>          bchoice
>             [do
>              let rf = (,) <$> identifier <*> (symbol_ ":" *> expr)
>              SetRef ex' <$> (symbol_ "!{" *> commaSep1 rf <* symbol "}")
>             ,do
>              exprSuffix ex' <**> bchoice [testPost
>                                          ,pure StExpr]]]
>            

very hacky

how to deal with this awful mess?
1. don't be lazy and wrap expr like epexpr does
just reproduce the whole ast of expr and pattern in a new type
2. don't be lazy and try to wrap this around the expr parser
refactor all the parsing to parse patterns or expressions
then can add the guards and wrappers to be able to parse either, or
only parse on or the other depending on the context. switching might
need to be scoped?
this will be a little dynamic, because we will assume when e.g. it's
in expression only mode, it will parse something that will convert to
an expr without the possibility of failing, which the type system
won't check. I think the alternative is a lot more duplication of
parsing code, which is worse, but either method is viable


> data EPExpr = EPExpr Expr
>             | EPSel EPSelector
>             | EPParens EPExpr
>             | EPApp Expr [EPExpr]
>             | EPShadowIden String
>             | EPAsP EPExpr Shadow String
>             deriving (Eq,Show) 

> data EPSelector = EPTupleP [EPExpr]
>                 | EPSelector Selector
>             deriving (Eq,Show) 

> epSelToSel :: EPSelector -> Maybe Selector
> epSelToSel (EPSelector s) = Just s
> epSelToSel (EPTupleP es) = do
>     es' <- mapM epExprToExpr es
>     pure $ TupleSel es'

> epExprToExpr :: EPExpr -> Maybe Expr
> epExprToExpr (EPExpr e) = Just e
> epExprToExpr (EPSel s) = Sel <$> epSelToSel s
> epExprToExpr (EPParens e) = Parens <$> epExprToExpr e
> epExprToExpr (EPApp e es) = do
>     es' <- mapM epExprToExpr es
>     pure $ App e es'
> epExprToExpr (EPShadowIden {}) = Nothing
> epExprToExpr (EPAsP {}) = Nothing


> epExprToPat :: EPExpr -> Maybe Pat
> epExprToPat (EPExpr (Iden i)) = Just $ IdenP NoShadow (PatName i)
> epExprToPat (EPApp (Iden f) ps) = do
>     ps' <- mapM epExprToPat ps
>     pure $ VariantP (PatName f) ps'
> epExprToPat (EPSel (EPTupleP ps)) = do
>     ps' <- mapM epExprToPat ps
>     pure $ TupleP ps'
> epExprToPat (EPSel (EPSelector (TupleSel es))) = do
>     ps' <- mapM (epExprToPat . EPExpr) es
>     pure $ TupleP ps'
> epExprToPat (EPExpr (Sel (TupleSel es))) = do
>     ps' <- mapM (epExprToPat . EPExpr) es
>     pure $ TupleP ps'
> epExprToPat (EPExpr (App (Iden f) es)) = do
>     ps' <- mapM (epExprToPat . EPExpr) es
>     pure $ VariantP (PatName f) ps'

> epExprToPat (EPAsP p s i) = do
>     p' <- epExprToPat p
>     pure $ AsP p' s i
> epExprToPat (EPShadowIden i) = Just $ IdenP Shadow (PatName i)
> epExprToPat _ = Nothing

todo: when the flags are added to tell the parser whether to accept
expressions or patterns or both, the error message should say
'expression' if expressions are accepted, and if only patterns are
accepted, it should say 'pattern', so if both are possible, it
should just say expression

> epExpr :: Parser EPExpr
> epExpr = do
>     x <- bchoice
>         [(EPShadowIden <$> (keyword_ "shadow" *> identifier))
>         ,(EPParens <$> parens epExpr)
>         ,tupleOrRecordP
>         ,EPExpr <$> expr]
>     bchoice [(EPAsP x <$> (keyword_ "as" *> boption NoShadow (Shadow <$ keyword_ "shadow"))
>                     <*> identifier)
>             ,pure x]


1. create ExprOrPattern
   add as, shadow iden
2. rewrite the exprToPattern to work on this
3. write eorptoExpr
4. see everything work5
5. add wrappers for just expr and just pattern
   switch them in
   adjust to use a flag in the parser to allow either either, just
   exprs or just patterns?
   can also add a flag to allow test operators or not


> pat :: Parser Pat
> pat = bchoice
>       [TupleP <$> ((symbol_ "{" <?> "") *> xSep ';' pat <* symbol_ "}")
>       ,do
>        s <- boption NoShadow (Shadow <$ keyword_ "shadow")
>        i <- identifier
>        i' <- choice [do
>                      j <- char '.' *> identifier
>                      pure $ QPatName i j
>                     ,pure $ PatName i]
>        choice [do
>                as <- parens (commaSep pat)
>                pure $ VariantP i' as
>               ,pure $ IdenP s i']
>       <**> boption id asPatSuffix]

> asPatSuffix :: Parser (Pat -> Pat)
> asPatSuffix = f <$> (keyword_ "as" *> boption NoShadow (Shadow <$ keyword_ "shadow"))
>                 <*> identifier
>    where
>      f a b c = AsP c a b

> {-vntPSuffix :: Parser (Pat -> Pat)
> vntPSuffix = do
>     x <- parens (commaSep pat)
>     pure (\(IdenP NoShadow y) -> VariantP y x)-}

> whenStmt :: Parser Stmt
> whenStmt = When <$> (keyword_ "when" *> expr)
>            <*> (symbol_ ":" *> expr <* keyword_ "end")

> varDecl :: Parser Stmt
> varDecl = VarDecl <$> (keyword_ "var" *> binding)

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
>     <*> (boptional whereBlock <* keyword_ "end")
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
>     <*> (boptional whereBlock <* keyword_ "end")
>   where
>     singleVariant = VariantDecl
>                     <$> identifier <*> boption [] (parens (commaSep fld))
>     variant = VariantDecl
>               <$> (symbol_ "|" *> identifier)
>               <*> boption [] (parens (commaSep fld))
>     fld = (,) <$> boption Con (Ref <$ keyword_ "ref") <*> identifier

-----------------------------------------

> testPost :: Parser (Expr -> Stmt)
> testPost = choice [isPred, postOp, inf]
>   where
>       isPred = do
>           t <- (keyword "is%" <|> keyword "is-not%") <?> ""
>           p <- parens expr
>           e2 <- expr
>           pure (\x -> TPred x t p e2)
>       postOp = do
>           keyword_ "does-not-raise" <?> ""
>           pure (flip TPostfixOp "does-not-raise")
>       inf = do
>           k <- choice (map keyword tks) <?> ""
>           e <- expr
>           pure (\x -> StExpr $ BinOp x k e)
>       tks = ["is-not", "is"
>             ,"raises-other-than", "raises-satisfies", "raises-violates"
>             ,"satisfies", "violates", "raises"]

> pmodule :: Parser Module
> pmodule = Module <$> many preludeStmt <*> many stmt

> preludeStmt :: Parser PreludeStmt
> preludeStmt = (provide <|> include <|> importStmt) <?> ""


> provide :: Parser PreludeStmt
> provide = Provide <$> (keyword_ "provide"
>                        *> symbol_ ":"
>                        *> commaSep provideStmt
>                        <* keyword_ "end")

> provideStmt :: Parser ProvideItem
> provideStmt = choice
>     [ProvideAll <$ symbol_ "*"
>     ,do
>      a <- identifier
>      bchoice [ProvideAlias a <$> (keyword_ "as" *> identifier)
>             ,pure $ ProvideName a]
>     ]

> include :: Parser PreludeStmt
> include = do
>     keyword_ "include"
>     choice [IncludeFrom
>             <$> (keyword_ "from" *> identifier <* symbol_ ":")
>             <*> (commaSep provideStmt <* keyword_ "end")
>            ,Include <$> importSource]

> importSource :: Parser ImportSource
> importSource = do
>     a <- identifier
>     bchoice [ImportSpecial a <$> parens (commaSep stringRaw)
>             ,pure $ ImportName a]


todo: try remove this try

> importStmt :: Parser PreludeStmt
> importStmt = keyword_ "import" *> (try importFrom <|> importAs)
>   where
>     importFrom = ImportNames
>                 <$> commaSep1 identifier
>                 <*> (keyword_ "from" *> importSource)
>                 
>     importAs = Import <$> importSource
>                       <*> (keyword_ "as" *> identifier)
