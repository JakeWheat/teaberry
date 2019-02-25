
TODO:

do all the tests from the pyret code

implement ask
implement not, and, or
  these are all sugar for if

add recursive functions

then think about a better concrete syntax, and making it more pyret-y


Other things to improve:

think about the minimum needed to reproduce the pyret lang
 implemention in this language:
  algebraic data types
  proper pattern binding (at least for constructors)
  list type
  can use the haskell s exp parser as a ffi function

think about if this is implemented in haskell, is it lazy in places?
how to make sure it's completely strict

what about representing closure values using a haskell function
  instead of an exprc, so the syntax doesn't leak into the values data
  type

see if can make a repl with this

add algebraic data types, case
recursive data types
add fuller case + stuff
will this cover tuples, records?

add parametric polymorphism


add types


> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module Lang3 where

> import Text.Megaparsec (Parsec
>                        --,many
>                        --,(<|>)
>                        ,parse
>                        ,eof
>                        ,some
>                        ,choice
>                        ,option
>                        --,(<?>)
>                        --,manyTill
>                        ,takeWhileP
>                        ,takeWhile1P
>                        ,try
>                        --,optional
>                        --,eitherP
>                        --,notFollowedBy
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
> import Test.Tasty
> import Test.Tasty.HUnit
> import Data.Scientific
> --import Debug.Trace



------------------------------------------------------------------------------

= high level syntax

> data ExprE = TrueE
>            | FalseE
>            | NumE Scientific
>            | StrE String
>            | IdenE String
>            | IfE ExprE ExprE ExprE
>            | AppE ExprE [ExprE]
>            | LamE [String] ExprE
>            | AskE [(ExprE,ExprE)] (Maybe ExprE)
>            | LetE [(String,ExprE)] ExprE
>            deriving (Eq,Show)


 # till line comment
 #| multiline
    comment |#
true
false
numbers
"strings"
also: "string\"\""
also: 'string"'
also: ``` multiline
  string```
identifiers, same for now

if expr:
  expr
else:
  expr
end

-> need to start adding reserved keywords

a op b -> works for a symbol, work out what this can be
stick with no precedence: must always use ()

ask is

ask:
  { | expr then: expr } *
  [ | otherwise: expr ]
end

ask:
end
is valid syntax, and will give a runtime error: no branch satisfied

ask:
  | otherwise: expr
end

is also valid


f(a,b) usual style

lam(x, y): x - y end

fun is-even(n):
  num-modulo(n, 2) == 0
end

-> shorthand for a binding


a program is a single expression:
binding * (incuding functions shorthand)
a single expression
see what can get out of this





------------------------------------------------------------------------------

= parser for sexp to high level syntax

> parseExpE :: SExp -> Either String ExprE
> parseExpE src = case src of
>     SNum n -> Right $ NumE n
>     SStr s -> Right $ StrE s
>     SSym "true" -> Right $ TrueE
>     SSym "false" -> Right $ FalseE
>     SSym i -> Right $ IdenE i
>     SList [SSym "lam", args, bdy] -> do
>         as <- case args of
>                   SList sas ->  let getArg (SSym x) = Right x
>                                     getArg y = Left $ "expected an iden in a lambda arg list, got " ++ show y
>                                 in mapM getArg sas
>                   SSym a -> Right [a]
>                   x -> Left $ "expected an iden or a list of idens for lambda arg list, got " ++ show x
>         LamE as <$> parseExpE bdy
>     SList [SSym "if", c, t, e] -> IfE <$> parseExpE c <*> parseExpE t <*> parseExpE e
>     SList (SSym "ask":args) -> let f :: [(ExprE,ExprE)] -> [SExp] -> Either String ([(ExprE,ExprE)], Maybe ExprE)
>                                    f acc [] = pure (reverse acc, Nothing)
>                                    f acc [SList [SSym "else", e]] = (\y -> (reverse acc, Just y)) <$> parseExpE e
>                                    f acc (SList [w,t]:rest) = do
>                                        w' <- parseExpE w
>                                        t' <- parseExpE t
>                                        f ((w',t'):acc) rest
>                                    f _acc x = Left $ "bad syntax in ask: " ++ show x
>                                in uncurry AskE <$> f [] args
>     SList [SSym "let", lets, bdy] -> do
>         lets' <- parseLets lets
>         LetE lets' <$> parseExpE bdy
>       where
>           parseLets (SList [SSym a, vale]) = (:[]) <$> (a,) <$> parseExpE vale
>           parseLets (SList lts) = mapM parseLet lts
>           parseLets x = Left $ "bad syntax in let binding: " ++ show x
>           parseLet (SList [SSym a, vale]) = (a,) <$> parseExpE vale
>           parseLet x = Left $ "bad syntax in let binding: " ++ show x
>     SList (f: args) -> AppE <$> parseExpE f <*> mapM parseExpE args
>     SList [] -> Left "cannot parse ()"

------------------------------------------------------------------------------

= tests for high level parser

> parseExamples :: [(String, ExprE)]
> parseExamples = [("3", NumE 3)
>                 ,("(+ 1 2)", AppE (IdenE "+") [NumE 1, NumE 2])
>                 ,("(* (+ 1 2) (* 2 5))"
>                  ,AppE (IdenE "*") [AppE (IdenE "+") [NumE 1, NumE 2]
>                                    ,AppE (IdenE "*") [NumE 2, NumE 5]])
>                 ,("(- 2 1)", AppE (IdenE "-") [NumE 2, NumE 1])
>                 ,("-1", NumE (-1))

>                 ,("(- 1)", AppE (IdenE "-") [NumE 1])
>                 ,("true", TrueE)
>                 ,("false", FalseE)
>                 ,("\"my string\"", StrE "my string")
>                 ,("(if true 1 2)", IfE TrueE (NumE 1) (NumE 2))
>                 ,("(ask (else 1))", AskE [] (Just $ NumE 1))
>                 ,("(ask (true 1))", AskE [(TrueE, NumE 1)] Nothing)
>                 ,("(ask (true 1) (true 2))"
>                  ,AskE [(TrueE, NumE 1), (TrueE, NumE 2)] Nothing)
>                 ,("(ask (true 1) (true 2) (else 3))"
>                  ,AskE [(TrueE, NumE 1), (TrueE, NumE 2)] (Just (NumE 3)))
>                 ,("s", IdenE "s")
>                 ,("(f 1)", AppE (IdenE "f") [NumE 1])
>                 ,("(f 1 2)", AppE (IdenE "f") [NumE 1, NumE 2])
>                 ,("(lam x (+ x 1))"
>                 ,LamE ["x"] (AppE (IdenE "+") [IdenE "x", NumE 1]))
>                 ,("((lam x (+ x 1)) 3)"
>                 ,AppE (LamE ["x"] (AppE (IdenE "+") [IdenE "x", NumE 1])) [NumE 3])

>                 ,("(lam (x y) (+ x y))"
>                 ,LamE ["x", "y"] (AppE (IdenE "+") [IdenE "x", IdenE "y"]))
>
>                 ,("(let (x 4) (+ x 5))"
>                  ,LetE [("x", NumE 4)] (AppE (IdenE "+") [IdenE "x", NumE 5]))
>                 ,("(let ((x 4) (y 5)) (+ x y))"
>                  ,LetE [("x", NumE 4), ("y", NumE 5)] (AppE (IdenE "+") [IdenE "x", IdenE "y"]))
>                 ]

> testParse :: (String, ExprE) -> TestTree
> testParse (src, e) = testCase ("parse " ++ src) $ do
>     case parseSExp "" src of
>         Left er -> error er
>         Right x -> case parseExpE x of
>              Left er -> error er
>              Right x1 -> assertEqual "" e x1

------------------------------------------------------------------------------

= interpreter syntax


> data ExprC = TrueC
>            | FalseC
>            | NumC Scientific
>            | StrC String
>            | IdenC String
>            | IfC ExprC ExprC ExprC
>            | AppC ExprC ExprC
>            | LamC String ExprC
>            | AppHaskellC String [ExprC]
>            deriving (Eq,Show)

------------------------------------------------------------------------------

= desugar

example of desugaring two args:

 (lambda (x y) (+ x y))
 (lambda (x) (lambda (y) (+ x y)))

 (x a b)
 ((x a) b)

((lambda (x) ((lambda (y) (+ x y)) 5)) 150)

> desugar :: ExprE -> Either String ExprC
> desugar TrueE = Right $ TrueC
> desugar FalseE = Right $ FalseC
> desugar (NumE n) = Right $ NumC n
> desugar (StrE n) = Right $ StrC n
> desugar (IdenE e) = Right $ IdenC e
> desugar (IfE c t e) = IfC <$> desugar c <*> desugar t <*> desugar e
> desugar (LamE [] _) = Left $ "lambda with no args"
> desugar (LamE [x] bdy) = LamC x <$> desugar bdy
> desugar (LamE (x:xs) bdy) = LamC x <$> desugar (LamE xs bdy)
> desugar (AppE _ []) = Left $ "app with no args"
> -- I'm not sure this is quite right
> -- will this always catch unary and binary -? what if they are used as values?
> desugar (AppE (IdenE "-") [e]) = desugar (AppE (IdenE "*") [NumE (-1), e])
> desugar (AppE (IdenE "-") [e,f]) =
>     desugar (AppE (IdenE "+") [e,  AppE (IdenE "*") [NumE (-1), f]])
> desugar (AppE f [x]) = AppC <$> desugar f <*> desugar x
> desugar (AppE f xs) = do
>     f' <- desugar f
>     xs' <- mapM desugar xs
>     let r _ [] = Left "internal error app with no args"
>         r g [y] = Right $ AppC g y
>         r g (y:ys) = r (AppC g y) ys
>     r f' xs'
> desugar (AskE {}) = Left "desugar ask: please implement me"
> 
> desugar (LetE [] bdy) = desugar bdy
> desugar (LetE ((n,lbdy):ls) bdy) = do
>     bdy' <- desugar (LetE ls bdy)
>     lbdy' <- desugar lbdy
>     Right $ AppC (LamC n bdy') lbdy'

------------------------------------------------------------------------------

= desugar tests

> desugarExamples :: [(String,ExprC)]
> desugarExamples = [
>     ("3", NumC 3)
>     ,("\"x\"", StrC "x")
>     ,("true", TrueC)
>     ,("false", FalseC)
>     ,("x", IdenC "x")
>     ,("(f 1)", AppC (IdenC "f") (NumC 1))
>     ,("(f 1 2)", AppC (AppC (IdenC "f") (NumC 1)) (NumC 2))
>     ,("(lam x (+ x 1))"
>      ,LamC "x" (AppC (AppC (IdenC "+") (IdenC "x")) (NumC 1)))

>     ,("(lam (x y) (+ x y))"
>      ,LamC "x" (LamC "y" (AppC (AppC (IdenC "+") (IdenC "x")) (IdenC "y"))))

>     ,("(+ 1 2)", AppC (AppC (IdenC "+") (NumC 1)) (NumC 2))
>     ,("(* 1 2)", AppC (AppC (IdenC "*") (NumC 1)) (NumC 2))
>     ,("(- 1 2)", binop "+" (NumC 1) (binop "*" (NumC (-1)) (NumC 2)))
>     ,("(- 2)", binop "*" (NumC (-1)) (NumC 2))
> 
>     --,("(ask 1)") is numC(1)
>     --,("(ask (true 1))") is ifC(trueC,numC(1),unopC("raise",stringC("ask: no branch conditions satisfied")))
>     --,("(ask (true 1) (false 2))") is
>     --  ifC(trueC,numC(1),ifC(falseC,numC(2),unopC("raise",stringC("ask: no branch conditions satisfied"))))
>     --,("(ask (true 1) 2)") is ifC(trueC,numC(1),numC(2))
>     --,("(ask (true 1) (false 2) 3)") is
>     --  ifC(trueC,numC(1),ifC(falseC,numC(2),numC(3)))

>     ,("(let (x 4) (+ x 5))"
>      ,let p = binop "+" (IdenC "x") (NumC 5)
>       in AppC (LamC "x" p) (NumC 4))
>
>     ,("(let ((x 4) (y 5)) (+ x y))"
>      ,let p = binop "+" (IdenC "x") (IdenC "y")
>       in AppC (LamC "x" (AppC (LamC "y" p) (NumC 5))) (NumC 4))
>     ]
>   where
>     binop nm a b = AppC (AppC (IdenC nm) a) b

> testDesugar :: (String, ExprC) -> TestTree
> testDesugar (src, e) = testCase ("desugar " ++ src) $ do
>     either error (assertEqual "" e) $ do
>         se <- parseSExp "" src
>         ex <- parseExpE se
>         desugar ex


------------------------------------------------------------------------------

= values and environments

> data Env = Env [(String,Value)]
>            deriving (Eq,Show)

> emptyEnv :: Env
> emptyEnv = Env []

> extendEnv :: String -> Value -> Env -> Env
> extendEnv n v (Env e) = Env ((n,v):e)

> extendsEnv :: [(String,Value)] -> Env -> Env
> extendsEnv ns (Env e) = Env $ foldr (:) e ns

> lookupEnv :: String -> Env -> Maybe Value
> lookupEnv nm (Env e) = lookup nm e

foldr :: (a -> b -> b) -> b -> [a] -> b


> haskellFunImpls :: [(String, [Value] -> Either String Value)]
> haskellFunImpls = [("+", \[NumV a, NumV b] -> Right $ NumV (a + b))
>                   ,("*", \[NumV a, NumV b] -> Right $ NumV (a * b))
>                   ,("raise", \[StrV s] -> Left s)
>                   ]

> data Value = NumV Scientific
>            | BoolV Bool
>            | StrV String
>            | ClosV ExprC Env
>            deriving (Eq,Show)

------------------------------------------------------------------------------

= interp

> interp :: ExprC -> Env -> Either String Value
> interp (NumC n) _ = Right $ NumV n
> interp (StrC s) _ = Right $ StrV s
> interp TrueC _ = Right $ BoolV True
> interp FalseC _ = Right $ BoolV False
> interp (IdenC e) env = maybe (Left $ "Identifier not found: " ++ e)
>                       Right $ lookupEnv e env
> interp (IfC c t e) env = do
>    c' <- interp c env
>    case c' of
>        BoolV True -> interp t env
>        BoolV False -> interp e env
>        _ -> Left $ "expected bool in if test, got " ++ show c'

> interp (AppHaskellC nm exps) env = do
>     f <- maybe (Left $ "ffi fn not found: " ++ nm) Right $ lookup nm haskellFunImpls
>     vs <- mapM (flip interp env) exps
>     f vs

> interp e@(LamC {}) env = Right $ ClosV e env
> interp (AppC f a) env = do
>     x <- interp f env
>     case x of
>         ClosV (LamC n bdy) env' -> do
>              argVal <- interp a env
>              interp bdy (extendEnv n argVal env')
>         ClosV ee _ -> Left $ "non lambda in closure expression: " ++ show ee
>         _ -> Left $ "non function in app position: " ++ show x
>     


> --interp

------------------------------------------------------------------------------

= interp tests

> interpExamples :: [(String,Env,Value)]
> interpExamples =
>     [("1", emptyEnv, NumV 1)
>     ,("true", emptyEnv, BoolV True) 
>     ,("false", emptyEnv, BoolV False)
>     ,("\"hello\"", emptyEnv, StrV "hello")
>     ,("myVar", simpleEnv, NumV 1)
>     ,("(+ 1 2)", simpleEnv, NumV 3)
>     ,("(- 3 2)", simpleEnv, NumV 1)
>     ,("(if true 1 2)", simpleEnv, NumV 1)
>     ,("(if false 1 2)", simpleEnv, NumV 2)
>     ,("(add2 1)", simpleEnv, NumV 3)
>     ,("(+ 5 (quad 3))", simpleEnv, NumV 17)
>     ,("(* (const5 3) 4)", simpleEnv, NumV 20)
>     ,("(+ 10 (const5 10))", simpleEnv, NumV (10 + 5))
>     ,("(+ 10 (double (+ 1 2)))", simpleEnv, NumV (10 + 3 + 3))
>     ,("(+ 10 (quad (+ 1 2)))", simpleEnv, NumV (10 + 3 + 3 + 3 + 3))
>     ,("((lam x (+ x 5)) 10)", simpleEnv, NumV 15)
>     ,("(let ((x 4) (y 5)) (+ x y))", simpleEnv, NumV 9)
>     ,("(let ((x 4) (y 5)) (- x y))", simpleEnv, NumV (-1))

>     ,("(let (myF (lam x (+ x 5))) (myF 20))", simpleEnv, NumV 25)

>     ]
>   where
>       simpleEnv =
>           either error id $
>           addFn "add2" "(lam a (+ a 2))"
>           =<< addFn "f4" "(lam x (if x 1 0))"
>           =<< addFn "const5" "(lam _ 5)"
>           =<< addFn "quad" "(lam x (double (double x)))"
>           =<< addFn "double" "(lam x (+ x x))"
>           (extendsEnv [("myVar", NumV 1)]
>               defaultHaskellFFIEnv)

> env2 :: Env
> env2 = either error id $
>           addFn "add2" "(lam a (+ a 2))"
>           =<< addFn "f4" "(lam x (if x 1 0))"
>           =<< addFn "const5" "(lam _ 5)"
>           =<< addFn "quad" "(lam x (double (double x)))"
>           =<< addFn "double" "(lam x (+ x x))"
>           (extendsEnv [("myVar", NumV 1)]
>               defaultHaskellFFIEnv)

> defaultHaskellFFIEnv :: Env
> defaultHaskellFFIEnv = 
>     extendsEnv [liftBinOp "*"
>                ,liftBinOp "+"
>                ,liftUnop "raise"
>                ] emptyEnv
>   where
>      liftUnop f = (f, ClosV (LamC "a" (AppHaskellC f [IdenC "a"])) emptyEnv)
>      liftBinOp f = (f, ClosV (LamC "a" (LamC "b" (AppHaskellC f [IdenC "a", IdenC "b"]))) emptyEnv)
>   

> s2p2d :: String -> Either String ExprC
> s2p2d s = desugar =<< parseExpE =<< parseSExp "" s

> addFn :: String -> String -> Env -> Either String Env
> addFn nm src env = do
>     ast <- s2p2d src
>     x <- interp ast env
>     case x of
>         ClosV {} -> Right $ extendEnv nm x env
>         _ -> Left $ "expected closure in addFn, got " ++ show x
>         
>         

> runIt2 :: Env -> String -> Either String Value
> runIt2 env s = flip interp env =<< desugar =<< parseExpE =<< parseSExp "" s

> runIt :: String -> Either String Value
> runIt s = runIt2 defaultHaskellFFIEnv s


> testInterp :: (String, Env, Value) -> TestTree
> testInterp (src, env, e) = testCase ("interp " ++ src) $ do
>     either error (assertEqual "" e) $ do
>         se <- parseSExp "" src
>         ex <- parseExpE se
>         ec <- desugar ex
>         interp ec env


------------------------------------------------------------------------------

= sexps and parser

> data SExp = SList [SExp]
>           | SNum Scientific
>           | SStr String
>           | SSym String
>     deriving (Eq,Show)

> parseSExp :: String -> String -> Either String SExp
> parseSExp fn src = either (Left . errorBundlePretty) Right $
>                    parse (whiteSpace *> sexp <* eof) fn src

> type Parser = Parsec Void String

> sexp :: Parser SExp
> sexp = choice [slist, snum, sstr, ssym]

> slist :: Parser SExp
> slist = SList <$> (lexeme_ (char '(') *> some sexp <* lexeme_ (char ')'))

> snum :: Parser SExp
> snum = try $ lexeme $ do
>     s <- option "" (string "-")
>     x <- takeWhile1P Nothing (`elem` "0123456789.")
>     maybe (fail $ "couldn't parse number: " ++ x) (pure . SNum) $ readMaybe (s ++ x)

> sstr :: Parser SExp
> sstr = SStr <$> lexeme (char '"' *> takeWhileP Nothing (/= '"') <* char '"')

> ssym :: Parser SExp
> ssym = SSym <$> lexeme (takeWhile1P Nothing (\x -> not(isSpace(x)) && (x `notElem` "()")))


> whiteSpace :: Parser ()
> whiteSpace = space {-*> choice [lineComment *> whiteSpace
>                              ,blockComment *> whiteSpace
>                              ,pure ()] <?> "" -}

> lexeme :: Parser a -> Parser a
> lexeme f = f <* whiteSpace

> lexeme_ :: Parser a -> Parser ()
> lexeme_ f = () <$ f <* whiteSpace


------------------------------------------------------------------------------

= sexp parsing tests

> sexpParseExamples :: [(String,SExp)]
> sexpParseExamples = [("a", SSym "a")
>                     ,("*", SSym "*")
>                     ,("\"test\"", SStr "test")
>                     ,("3", SNum 3)
>                     ,("3.3", SNum 3.3)
>                     ,("(a)", SList [SSym "a"])
>                     ,("(a 3)", SList [SSym "a", SNum 3])
>                     ]

> testSExpParse :: (String, SExp) -> TestTree
> testSExpParse (src, e) = testCase ("sexp parse " ++ src) $ do
>     case parseSExp "" src of
>         Left er -> error er
>         Right x -> assertEqual "" e x


------------------------------------------------------------------------------

= all tests

> runAllTests :: IO ()
> runAllTests = defaultMain $
>     testGroup "all"
>     [testGroup "sexp parse" $ map testSExpParse sexpParseExamples
>     ,testGroup "parse" $ map testParse parseExamples
>     ,testGroup "desugar" $ map testDesugar desugarExamples
>     ,testGroup "interp" $ map testInterp interpExamples
>     ]

