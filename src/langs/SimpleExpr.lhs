
Simple expression language example.

It has two data types: numbers, and functions

It has closures

It has multi arg functions + lambdas, and multi bind lets.

> {-# LANGUAGE TupleSections #-}

> module SimpleExpr (tests
>                   ,Expr(..)
>                   ,parse
>                   ,prettyExpr
>                   ,convExpr
>                   ,simpleInterpreterExamples
>                   ) where
> 
> import qualified Parse as P
> import qualified Syntax as S
> import Data.Scientific (Scientific)

> import Prelude hiding ((<>))
> import Text.PrettyPrint (render, text, (<>), (<+>), parens,
>                          nest, Doc, punctuate, comma, sep, vcat)


> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

------------------------------------------------------------------------------

syntax
------

> data Expr = Num Scientific
>           | Iden String
>           | Plus Expr Expr
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>            deriving (Eq, Show)

------------------------------------------------------------------------------

interpreter
-----------

> type Env = [(String,Value)]

> data Value = NumV Scientific
>            | FunV [String] Expr Env
>            deriving (Eq, Show)

> interp :: Env -> Expr -> Either String Value
> interp _ (Num n) = Right (NumV n)
> interp env (Iden i) = maybe (Left $ "Identifier not found: " ++ i) Right $ lookup i env
> interp env (Plus a b) = do
>     av <- interp env a
>     bv <- interp env b
>     case (av, bv) of
>         (NumV an, NumV bn) -> Right $ NumV $ an + bn
>         _ -> Left $ "bad args to  plus " ++ show (av, bv)

function application:
evaluate the function value and the arg values

take the function param names and the arg values
and extend the closure env with this (the one that comes with the function value)
execute the body with this env

for a version without closures:
leave the env arg off FunV
and execute the body in an environment made from just the function
  param names + arg values
you could call this 'expressions with function pointer values'
a language like this is straightforward to convert to C

> interp env (App f es) = do
>     fv <- interp env f
>     vs <- mapM (interp env) es
>     case fv of
>         FunV ps bdy env' | length vs == length ps ->
>                                let env'' = zip ps vs ++ env'
>                                in interp env'' bdy
>                          | otherwise -> Left $ "wrong number of args to function"
>         _ -> Left "non function value in app position"

A function value:
the list of parameter names
the body of the function
the closure of the function: the environment when the function value
  is created

> interp env (Lam ps e) = Right $ FunV ps e env
> interp env (Let bs e) = do
>     -- each let binding can see the previous binding
>     let newEnv en [] = Right en
>         newEnv en ((b,ex):bs') = do
>             v <- interp en ex
>             newEnv ((b,v):en) bs'
>     env' <- newEnv env bs
>     interp env' e

------------------------------------------------------------------------------

simplified interp that uses 'error' to avoid monads/do

> interpE :: Env -> Expr -> Value
> interpE _ (Num n) = NumV n
> interpE env (Iden i) =
>     maybe (error $ "Identifier not found: " ++ i) id $ lookup i env
> interpE env (Plus a b) =
>     case (interpE env a, interpE env b) of
>         (NumV an, NumV bn) -> NumV $ an + bn
>         _ -> error $ "bad args to  plus " ++ show (a, b)
> interpE env (App f es) =
>     case (interpE env f, map (interpE env) es) of
>         (FunV ps bdy env', vs)
>             | length vs == length ps ->
>                   let env'' = zip ps vs ++ env'
>                   in interpE env'' bdy
>             | otherwise -> error $ "wrong number of args to function"
>         _ -> error "non function value in app position"
> interpE env (Lam ps e) = FunV ps e env
> interpE env (Let bs e) =
>     let newEnv en [] = en
>         newEnv en ((b,ex):bs') = newEnv ((b,interpE en ex):en) bs'
>     in interpE (newEnv env bs) e

------------------------------------------------------------------------------

pretty printer
--------------

for error messages, etc.

> prettyExpr :: Expr -> String
> prettyExpr = render . expr

> expr :: Expr -> Doc
> expr (Num n) = text $ case S.extractInt n of
>                              Just x -> show x
>                              Nothing -> show n
> expr (Iden i) = text i
> expr (Plus e0 e1) = expr e0 <+> text "+" <+> expr e1
> expr (App f es) = expr f <> parens (commaSep $ map expr es)
> expr (Lam ps e) = vcat
>     [text "lam" <> parens (commaSep $ map text ps) <> text ":"
>     ,nest 2 (expr e)
>     ,text "end"]
> expr (Let bs e) =
>     vcat [text "let" <+> nest 2 bs' <> text ":"
>          ,nest 2 (expr e)
>          ,text "end"]
>   where
>     bs' | [b] <- bs = binding b
>         | otherwise = vcommaSep $ map binding bs
>     binding (n,be) =
>         text n <+> text "=" <+> nest 2 (expr be)

> commaSep :: [Doc] -> Doc
> commaSep ds = sep $ punctuate comma ds

> vcommaSep :: [Doc] -> Doc
> vcommaSep ds = vcat $ punctuate comma ds


------------------------------------------------------------------------------

parser
------

> parse :: String -> Either String Expr
> parse src =
>     case P.parseExpr "" src of
>       Right e -> convExpr e
>       Left e -> Left e
>
> convExpr :: S.Expr -> Either String Expr
> convExpr (S.Sel (S.Num x)) = Right $ Num x
> convExpr (S.Iden s) = Right $ Iden s
> convExpr (S.Parens e) = convExpr e
> convExpr (S.App f es) = App <$> (convExpr f) <*> mapM convExpr es
> convExpr (S.BinOp e "+" e1) = Plus <$> convExpr e <*> convExpr e1
> convExpr (S.Lam ps e) = do
>         ps' <- mapM pf ps
>         e' <- convExpr e
>         Right $ Lam ps' e'
>       where
>         pf (S.IdenP _ (S.PatName x)) = Right x
>         pf x = Left $ "unsupported pattern " ++ show x
> convExpr (S.Let bs e) = do
>         bs' <- mapM bf bs
>         e' <- convExpr e
>         Right $ Let bs' e'
>       where
>         bf (S.Binding (S.IdenP _ (S.PatName x)) ex) =
>             (x,) <$> convExpr ex
>         bf x = Left $ "unsupported binding " ++ show x
> convExpr x = Left $ "unsupported syntax " ++ show x

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     interp [] ast

> evaluateE :: String -> Value
> evaluateE s =
>     let ast = either error id $ parse s
>     in interpE [] ast


------------------------------------------------------------------------------

tests
-----

> simpleInterpreterExamples :: [(String, String)]
> simpleInterpreterExamples =
>     [("1", "1")
>     ,("1 + 2", "3")
>     ,("let x = 3: x end", "3")
>     ,("lam(x,y): x + y end(1,2)", "3")
>     ,("let f = lam(x,y): x + y end: f(1,2) end", "3")
>     ,("let f = lam(x): lam(y): x + y end end,\n\
>       \    g = f(2):\n\
>       \  g(3)\n\
>       \  end","5")]


> tests :: T.TestTree
> tests = T.testGroup "simpleexpr"
>         [T.testGroup "simplexpr either"
>          $ map (uncurry runTest) simpleInterpreterExamples
>         ,T.testGroup "simplexpr error"
>          $ map (uncurry runTestE) simpleInterpreterExamples
>         ]

> runTest :: String -> String -> T.TestTree
> runTest s v = T.testCase s $ do
>     let res = either error id $ evaluate s
>         expected = either error id $ evaluate v
>     T.assertEqual "" expected res

> runTestE :: String -> String -> T.TestTree
> runTestE s v = T.testCase s $ do
>     let res = evaluateE s
>         expected = evaluateE v
>     T.assertEqual "" expected res

