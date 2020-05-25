
Simple expression language example.

It has two data types: numbers, and functions

It has closures

It has multi arg functions + lambdas, and multi bind lets.

> {-# LANGUAGE TupleSections #-}

> module SimpleExpr (tests
>                   ,Expr(..)
>                   ,parse
>                   ,pretty
>                   ,convExpr
>                   ,simpleInterpreterExamples
>                   ,makeSimpleTests
>                   ,T.TestTree
>                   ) where
> 
> import qualified Parse as P
> import qualified Syntax as S
> import qualified Pretty as Pr
> import Data.Scientific (Scientific)

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

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     interp [] ast

------------------------------------------------------------------------------

pretty printer
--------------

for error messages, etc.

> pretty :: Expr -> String
> pretty x = Pr.prettyExpr $ unconv x

> unconv :: Expr -> S.Expr
> unconv (Num n) = S.Sel (S.Num n)
> unconv (Iden s) = S.Iden s
>
> unconv (Plus a b) = S.BinOp (unconv a) "+" (unconv b)
 
> unconv (App e fs) = S.App (unconv e) $ map unconv fs
> unconv (Lam ns e) = S.Lam (map unconvPattern ns) $ unconv e
> unconv (Let bs e) = S.Let (map (uncurry unconvBinding) bs) (unconv e)

> unconvBinding :: String -> Expr -> S.Binding
> unconvBinding n v = S.Binding (unconvPattern n) (unconv v)

> unconvPattern ::String -> S.Pat
> unconvPattern n = S.IdenP S.NoShadow (S.PatName n)


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
>       \    g = f(2),\n\
>       \    h = f(3):\n\
>       \  g(3)\n\
>       \  end","5")
>     ,("let f = lam(x): lam(y): x + y end end,\n\
>       \    g = f(2),\n\
>       \    h = f(3):\n\
>       \  h(3)\n\
>       \  end","6")]

> tests :: T.TestTree
> tests = makeSimpleTests "simplexpr" simpleInterpreterExamples evaluate



> makeSimpleTests :: (Eq a, Show a) => String -> [(String,String)] -> (String -> a) -> T.TestTree
> makeSimpleTests nm ts eval =
>     T.testGroup nm $ map (uncurry (runTest eval)) ts

> runTest :: (Eq a, Show a) => (String -> a) -> String -> String -> T.TestTree
> runTest eval s v = T.testCase s $ do
>     let res = eval s
>         expected = eval v
>     T.assertEqual "" expected res

