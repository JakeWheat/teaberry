
Simple expression language example.

It has two data types: ints and functions

It has closures

It has multi arg functions + lambdas, and multi bind lets.

> {-# LANGUAGE TupleSections #-}

> import qualified Parse as P
> import qualified Syntax as S
> import Data.Scientific (floatingOrInteger)

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

------------------------------------------------------------------------------

syntax
------

> data Expr = Num Int
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

> data Value = IntV Int
>            | FunV [String] Expr Env
>            deriving (Eq, Show)

> interp :: Env -> Expr -> Either String Value
> interp _ (Num n) = Right (IntV n)
> interp env (Iden i) = maybe (Left $ "Identifier not found: " ++ i) Right $ lookup i env
> interp env (Plus a b) = do
>     av <- interp env a
>     bv <- interp env b
>     case (av, bv) of
>         (IntV an, IntV bn) -> Right $ IntV $ an + bn
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

parser
------

> parse :: String -> Either String Expr
> parse src =
>     case P.parseExpr "" src of
>       Right e -> convExpr e
>       Left e -> Left e
>   where
>     convExpr (S.Sel (S.Num x)) | Right n <- (floatingOrInteger x :: Either Double Int) = Right $ Num n
>     convExpr (S.Iden s) = Right $ Iden s
>     convExpr (S.Parens e) = convExpr e
>     convExpr (S.App f es) = App <$> (convExpr f) <*> mapM convExpr es
>     convExpr (S.BinOp e "+" e1) = Plus <$>     convExpr e <*>     convExpr e1
>     convExpr (S.Lam ps e) = do
>         ps' <- mapM pf ps
>         e' <- convExpr e
>         Right $ Lam ps' e'
>       where
>         pf (S.IdenP _ (S.PatName x)) = Right x
>         pf x = Left $ "unsupported pattern " ++ show x
>     convExpr (S.Let bs e) = do
>         bs' <- mapM bf bs
>         e' <- convExpr e
>         Right $ Let bs' e'
>       where
>         bf (S.Binding (S.IdenP _ (S.PatName x)) ex) =
>             (x,) <$>     convExpr ex
>         bf x = Left $ "unsupported binding " ++ show x
>     convExpr x = Left $ "unsupported syntax " ++ show x

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     interp [] ast

------------------------------------------------------------------------------

tests
-----

> interpreterExamples :: [(String, String)]
> interpreterExamples =
>     [("1", "1")
>     ,("1 + 2", "3")
>     ,("let x = 3: x end", "3")
>     ,("lam(x,y): x + y end(1,2)", "3")
>     ,("let f = lam(x,y): x + y end: f(1,2) end", "3")]

> tests :: T.TestTree
> tests = T.testGroup "simpleexpr" $ map (uncurry runTest) interpreterExamples

> runTest :: String -> String -> T.TestTree
> runTest s v = T.testCase s $ do
>     let res = either error id $ evaluate s
>         expected = either error id $ evaluate v
>     T.assertEqual "" expected res

> main :: IO ()
> main = T.defaultMain tests
