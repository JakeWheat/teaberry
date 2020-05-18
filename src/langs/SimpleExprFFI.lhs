
First pass on a proper range of data types. It's a bit trivial, but
the base for some other things.

update: this is daft, it doesn't do anything, what's the motivation

bools: if, ask?
string: ?
binops
start by doing an ffi immediately
copy the existing one
then try to refactor it



start with
numbers
booleans
strings
functions

todo later:
variants
boxes
foreign types


> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}

> module SimpleExprFFI (tests) where

> import qualified Parse as P
> import qualified Syntax as S

> --import qualified SimpleExpr (Expr(..), parse)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
> import Data.Scientific (Scientific)

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

------------------------------------------------------------------------------

interpreter
-----------

> data Expr = Num Scientific
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>            deriving (Eq, Show)


> type Env = [(String,Value)]

------------------------------------------------------------------------------


> data Value = NumV Scientific
>            | BoolV Bool
>            | StringV String
>            | FunV [String] Expr Env
>            | HaskellFunV String
>            deriving (Eq, Show)

> type Interpreter = ReaderT Env (Except String)

> runInterp :: Env -> Expr -> Either String Value
> runInterp env expr = runExcept (runReaderT (interp expr) env)

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Iden i) = do
>     env <- ask
>     maybe (lift $ throwE $ "Identifier not found: " ++ i) pure $ lookup i env
> interp (App f es) = do
>     fv <- interp f
>     vs <- mapM interp es
>     case fv of
>         FunV ps bdy env' -> do
>             as <- safeZip ps vs
>             let env'' = as ++ env'
>             local (const env'') $ interp bdy
>         HaskellFunV nm -> do
>             hf <- maybe (lift $ throwE $ "ffi function not found: " ++ nm)
>                         pure $ lookup nm haskellFunctions
>             hf vs
>         _ -> lift $ throwE "non function value in app position"
>   where
>     safeZip ps vs | length vs == length ps = pure $ zip ps vs
>                   | otherwise = lift $ throwE $ "wrong number of args to function"
> interp (Lam ps e) = do
>     env <- ask
>     pure $ FunV ps e env
> interp (Let bs e) = do
>     let newEnv [] = interp e
>         newEnv ((b,ex):bs') = do
>             v <- interp ex
>             local ((b,v):) $ newEnv bs'
>     newEnv bs

------------------------------------------------------------------------------

function ffi: call haskell functions from in language

> testEnvironment :: Env
> testEnvironment = [("+", HaskellFunV "+")]

> haskellFunctions :: [(String, [Value] -> Interpreter Value)]
> haskellFunctions =
>     [("+"
>      ,\case
>           [NumV a, NumV b] -> pure $ NumV $ plus a b
>           as -> lift $ throwE $ "bad args to + " ++ show as
>      )]

> plus :: Scientific -> Scientific -> Scientific
> plus = (+)

TODO:

haskellFunctions is hardcoded in the interpreter
want to pass it as a value

want to write something like:
add function "+", plus
there's tons of boilerplate
want to make it easy to give much better errors
is there a way to automatically create the unwrapping?

what does it look like to use 'dynamic type classes'
so you create + as a typeclass function
then you can add a + for some particular types independently

eventually want a standard set of functions

with the ability to add your own haskell functions when running
something

and the ability to have a minimal built in set, or none at all

the next step after that is to implement haskell data types ffi
so you can pass haskell values around in language opaquely
and then add something that allows you to use them as conveniently as
in language values

it would be nice to make the Value type abstract for users of the code
too, to insulate at API level from implementation changes.

------------------------------------------------------------------------------

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
> convExpr (S.BinOp e f e1) = do
>     a <- convExpr e
>     b <- convExpr e1
>     pure $ App (Iden f) [a,b]
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

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     runInterp testEnvironment ast

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

> runTest :: String -> String -> T.TestTree
> runTest s v = T.testCase s $ do
>     let res = either error id $ evaluate s
>         expected = either error id $ evaluate v
>     T.assertEqual "" expected res

> tests :: T.TestTree
> tests = T.testGroup "simpleexprreader"
>            $ map (uncurry runTest) interpreterExamples
