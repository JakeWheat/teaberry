
create an aggressively simple desugaring and then interpret this
instead

functions have 1 arg, lambda only has one param
let is implemented as lambda

> module SimpleExprDesugared (tests) where

> import qualified SimpleExpr as S
>     (Expr(..)
>     ,parse
>     ,simpleInterpreterExamples
>     ,TestTree
>     ,makeSimpleTests
>     )

> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)

> import Scientific (Scientific)


------------------------------------------------------------------------------

interpreter syntax

> data Expr = Num Scientific
>           | Iden String
>           | Plus Expr Expr
>           | App Expr Expr
>           | Lam String Expr
>           deriving (Eq, Show)

------------------------------------------------------------------------------

desugarer

> desugar :: S.Expr -> Either String Expr
> desugar (S.Num i) = pure $ Num i
> desugar (S.Iden i) = pure $ Iden i
> desugar (S.Plus a b) = Plus <$> desugar a <*> desugar b
> desugar (S.App _ []) = Left $ "0 arg function not supported"
> desugar (S.App f [a]) = App <$> desugar f <*> desugar a

f(a,b)
->
(f(a))(b)

> desugar (S.App f (a:as)) = desugar (S.App (S.App f [a]) as)

> desugar (S.Lam [] _) = Left $ "0 arg lambda not supported"
> desugar (S.Lam [n] e) = Lam n <$> desugar e
> desugar (S.Lam (n:ns) e) = Lam n <$> desugar (S.Lam ns e)

let n = v, ...
->
(lam n ...)(v)

> desugar (S.Let [] _) = Left $ "zero binding let"
> desugar (S.Let [(n,v)] e) = do
>     inner <- desugar e
>     dv <- desugar v
>     pure $ App (Lam n inner) dv
> desugar (S.Let ((n,v):bs) e) = do
>     inner <- desugar (S.Let bs e)
>     dv <- desugar v
>     pure $ App (Lam n inner) dv

------------------------------------------------------------------------------

interpreter

> type Env = [(String,Value)]

> data Value = NumV Scientific
>            | FunV String Expr Env
>            deriving (Eq, Show)

> type Interpreter = ReaderT Env (Except String)

> runInterp :: Env -> Expr -> Either String Value
> runInterp env expr = runExcept (runReaderT (interp expr) env)

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Iden i) = do
>     env <- ask
>     maybe (lift $ throwE $ "Identifier not found: " ++ i) pure $ lookup i env
> interp (Plus a b) = do
>     av <- interp a
>     bv <- interp b
>     case (av, bv) of
>         (NumV an, NumV bn) -> pure $ NumV $ an + bn
>         _ -> lift $ throwE $ "bad args to plus " ++ show (av, bv)

these are more straightforward than the n arg implementations but the
complexity comes back in the desugarer/ the desugared asts for these
things, so it's a wash

plus having these in the interpreter makes debug messages much more
difficult to understand (maybe also user error messages and warnings
and stuff)

there's a type checker reason why you don't usually want to desugar
let to lambda

> interp (App f e) = do
>     fv <- interp f
>     ev <- interp e
>     case fv of
>         FunV p bdy env' ->
>             let env'' = (p,ev) : env'
>             in local (const env'') $ interp bdy
>         _ -> lift $ throwE "non function value in app position"
> interp (Lam p e) = do
>     env <- ask
>     pure $ FunV p e env
> {-interp (Let (n,v) e) = do
>     vv <- interp v
>     local ((n,vv):) $ interp e-}

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- S.parse s
>     iast <- desugar ast
>     runInterp [] iast

------------------------------------------------------------------------------

> tests :: S.TestTree
> tests = S.makeSimpleTests "simplexprdesugared" S.simpleInterpreterExamples evaluate
