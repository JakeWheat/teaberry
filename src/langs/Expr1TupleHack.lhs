
take simple expr
add the most simple kind of tuple support
it's used to be able to test multiple values

the only thing the language needs is the ability to construct tuples
from expressions

and the test framework needs to be able to compare tuples
(when this is moved in language, it just needs tuple equality in
language, also trivial)

> {-# LANGUAGE TupleSections #-}

> module Expr1TupleHack (tests
>                            ,runInterp
>                            ) where

> import SimpleExpr (simpleInterpreterExamples
>                   ,TestTree
>                   ,makeSimpleTests)

> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)

> import Scientific (Scientific)

> import qualified OldParse as P
> import qualified OldSyntax as S

------------------------------------------------------------------------------

syntax
------

> data Expr = Num Scientific
>           | TupleSel [Expr]
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
>            | TupleV [Value]
>            deriving (Eq, Show)

> type Interpreter = ReaderT Env (Except String)

> runInterp :: Env -> Expr -> Either String Value
> runInterp env expr = runExcept (runReaderT (interp expr) env)

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (TupleSel es) = TupleV <$> mapM interp es
> interp (Iden i) = do
>     env <- ask
>     maybe (lift $ throwE $ "Identifier not found: " ++ i) pure $ lookup i env
> interp (Plus a b) = do
>     av <- interp a
>     bv <- interp b
>     case (av, bv) of
>         (NumV an, NumV bn) -> pure $ NumV $ an + bn
>         _ -> lift $ throwE $ "bad args to plus " ++ show (av, bv)
> interp (App f es) = do
>     fv <- interp f
>     vs <- mapM interp es
>     case fv of
>         FunV ps bdy env' -> do
>             as <- safeZip ps vs
>             let env'' = as ++ env'
>             local (const env'') $ interp bdy
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


> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     runInterp [] ast


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
> convExpr (S.Sel (S.TupleSel fs)) = TupleSel <$> mapM convExpr fs

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

> examples :: [(String, String)]
> examples = simpleInterpreterExamples
>    ++ [("{1 + 2; 4}", "{3;4}")
>       ,("let f = lam(x): lam(y): x + y end end,\n\
>         \    g = f(2),\n\
>         \    h = f(3):\n\
>         \  {g(3);h(3)}\n\
>       \  end","{5;6}")]


> tests :: TestTree
> tests = makeSimpleTests "expr1tuplehack" examples evaluate
