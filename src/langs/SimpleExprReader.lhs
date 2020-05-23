

> {-# LANGUAGE TupleSections #-}

> module SimpleExprReader (tests
>                         ,runInterp
>                         ) where

> import SimpleExpr (Expr(..), parse, simpleInterpreterExamples)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)

> import Data.Scientific (Scientific)

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

------------------------------------------------------------------------------

interpreter
-----------

> type Env = [(String,Value)]

> data Value = NumV Scientific
>            | FunV [String] Expr Env
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

------------------------------------------------------------------------------

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     runInterp [] ast

------------------------------------------------------------------------------

tests
-----

> runTest :: String -> String -> T.TestTree
> runTest s v = T.testCase s $ do
>     let res = either error id $ evaluate s
>         expected = either error id $ evaluate v
>     T.assertEqual "" expected res

> tests :: T.TestTree
> tests = T.testGroup "simpleexprreader"
>            $ map (uncurry runTest) simpleInterpreterExamples
