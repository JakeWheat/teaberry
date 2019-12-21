
> module Interpreter (interp
>                    ,emptyEnv
>                    ,extendEnv
>                    ,extendsEnv
>                    ,Env
>                    ,defaultHaskellFFIEnv
>                    ,Value(..)) where

> import qualified InterpreterSyntax as I
> import Data.Scientific (Scientific)
> import Control.Monad (foldM)
> --import Debug.Trace
> --import Desugar (sugar)
> --import qualified Pretty as P
> import Control.Monad.Trans.Except
> --import Control.Monad.Trans.Class
> --import Control.Monad.IO.Class
> import Text.Show.Pretty (ppShow)

------------------------------------------------------------------------------

= values and environments

> data Env = Env [(String,Value)]
>            deriving (Eq,Show)

> emptyEnv :: Env
> emptyEnv = Env [("true", BoolV True)
>                ,("false", BoolV False)]

> extendEnv :: String -> Value -> Env -> Env
> extendEnv n v (Env e) = Env ((n,v):e)

> extendsEnv :: [(String,Value)] -> Env -> Env
> extendsEnv ns (Env e) = Env $ foldr (:) e ns

> lookupEnv :: String -> Env -> Maybe Value
> lookupEnv nm (Env e) = lookup nm e


todo: make this better: do better wrapping + error messages when the
 type is wrong

> haskellFunImpls :: [(String, [Value] -> Interpreter Value)]
> haskellFunImpls = [("+", \[NumV a, NumV b] -> return $ NumV (a + b))
>                   ,("-", \[NumV a, NumV b] -> return $ NumV (a - b))
>                   ,("*", \x -> case x of
>                                    [NumV a, NumV b] -> return $ NumV (a * b)
>                                    _ -> throwE $ "* needs two num args, got " ++ ppShow x)
>                   ,("==", \[a, b] -> return $ BoolV (a == b))
>                   ,("raise", \[StrV s] -> throwE s)
>                   ]

> defaultHaskellFFIEnv :: Env
> defaultHaskellFFIEnv = 
>     extendsEnv [liftBinOp "*"
>                ,liftBinOp "+"
>                ,liftBinOp "-"
>                ,liftBinOp "=="
>                ,liftUnop "raise"
>                ] emptyEnv
>   where
>      liftUnop f = (f, ClosV (I.Lam "a" (I.AppHaskell f [I.Iden "a"])) emptyEnv)
>      liftBinOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.AppHaskell f [I.Iden "a", I.Iden "b"]))) emptyEnv)


> data Value = NumV Scientific
>            | BoolV Bool
>            | StrV String
>            | ClosV I.Expr Env
>            | Variant String [(String,Value)]
>            deriving (Eq,Show)

------------------------------------------------------------------------------

= interpreter function

TODO: turn this into a monad stack
not sure how to handle env
not sure how to handle vars
but definitely want IO


> type Interpreter a = ExceptT String IO a

> --liftEither :: Either String a -> Interpreter a
> --liftEither = ExceptT . return

> interp :: Env -> [(String,I.Expr)] -> I.Expr -> IO (Either String Value)
> interp env defs e = runExceptT $ do
>     --liftIO $ putStrLn ("****\n" ++ ppShow e ++ "\n\n" ++ P.prettyExpr (sugar e) ++ "\n****")
>     env2 <- foldM (\env' (n,ne) -> do
>                           nv <- interp' env' ne
>                           return $ extendEnv n nv env'
>                   ) env defs
>     interp' env2 e

> interp' :: Env -> I.Expr -> Interpreter Value
> interp' _ (I.Sel (I.Num n)) = return $ NumV n
> interp' _ (I.Sel (I.Str s)) = return $ StrV s
> --interp' _ I.True = Right $ BoolV True
> --interp' _ I.False = Right $ BoolV False
> interp' env (I.Iden e) = maybe (throwE $ "Identifier not found: " ++ e)
>                         return $ lookupEnv e env
> interp' env _x@(I.If c t e) = do
>    c' <- interp' env c
>    --liftIO $ putStrLn $ P.prettyExpr $ sugar x
>    case c' of
>        BoolV True -> {-liftIO (putStrLn "if") >> -} interp' env t
>        BoolV False -> {-liftIO (putStrLn "else") >> -} interp' env e
>        _ -> throwE $ "expected bool in if test, got " ++ show c'

> interp' env (I.AppHaskell nm exps) = do
>     f <- maybe (throwE $ "ffi fn not found: " ++ nm) return $ lookup nm haskellFunImpls
>     vs <- mapM (interp' env) exps
>     f vs

> interp' env e@(I.Lam {}) = return $ ClosV e env
> interp' env (I.App f a) = do
>     x <- interp' env f
>     case x of
>         ClosV (I.Lam n bdy) env' -> do
>              argVal <- interp' env a
>              interp' (extendEnv n argVal env') bdy
>         ClosV ee _ -> throwE $ "non lambda in closure expression: " ++ show ee
>         _ -> throwE $ "non function in app position: " ++ show x

> interp' env (I.Fix f) = do
>    interp' env (I.App f (I.Fix f))
>    --iftIO (putStrLn (P.prettyExpr $ sugar f))
>    --interp' env (I.App (I.Lam n bdy) (I.Fix f))

eval (Fix f) = (eval f) (eval (Fix f))

EFix e -> eval env (EApp e (EFix e))


> interp' env (I.Let nm v bdy) = do
>     v' <- interp' env v
>     interp' (extendEnv nm v' env) bdy

> interp' _env (I.Block []) = throwE $ "empty block"
> interp' env (I.Block [s]) = interp' env s
> interp' env (I.Block (s:ss)) = interp' env s >> interp' env (I.Block ss)
> 
> --interp _ e = error $ show e
