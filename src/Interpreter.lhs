
> module Interpreter (interp
>                    ,emptyEnv
>                    ,extendEnv
>                    ,extendsEnv
>                    ,Env
>                    ,defaultHaskellFFIEnv
>                    ,Value(..)) where

> import qualified InterpreterSyntax as I
> import Data.Scientific (Scientific)

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

> haskellFunImpls :: [(String, [Value] -> Either String Value)]
> haskellFunImpls = [("+", \[NumV a, NumV b] -> Right $ NumV (a + b))
>                   ,("-", \[NumV a, NumV b] -> Right $ NumV (a - b))
>                   ,("*", \[NumV a, NumV b] -> Right $ NumV (a * b))
>                   ,("==", \[a, b] -> Right $ BoolV (a == b))
>                   ,("raise", \[StrV s] -> Left s)
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
>            deriving (Eq,Show)

------------------------------------------------------------------------------

= interpreter function

TODO: turn this into a monad stack
not sure how to handle env
not sure how to handle vars
but definitely want IO

> interp :: Env -> I.Expr -> Either String Value
> interp _ (I.Num n) = Right $ NumV n
> interp _ (I.Str s) = Right $ StrV s
> --interp _ I.True = Right $ BoolV True
> --interp _ I.False = Right $ BoolV False
> interp env (I.Iden e) = maybe (Left $ "Identifier not found: " ++ e)
>                         Right $ lookupEnv e env
> interp env (I.If c t e) = do
>    c' <- interp env c
>    case c' of
>        BoolV True -> interp env t
>        BoolV False -> interp env e
>        _ -> Left $ "expected bool in if test, got " ++ show c'

> interp env (I.AppHaskell nm exps) = do
>     f <- maybe (Left $ "ffi fn not found: " ++ nm) Right $ lookup nm haskellFunImpls
>     vs <- mapM (interp env) exps
>     f vs

> interp env e@(I.Lam {}) = Right $ ClosV e env
> interp env (I.App f a) = do
>     x <- interp env f
>     case x of
>         ClosV (I.Lam n bdy) env' -> do
>              argVal <- interp env a
>              interp (extendEnv n argVal env') bdy
>         ClosV ee _ -> Left $ "non lambda in closure expression: " ++ show ee
>         _ -> Left $ "non function in app position: " ++ show x

> interp _env (I.Block []) = Left $ "empty block"
> interp env (I.Block [s]) = interp env s
> interp env (I.Block (s:ss)) = interp env s >> interp env (I.Block ss)
