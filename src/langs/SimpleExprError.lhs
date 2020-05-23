
Simple expression language example using error instead of Either and
monads/do notation.


> {-# LANGUAGE TupleSections #-}

> module SimpleExprError (tests) where
>
> import SimpleExpr (Expr(..)
>                   ,parse
>                   ,simpleInterpreterExamples
>                   ,TestTree
>                   ,makeSimpleTests)

> import Data.Scientific (Scientific)

------------------------------------------------------------------------------

interpreter
-----------

> type Env = [(String,Value)]

> data Value = NumV Scientific
>            | FunV [String] Expr Env
>            deriving (Eq, Show)

> interp :: Env -> Expr -> Value
> interp _ (Num n) = NumV n
> interp env (Iden i) =
>     maybe (error $ "Identifier not found: " ++ i) id
>     $ lookup i env
> interp env (Plus a b) =
>     case (interp env a, interp env b) of
>         (NumV an, NumV bn) -> NumV $ an + bn
>         _ -> error $ "bad args to  plus " ++ show (a, b)
> interp env (App f es) =
>     case (interp env f, map (interp env) es) of
>         (FunV ps bdy env', vs)
>             | length vs == length ps ->
>                   let env'' = zip ps vs ++ env'
>                   in interp env'' bdy
>             | otherwise -> error $ "wrong number of args to function"
>         _ -> error "non function value in app position"
> interp env (Lam ps e) = FunV ps e env
> interp env (Let bs e) =
>     let newEnv en [] = en
>         newEnv en ((b,ex):bs') = newEnv ((b,interp en ex):en) bs'
>     in interp (newEnv env bs) e

> evaluate :: String -> Value
> evaluate s =
>     let ast = either error id $ parse s
>     in interp [] ast


------------------------------------------------------------------------------

tests
-----

> tests :: TestTree
> tests = makeSimpleTests "simpleexprerror" simpleInterpreterExamples evaluate

