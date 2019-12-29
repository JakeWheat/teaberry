
> module Engine (runCode
>               ,Value(..)
>               ) where

> --import Syntax
> import Parse (parseStmts)
> --import Pretty
> import Desugar (desugarStmts)
> import Interpreter (interp, defaultHaskellFFIEnv, Value(..))
> import InterpreterSyntax as I

> runCode :: String -> IO (Maybe Value)
> runCode src = do
>     let s = "block:\n" ++ src ++ "\nend"
>         ast = either error id $ parseStmts "" s
>         iast = either error id $ desugarStmts ast
>     x <- interp defaultHaskellFFIEnv (extract iast)
>     either error (pure . Just) x
>  where
>    extract [a@(I.StExpr {})] = a
>    extract x = error $ "not an stexpr:" ++ show x
