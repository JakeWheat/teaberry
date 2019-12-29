
TODO:

-c run the text given
-f load the file

parse command line args and make available to script

wrap the runtoplevel in a module

add provides and imports

> import Syntax
> import Parse
> import Pretty
> import Desugar
> import Interpreter
> import InterpreterSyntax as I

> runTopLevel :: String -> IO (Maybe Value)
> runTopLevel src = do
>     let s = "block:\n" ++ src ++ "\nend"
>         ast = either error id $ parseStmts "" s
>         iast = either error id $ desugarStmts ast
>     x <- interp defaultHaskellFFIEnv (extract iast)
>     either error (pure . Just) x
>  where
>    extract [a@(I.StExpr {})] = a
>    extract x = error $ "not an stexpr:" ++ show x


> main :: IO ()
> main = do
>     let cmd = "a = 5\nprint(a + 4)\na"
>     v <- runTopLevel cmd
>     case v of
>         Nothing -> pure ()
>         Just v' -> putStrLn $ show v'
