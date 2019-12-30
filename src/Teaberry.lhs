
-c run the text given
-f load the file



todo

parse command line args and make available to script

add provides and imports

add option to also run tests
and to run only the tests


> import System.Environment (getArgs)

> import Engine (runCode, runChecks, renderCheckResults, CheckResult(..))


> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         [] -> help
>         --  ["-x"] -> runX
>         ["-h"] -> help
>         ["-c", cmd] -> runc cmd
>         ["-f", fn] -> do
>             cmd <- readFile fn
>             runc cmd
>         _ -> help
>     --let cmd = "a = 5\nprint(a + 4)\na"
>   where
>       runc cmd = do
>           v <- runCode cmd
>           case v of
>               Nothing -> pure ()
>               Just v' -> putStrLn $ show v'
>       help = putStrLn "-c to run code in arg, -f to run code in file"
>       {-runX = putStrLn $ renderCheckResults
>              [CheckResult "a first block"
>               [("5 is 5", Nothing)
>               ,("4 is 5", Just "Values not equal:\n4\n5")]
>              ,CheckResult "a second block"
>              [("6 is 7", Just "Values not equal:\n6\n7")]
>              ]-}
>              
