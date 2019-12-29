
-c run the text given
-f load the file



todo

parse command line args and make available to script

add provides and imports

> import System.Environment (getArgs)

> import Engine (runCode)


> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         [] -> help
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
