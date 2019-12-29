
TODO:

-c run the text given
-f load the file

parse command line args and make available to script

add provides and imports

> import Engine (runCode)


> main :: IO ()
> main = do
>     let cmd = "a = 5\nprint(a + 4)\na"
>     v <- runCode cmd
>     case v of
>         Nothing -> pure ()
>         Just v' -> putStrLn $ show v'
