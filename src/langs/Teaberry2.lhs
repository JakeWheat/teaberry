

Simple repl front end using haskeline

> import Control.Monad.Trans
> import System.Console.Haskeline
>
> import Records1Embedded (TeaberryHandle
>                         ,newTeaberryHandle
>                         ,runScript
>                         ,valueToString
>                         )

> process :: TeaberryHandle -> String -> IO ()
> process h src = do
>     x <- runScript h [] src
>     case x of
>         Left y -> putStrLn $ "error: " ++ y
>         Right v -> case valueToString v of
>             Nothing -> pure ()
>             Just s -> putStrLn s

> repl :: TeaberryHandle -> InputT IO ()
> repl h = go
>   where
>     go = do
>         minput <- getInputLine "t > "
>         case minput of
>             Nothing -> pure ()
>             Just input -> do
>                 liftIO $ process h input
>                 go

> main :: IO ()
> main = do
>     h <- newTeaberryHandle
>     runInputT st (repl h)
>   where
>     st = defaultSettings {historyFile = (Just ".teaberryreplhistory")}
