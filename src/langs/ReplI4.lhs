
Simple repl front end using haskeline

> import Control.Monad.Trans
> import System.Console.Haskeline
>
> import Import4Repl (ReplImplHandle
>                     ,startReplImpl
>                     ,evaluateLine)

> type Repl a = InputT IO a

> process :: ReplImplHandle -> String -> IO ()
> process h src = do
>     x <- evaluateLine h src
>     case x of
>         Left y -> putStrLn $ "error: " ++ y
>         Right Nothing -> pure ()
>         Right (Just s) -> putStrLn s


> repl :: ReplImplHandle -> Repl ()
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
>     h <- startReplImpl
>     runInputT st (repl h)
>   where
>     st = defaultSettings {historyFile = (Just ".teaberryreplhistory")}
