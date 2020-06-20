
New command line front end

run script in file, passed on command line or get repl

todo: implement test levels

todo: multiple -f, -c
todo: ability to start the repl after doing -f, -c
todo: more flexibility, logging, only run for some modules variations
todo: ability to run the system tests via tasty (uses separate exe for now)
todo: catch all non-asynchronous exceptions
  ctrl-c special handling for now: reset the repl input buffer on a new line
    future: ability to interrupt a running process and return to the repl prompt cleanly and quickly
  make sure to pass things through to haskeline if needed (e.g. if
   ctrl d works like this)
todo: pass args from command line into scripts
todo: config files, init files, etc.


------------------------------------------------------------------------------

> {-# LANGUAGE ScopedTypeVariables #-}

> import Control.Monad.Trans
> import System.Console.Haskeline

> import Control.Exception.Safe (catch, SomeException, displayException)

> import qualified Front (newTeaberryHandle
>                        ,runScript
>                        ,valueToString
>                        ,TeaberryHandle
>                        )


> import Options.Applicative (Parser
>                            ,strOption
>                            ,long
>                            ,metavar
>                            ,help
>                            ,short
>                            ,option
>                            --,showDefault
>                            --,switch
>                            ,auto
>                            ,value
>                            ,execParser
>                            ,info
>                            ,(<**>)
>                            ,helper
>                            ,fullDesc
>                            ,progDesc
>                            ,header
>                            ,ParserInfo
>                            ,optional
>                            )
>

------------------------------------------------------------------------------

run file or script

> runFile :: FilePath -> IO ()
> runFile fp = do
>     src <- readFile fp
>     runSrc (Just fp) src

> runSrc :: Maybe String -> String -> IO ()
> runSrc fnm src = do
>     h <- Front.newTeaberryHandle
>     v <- Front.runScript h fnm [] src
>     case Front.valueToString v of
>         Nothing -> pure ()
>         Just s -> putStrLn s

repl

> process :: Front.TeaberryHandle -> String -> IO ()
> process h src = (do
>     v <- Front.runScript h Nothing [] src
>     case Front.valueToString v of
>             Nothing -> pure ()
>             Just s -> putStrLn s)
>     `catch` (\(e::SomeException) -> putStrLn $ "Error: " ++ displayException e)

> repl :: Front.TeaberryHandle -> InputT IO ()
> repl h = go
>   where
>     go = withInterrupt (do
>         minput <- getInputLine "t > "
>         case minput of
>             Nothing -> pure ()
>             Just input -> do
>                 liftIO $ process h input
>                 go)
>         -- ctrl-c resets to the prompt, doesn't exit the repl
>         `catch` (\(_::Interrupt) -> liftIO (putStr "^C") >> go)


> doRepl :: IO ()
> doRepl = do
>     h <- Front.newTeaberryHandle
>     runInputT st (repl h)
>   where
>     st = defaultSettings {historyFile = Just ".teaberryreplhistory"}

------------------------------------------------------------------------------

command line options
--------------------

-f load file
-c run script
starts repl if neither of these are specified
--full-test-output (default?) run all the tests and output everything
--hide-tests-successes run all the tests, only show the failures and the summary
--one-line-test-summary run all the tests, only show one line with the number of successes and failures
--backend backend-name  choose a language backend to use instead of the latest one
  probably only useful for limited troubleshooting

> data MyOpts = MyOpts
>   { file :: Maybe String
>   , script :: Maybe String
>   , testLevel :: Int}
>   deriving Show
>               
> 
> myOpts :: Parser MyOpts
> myOpts = MyOpts
>       <$> optional (strOption
>           (short 'f'
>            <> metavar "FILE"
>            <> help "file containing script to run"))
>       <*> optional (strOption
>           (short 'c'
>            <> metavar "SOURCE"
>            <> help "code to run"))
>       <*> option auto
>           (long "test-level"
>            <> value 1
>            <> metavar "INT"
>            <> help "test-level 0 = skip, 1= one line, 2 = show failures, 3 = show all")

> myOptsPlus :: ParserInfo MyOpts
> myOptsPlus = info (myOpts <**> helper)
>       ( fullDesc
>      <> progDesc "Teaberry script runner and repl"
>      <> header "Teaberry" )

------------------------------------------------------------------------------

main
----


> main :: IO ()
> main = do
>     os <- execParser myOptsPlus
>     case os of
>         MyOpts {file = Just {}, script = Just {}} -> error "please pass either a file or code to run, not both"
>         MyOpts {file = Just f} -> runFile f
>         MyOpts {script = Just c} -> runSrc Nothing c
>         _ -> doRepl
