
New command line front end

run script in file, passed on command line or get repl

todo: implement test levels
implement dump desugared

todo: multiple -f, -c
todo: ability to start the repl after doing -f, -c
todo: more flexibility, logging, only run for some modules variations
todo: ability to run the system tests via tasty (uses separate exe for now)
todo: switch dump desugared on and off in the repl too
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

>
> import qualified Import4Repl ({-TeaberryHandle
>                              ,-}newTeaberryHandle
>                              ,runScript
>                              ,valueToString
>                              --,Value
>                              )

> import qualified Records1Embedded (newTeaberryHandle
>                                   ,runScript
>                                   ,valueToString)

> import Options.Applicative (Parser
>                            ,strOption
>                            ,long
>                            ,metavar
>                            ,help
>                            ,short
>                            ,option
>                            --,showDefault
>                            ,switch
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

hacky engine to support multiple language implementations

> data Backend h v = Backend
>     {xNewHandle :: IO h
>     ,xRunScript :: h -> [(String,v)] -> String -> IO v
>     ,xValueToString :: v -> Maybe String
>     }

run file or script

> runFile :: Backend ht vt -> FilePath -> IO ()
> runFile be fp = do
>     src <- readFile fp
>     runSrc be src

> runSrc :: Backend ht vt -> String -> IO ()
> runSrc be src = do
>     h <- xNewHandle be
>     v <- (xRunScript be) h [] src
>     case (xValueToString be) v of
>         Nothing -> pure ()
>         Just s -> putStrLn s

repl

> process :: Backend h v -> h -> String -> IO ()
> process be h src = (do
>     v <- (xRunScript be) h [] src
>     case (xValueToString be) v of
>             Nothing -> pure ()
>             Just s -> putStrLn s)
>     `catch` (\(e::SomeException) -> putStrLn $ "Error: " ++ displayException e)

> repl :: Backend h v -> h -> InputT IO ()
> repl be h = go
>   where
>     go = withInterrupt (do
>         minput <- getInputLine "t > "
>         case minput of
>             Nothing -> pure ()
>             Just input -> do
>                 liftIO $ process be h input
>                 go)
>         -- ctrl-c resets to the prompt, doesn't exit the repl
>         `catch` (\(_::Interrupt) -> liftIO (putStr "^C") >> go)


> doRepl :: Backend h v -> IO ()
> doRepl be = do
>     h <- (xNewHandle be)
>     runInputT st (repl be h)
>   where
>     st = defaultSettings {historyFile = (Just ".teaberryreplhistory")}

------------------------------------------------------------------------------

command line options
--------------------

-f load file
-c run script
starts repl if neither of these are specified
--full-test-output (default?) run all the tests and output everything
--hide-tests-successes run all the tests, only show the failures and the summary
--one-line-test-summary run all the tests, only show one line with the number of successes and failures
--dump-desugared instead of running code, dump the pretty printed desugared stage
  in the repl, it does this for each line you enter instead of running it
--backend backend-name  choose a language backend to use instead of the latest one
  probably only useful for limited troubleshooting

> data MyOpts = MyOpts
>   { file :: Maybe String
>   , script :: Maybe String
>   , testLevel :: Int
>   , dumpDesugared :: Bool
>   , backend :: Maybe String}
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
>       <*> switch
>           (long "dump-desugared"
>            <> help "dump the desugared source instead of running")
>       <*> optional (strOption
>           (long "backend"
>            <> metavar "LANGNAME"
>            <> help "language to use"))

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
>     let defaultBackend = import4Repl
>         import4Repl = Backend {xNewHandle = Import4Repl.newTeaberryHandle
>                               ,xRunScript = Import4Repl.runScript
>                               ,xValueToString = Import4Repl.valueToString}
>         records1Embedded = Backend {xNewHandle = Records1Embedded.newTeaberryHandle
>                                    ,xRunScript = \h e s ->
>                                            either error id <$> Records1Embedded.runScript h e s
>                                    ,xValueToString = Records1Embedded.valueToString}
>     case backend os of
>         Nothing -> runWithBackend os defaultBackend
>         Just "default" -> runWithBackend os defaultBackend
>         Just "Import4Repl" -> runWithBackend os import4Repl
>         Just "Records1Embedded" -> runWithBackend os records1Embedded
>         Just x -> error $ "backend not recognised: " ++ x

> runWithBackend :: MyOpts -> Backend h v -> IO ()
> runWithBackend os be =
>     case os of
>         MyOpts {file = Just {}, script = Just {}} -> error "please pass either a file or code to run, not both"
>         MyOpts {file = Just f} -> runFile be f
>         MyOpts {script = Just c} -> runSrc be c
>         _ -> doRepl be
