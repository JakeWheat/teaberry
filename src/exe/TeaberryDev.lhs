
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

> import qualified DumpDesugared (newTeaberryHandle
>                                ,runScript
>                                ,valueToString
>                                )


> import qualified Records1Embedded (newTeaberryHandle
>                                   ,runScript
>                                   ,valueToString)

> import qualified OpaqueFFIValues (newTeaberryHandle
>                                   ,runScript
>                                   ,valueToString)

> import qualified Provide1 (newTeaberryHandle
>                                   ,runScript
>                                   ,valueToString)

> import qualified FixWhere (newTeaberryHandle
>                                   ,runScript
>                                   ,valueToString)


> import qualified Front (newTeaberryHandle
>                        ,runScript
>                        ,valueToString)


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

hacky engine to support multiple language implementations

> data Backend h v = Backend
>     {xNewHandle :: IO h
>     ,xRunScript :: h -> Maybe String -> [(String,v)] -> String -> IO v
>     ,xValueToString :: v -> Maybe String
>     }

run file or script

> runFile :: Backend ht vt -> FilePath -> IO ()
> runFile be fp = do
>     src <- readFile fp
>     runSrc be (Just fp) src

> runSrc :: Backend ht vt -> Maybe String -> String -> IO ()
> runSrc be fnm src = do
>     h <- xNewHandle be
>     v <- (xRunScript be) h fnm [] src
>     case (xValueToString be) v of
>         Nothing -> pure ()
>         Just s -> putStrLn s

repl

> process :: Backend h v -> h -> String -> IO ()
> process be h src = (do
>     v <- (xRunScript be) h Nothing [] src
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
>   , testLevel :: Int
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
>     let defaultBackend = front
>         front = Backend {xNewHandle = Front.newTeaberryHandle
>                         ,xRunScript = Front.runScript
>                         ,xValueToString = Front.valueToString}
>         dumpDesugared = Backend {xNewHandle = DumpDesugared.newTeaberryHandle
>                                 ,xRunScript = DumpDesugared.runScript
>                                 ,xValueToString = DumpDesugared.valueToString}
>         import4Repl = Backend {xNewHandle = Import4Repl.newTeaberryHandle
>                               ,xRunScript = Import4Repl.runScript
>                               ,xValueToString = Import4Repl.valueToString}
>         opaqueFFIValues = Backend {xNewHandle = OpaqueFFIValues.newTeaberryHandle
>                               ,xRunScript = OpaqueFFIValues.runScript
>                               ,xValueToString = OpaqueFFIValues.valueToString}
>         provide1 = Backend {xNewHandle = Provide1.newTeaberryHandle
>                               ,xRunScript = Provide1.runScript
>                               ,xValueToString = Provide1.valueToString}
>         fixWhere = Backend {xNewHandle = FixWhere.newTeaberryHandle
>                               ,xRunScript = FixWhere.runScript
>                               ,xValueToString = FixWhere.valueToString}
>         records1Embedded = Backend {xNewHandle = Records1Embedded.newTeaberryHandle
>                                    ,xRunScript = \h _ e s ->
>                                            either error id <$> Records1Embedded.runScript h e s
>                                    ,xValueToString = Records1Embedded.valueToString}
>     case backend os of
>         Nothing -> runWithBackend os defaultBackend
>         Just "default" -> runWithBackend os defaultBackend
>         Just "Front" -> runWithBackend os front
>         Just "Import4Repl" -> runWithBackend os import4Repl
>         Just "OpaqueFFIValues" -> runWithBackend os opaqueFFIValues
>         Just "Provide1" -> runWithBackend os provide1
>         Just "FixWhere" -> runWithBackend os fixWhere
>         Just "DumpDesugared" -> runWithBackend os dumpDesugared
>         Just "Records1Embedded" -> runWithBackend os records1Embedded
>         Just x -> error $ "backend not recognised: " ++ x

> runWithBackend :: MyOpts -> Backend h v -> IO ()
> runWithBackend os be =
>     case os of
>         MyOpts {file = Just {}, script = Just {}} -> error "please pass either a file or code to run, not both"
>         MyOpts {file = Just f} -> runFile be f
>         MyOpts {script = Just c} -> runSrc be Nothing c
>         _ -> doRepl be
