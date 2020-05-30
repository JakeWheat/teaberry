
New command line front end

run script in file, passed on command line or get repl

todo: implement test levels
implement dump desugared
test adding a backend



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

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> import Control.Monad.Trans
> import System.Console.Haskeline

> import Control.Exception.Safe (catch)

>
> import qualified Import4Repl as D (TeaberryHandle
>                                   ,newTeaberryHandle
>                                   ,runScript
>                                   ,valueToString
>                                   ,Value
>                                   )


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

run file or script

> runFile :: FilePath -> IO ()
> runFile fp = do
>     src <- readFile fp
>     runSrc src

> runSrc :: String -> IO ()
> runSrc src = do
>     h <- D.newTeaberryHandle
>     x <- D.runScript h [] src
>     case x of
>         Left e -> error e
>         Right v -> case D.valueToString v of
>             Nothing -> pure ()
>             Just s -> putStrLn s

------------------------------------------------------------------------------

repl

> process :: D.TeaberryHandle -> String -> IO ()
> process h src = do
>     x <- D.runScript h [] src
>     case x of
>         Left y -> putStrLn $ "error: " ++ y
>         Right v -> case D.valueToString v of
>             Nothing -> pure ()
>             Just s -> putStrLn s

> repl :: D.TeaberryHandle -> InputT IO ()
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
>     h <- D.newTeaberryHandle
>     runInputT st (repl h)
>   where
>     st = defaultSettings {historyFile = (Just ".teaberryreplhistory")}

------------------------------------------------------------------------------

run file or script

> {-runFile :: FilePath -> IO ()
> runFile fp = do
>     src <- readFile fp
>     runSrc src-}

> {-runSrcX :: Backend h v -> String -> IO ()
> runSrcX be src = do
>     h <- newHandle be
>     x <- (runScript be) h [] src
>     case x of
>         Left e -> error e
>         Right v -> case (valueToString be) v of
>             Nothing -> pure ()
>             Just s -> putStrLn s-}

------------------------------------------------------------------------------

repl

> {-process :: D.TeaberryHandle -> String -> IO ()
> process h src = do
>     x <- D.runScript h [] src
>     case x of
>         Left y -> putStrLn $ "error: " ++ y
>         Right v -> case D.valueToString v of
>             Nothing -> pure ()
>             Just s -> putStrLn s

> repl :: D.TeaberryHandle -> InputT IO ()
> repl h = go
>   where
>     go = do
>         minput <- getInputLine "t > "
>         case minput of
>             Nothing -> pure ()
>             Just input -> do
>                 liftIO $ process h input
>                 go

> doRepl :: IO ()
> doRepl = do
>     h <- D.newTeaberryHandle
>     runInputT st (repl h)
>   where
>     st = defaultSettings {historyFile = (Just ".teaberryreplhistory")}  -}


------------------------------------------------------------------------------

backends

 > class Backend h v | h -> v, v -> h where
 >     newHandle :: IO h
 >     runScript :: h -> [(String,v)] -> String -> IO (Either String v)
 >     valueToString :: v -> Maybe String
 >
 > data AnyBackend = forall a b. (Backend a b) => AnyBackend a b
 >
 > instance Backend AnyBackend where
 >     newHandle = newHandle
 >     runScript  :: h -> [(String,v)] -> String -> IO (Either String v)
 >     valueToString :: v -> Maybe String

class Renderable a where
 boundingSphere :: a -> Sphere
 hit :: a -> [Fragment] -- returns the "fragments" of all hits with ray

 > defaultBackend :: Backend
 > defaultBackend = Backend D.TeaberryHandle D.Value

 > instance Backed BackendC 

 data AnyRenderable = forall a. Renderable a => AnyRenderable a

  instance Renderable AnyRenderable where
      boundingSphere (AnyRenderable a) = boundingSphere a
      hit (AnyRenderable a) = hit a
  {-      ... -}

Now, create lists with type [AnyRenderable], for example,

    [ AnyRenderable x
    , AnyRenderable y
    , AnyRenderable z ]


> {-instance Backend D.TeaberryHandle D.Value where
>     newHandle = D.newTeaberryHandle
>     runScript = D.runScript
>     valueToString = D.valueToString-}
>
> {-data Backend h v = Backend
>     {newHandle :: IO h
>     ,runScript :: h -> [(String,v)] -> String -> IO (Either String v)
>     ,valueToString :: v -> Maybe String}

> defaultBackend = Backend {newHandle = D.newTeaberryHandle
>                          ,runScript = D.runScript
>                          ,valueToString = D.valueToString}-}

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
>     x <- execParser myOptsPlus
>     -- putStrLn $ show x
>     case x of
>         MyOpts {file = Just {}, script = Just {}} -> error "please pass either a file or code to run, not both"
>         MyOpts {file = Just f} -> runFile f
>         MyOpts {script = Just c} -> runSrc c
>         _ -> doRepl
