
-c run the text given
-f load the file



todo

parse command line args and make available to script

add provides and imports

add option to also run tests
and to run only the tests
possibly run tests quietly and exit more noisily if there is a failure
  -> this is interesting for non top level tests which are still pretty useful
     but don't really fit into a normal test only run
     think about how to make them more viable:
     option to execute every time the block is 'executed'
       or just the first time
     if you want to test these as part of a test only run
     -> want to log the results with all the other test results
     and maybe make some test assertions about how many times a test is run
     to make sure there's a failure if e.g. it isn't run at all


> import System.Environment (getArgs)
> import System.Exit (exitFailure)
> import Control.Monad (when)

> import Engine (runCode
>               ,runChecks
>               ,renderCheckResults
>               ,CheckResult(..)
>               ,allCheckResultsPassed
>               ,compileReport
>               ,format
>               ,Value(NothingV))

> import qualified Parserize


> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         [] -> help
>         ["-x"] -> runX
>         ["-h"] -> help
>         ["-c", cmd] -> runc "" cmd
>         ["-f", fn] -> do
>             cmd <- readFile fn
>             runc fn cmd
>         ["-g", fn] -> do
>             cmd <- readFile fn
>             runt fn cmd
>         ["-r", fn] -> do
>             cmd <- readFile fn
>             putStrLn $ either id id $ compileReport fn cmd
>         ["-p", fn] -> do
>             cmd <- readFile fn
>             putStrLn $ either id id $ format fn cmd
>         ["--parserize", fn] -> do
>             src <- readFile fn
>             putStrLn $ parserize fn src
>         ["--parserize-c", src] -> do
>             putStrLn $ parserize "" src
>         _ -> help
>     --let cmd = "a = 5\nprint(a + 4)\na"
>   where
>       runc fn cmd = do
>           v <- runCode fn cmd
>           case v of
>               Left e -> error e -- todo exit with error code
>               Right NothingV -> pure ()
>               Right v' -> putStrLn $ show v'
>       runt fn cmd = do
>           v <- runChecks fn cmd
>           case v of
>               Left e -> error e -- todo exit with error code
>               Right v1 -> do
>                   putStrLn $ renderCheckResults v1
>                   when (not $ allCheckResultsPassed v1)
>                       exitFailure
>       parserize fn src = do
>           Parserize.parserize fn src
>       help = putStrLn "-c to run code in arg, -f to run code in file"
>       runX = putStrLn $ renderCheckResults
>              [CheckResult "a first block"
>               [("5 is 5", Nothing)
>               ,("4 is 5", Just "Values not equal:\n4\n5")]
>              ,CheckResult "a second block"
>              [("6 is 7", Just "Values not equal:\n6\n7")]
>              ]
>              
