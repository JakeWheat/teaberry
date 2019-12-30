
TODO:
runCode -> runs the script without the tests
what does the version which also runs the tests look like?
  what's the api of tests passing or failing?
runTests -> runs only the tests in the script
can also think about doing a prepCode or something
  then you can run the tests, and run the script itself separately
  without "recompiling"

> module Engine (runCode
>               ,Value(..)
>               ,CheckResult(..)
>               ,renderCheckResults
>               ,runChecks
>               ) where

> import Data.List (intercalate, partition)
> import Data.Maybe (isNothing)

> --import Syntax
> import Parse (parseStmts)
> --import Pretty
> import Desugar (desugarStmts)
> import Interpreter (interp, defaultHaskellFFIEnv, Value(..))
> import InterpreterSyntax as I

> compileProgram :: String -> Either String I.Program
> compileProgram src = do
>     let s = "block:\n" ++ src ++ "\nend"
>     ast <- parseStmts "" s
>     uncurry Program <$> desugarStmts ast

> runCode :: String -> IO (Maybe Value)
> runCode src = do
>     let Program stmts _ = either error id $ compileProgram src
>     x <- interp defaultHaskellFFIEnv (extract stmts)
>     either error (pure . Just) x
>  where
>    extract [a@(I.StExpr {})] = a
>    extract x = error $ "not an stexpr:" ++ show x

todo: move this to an in language data type

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
> -- the second is just if it is a fail, it contains the failure
> -- message

> renderCheckResults :: [CheckResult] -> String
> renderCheckResults cs =
>     let bs = map renderCheck cs
>         totalPasses = sum $ map (\(n,_,_) -> n) bs
>         totalFails = sum $ map (\(_,n,_) -> n) bs
>         msgs = map (\(_,_,m) -> m) bs
>     in intercalate "\n\n" msgs
>        ++ "\n\n" ++ (show totalPasses) ++ "/" ++ show (totalPasses + totalFails)
>        ++ " tests passed in all check blocks"
>   where
>     renderCheck (CheckResult nm ts) =
>         let (ps,fs) = partition (isNothing . snd) ts
>             msgs = map renderTest ts
>         in (length ps
>            ,length fs
>            ,"Check block: " ++ nm ++ "\n"
>            ++ intercalate "\n" (map indent msgs)
>            ++ "  " ++ show (length ps) ++ "/" ++ show (length ts) ++ " tests passed in check block: " ++ nm
>            )
>     renderTest (a,b) =
>         "test (" ++ a ++ "): "
>         ++ case b of
>                Nothing -> "OK"
>                Just msg -> "failed, reason:\n" ++ indent msg
>     indent = unlines . map ("  " ++) . lines 
>            
>            

> runChecks :: String -> IO [CheckResult]
> runChecks = undefined

Check block: a first block
  test (5 is 5): ok
  test (4 is 5): failed, reason:
    Values not equal:
    4
    5
  1/2 tests passed in check block: a first block
 
Check block: a second block
  test (6 is 7): failed, reason:
    Values not equal:
    6
    7
  0/1 tests passed in check block: a first block
 
1/3 tests passed in all check blocks

