
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
>               ) where

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
>                               [Maybe String] -- Just means there was a failure

> renderCheckResults :: [CheckResult] -> String
> renderCheckResults = undefined

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
  The test failed.
 
1/3 tests passed in all check blocks

