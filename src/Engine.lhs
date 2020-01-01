
TODO:
runCode -> runs the script without the tests
what does the version which also runs the tests look like?
  what's the api of tests passing or failing?
runTests -> runs only the tests in the script
can also think about doing a prepCode or something
  then you can run the tests, and run the script itself separately
  without "recompiling"
once do imports, have an option to run all tests
  or just the tests in the immediately loaded file


> module Engine (runCode
>               ,Value(..)
>               ,CheckResult(..)
>               ,renderCheckResults
>               ,runChecks
>		,compileReport
>               ) where

> import Data.List (intercalate, partition)
> import Data.Maybe (isNothing)

> import Parse (parseProgram)
> import Desugar (desugarProgram)
> import Interpreter (interp, Value(..), CheckResult(..))
> import qualified Interpreter as I
> import InterpreterSyntax as I
> import Text.Show.Pretty (ppShow)
> --import Debug.Trace

> compileProgram :: String -> Either String I.Program
> compileProgram src = do
>     ast <- parseProgram "" src
>     desugarProgram ast


compile report

what would make this actually useful, is to be able to pretty print
 the interpreter syntax as concrete syntax
at the moment, it's totally unreadable and useless for debugging

> compileReport :: String -> Either String String
> compileReport src = do
>     ast <- parseProgram "" src
>     iast <- desugarProgram ast
>     pure (ppShow ast ++ "\n\n" ++ ppShow iast)


> runCode :: String -> IO (Maybe Value)
> runCode src = do
>     let p = either error id $ compileProgram src
>     x <- interp p
>     either error pure x

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
>            ++ "\n  " ++ show (length ps) ++ "/" ++ show (length ts) ++ " tests passed in check block: " ++ nm
>            )
>     renderTest (a,b) =
>         "test (" ++ a ++ "): "
>         ++ case b of
>                Nothing -> "OK"
>                Just msg -> "failed, reason:\n" ++ indent msg
>     indent [] = []
>     indent x = "  " ++ indent' x
>     indent' [] = []
>     indent' x@[_] = x
>     indent' ('\n':y@(_:_)) = "\n  " ++ indent' y
>     indent' (x:y) = x : indent' y
>            
>            

> runChecks :: String -> IO [CheckResult]
> runChecks src = do
>     let p = either error id $ compileProgram src
>     x <- I.runChecks p
>     either error pure x


