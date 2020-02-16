
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
>               ,compileReport
>               ,format
>               ) where

> import Data.List (intercalate, partition)
> import Data.Maybe (isNothing)
> import Text.Show.Pretty (ppShow)
> --import Debug.Trace (trace)

> import Pretty (prettyProgram)
> import Parse (parseProgram)
> import Desugar (loadProgramImports, desugarProgram)
> import Interpreter (runProgram, Value(..), CheckResult(..))
> import qualified Interpreter as I
> import qualified InterpreterSyntax as I
> import qualified PrettyInterpreter as I
> import qualified Syntax as S
> import System.FilePath (takeBaseName, takeDirectory)

> compileProgram :: FilePath -> String -> IO (Either String I.Program)
> compileProgram fn src = do
>     -- todo: sort out the monad transformer
>     let ast = either error id $ parseProgram fn src
>     let mn = S.ImportName $ stripExtras $ if fn == "" then "top-level" else fn
>     ps <- loadProgramImports (takeDirectory fn) mn ast
>     pure $ desugarProgram ps
>   where
>     -- translate a filename into a unique module name
>     -- todo: convert non identifier characters to _
>     stripExtras x = takeBaseName x


compile report

what would make this actually useful, is to be able to pretty print
 the interpreter syntax as concrete syntax
at the moment, it's totally unreadable and useless for debugging

> compileReport :: FilePath -> String -> Either String String
> compileReport fn src = do
>     ast <- parseProgram fn src
>     iast <- desugarProgram [(S.ImportName "top-level", ast)]
>     pure (ppShow ast ++ "\n==============================\n"
>           ++ ppShow iast ++ "\n==============================\n"
>           ++ I.prettyProgram iast)


> runCode :: FilePath -> String -> IO (Either String Value)
> runCode fn src = do
>     x <- compileProgram fn src
>     case x of
>         Left e -> pure $ Left e
>         Right p -> runProgram p

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

> runChecks :: FilePath -> String -> IO (Either String [CheckResult])
> runChecks fn src = do
>     x <- compileProgram fn src
>     case x of
>         Left e -> pure $ Left e
>         Right p -> I.runChecks p

> format :: FilePath -> String -> Either String String
> format fn src = do
>     ast <- parseProgram fn src
>     pure $ prettyProgram ast

