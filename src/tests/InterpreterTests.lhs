
> {-# LANGUAGE TupleSections #-}
> module InterpreterTests (interpreterTests, testInterpreter) where


> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

> {-import Parse (parseProgram)
> import Desugar (desugarProgram)-}
> import Interpreter (Value(..))
> --import qualified Interpreter as I
> --import qualified InterpreterSyntax as I

> interpreterTests :: [(String, String, Value)]
> interpreterTests =
>     [{-("2", [], NumV 2)
>     ,("true", [], BoolV True) 
>     ,("false", [],  BoolV False)
>     ,("\"hello\"", [], StrV "hello")
>     ,("nothing", [], NothingV)-}
>     ]

> testInterpreter :: (String, String, Value) -> T.TestTree
> testInterpreter (src, _defs, _ex) = T.testCase ("interp " ++ src) $ do
>     pure () {-
>     let s = "block:\n" ++ defs ++ "\n" ++ src ++ "\nend"
>         ast = either error id $ parseProgram "" s
>         iast = either error id $ desugarProgram ast
>     Right x <- I.runProgram iast
>     T.assertEqual "" ex x-}
