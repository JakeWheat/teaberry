
> {-# LANGUAGE TupleSections #-}
> module InterpreterTests (interpreterTests, testInterpreter) where


> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

> import Parse (parseProgram)
> import Desugar (desugarProgram)
> import Interpreter (Value(..))
> import qualified Interpreter as I
> --import qualified InterpreterSyntax as I

> interpreterTests :: [(String, String, Value)]
> interpreterTests =
>     [("2", [], NumV 2)
>     ,("true", [], BoolV True) 
>     ,("false", [],  BoolV False)
>     ,("\"hello\"", [], StrV "hello")
>     ]

> testInterpreter :: (String, String, Value) -> T.TestTree
> testInterpreter (src, defs, ex) = T.testCase ("interp " ++ src) $ do
>     let s = "block:\n" ++ defs ++ "\n" ++ src ++ "\nend"
>         ast = either error id $ parseProgram "" s
>         iast = either error id $ desugarProgram ast
>     Right (Just x) <- I.interp iast
>     T.assertEqual "" ex x
