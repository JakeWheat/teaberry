

> import ParseTests
> import DesugarTests
> import InterpreterTests

> import Test.Tasty
> --import Test.Tasty.HUnit

> main :: IO ()
> main = do
>     defaultMain $ testGroup "all"
>         [testGroup "parse" $ map testParse parseExamples
>         ,testGroup "desugar" $ map testDesugar desugarExamples
>         ,testGroup "interpret" $ map testInterpreter interpreterExamples
>         ]
