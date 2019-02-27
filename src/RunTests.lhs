

> import ParseTests
> import DesugarTests
> import InterpreterTests

> import Test.Tasty
> --import Test.Tasty.HUnit

> main :: IO ()
> main = do
>     defaultMain $ testGroup "all"
>         [testGroup "parse"
>             [testGroup "parseExpr" $ map testParseExpr parseExprExamples
>             ,testGroup "parseStmt" $ map testParseStmt parseStmtExamples
>             ,testGroup "parseStmts" $ map testParseStmts parseStmtsExamples]
>         ,testGroup "desugar" $ map testDesugar desugarExamples
>         ,testGroup "interpret" $ map testInterpreter interpreterExamples
>         ]
