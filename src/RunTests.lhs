

> import qualified Test.Tasty as T

> import ParseTests (testParseExpr
>                   ,parseExprExamples
>                   ,testParseStmt
>                   ,parseStmtExamples
>                   ,testParseStmts
>                   ,parseStmtsExamples)
> import DesugarTests (testDesugar,desugarExamples)
> import InterpreterTests (testInterpreter,interpreterExamples)


> main :: IO ()
> main = do
>     T.defaultMain $ T.testGroup "all"
>         [T.testGroup "parse"
>             [T.testGroup "parseExpr" $ map testParseExpr parseExprExamples
>             ,T.testGroup "parseStmt" $ map testParseStmt parseStmtExamples
>             ,T.testGroup "parseStmts" $ map testParseStmts parseStmtsExamples]
>         ,T.testGroup "desugar" $ map testDesugar desugarExamples
>         ,T.testGroup "interpret" $ map testInterpreter interpreterExamples
>         ]
