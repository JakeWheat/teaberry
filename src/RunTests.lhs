

> import qualified Test.Tasty as T

> import ParseTests (testParseExpr
>                   ,parseExprExamples
>                   ,testParseStmt
>                   ,parseStmtExamples
>                   ,testParseStmts
>                   ,parseStmtsExamples)
> import DesugarTests (testDesugarExpr
>                     ,desugarExprExamples
>                     ,testDesugarStmts
>                     ,desugarStmtsExamples)
> import InterpreterTests (testInterpreter,interpreterExamples)
> import TeaberryTests (sourceFiles
>                      ,testSourceFile
>                      ,detailedSourceFileTests
>                      ,testDetailedSourceFile)

> main :: IO ()
> main = do
>     --testTests <- T.testGroup "test tests" <$> mapM testDetailedSourceFile detailedSourceFileTests
>     sourceFileTests <- T.testGroup "source file tests" <$> mapM testSourceFile sourceFiles
>     T.defaultMain $ T.testGroup "all"
>         [T.testGroup "parse"
>             [T.testGroup "parseExpr" $ map testParseExpr parseExprExamples
>             ,T.testGroup "parseStmt" $ map testParseStmt parseStmtExamples
>             ,T.testGroup "parseStmts" $ map testParseStmts parseStmtsExamples]
>         ,T.testGroup "desugarExpr" $ map testDesugarExpr desugarExprExamples
>         ,T.testGroup "desugarStmts" $ map testDesugarStmts desugarStmtsExamples
>         ,T.testGroup "interpret" $ map testInterpreter interpreterExamples
>         ,sourceFileTests
>         --,T.testGroup "test tests" testTests
>         ]
