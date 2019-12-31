

> import qualified Test.Tasty as T

> import ParseTests (testParseExpr
>                   ,parseExprExamples
>                   ,testParseStmt
>                   ,parseStmtExamples
>                   ,testParseProgram
>                   ,parseProgramExamples)
> import DesugarTests (testDesugarExpr
>                     ,desugarExprExamples)
> import InterpreterTests (testInterpreter,interpreterExamples)
> import TeaberryTests (sourceFiles
>                      ,testSourceFile
>                      ,detailedSourceFileTests
>                      ,testDetailedSourceFile)

> main :: IO ()
> main = do
>     sourceFileTests <- T.testGroup "source file tests" <$> mapM testSourceFile sourceFiles
>     T.defaultMain $ T.testGroup "all"
>         [T.testGroup "parse"
>             [T.testGroup "parseExpr" $ map testParseExpr parseExprExamples
>             ,T.testGroup "parseStmt" $ map testParseStmt parseStmtExamples
>             ,T.testGroup "parseProgram" $ map testParseProgram parseProgramExamples]
>         ,T.testGroup "desugarExpr" $ map testDesugarExpr desugarExprExamples
>         ,T.testGroup "interpret" $ map testInterpreter interpreterExamples
>         ,T.testGroup "test tests" $ map testDetailedSourceFile detailedSourceFileTests
>         ,sourceFileTests
>         ]
