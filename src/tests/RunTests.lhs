

> import qualified Test.Tasty as T

> import ParseTests (testParseExpr
>                   ,parseExprExamples
>                   ,testParseStmt
>                   ,parseStmtExamples
>                   ,testParseProgram
>                   ,parseProgramExamples)
> import DesugarTests (testDesugarExpr
>                     ,desugarExprExamples)
> import InterpreterTests (testInterpreter,interpreterTests)
> import TeaberryTests (sourceFiles
>                      ,testSourceFile
>                      ,detailedSourceFileTests
>                      ,testDetailedSourceFile)
> import Langs (langTests)

> main :: IO ()
> main = do
>     -- todo: if there is a failure here, it stops all the tests from running
>     -- which is very bad
>     -- this should be fixed in the test desugaring, it should be able to catch
>     -- an exception and log a failure
>     sourceFileTests <- T.testGroup "source file tests" <$> mapM testSourceFile sourceFiles
>     T.defaultMain $ T.testGroup "all"
>         [T.testGroup "parse"
>             [T.testGroup "parseExpr" $ map testParseExpr parseExprExamples
>             ,T.testGroup "parseStmt" $ map testParseStmt parseStmtExamples
>             ,T.testGroup "parseProgram" $ map testParseProgram parseProgramExamples]
>         ,T.testGroup "desugarExpr" $ map testDesugarExpr desugarExprExamples
>         ,T.testGroup "interpret" $ map testInterpreter interpreterTests
>         ,T.testGroup "test tests" $ map testDetailedSourceFile detailedSourceFileTests
>         ,sourceFileTests
>         ,langTests
>         ]
