

> import qualified SimpleExpr
> import qualified SimpleExprError
> import qualified SimpleExprReader
> import qualified SimpleExprDesugared
> import qualified SimplestCheck
> import qualified SimpleExprFFI
> import qualified SimpleExprIf
> import qualified SimpleExprIfAndOr
> import qualified SimpleExprTupleHack
> import qualified SimpleStatements
> import qualified SimpleStatementsDesugarLetDecl
> import qualified SimpleStatementsSeq
> import qualified SimpleStatementsWithoutCheck
> import qualified SimpleStatementsCheck
> import qualified SimpleStatementsCheckDesugarRefactor

> import qualified Test.Tasty as T

> main :: IO ()
> main =
>     T.defaultMain $ T.testGroup "all" $ 
>        [SimpleExpr.tests
>        ,SimpleExprError.tests
>        ,SimpleExprReader.tests
>        ,SimpleExprDesugared.tests
>        ,SimplestCheck.tests
>        ,SimpleExprFFI.tests
>        ,SimpleExprIf.tests
>        ,SimpleExprIfAndOr.tests
>        ,SimpleExprTupleHack.tests
>        ,SimpleStatements.tests
>        ,SimpleStatementsDesugarLetDecl.tests
>        ,SimpleStatementsSeq.tests
>        ,SimpleStatementsWithoutCheck.tests
>        ,SimpleStatementsCheck.tests
>        ,SimpleStatementsCheckDesugarRefactor.tests
>        ]
