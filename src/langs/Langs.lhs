

> import qualified SimpleExpr
> import qualified SimpleExprReader
> import qualified SimpleExprDesugared

> import qualified Test.Tasty as T

> main :: IO ()
> main = T.defaultMain $ T.testGroup "all"
>        [SimpleExpr.tests
>        ,SimpleExprReader.tests
>        ,SimpleExprDesugared.tests
>        ]
