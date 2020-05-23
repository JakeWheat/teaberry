
expects a source in the following format:

check:
  expr1 is expr2
  ...
end

there must be a single anonymous check block
all statements in the check block must be 'is'
the expressions are any expressions supported

> {-# LANGUAGE QuasiQuotes #-}
> module SimplestCheck (tests) where

> import Text.RawString.QQ
> import qualified SimpleExprReader as SE
> import qualified SimpleExpr as SE
> import qualified Parse as P
> import qualified Syntax as S

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

------------------------------------------------------------------------------

syntax
------

> data Script = Script [(SE.Expr, SE.Expr)]
>             deriving (Eq, Show)


------------------------------------------------------------------------------

interpreter
-----------

> data Status = Pass | Fail

> evaluateScript :: Script -> Either String [(Status, String)]

> evaluateScript (Script []) = pure []
> evaluateScript (Script ((e, expected):ts)) = do
>     ev <- SE.runInterp [] e
>     expectedv <- SE.runInterp [] expected
>     let res = (if ev == expectedv
>              then Pass
>              else Fail
>             ,SE.prettyExpr e ++ " is " ++ SE.prettyExpr expected)
>     (res:) <$> evaluateScript (Script ts)

------------------------------------------------------------------------------

parsing
-------

> parse :: String -> Either String Script
> parse src = do
>     p <- P.parseProgram "" src
>     case p of
>         S.Program [] [S.Check Nothing is]
>             | Right ts <- mapM f is -> pure $ Script ts
>         _ -> Left $ "unsupported syntax: " ++ show p
>   where
>     f (S.StExpr (S.BinOp e0 "is" e1)) = do
>         a <- SE.convExpr e0
>         b <- SE.convExpr e1
>         pure (a,b)
>     f x = Left $ "st not supported " ++ show x

------------------------------------------------------------------------------

tests
-----

it's just an in language script now

> testScript :: String
> testScript = [r|
\begin{code} 

check:
  1 is 1
  1 + 2 is 3

  let x = 3:
    x
  end is 3

  lam(x,y):
    x + y
  end(1,2) is 3

  let f = lam(x,y): x + y end:
    f(1,2)
  end is 3

  let f = lam(x): lam(y): x + y end end,
      g = f(2):
      g(3)
  end is 5

end

\end{code}
>    |]
> tests :: T.TestTree
> tests =
>     let ts = either error id $ do
>                ast <- parse testScript
>                evaluateScript ast
>     -- todo: if executing the script itself fails
>     -- create a failing test for this
>     -- otherwise, do the actual tests
>     in T.testGroup "simplest check" $ map f ts
>   where
>     f (st, msg) = T.testCase msg $ do
>         case st of
>             Pass -> T.assertBool "" True
>             Fail -> T.assertFailure ""
>     
