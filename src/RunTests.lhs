

> import ParseTests

> import Test.Tasty
> import Test.Tasty.HUnit

> main :: IO ()
> main = do
>     defaultMain $ testGroup "all"
>         [testGroup "parse" $ map testParse parseExamples
>         ]
