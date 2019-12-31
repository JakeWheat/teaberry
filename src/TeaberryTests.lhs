
wrapper to lift tests written in the language into tests for the
 language implementation

> {-# LANGUAGE TupleSections #-}
> module TeaberryTests (sourceFiles
>                      ,testSourceFile
>                      ,detailedSourceFileTests
>                      ,testDetailedSourceFile) where


> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

> import qualified Engine as E


todo: run tests on a few example files and check the precise test results

this will also test that test failures are picked up

> -- tests which run the tests, and then check predicates on the results
> data DetailedSourceFileTest = A Int

> detailedSourceFileTests :: [DetailedSourceFileTest]
> detailedSourceFileTests = undefined

> testDetailedSourceFile :: T.TestTree
> testDetailedSourceFile = undefined

> -- these tests load the file, then run the tests from the files
> -- then hoist the tests into Tasty
> sourceFiles :: [FilePath]
> sourceFiles = map ("examples/tests/fulltests/" ++)
>     ["trivial_is.tea"]

> testSourceFile :: FilePath -> IO T.TestTree
> testSourceFile fp = do
>     src <- readFile fp
>     cs <- E.runChecks src
>     let ts :: [T.TestTree]
>         ts = map (getTests fp) cs
>     pure $ T.testGroup fp ts
>
> getTests :: FilePath -> E.CheckResult -> T.TestTree
> getTests fp (E.CheckResult cn ts) = T.testGroup (fp ++ " " ++ cn) $ do
>     flip map ts $ \(nm,msg) ->
>         T.testCase nm $ case msg of
>             Nothing -> T.assertBool "" True
>             Just fmsg -> T.assertFailure fmsg
