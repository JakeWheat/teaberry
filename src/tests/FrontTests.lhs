
> module FrontTests (tests) where

> import Front

> import System.FilePath ((</>))

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T
> --import Debug.Trace (trace)

> -- used for creating the test lists for tasty to run
> import System.IO.Unsafe (unsafePerformIO)



> tests :: T.TestTree
> tests = T.testGroup "refactortests" [tests1, tests4]

> testFiles :: [FilePath]
> testFiles =
>     ["agdt.tea"
>     ,"ahoy.tea"
>     ,"arithmetic.tea"
>     ,"binding.tea"
>     ,"blocks.tea"
>     ,"boolean.tea"
>     ,"built-in-functions.tea"
>     ,"catch.tea"
>
>     ,"check_block_closures.tea"
>     ,"check_closure.tea"
>     ,"check.tea"
>     ,"comparisons.tea"
>     ,"construct.tea"
>     ,"empty_check.tea"
>     ,"empty.tea"
>     ,"functions.tea"
>     ,"identifiers.tea"
>     ,"if_ask.tea"
>     ,"let.tea"
>     ,"lists-basics.tea"
>     ,"lists.tea"
>     ,"nested_comment.tea"

>     ,"prelude-combine-provides.tea"
>     ,"prelude-include-module.tea"
>     ,"prelude-local-module.tea"
>     ,"prelude-provide-all.tea"
>     ,"prelude-provide-x.tea"
>     --,"random.tea" -- change in letrec desugaring, possibly bug
>     ,"records.tea"
>     ,"recursive.tea"
>     ,"ref.tea"
>     ,"tour.tea"
>     ,"trivial_is.tea"
>     ,"tuples.tea"
>     ,"two_same_name_check.tea"
>     ,"vars.tea"
>     ,"when.tea"
>     ,"where.tea"
>     ,"curried.tea"
>     ,"simple-anomaly.tea"
>
>     ,"option.tea"
>     ,"either.tea"
>     ]

> tests1 :: T.TestTree
> tests1 = T.testGroup "refactortests1" $
>     map (\f -> makeTestsIO ("refactortests" ++ f)
>             $ runFileTests ("examples/tests/fulltests" </> f))
>     testFiles

> runFileTests :: FilePath -> IO (Either String [CheckResult])
> runFileTests fp = do
>     src <- readFile fp
>     ts <- ev (Just fp) src
>     pure $ Right ts
>   where
>     ev f src = do
>         h <- newTeaberryHandle
>         runScriptWithTests h f [] src

> tests4 :: T.TestTree
> tests4 = T.testGroup "refactortests4"
>     [testSanityArith
>     ,testEnvKept
>     ,testEnvOverridden
>     ,testStorePreserved
>     ,testAScript
>     ,testRunScriptWithValues
>     ,testRunFunctionSimple
>     --,testRunFunctionPartialApp
>     ,testRevisitBuiltins
>     ]

> testSanityArith :: T.TestTree
> testSanityArith = T.testCase "testSanityArith4" $ do
>     h <- newTeaberryHandle
>     v <- runScript h Nothing [] "1 + 2"
>     T.assertEqual "" (NumV 3) v


> testEnvKept :: T.TestTree
> testEnvKept = T.testCase "testEnvKept" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h Nothing [] "a = 1"
>     v <- runScript h Nothing [] "a"
>     T.assertEqual "" (NumV 1) v

> testEnvOverridden :: T.TestTree
> testEnvOverridden = T.testCase "testEnvOverridden" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h Nothing [] "a = 1"
>     _ <- runScript h Nothing [] "a = 4"
>     v <- runScript h Nothing [] "a"
>     T.assertEqual "" (NumV 4) v


> testStorePreserved :: T.TestTree
> testStorePreserved = T.testCase "testStorePreserved" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h Nothing [] "var a = 12"
>     v <- runScript h Nothing [] "a"
>     T.assertEqual "" (NumV 12) v


*****
TODO: test running something with tests and there's a test failure


> testAScript :: T.TestTree
> testAScript = T.testCase "testAScript" $ do
>     let script = "a = 3\nb = 4\na + b"
>     h <- newTeaberryHandle
>     v <- runScript h Nothing [] script
>     T.assertEqual "" (NumV 7) v

todo: lots of boilerplate and abstraction details to work out

> {-testFFI :: T.TestTree
> testFFI = T.testCase "testFFI" $ do
>     let myFFI :: [Value] -> Interpreter Value
>         myFFI = \case
>             [NumV a] = pure $ NumV $ a + 1
>             _ = lift $ throwE $ "expected num"
>     h <- newTeaberryHandle
>     addFFI h ("myfun", myFFI)
>     v <- runScript h "myfun(3)"
>     T.assertEqual "" (Right (NumV 4)) v-}

> testRunScriptWithValues :: T.TestTree
> testRunScriptWithValues = T.testCase "testRunScriptWithValues" $ do
>     h <- newTeaberryHandle
>     v <- runScript h Nothing
>                      [("a", NumV 3)
>                      ,("b", NumV 11)] "a + b"
>     T.assertEqual "" (NumV 14) v

how to implement this without it staying in the env?
todo: fix this, for now, they will stay in the env

when should stuff stay in the env generally?
"a = 1"
a should stay in the env
"a = 1
a + 2"
should a stay in the env? not sure


> testRunFunctionSimple :: T.TestTree
> testRunFunctionSimple = T.testCase "testRunFunctionSimple" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h Nothing [] "f = lam(x,y): x + y end"
>     v <- runFunction h "f" [NumV 5, NumV 11]
>     T.assertEqual "" (NumV 16) v

the next test should work as soon as partial application is
implemented

> {-testRunFunctionPartialApp :: T.TestTree
> testRunFunctionPartialApp = T.testCase "testRunFunctionPartialApp" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h [] "f = lam(x,y): x + y end"
>     v <- runFunction h "f(_,4)" [NumV 5]
>     T.assertEqual "" (Right (NumV 9)) v-}


> testRevisitBuiltins :: T.TestTree
> testRevisitBuiltins = T.testCase "testRevisitBuiltins" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h Nothing [] "a = [list: 1,2]"
>     _ <- runScript h Nothing [] "b = [list: 3,4]"
>     pure ()
>     --T.assertEqual "" (NumV 16) v




> makeTestsIO :: String -> IO (Either String [CheckResult]) -> T.TestTree
> makeTestsIO gnm cs =
>     case unsafePerformIO cs of
>         Left e -> T.testCase gnm $ T.assertFailure e
>         Right csx -> T.testGroup gnm $ map g csx
>   where
>     g (CheckResult hnm ts) = T.testGroup hnm $ map f ts  
>     f (nm,mfm) = T.testCase nm $ case mfm of
>         Nothing -> T.assertBool "" True
>         Just fmx -> T.assertFailure fmx
