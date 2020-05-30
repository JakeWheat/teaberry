
> module TestUtils (T.TestTree
>                  ,T.testGroup
>                  ,T.testCase
>                  ,T.assertEqual
>                  ,T.assertBool
>                  ,T.assertFailure
>                  ,CheckResult (..)
>                  ,TestResultLog(..)
>                  ,testLogToCheckResults
>                  ,renderCheckResults
>                  ,makeTests
>                  ,makeTestsIO
>                  ) where

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T
> import Control.Monad (when)
> import Data.List (partition, intercalate)
> import Data.Maybe (isNothing)

> --import Debug.Trace (trace)

> import Scientific (Scientific)

> -- used for creating the test lists for tasty to run
> import System.IO.Unsafe (unsafePerformIO)


test logging

> data TestResultLog = TestPass Scientific String -- check block id, test source
>                    | TestFail Scientific String String -- check block id, test source, failure message
>                    | TestBlock Scientific String

this is run after that, to convert the log of test events, into a
organised data structure to view the results

> testLogToCheckResults :: [TestResultLog] -> Either String [CheckResult]
> testLogToCheckResults trls = do
>     -- 'streaming' algorithm, very efficient
>     -- but the reason to do it is because it's an easy
>     -- way to catch a test result without a matching check block
>     -- relies on the test results being in the order you write
>     -- the corresponding tests in the source file
>     let f :: [(Scientific, CheckResult)] -> [TestResultLog] -> Either String [CheckResult]
>         -- finished, reverse the check results
>         f res [] = pure $ map reverseResults $ map snd $ reverse res
>         -- new check block, make sure it's id hasn't been seen already
>         f res ((TestBlock cid nm) : xs) = do
>             when (any (== cid) $ map fst res)
>                 $ Left $ "multiple check result name for check block with id " ++ show cid
>             f ((cid, CheckResult nm []) : res) xs
>         -- if we see a pass or a fail without having see a testblock
>         -- then it's a bug
>         f [] (TestPass {} : _) = Left $ "unmatched check block id in test result"
>         f [] (TestFail {} : _) = Left $ "unmatched check block id in test result"
>         -- pass or fail, the id should match the id of the last testblock entry
>         -- not sure if this extra checking is really needed
>         -- maybe it will be useful if try to run tests in multiple threads
>         -- in the future?
>         f ((cid, CheckResult cnm ts):cs) ((TestPass tid tnm) : xs)
>             | cid == tid =
>               f ((cid,CheckResult cnm ((tnm, Nothing):ts)):cs) xs
>             | otherwise = Left $ "unmatched check block id in test result"
>         f ((cid, CheckResult cnm ts):cs) ((TestFail tid tnm fmsg) : xs)
>             | cid == tid =
>               f ((cid,CheckResult cnm ((tnm, Just fmsg):ts)):cs) xs
>             | otherwise = Left $ "unmatched check block id in test result"
>     f [] trls
>   where
>     reverseResults (CheckResult nm ts) = CheckResult nm $ reverse ts

function to convert the check result data structure into a nicely
formatted string

> renderCheckResults :: [CheckResult] -> String
> renderCheckResults cs =
>     let bs = map renderCheck cs
>         totalPasses = sum $ map (\(n,_,_) -> n) bs
>         totalFails = sum $ map (\(_,n,_) -> n) bs
>         msgs = map (\(_,_,m) -> m) bs
>     in intercalate "\n\n" msgs
>        ++ "\n\n" ++ (show totalPasses) ++ "/" ++ show (totalPasses + totalFails)
>        ++ " tests passed in all check blocks"
>   where
>     renderCheck (CheckResult nm ts) =
>         let (ps,fs) = partition (isNothing . snd) ts
>             msgs = map renderTest ts
>         in (length ps
>            ,length fs
>            ,"Check block: " ++ nm ++ "\n"
>            ++ intercalate "\n" (map indent msgs)
>            ++ "\n  " ++ show (length ps) ++ "/" ++ show (length ts) ++ " tests passed in check block: " ++ nm
>            )
>     renderTest (a,b) =
>         "test (" ++ a ++ "): "
>         ++ case b of
>                Nothing -> "OK"
>                Just msg -> "failed, reason:\n" ++ indent msg
>     indent [] = []
>     indent x = "  " ++ indent' x
>     indent' [] = []
>     indent' x@[_] = x
>     indent' ('\n':y@(_:_)) = "\n  " ++ indent' y
>     indent' (x:y) = x : indent' y


test results

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
>                  deriving Show

convert tests to hunit tests

> makeTests :: String -> [CheckResult] -> T.TestTree
> makeTests gnm cs = --trace (renderCheckResults cs) $
>                    T.testGroup gnm $ map g cs
>   where
>     g (CheckResult hnm ts) = T.testGroup hnm $ map f ts  
>     f (nm,mfm) = T.testCase nm $ case mfm of
>         Nothing -> T.assertBool "" True
>         Just fmx -> T.assertFailure fmx

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

