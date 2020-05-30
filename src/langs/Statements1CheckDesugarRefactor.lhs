
Simplest version of the code which supports script with check blocks
and closures.

Refactored to have a separate desugar statements function.

TODO: this refactoring should be how it is in all the statements code

one exception: take the current base simple statements with the non
 top level statements handling, and make a copy which still does that

------------------------------------------------------------------------------


> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module Statements1CheckDesugarRefactor (tests
>                                        ) where

> import Text.RawString.QQ

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T


> import qualified Parse as P
> import qualified Syntax as S
> import qualified Pretty as Pr


> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, get, gets, state, put, modify)

> import Control.Monad (when)
> import Data.Maybe (isJust, isNothing)
>
> import Data.Char (isAlphaNum)

> import Scientific (Scientific, extractInt)
> import Data.List (intercalate, partition)
>
> import Debug.Trace (trace)

------------------------------------------------------------------------------

syntax
------

> data Stmt = StExpr Expr
>           | LetDecl String Expr
>           | Check (Maybe String) [Stmt]
>           deriving (Eq, Show)

> data Expr = Num Scientific
>           | Text String
>           | TupleSel [Expr]
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>           | Block [Stmt]
>           | Seq Expr Expr
>           | If Expr Expr Expr
>           deriving (Eq, Show)

------------------------------------------------------------------------------

desugar
-------

> type Desugarer = RWST () () DesugarState (Except String)

> data DesugarState = DesugarState {uniqueCtr :: Int
>                                  ,nextUnusedCheckBlockID :: Int
>                                  ,nextAnonymousBlockNumber :: Int
>                                  ,currentCheckBlockIDName :: Maybe String}

> startingDesugarState :: DesugarState
> startingDesugarState = DesugarState 0 0 0 Nothing

> makeUniqueVar :: String -> Desugarer String
> makeUniqueVar pref = state $ \s ->
>     let suff = uniqueCtr s
>     in (pref ++ "-" ++ show suff
>        ,s {uniqueCtr = suff + 1})


--------------------------------------

testing support in the desugarer monad

> enterNewCheckBlock :: Desugarer (String,Expr)
> enterNewCheckBlock = do
>     s <- get
>     when (isJust $ currentCheckBlockIDName s) $ lift $ throwE "trying to enter nested check block"
>     let newID = nextUnusedCheckBlockID s
>     nm <- makeUniqueVar "check-block-id"
>     put $ s {nextUnusedCheckBlockID = newID + 1
>             ,currentCheckBlockIDName = Just nm}
>     pure (nm,Num $ fromIntegral $ newID)

> exitCheckBlock :: Desugarer ()
> exitCheckBlock = modify $ \s -> s {currentCheckBlockIDName = Nothing}

> getAnonBlockName :: Desugarer String
> getAnonBlockName = state $ \s ->
>     let blockNo = nextAnonymousBlockNumber s
>     in ("check-block-" ++ show blockNo
>        ,s {nextAnonymousBlockNumber = blockNo + 1})

--------------------------------------

desugaring code

> runDesugar :: [Stmt] -> Either String Expr
> runDesugar stmts =
>     fst <$> runExcept (evalRWST (desugarStmts stmts) () startingDesugarState)

> desugar :: Expr -> Desugarer Expr

> desugar (Block sts) = desugarStmts sts

> desugar (Num i) = pure $ Num i
> desugar (Text i) = pure $ Text i
> desugar (TupleSel fs) = TupleSel <$> mapM desugar fs
> desugar (Iden i) = pure $ Iden i


> desugar (App (Iden "is") [a,b]) = do
>     uniqueV0 <- makeUniqueVar "is-v0"
>     uniqueV1 <- makeUniqueVar "is-v1"
>     uniqueName <- makeUniqueVar "testname"
>     checkBlockIDName <- (maybe (lift $ throwE "'is' test outside check block") pure)
>                         =<< (gets currentCheckBlockIDName)
>     desugar $ Block
>               [LetDecl uniqueV0 a
>               ,LetDecl uniqueV1 b
>               ,LetDecl uniqueName (Text $ pretty a ++ " is " ++ pretty b)
>               ,StExpr $
>                If (eqIdens uniqueV0 uniqueV1)
>                   (appI "log-test-pass" [Iden checkBlockIDName
>                                         ,Iden uniqueName])
>                   (appI "log-test-fail"
>                    [Iden checkBlockIDName
>                    ,Iden uniqueName
>                    ,Text "Values not equal:\n"
>                        `plus` appI "torepr" [Iden uniqueV0]
>                        `plus` Text "\n"
>                        `plus` appI "torepr" [Iden uniqueV1]])]
>   where
>     plus c d = appI "+" [c, d]
>     eqIdens c d = appI "==" [Iden c, Iden d]
>     appI i es = App (Iden i) es

> desugar (App f as) = App <$> desugar f <*> mapM desugar as
> desugar (Lam ns e) = Lam ns <$> desugar e
> desugar (Let bs e) = do
>     let f (n,v) = (n,) <$> desugar v
>     Let <$> mapM f bs <*> desugar e

shouldn't be hit?

> desugar (Seq a b) =
>     Seq <$> desugar a <*> desugar b

> desugar (If c t e) =
>     If <$> desugar c <*> desugar t <*> desugar e


> desugarStmts :: [Stmt] -> Desugarer Expr

> desugarStmts (Check nm bdy : es) = do
>     (uniqueCheckBlockIDVarName,uniqueCheckBlockID) <- enterNewCheckBlock
>     blockName <- maybe getAnonBlockName pure nm
>     desugaredCheck <- desugar $
>         appI "add-tests"
>         [Lam [] (Block $
>                  [LetDecl uniqueCheckBlockIDVarName uniqueCheckBlockID
>                  ,StExpr $ appI "log-check-block"
>                              [Iden uniqueCheckBlockIDVarName
>                              ,Text blockName]]
>                  ++ bdy)]
>     exitCheckBlock
>     case es of
>         [] -> pure desugaredCheck
>         _ -> Seq desugaredCheck <$> desugarStmts es
>   where
>     appI i as = App (Iden i) as

> desugarStmts [] = lift $ throwE $ "empty block"
> desugarStmts [StExpr e] = desugar e
> desugarStmts [LetDecl {}] = lift $ throwE $ "block ends with let"

> desugarStmts (LetDecl n v : es) = do
>     v' <- desugar v
>     Let [(n,v')] <$> desugarStmts es

> desugarStmts (StExpr e : es) =
>     Seq <$> desugar e <*> desugarStmts es


------------------------------------------------------------------------------

values
------

> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | TupleV [Value]
>            | NothingV
>            | FunV [String] Expr Env
>            | ForeignFunV String

> valueTypeName :: Value -> String
> valueTypeName (NumV {}) = "number"
> valueTypeName (TextV {}) = "text"
> valueTypeName (BoolV {}) = "boolean"
> valueTypeName (TupleV {}) = "tuple"
> valueTypeName (FunV {}) = "function"
> valueTypeName (NothingV) = "nothing"
> valueTypeName (ForeignFunV {}) = "foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (TupleV fs) = "TupleV [" ++ intercalate "," (map show fs) ++ "]"
>   show NothingV = "NothingV"
>   show (FunV {}) = "FunV stuff"
>   show (ForeignFunV n) = "ForeignFunV " ++ show n

> instance Eq Value where
>     NumV a == NumV b = a == b
>     TextV a == TextV b = a == b
>     BoolV a == BoolV b = a == b
>     TupleV fs == TupleV gs = fs == gs
>     NothingV == NothingV = True
>     _ == _ = False

------------------------------------------------------------------------------

> data InterpreterState =
>     InterpreterState
>     {addedTests :: [Value]
>     ,testResultLog :: [TestResultLog]
>     }

> emptyInterpreterState :: InterpreterState
> emptyInterpreterState = InterpreterState [] []

> type Interpreter = RWST Env () InterpreterState (Except String)

> runInterp :: Env -> Expr -> Either String [CheckResult]
> runInterp env expr =
>     fst <$> runExcept (evalRWST scriptWithTestStuff env emptyInterpreterState)
>   where
>     scriptWithTestStuff = do
>         _ <- interp expr
>         runAddedTests

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Text t) = pure (TextV t)
> interp (TupleSel es) = TupleV <$> mapM interp es
> interp (Iden i) = do
>     env <- ask
>     envLookup i env
> interp (App f es) = do
>     fv <- interp f
>     vs <- mapM interp es
>     app fv vs
> interp (Lam ps e) = do
>     env <- ask
>     pure $ FunV ps e env
> interp (Let bs e) = do
>     let newEnv [] = interp e
>         newEnv ((b,ex):bs') = do
>             v <- interp ex
>             local (extendEnv [(b,v)]) $ newEnv bs'
>     newEnv bs

> interp (Block {}) = lift $ throwE $ "undesugared block passed to interpreter"
> interp (Seq a b) = interp a *> interp b

> interp (If c t e) = do
>     c' <- interp c
>     case c' of
>         BoolV True -> interp t
>         BoolV False -> interp e
>         _ -> lift $ throwE $ "expected bool in if test, got " ++ show c'

> app :: Value -> [Value] -> Interpreter Value
> app fv vs = do
>     case fv of
>         FunV ps bdy env' -> do
>             as <- safeZip ps vs
>             let env'' = extendEnv as env'
>             local (const env'') $ interp bdy
>         ForeignFunV nm -> do
>             let tys = map valueTypeName vs
>             env <- ask
>             hf <- lookupForeignFun nm tys env
>             hf vs
>         _ -> lift $ throwE "non function value in app position"
>   where
>     safeZip ps xs | length xs == length ps = pure $ zip ps xs
>                   | otherwise = lift $ throwE $ "wrong number of args to function"


> evaluateWithChecks :: String -> Either String [CheckResult]
> evaluateWithChecks s =  do
>     ast <- parse s
>     ast' <- runDesugar ast
>     runInterp testEnv ast'

------------------------------------------------------------------------------

haskell side testing infrastructure
-----------------------------------

checkresult:

this is the user api. Is this as simple and direct as possible?

todo: will extend the name with the package and module as well
want to be able to do a filter on which tests to run
maybe the filter is just a predicate on the package, module and check
block name

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
>                  deriving Show
> -- the second is Nothing if it's a pass
> -- if it is Just, it's a fail, and contains the failure message

--------------------------------------

ffi functions used by the desugared test code

TODO: use/write helpers for working with state monad, etc.
create a wrapper to convert an Interpreter () into NothingV

> data TestResultLog = TestPass Scientific String -- check block id, test source
>                    | TestFail Scientific String String -- check block id, test source, failure message
>                    | TestBlock Scientific String

add-tests(fn)
takes a function value
these functions are run at the end of the script to do the testing

> addTests :: Value -> Interpreter Value
> addTests v = nothingWrapper $ \s -> s {addedTests = v : addedTests s}

log-check-block(unique-block-id, name)
says there is a new test block with a unique id and it's name

> logCheckBlock :: Scientific -> String -> Interpreter Value
> logCheckBlock n nm =
>     nothingWrapper $ \s -> s {testResultLog = TestBlock n nm : testResultLog s}

log-test-pass(blockid, text of test)

> logTestPass :: Scientific -> String -> Interpreter Value
> logTestPass n msg =
>     nothingWrapper $ \s -> s {testResultLog = TestPass n msg : testResultLog s}

log-test-fail(block,id, text of test, fail message)

> logTestFail :: Scientific -> String -> String -> Interpreter Value
> logTestFail n msg failmsg = nothingWrapper $ \s ->
>     s {testResultLog = TestFail n msg failmsg : testResultLog s}

> nothingWrapper :: (InterpreterState -> InterpreterState) -> Interpreter Value
> nothingWrapper f = modify f *> pure NothingV

--------------------------------------

this is run at the end of the script to run all the saved tests

> runAddedTests :: Interpreter [CheckResult]
> runAddedTests = do
>     ts <- reverse <$> gets addedTests
>     mapM_ (\v -> app v []) ts
>     testLog <- reverse <$> gets testResultLog
>     either (lift . throwE) pure $ testLogToCheckResults testLog

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

------------------------------------------------------------------------------

ffi catalog


> testEnv :: Env
> testEnv = either error id $ addForeignFuns' (
>    [("+", binaryOp unwrapNum unwrapNum wrapNum (+))
>    ,("*", binaryOp unwrapNum unwrapNum wrapNum (*))
>    ,("+", binaryOp unwrapText unwrapText wrapText (++))
>    ,("==", binaryOp unwrapNum unwrapNum wrapBool (==))
>    ,("==", binaryOp unwrapText unwrapText wrapBool (==))
>    ,("==", binaryOp unwrapTuple unwrapTuple wrapBool (==))

>    ,("add-tests", unarySimple "function" addTests)
>    ,("log-check-block", binaryV unwrapNum unwrapText logCheckBlock)
>    ,("log-test-pass", binaryV unwrapNum unwrapText logTestPass)
>    ,("log-test-fail", ternaryV unwrapNum unwrapText unwrapText logTestFail)

>    ]
>     ++ [("torepr", unarySimple x torepr) | x <- ["number", "text", "boolean", "tuple", "function", "foreign-function"]]
>     )

>    emptyEnv

> torepr :: Value -> Interpreter Value
> torepr x = pure $ TextV $ torepr' x

> torepr' :: Value -> String
> torepr' (NumV n) = case extractInt n of
>                              Just x -> show x
>                              Nothing ->  show n
> torepr' (BoolV n) = if n then "true" else "false"
> torepr' (FunV {}) = "<Function>"
> torepr' (ForeignFunV {}) = "<Function>"
> torepr' (TextV s) = "\"" ++ s ++ "\""
> torepr' (TupleV fs) =
>     "{" ++ intercalate ";" (map torepr' fs) ++ "}"

> torepr' NothingV = "nothing"



------------------------------------------------------------------------------

env, ffi boilerplate
--------------------

> data Env = Env
>     {envEnv :: [(String,Value)]
>     ,envForeignFuns :: [((String,[String]), [Value] -> Interpreter Value)]}

> emptyEnv :: Env
> emptyEnv = Env
>     {envEnv = [("true", BoolV True)
>               ,("false", BoolV False)]
>     ,envForeignFuns = []}

> extendEnv :: [(String,Value)] -> Env -> Env
> extendEnv bs env = env {envEnv = bs ++ envEnv env}
>
> envLookup :: String -> Env -> Interpreter Value
> envLookup nm env =
>     maybe (lift $ throwE $ "Identifier not found " ++ nm) pure
>     $ lookup nm (envEnv env)

> addForeignFun :: String -> [String] -> ([Value] -> Interpreter Value) -> Env -> Either String Env
> addForeignFun nm tys f env =
>     pure ((extendEnv [(nm, ForeignFunV nm)] env)
>          {envForeignFuns = ((nm,tys), f) : envForeignFuns env})

> lookupForeignFun :: String -> [String] -> Env -> Interpreter ([Value] -> Interpreter Value)
> lookupForeignFun nm tys env =
>     maybe (lift $ throwE $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")")
>       pure $ lookup (nm,tys) $ envForeignFuns env



> addForeignFun' :: String -> ([String], ([Value] -> Interpreter Value)) -> Env -> Either String Env
> addForeignFun' nm (tys, f) env = addForeignFun nm tys f env

> addForeignFuns' :: [(String, ([String], ([Value] -> Interpreter Value)))] -> Env -> Either String Env
> addForeignFuns' [] env = pure env
> addForeignFuns' ((x,y):xs) env = do
>     env' <- addForeignFun' x y env
>     addForeignFuns' xs env'
>     

> unarySimple :: String -> (Value -> Interpreter Value) -> ([String], [Value] -> Interpreter Value)
> unarySimple ty f = ([ty]
>                  ,\case
>                        [x] -> f x
>                        y -> lift $ throwE $ "expected 1 arg, got " ++ show y)
>                  

> unwrapTuple :: (String, Value -> Interpreter [Value])
> unwrapTuple = ("tuple", \case
>                           TupleV fs -> pure fs
>                           x -> lift $ throwE $ "type: expected tuple, got " ++ show x)

> unwrapNum :: (String, Value -> Interpreter Scientific)
> unwrapNum = ("number", \case
>                           NumV n -> pure n
>                           x -> lift $ throwE $ "type: expected number, got " ++ show x)

> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n



> _unwrapBool :: (String, Value -> Interpreter Bool)
> _unwrapBool = ("boolean", \case
>                           BoolV n -> pure n
>                           x -> lift $ throwE $ "type: expected boolean, got " ++ show x)

> wrapBool :: Bool -> Interpreter Value
> wrapBool n = pure $ BoolV n


> unwrapText :: (String, Value -> Interpreter String)
> unwrapText = ("text", \case
>                           TextV n -> pure n
>                           x -> lift $ throwE $ "type: expected text, got " ++ show x)

> wrapText :: String -> Interpreter Value
> wrapText n = pure $ TextV n

> _unaryOp :: (String, Value -> Interpreter a)
>         -> (a -> Interpreter Value)
>         -> (a -> a)
>         -> ([String], ([Value] -> Interpreter Value))
> _unaryOp unwrap0 wrap f =
>     ([fst unwrap0]
>     ,\as -> do
>             case as of
>                 [a] -> do
>                     ax <- (snd unwrap0) a
>                     wrap (f ax)
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 1, got " ++ show (length as))



> binaryOp :: (String, Value -> Interpreter a)
>          -> (String, Value -> Interpreter a)
>          -> (b -> Interpreter Value)
>          -> (a -> a -> b)
>          -> ([String], ([Value] -> Interpreter Value))
> binaryOp unwrap0 unwrap1 wrap f =
>     ([fst unwrap0, fst unwrap1]
>     ,\as -> do
>             case as of
>                 [a,b] -> do
>                     ax <- (snd unwrap0) a
>                     bx <- (snd unwrap1) b
>                     wrap (f ax bx)
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 2, got " ++ show (length as))

> binaryV :: (String, Value -> Interpreter a)
>         -> (String, Value -> Interpreter b)
>         -> (a -> b -> Interpreter Value)
>         -> ([String], ([Value] -> Interpreter Value))
> binaryV unwrap0 unwrap1 f =
>     ([fst unwrap0, fst unwrap1]
>     ,\as -> do
>             case as of
>                 [a,b] -> do
>                     ax <- (snd unwrap0) a
>                     bx <- (snd unwrap1) b
>                     f ax bx
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 2, got " ++ show (length as))


> ternaryV :: (String, Value -> Interpreter a)
>          -> (String, Value -> Interpreter b)
>          -> (String, Value -> Interpreter c)
>          -> (a -> b -> c -> Interpreter Value)
>          -> ([String], ([Value] -> Interpreter Value))
> ternaryV unwrap0 unwrap1 unwrap2 f =
>     ([fst unwrap0, fst unwrap1, fst unwrap2]
>     ,\as -> do
>             case as of
>                 [a,b,c] -> do
>                     ax <- (snd unwrap0) a
>                     bx <- (snd unwrap1) b
>                     cx <- (snd unwrap2) c
>                     f ax bx cx
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 3, got " ++ show (length as))


------------------------------------------------------------------------------

parse
-----

a rule is to try not to do any 'desugaring' here.

It's supposed to be used as if we wrote the parser for each version
from scratch, so the result should be what such a parser would
produce. It's slightly ambiguous because this entire 'parser' is more
like a desugaring process.


> parse :: String -> Either String [Stmt]
> parse src =
>     case P.parseProgram "" src of
>       Right (S.Program [] sts) -> convStmts sts
>       Right (S.Program x _) -> Left $ "prelude not supported " ++ show x
>       Left e -> Left e

> convStmts :: [S.Stmt] -> Either String [Stmt]
> convStmts = mapM convSt

> convExpr :: S.Expr -> Either String Expr
> convExpr (S.Sel (S.Num x)) = Right $ Num x
> convExpr (S.Sel (S.Str x)) = Right $ Text x
> convExpr (S.Sel (S.Tuple fs)) = TupleSel <$> mapM convExpr fs
> convExpr (S.Iden s) = Right $ Iden s
> convExpr (S.Parens e) = convExpr e
> convExpr (S.App f es) = App <$> (convExpr f) <*> mapM convExpr es
> convExpr (S.BinOp e f e1) = do
>     a <- convExpr e
>     b <- convExpr e1
>     pure $ App (Iden f) [a,b]
> convExpr (S.Lam ps e) = do
>         ps' <- mapM pf ps
>         e' <- convExpr e
>         Right $ Lam ps' e'
>       where
>         pf (S.IdenP _ (S.PatName x)) = Right x
>         pf x = Left $ "unsupported pattern " ++ show x
> convExpr (S.Let bs e) = do
>         bs' <- mapM bf bs
>         e' <- convExpr e
>         Right $ Let bs' e'
>       where
>         bf (S.Binding (S.IdenP _ (S.PatName x)) ex) =
>             (x,) <$> convExpr ex
>         bf x = Left $ "unsupported binding " ++ show x

> convExpr (S.Block sts) = Block <$> mapM convSt sts

> convExpr (S.If [(c,t)] (Just e)) = do
>     If <$> convExpr c <*> convExpr t <*> convExpr e

> convExpr x = Left $ "unsupported expression " ++ show x

> convSt :: S.Stmt -> Either String Stmt
> convSt (S.StExpr e) = StExpr <$> convExpr e
> convSt (S.LetDecl (S.Binding (S.IdenP _ (S.PatName nm)) v)) = LetDecl nm <$> convExpr v
> convSt (S.Check nm bdy) = Check nm <$> mapM convSt bdy

> convSt x = Left $ "unsupported statement " ++ show x

------------------------------------------------------------------------------

pretty
------

> pretty :: Expr -> String
> pretty x = Pr.prettyExpr $ unconv x


> unconv :: Expr -> S.Expr
> unconv (Num n) = S.Sel (S.Num n)
> unconv (Text n) = S.Sel (S.Str n)
> unconv (TupleSel fs) = S.Sel (S.Tuple $ map unconv fs)
> unconv (Iden s) = S.Iden s
>
> unconv (App (Iden e) [a,b]) | isOp e = S.BinOp (unconv a) e (unconv b)
>   where isOp x = not $ any (\z -> isAlphaNum z || z `elem` "_") x
> 
> unconv (App e fs) = S.App (unconv e) $ map unconv fs
> unconv (Lam ns e) = S.Lam (map unconvPattern ns) $ unconv e
> unconv (Let bs e) = S.Let (map (uncurry unconvBinding) bs) (unconv e)
> unconv (Block sts) = S.Block $ map unconvStmt sts
> unconv (Seq a b) = S.Block $ map unconvStmt [StExpr a, StExpr b]
> unconv (If c t e) = S.If [(unconv c, unconv t)] (Just $ unconv e)

> unconvStmt :: Stmt -> S.Stmt
> unconvStmt (LetDecl n e) = S.LetDecl (unconvBinding n e)
> unconvStmt (StExpr e) = S.StExpr (unconv e)
> unconvStmt (Check nm bs) = S.Check nm $ map unconvStmt bs

> unconvBinding :: String -> Expr -> S.Binding
> unconvBinding n v = S.Binding (unconvPattern n) (unconv v)

> unconvPattern ::String -> S.Pat
> unconvPattern n = S.IdenP S.NoShadow (S.PatName n)

------------------------------------------------------------------------------

tests
-----

> simpleTestScript :: String
> simpleTestScript = [r|
\begin{code} 

check "basic tests":
  1 is 1
  1 + 2 is 3

  let x = 3:
    x
  end is 3

  lam(x,y):
    x + y
  end(1,2) is 3

  let shadow f = lam(x,y): x + y end:
    f(1,2)
  end is 3

  let shadow f = lam(x): lam(y): x + y end end,
      g = f(2):
      g(3)
  end is 5

end


check "non basic expression stuff":
  'three' is 'three'
  {1 + 1; 2 + 3} is {2; 5}
  if true: 1 else: 2 end is 1
end

#check "user ffi functions":
  # + on strings
  # == on each kind of value
  # torepr on each kind of value
  # the values are: num, bool, text, tuple
  # (plus functions and foreign functions, but they are excluded)
#end

shadow f = lam(x): x * x end

check "closure outside block":
  f(5) is 25
end

check "simple block stuff":
  block:
    x = 3
    x is 3
  end

  block:
    3 + 4
    4 + 6
  end is 10

  block:
    x = block:
          3 + 4
          4 + 6
        end
    x + 2
  end is 12

  # show the binding being shadowed properly
  block:
    x = 1
    y = block:
      shadow x = 2
         x + 2
        end
    {x;y}
  end is {1;4}

end

\end{code}
>    |]


      
> tests :: T.TestTree
> tests =
>     let crs = either error id $ evaluateWithChecks simpleTestScript
>         f (CheckResult nm res) =
>             T.testGroup nm $ map g res
>         g (nm, fm) = T.testCase nm $
>             case fm of
>                 Nothing -> T.assertBool "" True
>                 Just fmx -> T.assertFailure fmx
>     in trace (renderCheckResults crs)
>        $ T.testGroup "simplestatementscheck" $ map f crs
