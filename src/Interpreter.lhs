
Key aspects of the interpreter:

The interpreter monads is rws combined with IO.

Control.Exception.Safe is used for errors - all errors are caught and
returned as a left when interpreting. this is used for the language
errors/raise implementation too

The reader value contains the read only bindings.

The state value contains the store for read/write values, and logs the
callbacks for the tests (which are gathered during script execution,
and then executed at the end).

The test results are put to the writer. No particular reason other
than it's there, maybe it would be more legit to put them in the
state.

After the interpretation finishes, the test log is turned into test
results for the user api.

The code is a little confused about variables and boxes right
now. This will be fixed when agdt then refs are implemented so users
can use both variables and explicit boxes.

The system comes with a small standard lib (really small) implemented
using a hack sort of ffi for haskell.


> {-# LANGUAGE ScopedTypeVariables,TupleSections #-}
> module Interpreter (runProgram
>                    ,CheckResult(..)
>                    ,runChecks
>                    ,Value(..)
>                    ,extractInt
>                    ) where

> import Control.Exception.Safe (Exception, throwM, catch)
> import Control.Monad (void, forM_{-, when-})
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.RWS (RWST, runRWST, ask, get, {-put,-} local, {-tell,-} state)
> import Data.List (partition, intercalate)
> import Data.Scientific (Scientific)
> import Text.Show.Pretty (ppShow)

> import qualified InterpreterSyntax as I
> import Syntax (extractInt)

------------------------------------------------------------------------------

= values

> data Value = NumV Scientific
>            | BoolV Bool
>            | StrV String
>            | ClosV I.Expr Env
>            | VariantV String -- ctor name
>                       [(String,Value)] -- fields, is it better to not include the names,
>                                        -- the desugarer will handle?
>            | BoxV Int
>            | VoidV
>            deriving (Eq,Show)

------------------------------------------------------------------------------

= data types and support for the interpreter monad

> type Interpreter = RWST InterpreterReader [()] InterpreterState IO


quick exceptions, can come back to this

> data MyException = MyException String
>     deriving Show

> instance Exception MyException


the env is used for read only bindings. the code distinguishes between
these and variables, not sure there is a good reason to do this

> data Env = Env [(String,Value)]
>            deriving (Eq,Show)

> emptyEnv :: Env
> emptyEnv = Env []

> extendEnv :: String -> Value -> Env -> Env
> extendEnv n v (Env e) = Env ((n,v):e)

> extendsEnv :: [(String,Value)] -> Env -> Env
> extendsEnv ns (Env e) = Env $ foldr (:) e ns

> lookupEnv :: String -> Env -> Maybe Value
> lookupEnv nm (Env e) = lookup nm e

the store holds variable values

> data Store = Store [(Int, Value)]

> emptyStore :: Store
> emptyStore = Store []

> newStoreLoc :: Store -> Int
> newStoreLoc (Store xs) = 
>     let is = map fst xs
>     in case is of
>            [] -> 0
>            _ -> maximum is + 1

> extendStore :: Int -> Value -> Store -> Store
> extendStore i v (Store xs) = Store ((i,v):xs)

> fetchStore :: Int -> Store -> Interpreter Value
> fetchStore i (Store xs) = maybe (throwM $ MyException $ "invalid fetch on store: " ++ show i) pure
>                           $ lookup i xs

> data InterpreterReader = InterpreterReader
>     {irEnv :: Env}
> -- todo: use lenses?
> updateIREnv :: (Env -> Env) -> (InterpreterReader -> InterpreterReader)
> updateIREnv f i = i {irEnv = f (irEnv i)}

> 
> data InterpreterState = InterpreterState
>     {isStore :: Store
>     ,isTestsToRun :: [Value]
>     ,isSavedTestResults :: [TestResultLog]
>     }
> updateISStore :: (Store -> Store) -> (InterpreterState -> InterpreterState)
> updateISStore f i = i {isStore = f (isStore i)}

> appendTestResults :: TestResultLog -> Interpreter ()
> appendTestResults lg =
>     state $ \s ->
>       ((), s {isSavedTestResults = lg : isSavedTestResults s})


> defaultInterpreterState :: InterpreterState
> defaultInterpreterState = InterpreterState emptyStore [] []

todo: create helper wrapper functions for the usual operations with
 the read value and the state value stuff
(i.e. a wrapper for local + updateIRenv)

------------------------------------------------------------------------------

= interpreter function

wrappers

> runProgram :: I.Program -> IO (Either String Value)
> runProgram p = do
>     x <- interpWrap False p
>     pure $ fmap fst x

> runChecks :: I.Program -> IO (Either String [CheckResult])
> runChecks p = do
>     x <- interpWrap True p
>     pure $ fmap snd x

the main interpreter function

todo:

this function should have the option of returning [Value] for the
 values of the top level statements for testing
in addition, it shouldn't return Value for the last value ever
and it should write these values to stdout (with a flag to suppress this?)
writing them out out matches the behaviour of pyret
a program doesn't have a value (in this way, at least)

> interpWrap :: Bool -> I.Program -> IO (Either String (Value, [CheckResult]))
> interpWrap runChks (I.Program ex) = (do
>     (result, _store, _lg) <-
>         runRWST (do
>                  x <- interp ex
>                  cr <- if runChks
>                        then do
>                            lg <- runSavedTests
>                            getCheckResults lg
>                        else pure []
>                  pure (x, cr)
>                 )
>         (InterpreterReader defaultEnv)
>         defaultInterpreterState
>     pure $ pure $ result
>     ) `catch` (\(MyException s) -> pure $ Left $ s)

--------------------------------------

interpreters for syntax nodes

> interp :: I.Expr -> Interpreter Value
> interp (I.Sel (I.Num n)) = pure $ NumV n
> interp (I.Sel (I.Str s)) = pure $ StrV s
> interp (I.Sel I.VoidS) = pure $ VoidV
> interp (I.Sel (I.Variant nm es)) = do
>     vs <- mapM (\(n,e) -> (n,) <$> interp e) es
>     pure $ VariantV nm vs

> interp (I.Iden e) = do
>     rd <- ask
>     v <- maybe (throwM $ MyException $ "Identifier not found: " ++ e)
>         pure $ lookupEnv e (irEnv rd)
>     case v of
>         BoxV i -> do
>                   st <- get
>                   fetchStore i (isStore st)
>         _ -> pure v
>   
> interp _x@(I.If c t e) = do
>    c' <- interp c
>    case c' of
>        BoolV True -> interp t
>        BoolV False -> interp e
>        _ -> throwM $ MyException $ "expected bool in if test, got " ++ show c'

the AppHaskell is only used in the default ffi env, the desugarer never produces
it

> interp (I.AppHaskell nm exps) = do
>     f <- maybe (throwM $ MyException $ "ffi fn not found: " ++ nm) pure $ lookup nm haskellFunImpls
>     vs <- mapM interp exps
>     f vs

> interp e@(I.Lam {}) = do
>     env <- irEnv <$> ask
>     pure $ ClosV e env
> interp e@(I.LamVoid {}) = do
>     env <- irEnv <$> ask
>     pure $ ClosV e env


> interp (I.App f a) = do
>     x <- interp f
>     case x of
>         ClosV (I.Lam n bdy) env' -> do
>              argVal <- interp a
>              local (updateIREnv (const $ extendEnv n argVal env')) $ interp bdy
>         ClosV (I.LamVoid bdy) env' -> do
>              case a of
>                  I.Sel I.VoidS -> local (updateIREnv (const env')) $ interp bdy
>                  _ -> throwM $ MyException $ "0 arg lambda called with something other than literal void: " ++ show a
>         ClosV ee _ -> throwM $ MyException $ "non lambda in closure expression: " ++ show ee
>         _ -> throwM $ MyException $ "non function in app position: " ++ show x

> interp (I.Let nm v bdy) = do
>     v' <- interp v
>     case nm of
>         "_" -> interp bdy
>         _ -> local (updateIREnv (extendEnv nm v')) $ interp bdy

> interp (I.Seq (I.LetDecl nm e) b) = do
>     v <- interp e
>     local (updateIREnv (extendEnv nm v)) $ interp b
> interp (I.Seq a b) = interp a >> interp b

> interp (I.Box e) = do
>     v <- interp e
>     i <- state $ \s ->
>          let i = newStoreLoc (isStore s)
>          in (i, updateISStore (extendStore i v) s)
>     pure $ BoxV i

> interp (I.SetBox b v) = do
>     b' <- do
>           env <- irEnv <$> ask
>           maybe (throwM $ MyException $ "Identifier not found: " ++ b)
>               pure $ lookupEnv b env
>     v' <- interp v
>     i <- case b' of
>              BoxV i -> pure i
>              _ -> throwM $ MyException $ "attemped to setbox non box value: " ++ show b'
>     state $ \s ->
>         ((), updateISStore (extendStore i v') s)
>     pure v'

> interp x = error $ "Interpreter: interp " ++ show x




------------------------------------------------------------------------------

= Testing

> data TestResultLog = TestPass Scientific String -- check block id, test source
>                    | TestFail Scientific String String -- check block id, test source, failure message
>                    | TestBlock Scientific String

testing support functions - these are used in the desugaring of tests,
they are in language functions

> logTestPass :: [Value] -> Interpreter Value
> logTestPass = \[NumV n, StrV t] -> do
>     appendTestResults (TestPass n t)
>     pure VoidV

> logTestFail :: [Value] -> Interpreter Value
> logTestFail = \[NumV n, StrV t, StrV m] -> do
>     appendTestResults (TestFail n t m)
>     pure VoidV

> logCheckBlock :: [Value] -> Interpreter Value
> logCheckBlock = \[NumV i, StrV s] -> do
>     appendTestResults (TestBlock i s)
>     pure VoidV

> addTests :: [Value] -> Interpreter Value
> addTests = \[t@(ClosV (I.LamVoid _) _)] -> do
>     -- save the test so it can be run at the end
>     -- is it worth optimising this out if the tests aren't going to be run?
>     appendTest t
>     pure VoidV
>   where
>     appendTest :: Value -> Interpreter ()
>     appendTest t = state $ \s ->
>         ((), s {isTestsToRun = t : isTestsToRun s})


this is called at the end of a script if the tests are enabled

> -- runs all the tests saved in the run
> runSavedTests :: Interpreter [TestResultLog]
> runSavedTests = do
>     ts <- (reverse . isTestsToRun) <$> get
>     forM_ ts $ \(ClosV (I.LamVoid bdy) env') ->
>         void $ local (updateIREnv (const env')) $ interp bdy
>     isSavedTestResults <$> get
>     

the data type which is part of the user api for testing for now

the plan is to make this all more in-language-y once agdt are
supported

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
> -- the second is just if it is a fail, it contains the failure
> -- message

> getCheckResults :: [TestResultLog] -> Interpreter [CheckResult]
> getCheckResults lg = do
>         let (blocknmsx, testresults) = partition isTestBlock lg
>             blocknms :: [(Scientific, String)]
>             blocknms = map (\(TestBlock nm tid) -> (nm, tid)) blocknmsx
>         gs <- mapM (\x -> (,) <$> blockID x <*> toCheckResult x) testresults
>         let  gs' = partitionN gs
>         ts <- mapM (\(tid,nm) -> case lookup tid blocknms of
>                                        Nothing -> throwM $ MyException $ "internal error block id"
>                                        Just b -> pure $ CheckResult b nm) gs'
>         pure ts
>   where
>     isTestBlock (TestBlock {}) = True
>     isTestBlock _ = False
>     blockID (TestPass x _) = pure x
>     blockID (TestFail x _ _) = pure x
>     blockID (TestBlock {}) = throwM $ MyException $ "Interpreter: testblock in wrong place"
>     toCheckResult (TestPass _ x) = pure (x,Nothing)
>     toCheckResult (TestFail _ x m) = pure (x,Just m)
>     toCheckResult (TestBlock {}) = throwM $ MyException $ "Interpreter: testblock in wrong place b"

> partitionN :: Eq a => [(a,b)] -> [(a,[b])]
> partitionN [] = []
> partitionN vs@((k,_):_) =
>     let (x,y) = partition ((==k) . fst) vs
>     in (k,map snd x) : partitionN y


------------------------------------------------------------------------------

= haskell native functions

a bit hacky at the moment, being used to bootstrap the system before
some of these are written in language and others using a proper ffi
system

todo: make this better: do better wrapping + error messages when the
type is wrong
haskellfunimpls and default env duplicates a bunch of stuff

> haskellFunImpls :: [(String, [Value] -> Interpreter Value)]
> haskellFunImpls =
>     [-- arithmetic
>      ("+", \x -> case x of
>                      [StrV a, StrV b] -> pure $ StrV (a ++ b)
>                      [NumV a, NumV b] -> pure $ NumV (a + b)
>                      _ -> throwM $ MyException $ "Interpreter: plus implementation" ++ show x)
>     ,("-", \[NumV a, NumV b] -> pure $ NumV (a - b))
>     ,("*", \x -> case x of
>                      [NumV a, NumV b] -> pure $ NumV (a * b)
>                      _ -> throwM $ MyException $ "* needs two num args, got " ++ ppShow x)
>     ,("/", \x -> case x of
>                      [NumV a, NumV b] -> pure $ NumV (a / b)
>                      _ -> throwM $ MyException $ "/ needs two num args, got " ++ ppShow x)
>     -- comparisons - needs some work
>     ,("<", \x -> case x of
>                      [NumV a, NumV b] -> pure $ BoolV (a < b)
>                      _ -> throwM $ MyException $ "< needs two num args, got " ++ ppShow x)

>     ,(">", \x -> case x of
>                      [NumV a, NumV b] -> pure $ BoolV (a > b)
>                      _ -> throwM $ MyException $ "< needs two num args, got " ++ ppShow x)
>     ,("==", \[a, b] -> pure $ BoolV (a == b))

>     -- boolean ops
>     ,("and", \[BoolV a, BoolV b] -> pure $ BoolV (a && b))
>     ,("or", \[BoolV a, BoolV b] -> pure $ BoolV (a || b))
>     ,("not", \[BoolV a] -> pure $ BoolV (not a))

>     -- misc
>     ,("raise", \[StrV s] -> throwM $ MyException s)
>     ,("print", \[xx@(StrV x)] -> liftIO (putStrLn x) >> pure xx)
>     ,("torepr", \[x] -> pure $ torepr x)
>     ,("to-repr", \[x] -> pure $ torepr x)
>
>      -- lists hardcoded into the system to bootstrap since it needs them
>      -- to be able to create agdt in the implementation used
>     ,("link", listLink)
>     ,("is-empty", listIsEmpty)
>     ,("is-link", listIsLink)
>     ,("is-list", listIsList)

>      -- some internals
>     ,("log-check-block", logCheckBlock)
>     ,("log-test-pass", logTestPass)
>     ,("log-test-fail", logTestFail)
>     ,("add-tests", addTests)
>
>
>     ,("make-variant", makeVariant)
>     ,("variant-name", variantName)
>     ,("variant-field-get", \[v@(VariantV _ fs), StrV x] -> do
>              maybe (throwM $ MyException $ "variant field not found " ++ x ++ ": " ++ show v)
>                         pure
>                         $ lookup x fs)

>     ]

> defaultEnv :: Env
> defaultEnv = extendsEnv
>     [("true", BoolV True)
>     ,("false", BoolV False)
>     ,("empty", VariantV "empty" [])
>     ,liftBinOp "*"
>     ,liftBinOp "/"
>     ,liftBinOp "+"
>     ,liftBinOp "-"
>     ,liftBinOp "=="
>     ,liftBinOp "<"
>     ,liftBinOp ">"
>     ,liftBinOp "and"
>     ,liftBinOp "or"
>     ,liftUnOp "not"
>     ,liftUnOp "raise"
>     ,liftUnOp "print"
>     ,liftUnOp "torepr"
>     ,liftUnOp "to-repr"
>     ,liftBinOp "log-test-pass"
>     ,liftTriOp "log-test-fail"
>     ,liftBinOp "log-check-block"
>     ,liftUnOp "add-tests"
>     ,liftBinOp "variant-field-get"
>     ,liftBinOp "link"
>     ,liftUnOp "is-empty"
>     ,liftUnOp "is-link"
>     ,liftUnOp "is-list"
>     ,liftUnOp "variant-name"
>     ,liftBinOp "make-variant"
>     ] emptyEnv
>   where
>      liftUnOp f = (f, ClosV (I.Lam "a" (I.AppHaskell f [I.Iden "a"])) emptyEnv)
>      liftBinOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.AppHaskell f [I.Iden "a", I.Iden "b"]))) emptyEnv)
>      liftTriOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.Lam "c"
>                         (I.AppHaskell f [I.Iden "a", I.Iden "b", I.Iden "c"])))) emptyEnv)

> torepr :: Value -> Value
> torepr = StrV . torepr'
 
> torepr' :: Value -> String
> torepr' (NumV n) = case extractInt n of
>                              Just x -> show x
>                              Nothing ->  show n
> torepr' (BoolV n) = if n then "true" else "false"
> torepr' (ClosV {}) = "<Function>"
> torepr' (VariantV "tuple" fs) =
>     "{" ++ intercalate ";" (map (torepr' . snd) fs) ++ "}"
> torepr' (VariantV "link" xs) | Just vs <- mapM getList xs =
>     "[list: " ++ intercalate "," (map torepr' vs) ++ "]"
>   where
>     getList _ = Nothing -- (VariantV "list" (x:xs))
>   
> torepr' (VariantV "empty" []) = "empty"
> torepr' (VariantV "link" [("first",x),("rest",xs)]) =
>     "link(" ++ torepr' x ++ ", " ++ torepr' xs ++ ")"

> torepr' x = error $ "Interpreter: torepr implementation " ++ show x

> listLink :: [Value] -> Interpreter Value
> listLink [v1, v2] = pure $ VariantV "link" [("first", v1)
>                                            ,("rest", v2)]
> listLink x = throwM $ MyException $ "link called on " ++ show (length x) ++ " args, should be 2"

> listIsEmpty :: [Value] -> Interpreter Value
> listIsEmpty [VariantV "empty" []] = pure $ BoolV True
> listIsEmpty [_] = pure $ BoolV False
> listIsEmpty x = throwM $ MyException $ "is-empty called on " ++ show (length x) ++ " args, should be 1"

> listIsLink :: [Value] -> Interpreter Value
> listIsLink [VariantV "link" _] = pure $ BoolV True
> listIsLink [_] = pure $ BoolV False
> listIsLink x = throwM $ MyException $ "is-link called on " ++ show (length x) ++ " args, should be 1"

> listIsList :: [Value] -> Interpreter Value
> listIsList [VariantV x _] | x `elem` ["empty", "link"] = pure $ BoolV True
> listIsList [_] = pure $ BoolV False
> listIsList x = throwM $ MyException $ "is-list called on " ++ show (length x) ++ " args, should be 1"

> variantName :: [Value] -> Interpreter Value
> variantName [VariantV x _] = pure $ StrV x
> variantName [_] = pure $ StrV "nowt"
> variantName x = throwM $ MyException $ "variant-name called on " ++ show (length x) ++ " args, should be 1"

> makeVariant :: [Value] -> Interpreter Value
> makeVariant [StrV ctor, listargs] = do
>     vs <- listToHaskList listargs
>     cd <- mapM unpackTuple vs
>     pure $ VariantV ctor cd
>   where
>     unpackTuple (VariantV "tuple" [(_,StrV nm),(_,v)]) = pure (nm,v)
>     unpackTuple x = throwM $ MyException $ "value in list in make-variant, expected tuple of name and val, got " ++ show x
> makeVariant x = throwM $ MyException $ "make-variant called on " ++ show (length x) ++ " args, should be 2"

> listToHaskList :: Value -> Interpreter [Value]
> listToHaskList (VariantV "empty" []) = pure []
> listToHaskList (VariantV "link" [("first",v),("rest",x)]) = (v:) <$> listToHaskList x
> listToHaskList x = throwM $ MyException $ "interpreter: listToHaskList: " ++ show x
