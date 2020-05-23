
Key aspects of the interpreter:

The interpreter monads is rws combined with IO.

Control.Exception.Safe is used for errors - all errors are caught and
returned as a left when interpreting. this is used for the language
errors/raise implementation too

The reader value contains the read only bindings.

The state value contains the store for read/write values, and logs the
callbacks for the tests (which are gathered during script execution,
and then executed at the end).

The test results are put to the state.

After the interpretation finishes, the test log is turned into test
results for the user api.

The system comes with a small standard lib (really small) implemented
using a hack sort of ffi for haskell.


> {-# LANGUAGE ScopedTypeVariables,TupleSections, LambdaCase #-}
> module Interpreter (runProgram
>                    ,CheckResult(..)
>                    ,runChecks
>                    ,allCheckResultsPassed
>                    ,Value(..)
>                    ,extractInt
>                    ) where

> import Control.Exception.Safe (Exception, throwM, catch)
> import Control.Monad (void, forM_{-, when-})
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.RWS (RWST, runRWST, ask, get, {-put,-} local, {-tell,-} state)
> import Data.Maybe (isNothing)
> import Data.List (partition, intercalate, sortOn)
> import Data.Scientific (Scientific)
> import Text.Show.Pretty (ppShow)

> import qualified InterpreterSyntax as I
> import Syntax (extractInt)
> import PrettyInterpreter (prettyExpr)

> --import Debug.Trace(trace)

------------------------------------------------------------------------------

= values

> data Value = NumV Scientific
>            | BoolV Bool
>            | StrV String
>            | ClosV I.Expr Env
>            | VariantV String -- variant name
>                       [(String,Value)] -- fields, is it better to not include the names,
>                                        -- the desugarer will handle?
>            | BoxV Int
>            | NothingV -- does nothing need to be separate, can it be a variant value?
>            deriving (Eq,Show)

------------------------------------------------------------------------------

= data types and support for the interpreter monad

> type Interpreter = RWST InterpreterReader [()] InterpreterState IO


quick exceptions, can come back to this

> data InterpreterException =
>       UserException String -- a programming error - represents something
>                            -- that cannot be caught or tested
>                            --  for
>     | InternalException String -- an error in this implementation
>     | ValueException Value -- used when 'raise' -ing in
>                                         --  language, something
>                                         --  which testing can test
>                                         --  for (and the hacky catch
>                                         --  function can catch
>     deriving Show

> instance Exception InterpreterException

> interpreterExceptionToString :: InterpreterException -> String
> interpreterExceptionToString (UserException s) = s
> interpreterExceptionToString (InternalException s) = "Internal error: " ++ s
> interpreterExceptionToString (ValueException v) = torepr' v



The env is used for read only bindings. The code distinguishes between
these and variables, a reason why this possibly makes sense is it's
easy to manage vars with the state, and non vars using rws local
function - combined with closures, this means the non vars lifetime
and scope is handled a little more easily. This doesn't exactly
justify implementing constant bindings using the store though

> data Env = Env [(String,Value)]
>            deriving (Eq,Show)

> envNames :: Env -> [String]
> envNames (Env e) = map fst e

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
> fetchStore i (Store xs) = maybe (throwM $ InternalException $ "invalid fetch on store: " ++ show i) pure
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
>     ) `catch` (\e -> pure $ Left $ interpreterExceptionToString e)

--------------------------------------

interpreters for syntax nodes

> interp :: I.Expr -> Interpreter Value
> interp (I.Sel (I.Num n)) = pure $ NumV n
> interp (I.Sel (I.Str s)) = pure $ StrV s
> interp (I.Sel I.NothingS) = pure $ NothingV
> interp (I.Sel (I.Variant nm es)) = do
>     vs <- mapM (\(n,e) -> (n,) <$> interp e) es
>     pure $ VariantV nm vs

> interp (I.Iden e) = do
>     rd <- ask
>     maybe (throwM $ UserException $ "Identifier not found: " ++ e ++ "\n" ++ ppShow (envNames $ irEnv rd))
>         pure $ lookupEnv e (irEnv rd)
>   
> interp _x@(I.If c t e) = do
>    c' <- interp c
>    case c' of
>        BoolV True -> interp t
>        BoolV False -> interp e
>        _ -> throwM $ UserException $ "expected bool in if test, got " ++ torepr' c'

the AppHaskell is only used in the default ffi env, the desugarer never produces
it

> interp (I.AppHaskell nm exps) = do
>     f <- maybe (throwM $ InternalException $ "ffi fn not found: " ++ nm) pure $ lookup nm haskellFunImpls
>     vs <- mapM interp exps
>     f vs

> interp e@(I.Lam {}) = do
>     env <- irEnv <$> ask
>     pure $ ClosV e env
> interp e@(I.LamVoid {}) = do
>     env <- irEnv <$> ask
>     pure $ ClosV e env


> interp (I.App fe ae) = do
>     f <- interp fe
>     a <- interp ae
>     app f a

> interp (I.Let nm v bdy) = do
>     v' <- interp v
>     case nm of
>         "_" -> interp bdy
>         _ -> local (updateIREnv (extendEnv nm v')) $ interp bdy

> interp (I.Seq (I.LetDecl nm e) b) = do
>     v <- interp e
>     local (updateIREnv (extendEnv nm v)) $ interp b
> interp (I.Seq a b) = interp a >> interp b

> interp (I.Unbox e) =
>     interp e >>= \case
>         BoxV i -> do
>                   st <- get
>                   fetchStore i (isStore st)
>         v -> throwM $ UserException $ "Unbox on non box: " ++ torepr' v


> interp (I.Box e) = do
>     v <- interp e
>     box v

> interp (I.SetBox b v) = do
>     b' <- interp b
>     v' <- interp v
>     i <- case b' of
>              BoxV i -> pure i
>              _ -> throwM $ UserException $ "attemped to setbox non box value: " ++ torepr' b'
>     state $ \s ->
>         ((), updateISStore (extendStore i v') s)
>     pure v'


> interp x@(I.LetDecl {}) = throwM $ UserException $ "Interpreter: block ends with let: " ++ prettyExpr x

> interp (I.Catch e c) = interp e `catch` (\case
>     ValueException v -> do
>         cf <- interp c
>         app cf v
>     -- is rethrowing an exception like this bad in haskell?
>     s -> throwM s)


> --interp x = error $ "Interpreter: interp " ++ show x



> box :: Value -> Interpreter Value
> box v = do
>     i <- state $ \s ->
>          let i = newStoreLoc (isStore s)
>          in (i, updateISStore (extendStore i v) s)
>     pure $ BoxV i

> -- helper for function implemention use
> app :: Value -> Value -> Interpreter Value
> app f a = case f of
>         ClosV (I.Lam n bdy) env' -> do
>              local (updateIREnv (const $ extendEnv n a env')) $ interp bdy
>         ClosV (I.LamVoid bdy) env' -> do
>              case a of
>                  NothingV -> local (updateIREnv (const env')) $ interp bdy
>                  _ -> throwM $ InternalException $ "0 arg lambda called with something other than literal nothing: " ++ torepr' a
>         ClosV ee _ -> throwM $ InternalException $ "non lambda in closure expression: " ++ prettyExpr ee
>         _ -> throwM $ UserException $ "non function in app position: " ++ torepr' f

> appN :: Value -> [Value] -> Interpreter Value
> appN f [] = app f NothingV
> appN f [x] = app f x
> appN f (x:xs) = do
>    f' <- app f x
>    appN f' xs

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
>     pure NothingV

> logTestFail :: [Value] -> Interpreter Value
> logTestFail = \[NumV n, StrV t, StrV m] -> do
>     appendTestResults (TestFail n t m)
>     pure NothingV

> logCheckBlock :: [Value] -> Interpreter Value
> logCheckBlock = \[NumV i, StrV s] -> do
>     appendTestResults (TestBlock i s)
>     pure NothingV

> addTests :: [Value] -> Interpreter Value
> addTests = \[t@(ClosV (I.LamVoid _) _)] -> do
>     -- save the test so it can be run at the end
>     -- is it worth optimising this out if the tests aren't going to be run?
>     appendTest t
>     pure NothingV
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

> allCheckResultsPassed :: [CheckResult] -> Bool
> allCheckResultsPassed cs = all f cs
>   where
>     f (CheckResult _ rs) = all isNothing $ map snd rs

> getCheckResults :: [TestResultLog] -> Interpreter [CheckResult]
> getCheckResults lg' = do
>         let lg = reverse lg'
>             (blocknmsx, testresults) = partition isTestBlock lg
>             blocknms :: [(Scientific, String)]
>             blocknms = map (\(TestBlock nm tid) -> (nm, tid)) blocknmsx
>         gs <- mapM (\x -> (,) <$> blockID x <*> toCheckResult x) testresults
>         let  gs' = partitionN gs
>         ts <- mapM (\(tid,nm) -> case lookup tid blocknms of
>                                        Nothing -> throwM $ InternalException $ "internal error block id"
>                                        Just b -> pure $ CheckResult b nm) gs'
>         pure ts
>   where
>     isTestBlock (TestBlock {}) = True
>     isTestBlock _ = False
>     blockID (TestPass x _) = pure x
>     blockID (TestFail x _ _) = pure x
>     blockID (TestBlock {}) = throwM $ InternalException $ "Interpreter: testblock in wrong place"
>     toCheckResult (TestPass _ x) = pure (x,Nothing)
>     toCheckResult (TestFail _ x m) = pure (x,Just m)
>     toCheckResult (TestBlock {}) = throwM $ InternalException $ "Interpreter: testblock in wrong place b"

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

try to refactor so ops for a type are all together
(like comparisons and torepr)

todo: add a better ffi
and try to put all these functions in pyret files
  including a prelude file which works like haskell
  (always included, unless you do it explicitly)
  but don't call it prelude, since that is used already for
  part of the syntax of a pyret file



> haskellFunImpls :: [(String, [Value] -> Interpreter Value)]
> haskellFunImpls =
>     [-- arithmetic
>      ("+", \case
>              [StrV a, StrV b] -> pure $ StrV (a ++ b)
>              [NumV a, NumV b] -> pure $ NumV (a + b)
>              x -> throwM $ UserException $ "Interpreter: plus implementation" ++ listToRepr x)
>     ,("-", \[NumV a, NumV b] -> pure $ NumV (a - b))
>     ,("*", \case
>              [NumV a, NumV b] -> pure $ NumV (a * b)
>              x -> throwM $ UserException $ "* needs two num args, got " ++ listToRepr x)
>     ,("/", \case
>              [NumV a, NumV b] -> pure $ NumV (a / b)
>              x -> throwM $ UserException $ "/ needs two num args, got " ++ listToRepr x)
>     -- comparisons - needs some work
>     ,("<", \case
>              [NumV a, NumV b] -> pure $ BoolV (a < b)
>              x -> throwM $ UserException $ "< needs two num args, got " ++ listToRepr x)

>     ,(">", \case
>              [NumV a, NumV b] -> pure $ BoolV (a > b)
>              x -> throwM $ UserException $ "< needs two num args, got " ++ listToRepr x)
>     ,("==", \[a, b] -> pure $ BoolV $ valuesEqual a b)
>     ,("<>", \[a, b] -> pure $ BoolV $ not $ valuesEqual a b)

>     -- app operators
>     ,("^", \[a, f] -> app f a)
>     ,("|>", \[f, a] -> app f a)
> 
>     -- boolean ops
>     ,("and", \[BoolV a, BoolV b] -> pure $ BoolV (a && b))
>     ,("or", \[BoolV a, BoolV b] -> pure $ BoolV (a || b))
>     ,("not", \[BoolV a] -> pure $ BoolV (not a))

>     -- misc
>     ,("raise", \case
>                   [v] -> throwM $ ValueException v
>                   xs -> throwM $ UserException $ "expected 1 arg for raise, got " ++ show (length xs)
>                                                ++ ", " ++ intercalate "," (map torepr' xs))
>     ,("print", \[x] ->
>              let StrV s = tostring x
>              in liftIO (putStrLn s) >> pure (StrV s))
>     ,("torepr", \[x] -> pure $ torepr x)
>     ,("to-repr", \[x] -> pure $ torepr x)
>     ,("tostring", \[x] -> pure $ tostring x)
>     ,("to-string", \[x] -> pure $ tostring x)
>
>     ,("is-boolean", \case
>              [BoolV _] -> pure $ BoolV True
>              [_] -> pure $ BoolV False
>              es -> throwWrongArgs "is-boolean" 1 es)

>     ,("is-string", \case
>              [StrV _] -> pure $ BoolV True
>              [_] -> pure $ BoolV False
>              es -> throwWrongArgs "is-string" 1 es)

>     ,("is-number", \case
>              [NumV _] -> pure $ BoolV True
>              [_] -> pure $ BoolV False
>              es -> throwWrongArgs "is-number" 1 es)

>     ,("is-function", \case
>              [ClosV {}] -> pure $ BoolV True
>              [_] -> pure $ BoolV False
>              es -> throwWrongArgs "is-function" 1 es)

>     ,("is-nothing", \case
>              [NothingV] -> pure $ BoolV True
>              [_] -> pure $ BoolV False
>              es -> throwWrongArgs "is-nothing" 1 es)

>     ,("is-tuple", \case
>              [VariantV "tuple" _] -> pure $ BoolV True
>              [_] -> pure $ BoolV False
>              es -> throwWrongArgs "is-tuple" 1 es)

>     ,("is-record", \case
>              [VariantV "record" _] -> pure $ BoolV True
>              [_] -> pure $ BoolV False
>              es -> throwWrongArgs "is-record" 1 es)



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
>     ,("tostring-equals", tostringEquals)
>
>     
>     ,("call-construct-make", callConstructMake)
>     ,("make-variant", makeVariant)
>     ,("variant-name", variantName)
>     ,("safe-variant-name", safeVariantName)
>     ,("variant-field-get", \[v@(VariantV _ fs), StrV x] -> do
>              maybe (throwM $ UserException $ "variant field not found " ++ x ++ ": " ++ torepr' v)
>                         pure
>                         $ lookup x fs)

>     ]

> throwWrongArgs :: String -> Int -> [Value] -> Interpreter a
> throwWrongArgs fnname want es = 
>     throwM $ UserException $ "expected " ++ show want ++ " arg for " ++ fnname ++ ", got " ++ show (length es)
>     ++ ", " ++ intercalate "," (map torepr' es)


> defaultEnv :: Env
> defaultEnv = extendsEnv
>     [("true", BoolV True)
>     ,("false", BoolV False)
>     ,("empty", VariantV "empty" [])
>     ,("nothing", NothingV)
>     ,liftBinOp "*"
>     ,liftBinOp "/"
>     ,liftBinOp "+"
>     ,liftBinOp "-"
>     ,liftBinOp "=="
>     ,liftBinOp "<>"
>     ,liftBinOp "<"
>     ,liftBinOp ">"
>     ,liftBinOp "^"
>     ,liftBinOp "|>"
>     ,liftBinOp "and"
>     ,liftBinOp "or"
>     ,liftUnOp "not"
>     ,liftUnOp "raise"
>     ,liftUnOp "print"
>     ,liftUnOp "torepr"
>     ,liftUnOp "to-repr"
>     ,liftUnOp "tostring"
>     ,liftUnOp "to-string"
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
>     ,liftUnOp "safe-variant-name"
>     ,liftUnOp "is-boolean"
>     ,liftUnOp "is-string"
>     ,liftUnOp "is-function"
>     ,liftUnOp "is-nothing"
>     ,liftUnOp "is-tuple"
>     ,liftUnOp "is-record"
>     ,liftUnOp "is-number"
>     ,liftBinOp "make-variant"
>     ,liftBinOp "tostring-equals"
>     ,liftBinOp "call-construct-make"
>     ] emptyEnv
>   where
>      liftUnOp f = (f, ClosV (I.Lam "a" (I.AppHaskell f [I.Iden "a"])) emptyEnv)
>      liftBinOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.AppHaskell f [I.Iden "a", I.Iden "b"]))) emptyEnv)
>      liftTriOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.Lam "c"
>                         (I.AppHaskell f [I.Iden "a", I.Iden "b", I.Iden "c"])))) emptyEnv)

> valuesEqual :: Value -> Value -> Bool
> valuesEqual (VariantV "record" as) (VariantV "record" bs) = sortOn fst as == sortOn fst bs
> valuesEqual a b = a == b

> listToRepr :: [Value] -> String
> listToRepr = intercalate "," . map torepr'

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

> torepr' (VariantV "record" fs) =
>     "{" ++ intercalate ";" (map (\(a,b) -> a ++ ":" ++ torepr' b) fs) ++ "}"
> torepr' (VariantV v fs) =
>     v ++ "(" ++ intercalate ", " (map (torepr' . snd) fs) ++ ")"

> torepr' (BoxV x) = "Box " ++ show x

> torepr' (StrV s) = "\"" ++ s ++ "\""

> torepr' NothingV = "nothing"

> tostring :: Value -> Value
> tostring (StrV v) = StrV v
> tostring x = torepr x

tostring equals is used in implementing raises with raises-satisfies

> tostringEquals :: [Value] -> Interpreter Value
> tostringEquals [e0, e1] = do
>     let x = tostring e1
>     pure $ BoolV $ valuesEqual e0 x

> tostringEquals x = throwM $ UserException $ "tostring-equals called on " ++ show (length x) ++ " args, should be 2"


> listLink :: [Value] -> Interpreter Value
> listLink [v1, v2] = pure $ VariantV "link" [("first", v1)
>                                            ,("rest", v2)]
> listLink x = throwM $ UserException $ "link called on " ++ show (length x) ++ " args, should be 2"

> listIsEmpty :: [Value] -> Interpreter Value
> listIsEmpty [VariantV "empty" []] = pure $ BoolV True
> listIsEmpty [_] = pure $ BoolV False
> listIsEmpty x = throwM $ UserException $ "is-empty called on " ++ show (length x) ++ " args, should be 1"

> listIsLink :: [Value] -> Interpreter Value
> listIsLink [VariantV "link" _] = pure $ BoolV True
> listIsLink [_] = pure $ BoolV False
> listIsLink x = throwM $ UserException $ "is-link called on " ++ show (length x) ++ " args, should be 1"

> listIsList :: [Value] -> Interpreter Value
> listIsList [x] = pure $ BoolV $ hlistIsList x
> listIsList x = throwM $ UserException $ "is-list called on " ++ show (length x) ++ " args, should be 1"

> hlistIsList :: Value -> Bool
> hlistIsList (VariantV x _) | x `elem` ["empty", "link"] = True
> hlistIsList _ = False

> variantName :: [Value] -> Interpreter Value
> variantName [VariantV x _] = pure $ StrV x
> variantName [x] = throwM $ UserException $ "variant-name called on non variant: " ++ torepr' x
> variantName x = throwM $ UserException $ "variant-name called on " ++ show (length x) ++ " args, should be 1"

> safeVariantName :: [Value] -> Interpreter Value
> safeVariantName [VariantV x _] = pure $ StrV x
> safeVariantName [_] = pure NothingV
> safeVariantName x = throwM $ UserException $ "safe-variant-name called on " ++ show (length x) ++ " args, should be 1"

algo: if there is a matching makeN in the record, use it, else if
 there is a make, use that, else error
todo: can do this in the desugaring once there is a type checker

> callConstructMake :: [Value] -> Interpreter Value
> callConstructMake = \case
>     [] -> throwM $ InternalException $ "construct make called on 0 args"
>     [VariantV "record" ts, args]
>         | hlistIsList args -> do
>               args' <- listToHaskList args
>               let makeN = "make" ++ show (length args')
>               case () of
>                      _ | Just f <- lookup makeN ts -> appN f args'
>                        | Just f <- lookup "make" ts -> app f args
>                        | otherwise -> throwM $ UserException "The left side was not a defined convenience constructor."
>         | otherwise -> throwM $ InternalException $ "construct make called on non list: " ++ torepr' args
>     [_,_] -> throwM $ UserException "The left side was not a defined convenience constructor."
>     x -> throwM $ InternalException $ "construct make called on " ++ show (length x) ++ " args"
>         


> makeVariant :: [Value] -> Interpreter Value
> makeVariant [StrV vnt, listargs] = do
>     vs <- listToHaskList listargs
>     cd <- mapM unpackTuple vs
>     pure $ VariantV vnt cd
>   where
>     unpackTuple (VariantV "tuple" [(_,BoolV ref),(_,StrV nm),(_,v)]) = do
>         x <- if ref
>              then box v
>              else pure v
>         pure (nm,x)
>     unpackTuple (VariantV "tuple" x) = throwM $ UserException $ "value in list in make-variant, expected tuple of is-ref, name and val, got " ++ show (map (\(_,b) -> torepr' b) x)
>     unpackTuple x = throwM $ UserException $ "expected tuple in make-variant, got " ++ torepr' x
> makeVariant x = throwM $ UserException $ "make-variant called on " ++ show (length x) ++ " args, should be 2"

> listToHaskList :: Value -> Interpreter [Value]
> listToHaskList (VariantV "empty" []) = pure []
> listToHaskList (VariantV "link" [("first",v),("rest",x)]) = (v:) <$> listToHaskList x
> listToHaskList x = throwM $ UserException $ "interpreter: listToHaskList: " ++ torepr' x
