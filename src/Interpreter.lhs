
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


> {-# LANGUAGE ScopedTypeVariables #-}
> module Interpreter (runProgram
>                    ,CheckResult(..)
>                    ,runChecks
>                    ,Value(..)
>                    ,extractInt
>                    ) where

> import Control.Exception.Safe (Exception, throwM, catch)
> import Control.Monad (void, forM_, when)
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.RWS (RWST, runRWST, ask, get, {-put,-} local, tell, state)
> import Data.List (partition)
> import Data.Scientific (Scientific)
> import Text.Show.Pretty (ppShow)

> import qualified InterpreterSyntax as I
> import Syntax (extractInt)

------------------------------------------------------------------------------

= values

> data Value = NumV Scientific
>            | BoolV Bool
>            | StrV String
>            | TupleV [Value] -- todo use variant, which is also used for plain records
>            | ClosV I.Expr Env
>            | VariantV String String [(String,Value)]
>            | BoxV Int
>            | VoidV
>            deriving (Eq,Show)

------------------------------------------------------------------------------

= data types and support for the interpreter monad

> type Interpreter = RWST InterpreterReader [TestResultLog] InterpreterState IO


quick exceptions, can come back to this

> data MyException = MyException String
>     deriving Show

> instance Exception MyException


the env is used for read only bindings. the code distinguishes between
these and variables, not sure there is a good reason to do this

> data Env = Env [(String,Value)]
>            deriving (Eq,Show)

> emptyEnv :: Env
> emptyEnv = Env [("true", BoolV True)
>                ,("false", BoolV False)]

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
>     }
> updateISStore :: (Store -> Store) -> (InterpreterState -> InterpreterState)
> updateISStore f i = i {isStore = f (isStore i)}

> defaultInterpreterState :: InterpreterState
> defaultInterpreterState = InterpreterState emptyStore []

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
>     (result, _store, lg) <-
>         runRWST (do
>                  x <- interp ex
>                  when runChks runSavedTests
>                  pure x
>                 )
>         (InterpreterReader defaultHaskellFFIEnv)
>         defaultInterpreterState
>     let cr = if runChks
>              then getCheckResults lg
>              else []
>     pure $ pure $ (result, cr)
>     ) `catch` (\(MyException s) -> pure $ Left $ s)

--------------------------------------

interpreters for syntax nodes

> interp :: I.Expr -> Interpreter Value
> interp (I.Sel (I.Num n)) = pure $ NumV n
> interp (I.Sel (I.Str s)) = pure $ StrV s
> interp (I.Sel I.VoidS) = pure $ VoidV
> interp (I.Sel (I.Tuple es)) = do
>     vs <- mapM interp es
>     pure $ TupleV vs

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
>     local (updateIREnv (extendEnv nm v')) $ interp bdy

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
>     tell [TestPass n t]
>     pure VoidV

> logTestFail :: [Value] -> Interpreter Value
> logTestFail = \[NumV n, StrV t, StrV m] -> do
>     tell [TestFail n t m]
>     pure VoidV

> logCheckBlock :: [Value] -> Interpreter Value
> logCheckBlock = \[NumV i, StrV s] -> do
>     tell [TestBlock i s]
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
> runSavedTests :: Interpreter ()
> runSavedTests = do
>     ts <- (reverse . isTestsToRun) <$> get
>     forM_ ts $ \(ClosV (I.LamVoid bdy) env') ->
>         void $ local (updateIREnv (const env')) $ interp bdy

the data type which is part of the user api for testing for now

the plan is to make this all more in-language-y once agdt are
supported

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
> -- the second is just if it is a fail, it contains the failure
> -- message

> getCheckResults :: [TestResultLog] -> [CheckResult]
> getCheckResults lg =
>         let (blocknmsx, testresults) = partition isTestBlock lg
>             blocknms :: [(Scientific, String)]
>             blocknms = map (\(TestBlock nm tid) -> (nm, tid)) blocknmsx
>             gs = map (\x -> (blockID x, toCheckResult x)) testresults
>             gs' = partitionN gs
>             ts = map (\(tid,nm) -> case lookup tid blocknms of
>                                        Nothing -> error "internal error block id"
>                                        Just b -> CheckResult b nm) gs'
>         in ts
>   where
>     isTestBlock (TestBlock {}) = True
>     isTestBlock _ = False
>     blockID (TestPass x _) = x
>     blockID (TestFail x _ _) = x
>     blockID (TestBlock {}) = error "Interpreter: testblock in wrong place"
>     toCheckResult (TestPass _ x) = (x,Nothing)
>     toCheckResult (TestFail _ x m) = (x,Just m)
>     toCheckResult (TestBlock {}) = error "Interpreter: testblock in wrong place b"

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

> haskellFunImpls :: [(String, [Value] -> Interpreter Value)]
> haskellFunImpls =
>     [-- arithmetic
>      ("+", \x -> case x of
>                      [StrV a, StrV b] -> pure $ StrV (a ++ b)
>                      [NumV a, NumV b] -> pure $ NumV (a + b)
>                      _ -> error $ "Interpreter: plus implementation" ++ show x)
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
>
>      -- some internals
>     ,("log_check_block", logCheckBlock)
>     ,("log_test_pass", logTestPass)
>     ,("log_test_fail", logTestFail)
>     ,("add_tests", addTests)
>      
>     ,("tupleget", \[TupleV vs, NumV x] -> do
>              i <- maybe (throwM $ MyException $ "expected integral, got " ++ show x)
>                         pure (extractInt x)
>              pure $ vs !! i)
>     ]

> defaultHaskellFFIEnv :: Env
> defaultHaskellFFIEnv = 
>     extendsEnv [liftBinOp "*"
>                ,liftBinOp "/"
>                ,liftBinOp "+"
>                ,liftBinOp "-"
>                ,liftBinOp "=="
>                ,liftBinOp "<"
>                ,liftBinOp ">"
>                ,liftBinOp "and"
>                ,liftBinOp "or"
>                ,liftUnOp "not"
>                ,liftUnOp "raise"
>                ,liftUnOp "print"
>                ,liftUnOp "torepr"
>                ,liftBinOp "log_test_pass"
>                ,liftTriOp "log_test_fail"
>                ,liftBinOp "log_check_block"
>                ,liftUnOp "add_tests"
>                ,liftBinOp "tupleget"
>                ] emptyEnv
>   where
>      liftUnOp f = (f, ClosV (I.Lam "a" (I.AppHaskell f [I.Iden "a"])) emptyEnv)
>      liftBinOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.AppHaskell f [I.Iden "a", I.Iden "b"]))) emptyEnv)
>      liftTriOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.Lam "c"
>                         (I.AppHaskell f [I.Iden "a", I.Iden "b", I.Iden "c"])))) emptyEnv)

> torepr :: Value -> Value
> torepr (NumV n) = StrV $ case extractInt n of
>                              Just x -> show x
>                              Nothing ->  show n
> torepr (BoolV n) = StrV $ if n then "true" else "false"
> torepr (ClosV {}) = StrV "<Function>"
> torepr x = error $  "Interpreter: torepr implementation " ++ show x
