
> {-# LANGUAGE ScopedTypeVariables #-}
> module Interpreter (interp
>                    ,CheckResult(..)
>                    ,runChecks
>                    ,emptyEnv
>                    ,extendEnv
>                    ,extendsEnv
>                    ,Env
>                    ,Value(..)
>                    ,extractInt
>                    ) where

> import Control.Exception.Safe (Exception, throwM, catch)
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.RWS (RWST, runRWST, ask, get, put, local, tell)
> import Data.List (partition)
> import Data.Scientific (Scientific)
> import Text.Show.Pretty (ppShow)

> import qualified InterpreterSyntax as I
> import Syntax (extractInt)

------------------------------------------------------------------------------

= values and environments

> data Value = NumV Scientific
>            | BoolV Bool
>            | StrV String
>            | TupleV [Value] -- todo use variant, which is also used for plain records
>            | ClosV I.Expr Env
>            | VariantV String String [(String,Value)]
>            | BoxV Int
>            | VoidV
>            deriving (Eq,Show)

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
>     ,("add_test", addTest)
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
>                ,liftUnOp "add_test"
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

------------------------------------------------------------------------------

temp testing until agdt are implemented

> data TestResultLog = TestPass Scientific String -- check block id, test source
>                    | TestFail Scientific String String -- check block id, test source, failure message
>                    | TestBlock Scientific String


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

> addTest :: [Value] -> Interpreter Value
> addTest = \[ClosV (I.LamVoid bdy) env'] -> do
>     local (const env') $ interp' bdy
>     pure VoidV


------------------------------------------------------------------------------

= store

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

------------------------------------------------------------------------------

= interpreter function

> type Interpreter a = RWST Env [TestResultLog] Store IO a
>

> data MyException = MyException String
>     deriving Show

> instance Exception MyException

> interp :: I.Program -> IO (Either String Value)
> interp (I.Program ex _) = (do
>     (result, _store, _log) <- runRWST (interp' ex) defaultHaskellFFIEnv emptyStore
>     pure $ pure $ result
>     ) `catch` (\(MyException s) -> pure $ Left $ s)

the check result is temporary. when agdt are added, the result will be
a Value

you run will run a script you have these options:
1. run tests or not
2. collect output -> returns a list of values for the top level
3. print output to stdout
the return will be Either String Value
there is a separate wrapper to collect the values, which returns
Either String (Value, [Value])

> runChecks :: I.Program -> IO (Either String [CheckResult])
> runChecks (I.Program _ cbs) = (do
>     let st = map f cbs
>     (_result, _store, lg) <- runRWST (mapM interp' st) defaultHaskellFFIEnv emptyStore
>     let (blocknmsx, testresults) = partition isTestBlock lg
>         blocknms :: [(Scientific, String)]
>         blocknms = map (\(TestBlock nm id) -> (nm, id)) blocknmsx
>         gs = map (\x -> (blockID x, toCheckResult x)) testresults
>         gs' = partitionN gs
>         ts = map (\(id,nm) -> case lookup id blocknms of
>                                    Nothing -> error "internal error block id"
>                                    Just b -> CheckResult b nm) gs'
>     pure $ pure ts
>     ) `catch` (\(MyException s) -> pure $ Left $ s)
>   where
>     isTestBlock (TestBlock {}) = True
>     isTestBlock _ = False
>     f (I.CheckBlock _ s) = s
>     blockID (TestPass x _) = x
>     blockID (TestFail x _ _) = x
>     toCheckResult (TestPass _ x) = (x,Nothing)
>     toCheckResult (TestFail _ x m) = (x,Just m)


 data TestResultLog = TestPass Scientific String -- check block id, test source
                    | TestFail Scientific String String -- check block id, test source, failure message
                    | TestBlock Scientific String


todo: move this to an in language data type

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
> -- the second is just if it is a fail, it contains the failure
> -- message


> partitionN :: Eq a => [(a,b)] -> [(a,[b])]
> partitionN [] = []
> partitionN vs@((k,_):_) =
>     let (x,y) = partition ((==k) . fst) vs
>     in (k,map snd x) : partitionN y




--------------------------------------

> interp' :: I.Expr -> Interpreter Value
> interp' (I.Sel (I.Num n)) = pure $ NumV n
> interp' (I.Sel (I.Str s)) = pure $ StrV s
> interp' (I.Sel I.VoidS) = pure $ VoidV
> interp' (I.Sel (I.Tuple es)) = do
>     vs <- mapM interp' es
>     pure $ TupleV vs

> interp' (I.Iden e) = do
>     env <- ask
>     v <- maybe (throwM $ MyException $ "Identifier not found: " ++ e)
>         pure $ lookupEnv e env
>     case v of
>         BoxV i -> do
>                   store <- get
>                   fetchStore i store
>         _ -> pure v
>   
> interp' _x@(I.If c t e) = do
>    c' <- interp' c
>    case c' of
>        BoolV True -> interp' t
>        BoolV False -> interp' e
>        _ -> throwM $ MyException $ "expected bool in if test, got " ++ show c'

the AppHaskell is only used in the env, the desugarer never produces
it

> interp' (I.AppHaskell nm exps) = do
>     f <- maybe (throwM $ MyException $ "ffi fn not found: " ++ nm) pure $ lookup nm haskellFunImpls
>     vs <- mapM interp' exps
>     f vs

> interp' e@(I.Lam {}) = do
>     env <- ask
>     pure $ ClosV e env
> interp' e@(I.LamVoid {}) = do
>     env <- ask
>     pure $ ClosV e env


TODO: add a different error message when it gets lam or lamvoid
instead of the other one
 
> interp' (I.App f a) = do
>     x <- interp' f
>     case x of
>         ClosV (I.Lam n bdy) env' -> do
>              argVal <- interp' a
>              local (const $ extendEnv n argVal env') $ interp' bdy
>         ClosV (I.LamVoid bdy) env' -> do
>              case a of
>                  I.Sel I.VoidS -> local (const env') $ interp' bdy
>                  _ -> throwM $ MyException $ "0 arg lambda called with something other than literal void: " ++ show a
>         ClosV ee _ -> throwM $ MyException $ "non lambda in closure expression: " ++ show ee
>         _ -> throwM $ MyException $ "non function in app position: " ++ show x

> interp' (I.Let nm v bdy) = do
>     v' <- interp' v
>     local (extendEnv nm v') $ interp' bdy

> interp' (I.Seq (I.LetDecl nm e) b) = do
>     v <- interp' e
>     local (extendEnv nm v) $ interp' b
> interp' (I.Seq a b) = interp' a >> interp' b

> interp' (I.Box e) = do
>     v <- interp' e
>     store <- get
>     let i = newStoreLoc store
>     put $ extendStore i v store
>     pure $ BoxV i

> interp' (I.SetBox b v) = do
>     b' <- do
>           env <- ask
>           maybe (throwM $ MyException $ "Identifier not found: " ++ b)
>               pure $ lookupEnv b env
>     v' <- interp' v
>     store <- get
>     case b' of
>         BoxV i -> put $ extendStore i v' store
>         _ -> throwM $ MyException $ "attemped to setbox non box value: " ++ show v
>     pure v'

> interp' x = error $ "interp' " ++ show x
