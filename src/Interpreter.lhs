
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


todo: make this better: do better wrapping + error messages when the
type is wrong

> haskellFunImpls :: [(String, [Value] -> Interpreter Value)]
> haskellFunImpls =
>     [("+", \x -> case x of
>                      [StrV a, StrV b] -> pure $ StrV (a ++ b)
>                      [NumV a, NumV b] -> pure $ NumV (a + b))
>     ,("-", \[NumV a, NumV b] -> pure $ NumV (a - b))
>     ,("*", \x -> case x of
>                      [NumV a, NumV b] -> pure $ NumV (a * b)
>                      _ -> throwM $ MyException $ "* needs two num args, got " ++ ppShow x)
>     ,("==", \[a, b] -> pure $ BoolV (a == b))
>     ,("raise", \[StrV s] -> throwM $ MyException s)
>     ,("print", \[xx@(StrV x)] -> liftIO (putStrLn x) >> pure xx)
>     ,("log_test_pass", \[StrV c, StrV t] -> do
>           tell [TestPass c t]
>           pure $ BoolV True)
>     ,("log_test_fail", \[StrV c, StrV t, StrV m] -> do
>           tell [TestFail c t m]
>           pure $ BoolV True)
>     ,("torepr", \[x] -> pure $ torepr x)
>     ]

temp testing until agdt are implemented

> data TestResultLog = TestPass String String -- check block name, test source
>                    | TestFail String String String -- check block name, test source, failure message

> defaultHaskellFFIEnv :: Env
> defaultHaskellFFIEnv = 
>     extendsEnv [liftBinOp "*"
>                ,liftBinOp "+"
>                ,liftBinOp "-"
>                ,liftBinOp "=="
>                ,liftUnOp "raise"
>                ,liftUnOp "print"
>                ,liftUnOp "torepr"
>                ,liftBinOp "log_test_pass"
>                ,liftTriOp "log_test_fail"
>                ] emptyEnv
>   where
>      liftUnOp f = (f, ClosV (I.Lam "a" (I.AppHaskell f [I.Iden "a"])) emptyEnv)
>      liftBinOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.AppHaskell f [I.Iden "a", I.Iden "b"]))) emptyEnv)
>      liftTriOp f = (f, ClosV (I.Lam "a" (I.Lam "b" (I.Lam "c"
>                         (I.AppHaskell f [I.Iden "a", I.Iden "b", I.Iden "c"])))) emptyEnv)


> data Value = NumV Scientific
>            | BoolV Bool
>            | StrV String
>            | ClosV I.Expr Env
>            | VariantV String String [(String,Value)]
>            | BoxV Int
>            deriving (Eq,Show)

> torepr :: Value -> Value
> torepr (NumV n) = StrV $ case extractInt n of
>                              Just x -> show x
>                              Nothing ->  show n
> torepr (BoolV n) = StrV $ if n then "true" else "false"
> torepr (ClosV {}) = StrV "<Function>"

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

> interp :: I.Program -> IO (Either String (Maybe Value))
> interp (I.Program sts _) = (do
>     case sts of
>         Nothing -> pure $ pure Nothing
>         Just x -> do
>             (result, _store, _log) <- runRWST (interpStmt' x) defaultHaskellFFIEnv emptyStore
>             pure $ pure $ Just $ result
>     ) `catch` (\(MyException s) -> pure $ Left $ s)

> runChecks :: I.Program -> IO (Either String [CheckResult])
> runChecks (I.Program _ cbs) = do
>     let st = map f cbs
>     (_result, _store, lg) <- runRWST (mapM interpStmt' st) defaultHaskellFFIEnv emptyStore
>     let gs = map (\x -> (blockName x, toCheckResult x)) lg
>         gs' = partitionN gs
>         ts = map (uncurry CheckResult) gs'
>     pure $ pure ts
>   where
>     f (I.CheckBlock _ s) = s
>     blockName (TestPass x _) = x
>     blockName (TestFail x _ _) = x
>     toCheckResult (TestPass _ x) = (x,Nothing)
>     toCheckResult (TestFail _ x m) = (x,Just m)


> partitionN :: Eq a => [(a,b)] -> [(a,[b])]
> partitionN [] = []
> partitionN vs@((k,_):_) =
>     let (x,y) = partition ((==k) . fst) vs
>     in (k,map snd x) : partitionN y


todo: move this to an in language data type

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
> -- the second is just if it is a fail, it contains the failure
> -- message


--------------------------------------

> interpStmt' :: I.Stmt -> Interpreter Value
> interpStmt' (I.LetDecl {}) = throwM $ MyException $ "unattached let decl"
> interpStmt' (I.StExpr e) = interp' e

--------------------------------------

> interp' :: I.Expr -> Interpreter Value
> interp' (I.Sel (I.Num n)) = pure $ NumV n
> interp' (I.Sel (I.Str s)) = pure $ StrV s
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
>         ClosV ee _ -> throwM $ MyException $ "non lambda in closure expression: " ++ show ee
>         _ -> throwM $ MyException $ "non function in app position: " ++ show x

> interp' (I.AppVoid f) = do
>     x <- interp' f
>     case x of
>         ClosV (I.LamVoid bdy) env' -> do
>              local (const env') $ interp' bdy
>         ClosV ee _ -> throwM $ MyException $ "non lambda in closure expression: " ++ show ee
>         _ -> throwM $ MyException $ "non function in app position: " ++ show x



> interp' (I.Let nm v bdy) = do
>     v' <- interp' v
>     local (extendEnv nm v') $ interp' bdy

> interp' (I.Seq (I.LetDecl nm e) b) = do
>     v <- interp' e
>     local (extendEnv nm v) $ interpStmt' b
> interp' (I.Seq a@(I.StExpr {}) b) = interpStmt' a >> interpStmt' b

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
