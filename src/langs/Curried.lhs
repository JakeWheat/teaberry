

curried functions, including binary operators

e.g.

f(3,_)
_ + _
a == _

strategy:
find these in the syntax during desugaring app,
turn them into lambdas at this point

------------------------------------------------------------------------------


> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE MultiWayIf #-}

> module Curried (tests) where

> import Text.RawString.QQ

> import qualified Parse2 as P
> import qualified Syntax2 as S
> import qualified Pretty2 as Pr

> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, get, gets,asks, state, put, modify)

> import Control.Monad (when)
>
> import Data.Char (isAlphaNum)

> import Scientific1 (Scientific)
> import Data.List (intercalate)
>
> --import Debug.Trace (trace)

> import Syntax2
> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Data (Data)
> import Control.Exception.Safe (Exception, throwM, catch)

> import qualified TestUtils as T

  
------------------------------------------------------------------------------

interpreter syntax
------------------

> data IExpr = INum Scientific
>            | IText String
>            | ITupleSel [IExpr]
>            | IIden String
>            | IApp IExpr [IExpr]
>            | ILam [String] IExpr
>            | ILet [(String,IExpr)] IExpr
>            | ISeq IExpr IExpr
>            | IIf IExpr IExpr IExpr
>            deriving (Eq, Show, Data)

------------------------------------------------------------------------------

interpreter

evaluate

> evaluateWithChecks :: String -> IO [T.CheckResult]
> evaluateWithChecks src = do
>     let ast = case P.parseModule "" src of
>                   Right (S.Module [] sts) -> sts
>                   Right (S.Module x _) -> error $ "prelude not supported " ++ show x
>                   Left e -> error e
>         ast' = either error id $ runDesugar ast
>         ast'' = simplify ast'
>     when False $ putStrLn (prettyIExpr ast'')     
>     runInterp defaultFFI ast''


---------------------------------------

simplify

> simplify :: IExpr -> IExpr
> simplify = transformBi $ \case
>     ILet bs (ILet bs' x) -> ILet (bs ++ bs') x
>     x1 -> x1

values
------

> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | TupleV [Value]
>            | NothingV
>            | FunV [String] IExpr Env
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

---------------------------------------

interpreter types
=================

  
> data InterpreterState =
>     InterpreterState
>     {addedTests :: [Value]
>     ,testResultLog :: [T.TestResultLog]
>     }

> emptyInterpreterState :: InterpreterState
> emptyInterpreterState = InterpreterState [] []

> type Interpreter = RWST Env () InterpreterState IO

> data InterpreterException = ValueException Value
>                           | UserException String
>                           deriving Show

> instance Exception InterpreterException where

> throwInterp :: String -> Interpreter a
> throwInterp s = throwM $ UserException s

> throwInterpE :: InterpreterException -> Interpreter a
> throwInterpE e = throwM e

      
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
>     maybe (throwInterp $ "Identifier not found " ++ nm) pure
>     $ lookup nm (envEnv env)

> addForeignFun :: String -> [String] -> ([Value] -> Interpreter Value) -> Env -> Either String Env
> addForeignFun nm tys f env =
>     pure ((extendEnv [(nm, ForeignFunV nm)] env)
>          {envForeignFuns = ((nm,tys), f) : envForeignFuns env})

> lookupForeignFun :: String -> [String] -> Env -> Interpreter ([Value] -> Interpreter Value)
> lookupForeignFun nm tys env =
>     if | Just f <- lookup (nm,tys) $ envForeignFuns env -> pure f
>        -- well dodgy "generic" functions, only works if all args are any
>        | Just f <- lookup (nm, map (const "any") tys) $ envForeignFuns env -> pure f
>        | otherwise -> throwInterp $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")"


> addForeignFun' :: String -> ([String], ([Value] -> Interpreter Value)) -> Env -> Either String Env
> addForeignFun' nm (tys, f) env = addForeignFun nm tys f env

> addForeignFuns' :: [(String, ([String], ([Value] -> Interpreter Value)))] -> Env -> Either String Env
> addForeignFuns' [] env = pure env
> addForeignFuns' ((x,y):xs) env = do
>     env' <- addForeignFun' x y env
>     addForeignFuns' xs env'
>     

ffi catalog
  
> defaultFFI :: Env
> defaultFFI = either error id $ addForeignFuns' (
>    [("+", binaryOp unwrapNum unwrapNum wrapNum (+))
>    ,("*", binaryOp unwrapNum unwrapNum wrapNum (*))
>    ,("==", binaryOp anyIn anyIn wrapBool (==))
>    ,("+", binaryOp unwrapText unwrapText wrapText (++))

>    ,("add-tests", unarySimple "function" addTests)
>    ,("log-check-block", binaryOp unwrapNum unwrapText id logCheckBlock)
>    ,("log-test-pass", binaryOp unwrapNum unwrapText id logTestPass)
>    ,("log-test-fail", ternaryOp unwrapNum unwrapText unwrapText id logTestFail)
>    ,("raise", unaryOp anyIn id raise)
>    ,("tostring", unaryOp anyIn pure tostring)
>    ,("to-string", unaryOp anyIn pure tostring)

>    ,("torepr", unaryOp anyIn pure torepr)
>    ,("to-repr", unaryOp anyIn pure torepr)
     
>    ]

>     )

>    emptyEnv

> raise :: Value -> Interpreter Value
> raise v = throwM $ ValueException v

> tostring :: Value -> Value
> tostring x@(TextV {}) = x
> tostring x = torepr x

  
> torepr :: Value -> Value
> torepr x = TextV $ torepr' x

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

> runInterp :: Env -> IExpr -> IO [T.CheckResult]
> runInterp env expr =
>     fst <$> evalRWST scriptWithTestStuff env emptyInterpreterState
>   where
>     scriptWithTestStuff = do
>         _ <- interp expr
>         runAddedTests

> interp :: IExpr -> Interpreter Value

> interp (IApp (IIden "catch") [e, c]) = interp e `catch` (\case
>     ValueException v -> do
>         cf <- interp c
>         app cf [v]
>     -- is rethrowing an exception like this bad in haskell?
>     s -> throwInterpE s)


> interp (INum n) = pure (NumV n)
> interp (IText t) = pure (TextV t)
> interp (ITupleSel es) = TupleV <$> mapM interp es
> interp (IIden i) = do
>     env <- ask
>     envLookup i env
> interp (IApp f es) = do
>     fv <- interp f
>     vs <- mapM interp es
>     app fv vs
> interp (ILam ps e) = do
>     env <- ask
>     pure $ FunV ps e env
> interp (ILet bs e) = do
>     let newEnv [] = interp e
>         newEnv ((b,ex):bs') = do
>             v <- interp ex
>             local (extendEnv [(b,v)]) $ newEnv bs'
>     newEnv bs

> interp (ISeq a b) = interp a *> interp b

> interp (IIf c t e) = do
>     c' <- interp c
>     case c' of
>         BoolV True -> interp t
>         BoolV False -> interp e
>         _ -> throwInterp $ "expected bool in if test, got " ++ show c'

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
>         _ -> throwInterp "non function value in app position"
>   where
>     safeZip ps xs | length xs == length ps = pure $ zip ps xs
>                   | otherwise = throwInterp $ "wrong number of args to function"


------------------------------------------------------------------------------

desugar
-------

> type Desugarer = RWST DesugarReader () DesugarState (Except String)

> data DesugarReader = DesugarReader {currentCheckBlockIDName :: Maybe String}

> data DesugarState = DesugarState {uniqueCtr :: Int
>                                  ,nextUnusedCheckBlockID :: Int
>                                  ,nextAnonymousBlockNumber :: Int}

> startingDesugarState :: DesugarState
> startingDesugarState = DesugarState 0 0 0

> makeUniqueVar :: String -> Desugarer String
> makeUniqueVar pref = state $ \s ->
>     let suff = uniqueCtr s
>     in (pref ++ "-" ++ show suff
>        ,s {uniqueCtr = suff + 1})


--------------------------------------

desugaring code

> runDesugar :: [Stmt] -> Either String IExpr
> runDesugar stmts =
>     fst <$> runExcept (evalRWST (desugarStmts stmts) (DesugarReader Nothing) startingDesugarState)

> throwDesugar :: String ->  Expr
> throwDesugar e = App (Iden "raise") [Text e]
  
 
> desugar :: Expr -> Desugarer IExpr
> desugar (Block s) = desugarStmts s
> desugar (App (Iden "is") [a,b]) = do
>     uniqueV0 <- makeUniqueVar "is-v0"
>     uniqueV1 <- makeUniqueVar "is-v1"
>     uniqueName <- makeUniqueVar "testname"
>     checkBlockIDName <- (maybe (throwDesugar "'is' test outside check block") Iden)
>                         <$> (asks currentCheckBlockIDName)
>     desugarStmts
>               [letDecl uniqueV0 a
>               ,letDecl uniqueV1 b
>               ,letDecl uniqueName (Text $ Pr.prettyExpr a ++ " is " ++ Pr.prettyExpr b)
>               ,StExpr $
>                If [(eqIdens uniqueV0 uniqueV1,
>                    appI "log-test-pass" [checkBlockIDName
>                                         ,Iden uniqueName])]
>                   (Just $ appI "log-test-fail"
>                    [checkBlockIDName
>                    ,Iden uniqueName
>                    ,Text "Values not equal:\n"
>                        `plus` appI "torepr" [Iden uniqueV0]
>                        `plus` Text "\n"
>                        `plus` appI "torepr" [Iden uniqueV1]])]
>   where
>     plus c d = appI "+" [c, d]
>     eqIdens c d = appI "==" [Iden c, Iden d]
>     appI i es = App (Iden i) es
> desugar (App (Iden "raises") [e0, e1]) = do
>     desugar (App (Iden "raises-satisfies")
>            [e0
>            ,lam ["a"] $ (App (Iden "tostring") [Iden "a"] `eq` e1)])
>   where
>     eq a b = App (Iden "==") [a,b]

> desugar x@(App (Iden "raises-satisfies") [e0,e1]) =
>   desugar =<< desugarRaises  (Pr.prettyExpr x) e0 e1

> desugar (Num i) = pure $ INum i
> desugar (Text i) = pure $ IText i
> desugar (TupleSel fs) = ITupleSel <$> mapM desugar fs
> desugar (Iden i) = pure $ IIden i



> desugar (App f as) = do
>     -- look for curried apps
>     (as', las) <- g [] [] as
>     case las of
>         [] -> IApp <$> desugar f <*> mapM desugar as
>         _ -> desugar (lam las (App f as'))
>   where
>     g as' las [] = pure (reverse as', reverse las)
>     g as' las (Iden "_":xs) = do
>         n <- makeUniqueVar "c"
>         g (Iden n:as') (n:las) xs
>     g as' las (x:xs) = do
>         g (x:as') las xs

> desugar (Lam ns e) = ILam (map f ns) <$> desugar e
>     where
>       f (PatName _ n) = n
> desugar (Let bs e) = do
>     let f (PatName _ n, v) = (n,) <$> desugar v
>     ILet <$> mapM f bs <*> desugar e

> desugar x@(RecordSel {}) = desugar $ throwDesugar ("records not supported: " ++ show x)
  


> desugar (If [(c,t)] (Just e)) =
>     IIf <$> desugar c <*> desugar t <*> desugar e

> desugar x@(If {}) = desugar $ (throwDesugar $ "only if with one branch and else supported: " ++ show x)

> desugar (Parens e) = desugar e
> desugar (S.BinOp e f e1) = desugar $ App (Iden f) [e,e1]
> desugar x = desugar $ throwDesugar ("syntax not supported " ++ show x)

> letDecl :: String -> Expr -> Stmt
> letDecl n v = LetDecl (PatName NoShadow n) v

> lam :: [String] -> Expr -> Expr
> lam ps e = Lam (map f ps) e
>   where
>     f i = PatName NoShadow i
  
> desugarStmts :: [Stmt] -> Desugarer IExpr

> desugarStmts (Check nm bdy : es) = do
>     uniqueCheckBlockIDVarName <- makeUniqueVar "check-block-id"
>     desugaredCheck <- local (\x -> x {currentCheckBlockIDName = Just uniqueCheckBlockIDVarName}) $ do
>       s <- get
>       put $ s {nextUnusedCheckBlockID = nextUnusedCheckBlockID s + 1}
>       let uniqueCheckBlockID = Num $ fromIntegral $ nextUnusedCheckBlockID s
>       blockName <- maybe getAnonBlockName pure nm
>       desugar $
>           appI "add-tests"
>           [Lam [] (Block $
>                    [letDecl uniqueCheckBlockIDVarName uniqueCheckBlockID
>                    ,StExpr $ appI "log-check-block"
>                                [Iden uniqueCheckBlockIDVarName
>                                ,Text blockName]]
>                    ++ bdy)]
>     case es of
>         [] -> pure desugaredCheck
>         _ -> ISeq desugaredCheck <$> desugarStmts es
>   where
>     appI i as = App (Iden i) as

> desugarStmts [] = desugar $ throwDesugar "empty block"
  
> desugarStmts [StExpr e] = desugar e
> desugarStmts [LetDecl {}] = desugar $ throwDesugar "block ends with let"
  
> desugarStmts (LetDecl n v : es) =
>     desugar (Let [(n,v)] $ Block es)

> desugarStmts (StExpr e : es) =
>     ISeq <$> desugar e <*> desugar (Block es)

> desugarStmts (x : xs) = desugarStmts (StExpr (throwDesugar ("unsupported statement: " ++ show x)) : xs)


> desugarRaises :: String -> Expr -> Expr -> Desugarer Expr
> desugarRaises syn e e1 = do
>     let failMsg = "The test operation raises-satisfies failed for the test "
>                   ++ Pr.prettyExpr e1
>     checkBlockIDName <- (maybe (lift $ throwE "'is' test outside check block") pure)
>                         =<< (asks currentCheckBlockIDName)
>     
>     nameit <- makeUniqueVar "isname"
>     v1 <- makeUniqueVar "isv1"
>     pure $ App (Iden "catch")[
>         (Block [StExpr e
>                ,LetDecl (patName nameit) $ Text syn
>                ,StExpr $ appx "log-test-fail" [Iden checkBlockIDName
>                                               ,Iden nameit
>                                               ,Text "No exception raised"]])
>         ,(lam ["a"] $ Block
>          [LetDecl (patName v1) e1
>          ,LetDecl (patName nameit) $ Text syn
>          ,StExpr $ If [(appx v1 [Iden "a"]
>                        ,appx "log-test-pass" [Iden checkBlockIDName, Iden nameit])]
>                    $ Just $ appx "log-test-fail"
>                             [Iden checkBlockIDName
>                             ,Iden nameit
>                             ,Text failMsg `plus`
>                              Text ", value was " `plus`
>                              appx "torepr" [Iden "a"]]])]
>   where
>       plus a b = appx "+" [a,b]
>       appx nm es = App (Iden nm) es
>       patName = PatName NoShadow

------------------------------------------------------------------------------


test infra
==========

> getAnonBlockName :: Desugarer String
> getAnonBlockName = state $ \s ->
>     let blockNo = nextAnonymousBlockNumber s
>     in ("check-block-" ++ show blockNo
>        ,s {nextAnonymousBlockNumber = blockNo + 1})

add-tests(fn)
takes a function value
these functions are run at the end of the script to do the testing

> addTests :: Value -> Interpreter Value
> addTests v = nothingWrapper $ \s -> s {addedTests = v : addedTests s}

log-check-block(unique-block-id, name)
says there is a new test block with a unique id and it's name

> logCheckBlock :: Scientific -> String -> Interpreter Value
> logCheckBlock n nm =
>     nothingWrapper $ \s -> s {testResultLog = T.TestBlock n nm : testResultLog s}

log-test-pass(blockid, text of test)

> logTestPass :: Scientific -> String -> Interpreter Value
> logTestPass n msg =
>     nothingWrapper $ \s -> s {testResultLog = T.TestPass n msg : testResultLog s}

log-test-fail(block,id, text of test, fail message)

> logTestFail :: Scientific -> String -> String -> Interpreter Value
> logTestFail n msg failmsg = nothingWrapper $ \s ->
>     s {testResultLog = T.TestFail n msg failmsg : testResultLog s}

> nothingWrapper :: (InterpreterState -> InterpreterState) -> Interpreter Value
> nothingWrapper f = modify f *> pure NothingV

> runAddedTests :: Interpreter [T.CheckResult]
> runAddedTests = do
>     ts <- reverse <$> gets addedTests
>     mapM_ (\v -> app v []) ts
>     testLog <- reverse <$> gets testResultLog
>     either throwInterp pure $ T.testLogToCheckResults testLog

------------------------------------------------------------------------------

ffi boilerplate
--------------------


> unarySimple :: String -> (Value -> Interpreter Value) -> ([String], [Value] -> Interpreter Value)
> unarySimple ty f = ([ty]
>                  ,\case
>                        [x] -> f x
>                        y -> throwInterp $ "expected 1 arg, got " ++ show y)
>                  

> _unwrapTuple :: (String, Value -> Interpreter [Value])
> _unwrapTuple = ("tuple", \case
>                           TupleV fs -> pure fs
>                           x -> throwInterp $ "type: expected tuple, got " ++ show x)

> unwrapNum :: (String, Value -> Interpreter Scientific)
> unwrapNum = ("number", \case
>                           NumV n -> pure n
>                           x -> throwInterp $ "type: expected number, got " ++ show x)

> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n



> _unwrapBool :: (String, Value -> Interpreter Bool)
> _unwrapBool = ("boolean", \case
>                           BoolV n -> pure n
>                           x -> throwInterp $ "type: expected boolean, got " ++ show x)

> wrapBool :: Bool -> Interpreter Value
> wrapBool n = pure $ BoolV n


> unwrapText :: (String, Value -> Interpreter String)
> unwrapText = ("text", \case
>                           TextV n -> pure n
>                           x -> throwInterp $ "type: expected text, got " ++ show x)

> wrapText :: String -> Interpreter Value
> wrapText n = pure $ TextV n

> anyIn :: (String, Value -> Interpreter Value)
> anyIn = ("any", pure)

  
> unaryOp :: (String, Value -> Interpreter a)
>         -> (b -> Interpreter Value)
>         -> (a -> b)
>         -> ([String], [Value] -> Interpreter Value)
> unaryOp unwrap0 wrap f =
>     ([fst unwrap0]
>     ,\as -> case as of
>                 [a] -> do
>                     ax <- (snd unwrap0) a
>                     wrap (f ax)
>                 _ -> throwInterp $ "wrong number of args to function, expected 1, got " ++ show (length as))



> binaryOp :: (String, Value -> Interpreter a)
>          -> (String, Value -> Interpreter b)
>          -> (c -> Interpreter Value)
>          -> (a -> b -> c)
>          -> ([String], [Value] -> Interpreter Value)
> binaryOp unwrap0 unwrap1 wrap f =
>     ([fst unwrap0, fst unwrap1]
>     ,\as -> case as of
>                 [a,b] -> do
>                     ax <- (snd unwrap0) a
>                     bx <- (snd unwrap1) b
>                     wrap (f ax bx)
>                 _ -> throwInterp $ "wrong number of args to function, expected 2, got " ++ show (length as))

> ternaryOp :: (String, Value -> Interpreter a)
>           -> (String, Value -> Interpreter b)
>           -> (String, Value -> Interpreter c)
>           -> (d -> Interpreter Value)
>           -> (a -> b -> c -> d)
>           -> ([String], [Value] -> Interpreter Value)
> ternaryOp unwrap0 unwrap1 unwrap2 wrap f =
>     ([fst unwrap0, fst unwrap1, fst unwrap2]
>     ,\as -> do
>             case as of
>                 [a,b,c] -> do
>                     ax <- (snd unwrap0) a
>                     bx <- (snd unwrap1) b
>                     cx <- (snd unwrap2) c
>                     wrap (f ax bx cx)
>                 _ -> throwInterp $ "wrong number of args to function, expected 4, got " ++ show (length as))


------------------------------------------------------------------------------

pretty interpreter syntax
=========================

  
> prettyIExpr :: IExpr -> String
> prettyIExpr x = Pr.prettyExpr $ unconvI x

> unconvI :: IExpr -> Expr

> unconvI (INum n) = Num n
> unconvI (IText n) = Text n
> unconvI (ITupleSel n) = TupleSel $ map unconvI n

> unconvI (IIden s) = Iden s
> unconvI (IApp (IIden e) [a,b]) | isOp e = BinOp (unconvI a) e (unconvI b)
>   where isOp x = not $ any (\z -> isAlphaNum z || z `elem` "_") x
> 
> unconvI (IApp e fs) = App (unconvI e) $ map unconvI fs
> unconvI (ILam ns e) = Lam (map unconvIPatName ns) $ unconvI e
> unconvI (ILet bs e) = Let (map (uncurry unconvIBinding) bs) (unconvI e)
> unconvI (ISeq a b) = Block $ map (StExpr . unconvI) [a, b]
> unconvI (IIf c t e) = If [(unconvI c, unconvI t)] (Just $ unconvI e)

> unconvIBinding :: String -> IExpr -> (PatName, Expr)
> unconvIBinding n v = (unconvIPatName n, unconvI v)

> unconvIPatName :: String -> PatName
> unconvIPatName n = PatName NoShadow n

  
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


check "catches":
  add1 = lam(a): a + 1 end
  catch(1, add1) is 1
  catch(raise(2), add1) is 3
  raise("hello") raises "hello"
  raise("hello") raises-satisfies lam(x): x == "hello" end

end

check "curried":

  f = lam(x,y): x + y end

  g = f(3,_)
  h = _ + _
  a = 5
  j = a == _

  g(4) is 7
  f(_,1)(2) is 3
  h(0,2) is 2
  j(5) is true
  j(4) is false

end

\end{code}
>    |]


      
> tests :: T.TestTree
> tests = T.makeTestsIO "simple base" $ (Right <$> evaluateWithChecks simpleTestScript)
