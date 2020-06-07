
add letrec* to statements1check

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module Statements1FunRec1 (tests
>                          ) where

> import Text.RawString.QQ

> import qualified Parse as P
> import qualified Syntax as S
> import qualified Pretty as Pr


> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, get, gets, state, put, modify)

> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Data (Data)

  
> import Control.Monad (when)
> import Data.Maybe (isJust, mapMaybe)
>
> import Data.Char (isAlphaNum)

> import Scientific (Scientific, extractInt)
> import Data.List (intercalate)
>
> --import Debug.Trace (trace)

> import qualified TestUtils as T

------------------------------------------------------------------------------

syntax
------

> data Stmt = StExpr Expr
>           | LetDecl String Expr
>           | Check (Maybe String) [Stmt]
>           deriving (Eq, Show, Data)

> data Expr = Num Scientific
>           | Text String
>           | TupleSel [Expr]
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>           | LetRec [(String,Expr)] Expr
>           | Block [Stmt]
>           | Seq Expr Expr
>           | If Expr Expr Expr
>           deriving (Eq, Show, Data)

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

> runDesugar :: Expr -> Either String Expr
> runDesugar expr =
>     fst <$> runExcept (evalRWST (desugar expr) () startingDesugarState)

> desugar :: Expr -> Desugarer Expr

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

> desugar (Block sts) = desugarStmts sts


> desugar (Num i) = pure $ Num i
> desugar (Text i) = pure $ Text i
> desugar (TupleSel fs) = TupleSel <$> mapM desugar fs
> desugar (Iden i) = pure $ Iden i
> desugar (App f as) = App <$> desugar f <*> mapM desugar as
> desugar (Lam ns e) = Lam ns <$> desugar e
> desugar (Let bs e) = do
>     let f (n,v) = (n,) <$> desugar v
>     Let <$> mapM f bs <*> desugar e


> desugar (LetRec bs e) = do
>     desugar (Let (map convLam bs ++ mapMaybe createBind bs) e)
>   where
>     newName = (++"'")
>     bindNames = map fst bs
>     bindNames' = map newName bindNames
>     -- fX = lam (asX): bdyX end -> fX' = lam (f0,f1,...,*asX): bdyX' end
>     convLam (n,Lam as bdy) =
>         (newName n, Lam (bindNames ++ as) $ patchBody bdy)
>     -- fX = bdyX (something not a lam) -> fX = bdyX'
>     -- not sure about this one
>     convLam (n,x) = (newName n, patchBody x)
>     -- fX = lam (asX): bdyX end -> fX = lam(asX): fX'(f0',f1',...,*asX) end
>     createBind (n,Lam as _bdy) =
>         Just (n, Lam as $ App (Iden $ newName n) (map Iden (bindNames' ++ as)))
>     createBind _ = Nothing
>     --bdyN with fN(as) replaced with fN(f0,...fX,*as)
>     patchBody = transformBi $ \case
>         App (Iden f) args | f `elem` bindNames -> App (Iden f) (map Iden bindNames ++ args)
>         x -> x


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
> desugarStmts [x@(LetDecl {})] = desugarStmts [x, StExpr $ Iden "nothing"]

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
>     ,testResultLog :: [T.TestResultLog]
>     }

> emptyInterpreterState :: InterpreterState
> emptyInterpreterState = InterpreterState [] []

> type Interpreter = RWST Env () InterpreterState (Except String)

> throwInterp :: String -> Interpreter a
> throwInterp = lift . throwE
  
> runInterp :: Env -> Expr -> Either String [T.CheckResult]
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

> interp (Block {}) = throwInterp $ "undesugared block passed to interpreter"
> interp (Seq a b) = interp a *> interp b

> interp (If c t e) = do
>     c' <- interp c
>     case c' of
>         BoolV True -> interp t
>         BoolV False -> interp e
>         _ -> throwInterp $ "expected bool in if test, got " ++ show c'

> interp x@(LetRec {}) = throwInterp $ "undesugared letrec in interp " ++ show x

  
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


> evaluateWithChecks :: String -> Either String [T.CheckResult]
> evaluateWithChecks s =  do
>     ast <- parse s
>     ast' <- runDesugar ast
>     runInterp testEnv ast'

------------------------------------------------------------------------------

haskell side testing infrastructure
-----------------------------------

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

--------------------------------------

this is run at the end of the script to run all the saved tests

> runAddedTests :: Interpreter [T.CheckResult]
> runAddedTests = do
>     ts <- reverse <$> gets addedTests
>     mapM_ (\v -> app v []) ts
>     testLog <- reverse <$> gets testResultLog
>     either throwInterp pure $ T.testLogToCheckResults testLog

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
>    ,("-", binaryOp unwrapNum unwrapNum wrapNum (-))

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
>     maybe (throwInterp $ "Identifier not found " ++ nm) pure
>     $ lookup nm (envEnv env)

> addForeignFun :: String -> [String] -> ([Value] -> Interpreter Value) -> Env -> Either String Env
> addForeignFun nm tys f env =
>     pure ((extendEnv [(nm, ForeignFunV nm)] env)
>          {envForeignFuns = ((nm,tys), f) : envForeignFuns env})

> lookupForeignFun :: String -> [String] -> Env -> Interpreter ([Value] -> Interpreter Value)
> lookupForeignFun nm tys env =
>     maybe (throwInterp $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")")
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
>                        y -> throwInterp $ "expected 1 arg, got " ++ show y)
>                  

> unwrapTuple :: (String, Value -> Interpreter [Value])
> unwrapTuple = ("tuple", \case
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
>                 _ -> throwInterp $ "wrong number of args to function, expected 1, got " ++ show (length as))



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
>                 _ -> throwInterp $ "wrong number of args to function, expected 2, got " ++ show (length as))

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
>                 _ -> throwInterp $ "wrong number of args to function, expected 2, got " ++ show (length as))


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
>                 _ -> throwInterp $ "wrong number of args to function, expected 3, got " ++ show (length as))


------------------------------------------------------------------------------

parse
-----

a rule is to try not to do any 'desugaring' here.

It's supposed to be used as if we wrote the parser for each version
from scratch, so the result should be what such a parser would
produce. It's slightly ambiguous because this entire 'parser' is more
like a desugaring process.


> parse :: String -> Either String Expr
> parse src =
>     case P.parseModule "" src of
>       Right (S.Module [] sts) -> convExpr (S.Block sts)
>       Right (S.Module x _) -> Left $ "prelude not supported " ++ show x
>       Left e -> Left e


> convExpr :: S.Expr -> Either String Expr
> convExpr (S.Sel (S.Num x)) = Right $ Num x
> convExpr (S.Sel (S.Text x)) = Right $ Text x
> convExpr (S.Sel (S.TupleSel fs)) = TupleSel <$> mapM convExpr fs
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
> convExpr (S.LetRec bs e) = do
>         bs' <- mapM bf bs
>         e' <- convExpr e
>         Right $ LetRec bs' e'
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
> unconv (Text n) = S.Sel (S.Text n)
> unconv (TupleSel fs) = S.Sel (S.TupleSel $ map unconv fs)
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
> unconv (LetRec bs e) = S.LetRec (map (uncurry unconvBinding) bs) (unconv e)

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

> recursiveTests :: String
> recursiveTests = [r|
\begin{code} 

check "letrec":
  letrec fac = lam(x): if x == 0: 1 else: x * fac(x - 1) end end:
    {fac(4);fac(5);fac(1)} end
    is {24;120;1}

  letrec
    addeven = lam(x): if x == 0: 0 else: x + addodd(x - 1) end end,
    addodd = lam(x):  if x == 0: 0 else: x + addeven(x - 1) end end:
    {addeven(2);addodd(2);addodd(5)}
  end is {3;3;15}

end

\end{code}
>    |]


      
> tests :: T.TestTree
> tests = T.testGroup "Statements1FunRec1" [tests1,tests2]

> tests1 :: T.TestTree
> tests1 = T.makeTestsIO "Statements1FunRec1a" $ pure (evaluateWithChecks simpleTestScript)

> tests2 :: T.TestTree
> tests2 = T.makeTestsIO "Statements1FunRec1b" $ pure (evaluateWithChecks recursiveTests)
