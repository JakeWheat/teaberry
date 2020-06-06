

Implement records as variants properly

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE MultiWayIf #-}

> module Records2 (tests
>                  ) where

> import Text.RawString.QQ

> import qualified Parse as P
> import qualified Syntax as S
> import qualified Pretty as Pr


> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, get, gets, state, put, modify)

> import Control.Monad (when)
> import Data.Maybe (isJust)
>
> import Data.Char (isAlphaNum)

> import Scientific (Scientific, extractInt)
> import Data.List (intercalate, sortOn)
>
> --import Debug.Trace (trace)

> import qualified TestUtils as T

  
------------------------------------------------------------------------------

syntax
------

> data Stmt = StExpr Expr
>           | LetDecl String Expr
>           | Check (Maybe String) [Stmt]
>           | DataDecl String [VariantDecl]
>           deriving (Eq, Show)

> data VariantDecl = VariantDecl String [String]
>                  deriving (Eq,Show) 
  
> data Expr = -- selectors
>             Num Scientific
>           | Text String
>           | TupleSel [Expr]
>           | RecordSel [(String,Expr)]
>           | Construct Expr [Expr]
>             -- other things
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>           | Block [Stmt]
>           | Seq Expr Expr
>           | If [(Expr,Expr)] (Maybe Expr)
>           | DotExpr Expr String
>           | Cases String Expr [(String, [String], Expr)] (Maybe Expr)
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
>                If [(eqIdens uniqueV0 uniqueV1
>                   ,appI "log-test-pass" [Iden checkBlockIDName
>                                         ,Iden uniqueName])]
>                   (Just $ appI "log-test-fail"
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

> desugar (App (Iden "or") [a,b]) =
>     desugar (If [(a, Iden "true")] (Just b))

> desugar (App f as) = App <$> desugar f <*> mapM desugar as
> desugar (Lam ns e) = Lam ns <$> desugar e
> desugar (Let bs e) = do
>     let f (n,v) = (n,) <$> desugar v
>     Let <$> mapM f bs <*> desugar e

> desugar (Seq a b) =
>     Seq <$> desugar a <*> desugar b

> desugar (If bs e) =
>     If <$> mapM f bs <*> case e of
>                              Nothing -> pure Nothing
>                              Just e1 -> Just <$> desugar e1
>   where
>     f (c,t) = (,) <$> desugar c <*> desugar t
  
> desugar (DotExpr e f) = do
>     desugar (App (Iden "variant-field-get") [Text f, e])

hardcoded list constructor. It isn't quite right since it matches
on the literal list, instead of working for any list value here

> desugar (Construct (Iden "list") es) = desugar $ f es
>   where
>     f [] = Iden "empty"
>     f (v:vs) = App (Iden "link") [v,f vs]

> desugar (Construct x _) = lift $ throwE $ "non list construct not supported " ++ show x


> desugar (RecordSel fs) =
>     desugar (App (Iden "make-variant")
>              [Text "record", Construct (Iden "list") $ map f fs])
>   where
>     f (n,v) = TupleSel [Text n, v]

  cases(List) l:
-> assert typeof l is List
  l-unique = l (to evaluate once)
  will need to be able to find the type from the variant name in the runtime
    | empty => true
    -> if variant-name == empty:
         true
    | link(f, r) => 1 + length(r)
    ->
      if variantname == link
        f = l.{0}
        r = l.{1}
        1 + length(r)

> desugar (Cases _ty t bs els) = do
>     tv <- makeUniqueVar "casest"
>     bs' <- mapM (f tv) bs
>     desugar $ Let [(tv, t)] $ If bs' els
>   where
>     f tv (vnm, fnms, e) =
>         let tst = appI "safe-variant-name" [Iden tv] `equals` Text vnm
>             thn = zipWith (\fnm n -> LetDecl fnm (appI "variant-field-get-ord" [Num $ fromIntegral n, Iden tv]))
>                           fnms [(0::Int)..]
>         in pure (tst, Block (thn ++ [StExpr e]))
>     appI i as = App (Iden i) as
>     equals a b = App (Iden "==") [a,b]



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

> desugarStmts [] = desugar (Iden "nothing")
> desugarStmts [StExpr e] = desugar e
> desugarStmts [LetDecl {}] = pure $ Iden "nothing" -- lift $ throwE $ "block ends with let"

> desugarStmts (LetDecl n v : es) = do
>     v' <- desugar v
>     Let [(n,v')] <$> desugarStmts es

> desugarStmts (StExpr e : es) =
>     Seq <$> desugar e <*> desugarStmts es

> desugarStmts (DataDecl nm varnts  : es) = do
>     x <- (\a b -> a ++ [b]) <$> (mapM makeIsVar varntNms) <*> makeIsDat
>     y <- mapM makeVarnt varnts
>     desugarStmts (x ++ y ++ es)
>  where
>    varntNms = map (\(VariantDecl vnm _) -> vnm) varnts
>    makeIsVar vnm = do
>        arg <- makeUniqueVar "is-x"
>        pure $ LetDecl ("is-" ++ vnm)
>               (Lam [arg] (appI "safe-variant-name" [Iden arg] `equals` Text vnm))
>    makeIsDat = do
>        arg <- makeUniqueVar "is-dat"
>        let varChecks = map (\vnm -> appI ("is-" ++ vnm) [Iden arg]) varntNms
>            f [] = lift $ throwE $ "make is dat with no variants"  
>            f [a] = pure a
>            f (a:as) = (a `orf`) <$> f as
>        bdy <- f varChecks
>        pure $ LetDecl ("is-" ++ nm) (Lam [arg] bdy)

if there are no args to the variant, it's a binding to a non lambda
value, not a lambda

>    makeVarnt (VariantDecl vnm []) = pure $ LetDecl vnm (appI "make-variant" [Text vnm, listSel []])

pt = lam (x,y): I.App "make-variant" ["pt",[list: {"x";x},{"y";y}]]

>    makeVarnt (VariantDecl vnm fs) =
>        let fields = listSel $ flip map fs $ \f -> TupleSel [Text f, Iden f]
>        in pure $ LetDecl vnm
>                  (Lam fs $ appI "make-variant" [Text vnm, fields])
>    listSel xs = Construct (Iden "list") xs
                                    

>    appI i as = App (Iden i) as
>    equals a b = App (Iden "==") [a,b]
>    orf a b = App (Iden "or") [a,b]


------------------------------------------------------------------------------

simplify
--------

simplify the ast to help with pretty printing

todo:

let a: let b ...
->
let a,b ...

block
  one expr
end
->
one expr

todo: add more
decide if this can be integrated with desugar instead of being a separate pass

bug: pretty prints \n in strings as an actual newline instead of as '\n'
  it doesn't escape " either


------------------------------------------------------------------------------

values
------

> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | TupleV [Value]
>            | NothingV
>            | VariantV String -- variant name
>                       [(String,Value)] -- fields
>            | FunV [String] Expr Env
>            | ForeignFunV String

> valueTypeName :: Value -> String
> valueTypeName (NumV {}) = "number"
> valueTypeName (TextV {}) = "text"
> valueTypeName (BoolV {}) = "boolean"
> valueTypeName (TupleV {}) = "tuple"
> valueTypeName (VariantV {}) = "variant" -- or should it be the variant's type name?
> valueTypeName (FunV {}) = "function"
> valueTypeName (NothingV) = "nothing"
> valueTypeName (ForeignFunV {}) = "foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (TupleV fs) = "TupleV [" ++ intercalate "," (map show fs) ++ "]"
>   show (VariantV nm fs) = "VariantV " ++ nm ++ "[" ++ intercalate "," (map show fs) ++ "]"
>   show NothingV = "NothingV"
>   show (FunV {}) = "FunV stuff"
>   show (ForeignFunV n) = "ForeignFunV " ++ show n

> instance Eq Value where
>     NumV a == NumV b = a == b
>     TextV a == TextV b = a == b
>     BoolV a == BoolV b = a == b
>     TupleV fs == TupleV gs = fs == gs
>     VariantV "record" as == VariantV "record" bs =
>         sortOn fst as == sortOn fst bs
>     VariantV nm fs == VariantV lm gs = (nm,fs) == (lm,gs)
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

> runInterp :: Env -> Expr -> Either String [T.CheckResult]
> runInterp env expr =
>     fst <$> runExcept (evalRWST scriptWithTestStuff env emptyInterpreterState)
>   where
>     scriptWithTestStuff = do
>         _ <- interp expr
>         runAddedTests

> throwInterp :: String -> Interpreter a
> throwInterp = lift . throwE

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Text t) = pure (TextV t)
> interp (TupleSel es) = TupleV <$> mapM interp es

> 
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

> interp z@(If bs e) = do
>     let f ((c,t):bs') = do
>             c' <- interp c
>             case c' of
>                 BoolV True -> interp t
>                 BoolV False -> f bs'
>                 _ -> throwInterp $ "expected bool in if test, got " ++ show c'
>         f [] = case e of
>                    Just x -> interp x
>                    Nothing -> throwInterp $ "no if branches matched and no else:\n"
>                               ++ pretty z
>     f bs

> interp e@(DotExpr {}) = throwInterp $ "interp: undesugared dotexpr " ++ show e
> interp e@(Cases {}) = throwInterp $ "interp: undesugared cases " ++ show e
> interp e@(Construct {}) = throwInterp $ "interp: undesugared construct " ++ show e
> interp e@(RecordSel {}) = throwInterp $ "interp: undesugared recordsel " ++ show e

  
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
>     --trace (pretty ast') $
>     runInterp testEnv ast'

------------------------------------------------------------------------------

haskell side testing infrastructure
-----------------------------------

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

this is run at the end of the script to run all the saved tests

> runAddedTests :: Interpreter [T.CheckResult]
> runAddedTests = do
>     ts <- reverse <$> gets addedTests
>     mapM_ (\v -> app v []) ts
>     testLog <- reverse <$> gets testResultLog
>     either (lift . throwE) pure $ T.testLogToCheckResults testLog

------------------------------------------------------------------------------

ffi catalog


> testEnv :: Env
> testEnv = either error id $ addForeignFuns' (
>    [("+", binaryOp unwrapNum unwrapNum wrapNum (+))
>    ,("*", binaryOp unwrapNum unwrapNum wrapNum (*))
>    ,("+", binaryOp unwrapText unwrapText wrapText (++))
>    ,("==", binaryOp anyIn anyIn wrapBool (==))

>    ,("add-tests", unaryOp functionIn id addTests)
>    ,("log-check-block", binaryOp unwrapNum unwrapText id logCheckBlock)
>    ,("log-test-pass", binaryOp unwrapNum unwrapText id logTestPass)
>    ,("log-test-fail", ternaryOp unwrapNum unwrapText unwrapText id logTestFail)

>    ,("variant-field-get", binaryOp unwrapText variantIn id variantFieldGet)
>    ,("variant-field-get", binaryOp unwrapText variantIn id variantFieldGet)
>    ,("variant-field-get-ord", binaryOp unwrapNum variantIn id variantFieldGetOrd)
>    ,("safe-variant-name", unaryOp variantIn pure safeVariantName)
>    -- hack to make it work for any data type     
>    ,("safe-variant-name", unaryOp anyIn pure (const $ NothingV))
>    ,("make-variant", binaryOp unwrapText unwrapList id makeVariant)
>    ,("link", binaryOp anyIn anyIn pure listLink)
     
>    ,("torepr", unaryOp anyIn pure torepr)
>    ,("tostring", unaryOp anyIn pure tostring)
>    ]
>     ++ [])
>    $ emptyEnv {envEnv = [("true", BoolV True)
>                         ,("false", BoolV False)
>                         ,("empty", VariantV "empty" [])
>                         ,("nothing", NothingV)]}


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
> torepr' (VariantV nm fs) =
>     nm ++ "(" ++ intercalate "," (map (torepr' . snd) fs) ++ ")"

 
> torepr' NothingV = "nothing"


> tostring :: Value -> Value
> tostring x@(TextV {}) = x
> tostring x = torepr x


> listLink :: Value -> Value -> Value
> listLink a b = VariantV "link" [("first",a),("rest",b)]

> safeVariantName :: Value -> Value
> safeVariantName (VariantV x _) = TextV x
> safeVariantName _ = NothingV

> variantFieldGet :: String -> Value -> Interpreter Value
> variantFieldGet fieldNm v@(VariantV _ fs) =
>     maybe (throwInterp $ "variant field not found " ++ fieldNm ++ ": " ++ torepr' v)
>           pure $ lookup fieldNm fs
> variantFieldGet _ x =
>     throwInterp $ "variant field get called on " ++ torepr' x

> variantFieldGetOrd :: Scientific -> Value -> Interpreter Value
> variantFieldGetOrd fieldIndex v@(VariantV _ fs) =
>     case extractInt fieldIndex of
>         Nothing -> throwInterp $ "variant field get ord passed non integer: " ++ show fieldIndex
>         Just i -> maybe (throwInterp $ "variant field # not found " ++ show fieldIndex ++ ": " ++ torepr' v)
>                   pure (safeIndex (map snd fs) i)
>   where
>     safeIndex [] _ = Nothing
>     safeIndex _ n | n < 0 = Nothing
>     safeIndex (x:_) 0 = Just x
>     safeIndex (_:xs) n = safeIndex xs (n - 1)
     
       
> variantFieldGetOrd _ x =
>     throwInterp $ "variant field get ord called on " ++ torepr' x


> makeVariant :: String -> [Value] -> Interpreter Value
> makeVariant vnt listargs = do
>     cd <- mapM unpackTuple listargs
>     pure $ VariantV vnt cd
>   where
>     unpackTuple (TupleV [TextV nm, v]) = do
>         pure (nm,v)
>     unpackTuple (VariantV "tuple" x) = throwInterp $ "value in list in make-variant, expected tuple of is-ref, name and val, got " ++ show (map (\(_,b) -> torepr' b) x)
>     unpackTuple x = throwInterp $ "expected tuple in make-variant, got " ++ torepr' x

------------------------------------------------------------------------------

env, ffi boilerplate
--------------------

> data Env = Env
>     {envEnv :: [(String,Value)]
>     ,envForeignFuns :: [((String,[String]), [Value] -> Interpreter Value)]}

> emptyEnv :: Env
> emptyEnv = Env
>     {envEnv = []
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

> variantIn :: (String, Value -> Interpreter Value)
> variantIn = ("variant", \case
>                           x@(VariantV {}) -> pure x
>                           x -> throwInterp $ "type: expected variant, got " ++ show x)



> unwrapList :: (String, Value -> Interpreter [Value])
> unwrapList = ("variant", listToHaskellI)
>   where
>     listToHaskellI :: Value -> Interpreter [Value]
>     listToHaskellI x =
>         maybe (throwInterp $ "bad list value " ++ show x) pure $ listToHaskell x

>     listToHaskell :: Value -> Maybe [Value]
>     -- hardcoded understanding of the list data type
>     -- to improve this, find some way to link the ffi boilerplate
>     -- to the list source code
>     -- can look into this again once do the error handling
>     -- or even wait until have types
>     listToHaskell (VariantV "empty" []) = pure []
>     listToHaskell (VariantV "link" [("first", v), ("rest", vs)]) = (v:) <$> listToHaskell vs
>     listToHaskell _ = Nothing

  
> anyIn :: (String, Value -> Interpreter Value)
> anyIn = ("any", pure)

> functionIn :: (String, Value -> Interpreter Value)
> functionIn = ("function", \case
>                           x@(FunV {}) -> pure x
>                           x@(ForeignFunV {}) -> pure x
>                           x -> throwInterp $ "type: expected function, got " ++ show x)
  
> unaryOp :: (String, Value -> Interpreter a)
>         -> (b -> Interpreter Value)
>         -> (a -> b)
>         -> ([String], ([Value] -> Interpreter Value))
> unaryOp unwrap0 wrap f =
>     ([fst unwrap0]
>     ,\as -> do
>             case as of
>                 [a] -> do
>                     ax <- (snd unwrap0) a
>                     wrap (f ax)
>                 _ -> throwInterp $ "wrong number of args to function, expected 1, got " ++ show (length as))



> binaryOp :: (String, Value -> Interpreter a)
>          -> (String, Value -> Interpreter b)
>          -> (c -> Interpreter Value)
>          -> (a -> b -> c)
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

> ternaryOp :: (String, Value -> Interpreter a)
>           -> (String, Value -> Interpreter b)
>           -> (String, Value -> Interpreter c)
>           -> (d -> Interpreter Value)
>           -> (a -> b -> c -> d)
>           -> ([String], ([Value] -> Interpreter Value))
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
>       Right (S.Program x _) -> Left $ "parse: prelude not supported " ++ show x
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
>         pf x = Left $ "parse: unsupported pattern " ++ show x
> convExpr (S.Let bs e) = do
>         bs' <- mapM bf bs
>         e' <- convExpr e
>         Right $ Let bs' e'
>       where
>         bf (S.Binding (S.IdenP _ (S.PatName x)) ex) =
>             (x,) <$> convExpr ex
>         bf x = Left $ "parse: unsupported binding " ++ show x

> convExpr (S.Sel (S.Record fs)) = RecordSel <$> mapM f fs
>   where
>     f (a,b) = (a,) <$> convExpr b

> convExpr (S.Block sts) = Block <$> mapM convSt sts

> convExpr (S.If bs e) = do
>     If <$> mapM f bs <*> maybe (pure Nothing) ((Just <$>) . convExpr) e
>   where
>     f (c,t) = (,) <$> convExpr c <*> convExpr t

> convExpr (S.DotExpr e f) =
>     flip DotExpr f <$> (convExpr e)

> convExpr (S.Cases ty e bs els) =
>     Cases ty <$> convExpr e <*> mapM f bs <*> maybe (pure Nothing) ((Just <$>) .  convExpr) els
>   where
>     f (S.VariantP (S.PatName vnm) ps, ve)
>         | Right ps' <- mapM unpat ps = (vnm, ps',) <$> convExpr ve
>     f (S.IdenP _ (S.PatName vnm), ve) = (vnm, [],) <$> convExpr ve
>     f x = Left $ "parse: unsupported pattern: " ++ show x
>     unpat (S.IdenP _ (S.PatName x)) = pure x
>     unpat x = Left $ "parse: unsupported pattern: " ++ show x

> convExpr (S.Construct e es) = Construct <$> convExpr e <*> mapM convExpr es

> convExpr x = Left $ "parse: unsupported expression " ++ show x

> convSt :: S.Stmt -> Either String Stmt
> convSt (S.StExpr e) = StExpr <$> convExpr e
> convSt (S.LetDecl (S.Binding (S.IdenP _ (S.PatName nm)) v)) = LetDecl nm <$> convExpr v
> convSt (S.Check nm bdy) = Check nm <$> mapM convSt bdy
> convSt (S.DataDecl nm fs Nothing) =
>     DataDecl nm <$> mapM convVarDecl fs

> convSt x = Left $ "parse: unsupported statement " ++ show x

> convVarDecl :: S.VariantDecl -> Either String VariantDecl
> convVarDecl (S.VariantDecl nm fs) = pure $ VariantDecl nm (map snd fs)

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
> unconv (If bs e) = S.If (map f bs) (fmap unconv e)
>   where
>     f (c,t) = (unconv c, unconv t)
> unconv (DotExpr e f) = S.DotExpr (unconv e) f  
> unconv (Cases ty t bs els) =
>     S.Cases ty (unconv t) (map f bs) (fmap unconv els)
>   where
>     f (n,fs,e) = (S.VariantP (S.PatName n) (map unconvPattern fs), unconv e)
>
> unconv (Construct e es) = S.Construct (unconv e) (map unconv es)  
> unconv (RecordSel fs) = S.Sel (S.Record (map f fs))
>   where
>     f (a,b) = (a,unconv b)

> --unconv x = error $ "unconv: " ++ show x

> unconvStmt :: Stmt -> S.Stmt
> unconvStmt (LetDecl n e) = S.LetDecl (unconvBinding n e)
> unconvStmt (StExpr e) = S.StExpr (unconv e)
> unconvStmt (Check nm bs) = S.Check nm $ map unconvStmt bs
> unconvStmt (DataDecl nm vs) =
>      S.DataDecl nm (map f vs) Nothing
>    where
>      f (VariantDecl vnm fs) = S.VariantDecl vnm $ map (S.Con,) fs

> unconvBinding :: String -> Expr -> S.Binding
> unconvBinding n v = S.Binding (unconvPattern n) (unconv v)

> unconvPattern :: String -> S.Pat
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

check "if":
  a = 4
  if a == 2: 1
  else if a == 3: 2
  else if a == 4: 3
  end is 3
end

data Point:
  | pt(x, y)
end

p1 = pt(1,2)


check:
  is-Point(1) is false
  is-Point({1;3}) is false
  is-Point(p1) is true
  is-pt(p1) is true
  p1.x is 1
  p1.y is 2
end

data Two:
  | pt1(x, y)
  | pt2(x, z)
end

t1 = pt1(1,2)
t2 = pt2(3,4)

pf = lam(a,n):
  cases (Two) a:
    | pt1(x,y) => if n == 0: x else: y end
    | pt2(x,y) => if n == 0: x else: y end
  end
end


check:
  is-Two(t1) is true
  is-Two(t2) is true
  is-Point(t1) is false
  is-pt1(t1) is true
  is-pt2(t1) is false
  is-pt1(t2) is false
  is-pt2(t2) is true
  t1.x is 1
  t1.y is 2
  t2.x is 3
  t2.z is 4

  pf(t1,0) is 1
  pf(t1,1) is 2
  pf(t2,0) is 3
  pf(t2,1) is 4

end


data MyOption:
  | my-none
  | my-some(a)
end

check:
  f = lam(a): cases(MyOption) a:
      | my-none => "none"
      | my-some(b) => "some" + tostring(b)
      | else => a
    end
  end
  x = my-none
  f(x) is "none"
  y = my-some(1)
  f(y) is "some1"
end

data BTree:
  | node(value #|:: Number|#, left #|:: BTree|#, right #|:: BTree|#)
  | leaf(value #|:: Number|#)
# where:

end
check:

  a-btree = node(1, leaf(2), node(3, leaf(4), leaf(5)))

  is-BTree(a-btree) is true
  is-BTree("not-a-tree") is false
  is-BTree(leaf(5)) is true
  is-leaf(leaf(5)) is true
  is-leaf(a-btree) is false
  is-leaf("not-a-tree") is false
  is-node(leaf(5)) is false
  is-node(a-btree) is true
  is-node("not-a-tree") is false

  a-btree.value is 1
  a-btree.left.value is 2
  a-btree.right.value is 3
  a-btree.right.left.value is 4
  a-btree.right.right.value is 5

end



\end{code}
>    |]


> listTests :: String
> listTests = [r|
\begin{code} 


data List:
  | empty
  | link(first, rest)
end

check:
  empty is [list:]
  link(1, empty) is [list: 1]
  link(1, link(2, empty)) is [list: 1,2]

end

check:
  l1 = [list: 1]
  mt = empty
  is-empty(mt) is true
  is-empty(l1) is false
  is-link(mt) is false
  is-link(l1) is true

  is-List(l1) is true
  is-List(mt) is true
end

check:
  l1 = [list: 1,2,3]
  l1.first is 1
  l1.rest is [list: 2,3]
end

check:
  list1 = [list: 2,3]
  list2 = link(1, list1)
  list2 is [list: 1,2,3]

  [list: 1,2,3].first is 1
  [list: 1,2,3].rest  is [list: 2,3]
end

check:
  empty is [list:]
  link(1, empty) is [list: 1]
  link(1, link(2, empty)) is [list: 1,2]
  link(empty, link(empty, [list: ])) is [list: [list:],[list:]]
end

check:
  x = [list:1]
  cases(list) x:
    | empty => true
    | else => false
  end is false

  shadow x = [list:]
  cases(list) x:
    | empty => true
    | else => false
  end is true

  shadow x = [list:1]
  cases(list) x:
    | link(a,b) => block:
        a is 1
        b is empty
        nothing
      end
    | else => nothing
  end
  
end


#|
fun my-is-empty(l):
  cases(List) l:
    | empty => true
    | else => false
  end
end

fun length(l):
  cases(List) l:
    | empty => 0
    | link(f, r) => 1 + length(r)
  end
end

check:
  l1 = empty
  l2 = [list: 1,2,3]
  my-is-empty(l1) is true
  my-is-empty(l2) is false

  length(l1) is 0
  length(l2) is 3
end
|#


\end{code}
>    |]

> recordsTests :: String
> recordsTests = [r|
\begin{code} 


check:
  x = 33
  a = {a: "one", b : 2, c : x }
  a.a is "one"
  a.b is 2
  a.c is 33

  my-obj = {s: "Hello", b: true, n: 42}
  g = lam(thing #|:: {n :: Number}|#)#| -> Number|#:
    thing.n
  end

  g(my-obj) is 42

  # check non ordering of fields in equality

  {a:1, b:2} is { b:2, a:1}



  
end

\end{code}
>    |]


> tests :: T.TestTree
> tests = T.testGroup "records2" [tests1,tests2,tests3]

> tests1 :: T.TestTree
> tests1 = T.makeTestsIO "records2a" $ pure (evaluateWithChecks simpleTestScript)

> tests2 :: T.TestTree
> tests2 = T.makeTestsIO "records2b" $ pure (evaluateWithChecks listTests)

> tests3 :: T.TestTree
> tests3 = T.makeTestsIO "records2c" $ pure (evaluateWithChecks recordsTests)

