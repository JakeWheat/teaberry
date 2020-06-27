 
Anomaly testing version one:

start with a simple language

do tests for every way you can mistakenly use the language (excluding
parse errors)

add tests for all of these to see them

a big part of the goal is to develop the infrastructure to do this
kind of testing

=============================================================================

imports

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE MultiWayIf #-}

> module Anomaly1 (tests) where

> import Text.RawString.QQ

> import qualified Parse2 as P
> import qualified Syntax2 as S
> import qualified Pretty2 as Pr

> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, get, gets,asks, state, put, modify)

> import Control.Monad (when, void)
>
> import Data.Char (isAlphaNum)

> import Scientific1 (Scientific)
> import Data.List (intercalate, sortOn, nub, (\\), find)
>
> --import Debug.Trace (trace)

> import Syntax2
> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Data (Data)
> import Control.Exception.Safe (Exception, throwM, catch)

> import qualified TestUtils as T

  
------------------------------------------------------------------------------

interpreter syntax
==================


> data IExpr
>     = INum Scientific
>     | IText String
>     | IVariantSel String [(String,IExpr)]
>     | IIden String
>     | IApp IExpr [IExpr]
>     | ILam [String] IExpr
>     | ILet [(String,IExpr)] IExpr
>     | ISeq IExpr IExpr
>     | IIf [(IExpr,IExpr)] (Maybe IExpr)
>     deriving (Eq, Show, Data)

------------------------------------------------------------------------------

interpreter

evaluate

> parse :: String -> Either String [Stmt]
> parse src = case P.parseModule "" src of
>     Right (S.Module [] sts) -> pure sts
>     Right (S.Module x _) -> Left $ "prelude not supported " ++ show x
>     Left e -> Left e
  
> evaluateWithChecks :: String -> IO [T.CheckResult]
> evaluateWithChecks src = do
>     let ast = either error id $ parse (myFirstPrelude ++ src)
>         ast' = either error id $ runDesugar ast
>         ast'' = simplify ast'
>     when False $ putStrLn (prettyIExpr ast'')     
>     runInterp defaultFFI ast''
>   where
>      myFirstPrelude = [r|
\begin{code} 

data List:
  | empty
  | link(first, rest)
end

data Nothing:
   | nothing
end

data Language-error:
  | unbound-identifier(i)
  | not-function-value(v)
  | function-wrong-num-args(e,r)
  | function-wrong-types(e,r)
  | duplicate-name(a)
  | expected-type(e,r)
  | no-branches-satisfied
  | only-one-branch
  | empty-block
  | block-ends-with-let
end


\end{code}
>           |]

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
>            | VariantV String -- variant name
>                       [(String,Value)] -- fields
>            | FunV [String] IExpr Env
>            | ForeignFunV String

> valueTypeName :: Value -> String
> valueTypeName (NumV {}) = "Number"
> valueTypeName (TextV {}) = "Text"
> valueTypeName (BoolV {}) = "Boolean"
> valueTypeName (FunV {}) = "Function"
> valueTypeName (VariantV {}) = "Variant" -- or should it be the variant's type name?
> valueTypeName (ForeignFunV {}) = "Foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (VariantV nm fs) = "VariantV " ++ nm ++ "[" ++ intercalate "," (map show fs) ++ "]"
>   show (FunV {}) = "FunV stuff"
>   show (ForeignFunV n) = "ForeignFunV " ++ show n

> instance Eq Value where
>     NumV a == NumV b = a == b
>     BoolV a == BoolV b = a == b
>     TextV a == TextV b = a == b
>     VariantV "record" as == VariantV "record" bs =
>         sortOn fst as == sortOn fst bs
>     VariantV nm fs == VariantV lm gs = (nm,fs) == (lm,gs)
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

> eapp :: String -> [Expr] -> Expr
> eapp f as = App (Iden f) as
  
> throwUnboundIdentifier :: String -> Interpreter a
> throwUnboundIdentifier i = raiseValue $ eapp "unbound-identifier" [Text i]

> raiseValue :: Expr -> Interpreter a
> raiseValue e = do
>     runAstInterp $ [StExpr $ eapp "raise" [e]]
>     throwInterp "internal error: should have raised"

todo: should this be a tostring, or should it store the actual value
might be better to have the value, just have to port some more of the
runscriptinterp infrastructure
  
> throwNotFunctionValue :: String -> Interpreter a
> throwNotFunctionValue i = raiseValue $ eapp "not-function-value" [Text i]

> throwWrongNumberOfArgs :: Int -> Int -> Interpreter a
> throwWrongNumberOfArgs e g =
>    raiseValue $ eapp "function-wrong-num-args" [Num $ fromIntegral e, Num $ fromIntegral g]

> throwWrongTypes :: [String] -> [String] -> Interpreter a
> throwWrongTypes e g =
>    raiseValue $ eapp "function-wrong-types" [toList e, toList g]
>   where
>     toList = Construct (Iden "list") . map Text

> throwDuplicateName :: String -> Interpreter a
> throwDuplicateName i = raiseValue $ eapp "duplicate-name" [Text i]

> throwExpectedType :: String -> String -> Interpreter a
> throwExpectedType e g = raiseValue $ eapp "expected-type" [Text e, Text g]

> throwNoBranchesSatisfied :: Interpreter a
> throwNoBranchesSatisfied = raiseValue $ Iden "no-branches-satisfied"

> throwOnlyOneBranch :: Interpreter a
> throwOnlyOneBranch = raiseValue $ Iden "only-one-branch"

> runAstInterp :: [Stmt] -> Interpreter ()
> runAstInterp ast = do
>     y <- either throwInterp pure $ runDesugar ast
>     void $ interp (simplify y)

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
>     maybe (throwUnboundIdentifier nm) pure $ lookup nm (envEnv env)
 
> addForeignFun :: String -> [String] -> ([Value] -> Interpreter Value) -> Env -> Either String Env
> addForeignFun nm tys f env =
>     pure ((extendEnv [(nm, ForeignFunV nm)] env)
>          {envForeignFuns = ((nm,tys), f) : envForeignFuns env})

> lookupForeignFun :: String -> [String] -> Env -> Interpreter ([Value] -> Interpreter Value)
> lookupForeignFun nm tys env =
>     if | Just f <- lookup (nm,tys) $ envForeignFuns env -> pure f
>        | Just f <- find (matchesAny . fst) $ envForeignFuns env -> pure $ snd f
>        -- check for matching function, but wrong number of args
>        | Just f <- find ((== nm) . fst . fst) $ envForeignFuns env
>          -> if length (snd $ fst f) == length tys
>             then throwWrongTypes (snd $ fst f) tys
>             else throwWrongNumberOfArgs (length (snd $ fst f)) (length tys)
>        | otherwise -> throwInterp $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")"
>   where
>     matchesAny (nmx,tys') =
>         if nm == nmx && length tys == length tys'
>            && and (zipWith matchEqualOrAny tys tys')
>         then True
>         else False
>     matchEqualOrAny "Any" _ = True          
>     matchEqualOrAny _ "Any" = True          
>     matchEqualOrAny a b = a == b


> addForeignFun' :: String -> ([String], [Value] -> Interpreter Value) -> Env -> Either String Env
> addForeignFun' nm (tys, f) env = addForeignFun nm tys f env

> addForeignFuns' :: [(String, ([String], [Value] -> Interpreter Value))] -> Env -> Either String Env
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


>    ,("torepr", unaryOp anyIn pure torepr)
>    ,("to-repr", unaryOp anyIn pure torepr)
>    ,("tostring", unaryOp anyIn pure tostring)
>    ,("to-string", unaryOp anyIn pure tostring)


>    ,("variant-field-get", binaryOp unwrapText variantIn id variantFieldGet)
>    ,("variant-field-get-ord", binaryOp unwrapNum variantIn id variantFieldGetOrd)
>    ,("safe-variant-name", unaryOp anyIn id safeVariantName)
>    ,("make-variant", binaryOp unwrapText unwrapList id makeVariant)
>    ,("make-variant-0", unaryOp unwrapText id makeVariant0)
>    ,("make-variant-2", ternaryOp anyIn anyIn anyIn id makeVariant2)

>    ,("raise", unaryOp anyIn id raise)

>    ,("add-tests", unarySimple "Function" addTests)
>    ,("log-check-block", binaryOp unwrapNum unwrapText id logCheckBlock)
>    ,("log-test-pass", binaryOp unwrapNum unwrapText id logTestPass)
>    ,("log-test-fail", ternaryOp unwrapNum unwrapText unwrapText id logTestFail)

>    ,("test-string-ffi", unaryOp unwrapText pure (const nothingValueHack))

>    ]

>     )
>    $ emptyEnv {envEnv = [("true", BoolV True)
>                         ,("false", BoolV False)]}

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

> torepr' (VariantV nm []) = nm
> torepr' (VariantV nm fs) =
>     nm ++ "(" ++ intercalate "," (map (torepr' . snd) fs) ++ ")"

> safeVariantName :: Value -> Interpreter Value
> safeVariantName (VariantV x _) = pure $ TextV x
> safeVariantName _ = pure $ nothingValueHack

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

> makeVariant0 :: String -> Interpreter Value
> makeVariant0 vnt = makeVariant vnt []

> makeVariant2 :: Value -> Value -> Value -> Interpreter Value
> makeVariant2 (TextV vnt) f0 f1 = makeVariant vnt [f0,f1]
> makeVariant2 x _ _  = throwInterp $ "makeVariant2: expected text for first arg, got " ++ show x

> makeVariant :: String -> [Value] -> Interpreter Value
> makeVariant vnt listargs = do
>     cd <- mapM unpackTuple listargs
>     pure $ VariantV vnt cd
>   where
>     unpackTuple (VariantV "tuple" [("0", TextV nm), ("1", v)]) = pure (nm,v)
>     unpackTuple (VariantV "tuple" x) = throwInterp $ "value in list in make-variant, expected tuple of name and val, got " ++ show (map (\(_,b) -> torepr' b) x)
>     unpackTuple x = throwInterp $ "expected tuple in make-variant, got " ++ torepr' x


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
> interp (IVariantSel nm fs) = VariantV nm <$> mapM f fs
>   where
>     f (n,v) = (n,) <$> interp v
> interp (IIden a) = do
>     env <- ask
>     envLookup a env
> interp (IApp f es) = do
>     fv <- interp f
>     vs <- mapM interp es
>     app fv vs
> interp (ILam ps e) = do
>     case ps \\ nub ps of
>         (x:_) -> throwDuplicateName x        
>         [] -> pure ()
>     env <- ask
>     pure $ FunV ps e env
> interp (ILet bs e) = do
>     let ps = map fst bs      
>     case ps \\ nub ps of
>         (x:_) -> throwDuplicateName x        
>         [] -> pure ()
>     let newEnv [] = interp e
>         newEnv ((b,ex):bs') = do
>             v <- interp ex
>             local (extendEnv [(b,v)]) $ newEnv bs'
>     newEnv bs

> interp (ISeq a b) = interp a *> interp b

> interp (IIf [_] Nothing) = throwOnlyOneBranch
        
> interp (IIf bs e) = do
>     let f ((c,t):bs') = do
>             c' <- interp c
>             case c' of
>                 BoolV True -> interp t
>                 BoolV False -> f bs'
>                 _ -> throwExpectedType "Boolean" (valueTypeName c')
>                      -- (throwInterp $ "expected bool in if test, got " ++ show c'
>         f [] = case e of
>                    Just x -> interp x
>                    Nothing -> throwNoBranchesSatisfied
>                               --throwInterp $ "no if branches matched and no else:\n"
>                               -- ++ prettyIExpr z
>     f bs

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
>         _ -> throwNotFunctionValue $ torepr' fv
>   where
>     safeZip ps xs | length ps == length xs  = pure $ zip ps xs
>                   | otherwise = throwWrongNumberOfArgs (length ps) (length xs)
>                     --throwInterp $ "wrong number of args to function"


------------------------------------------------------------------------------

desugaring
==========

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
  
> throwDesugarV :: Expr ->  Expr
> throwDesugarV v = App (Iden "raise") [v]
  

  
> desugar :: Expr -> Desugarer IExpr
> desugar (Block []) = desugar $ throwDesugarV (Iden "empty-block")
> desugar (Block [x@LetDecl {}]) =
>     desugarStmts [x, StExpr $ throwDesugarV (Iden "block-ends-with-let")]

> desugar (Block sts) = desugarStmts sts
> desugar (Num i) = pure $ INum i
> desugar (Text i) = pure $ IText i
> desugar (TupleSel fs) =
>     desugar $ VariantSel "tuple" $ zipWith f [(0::Int)..] fs
>   where
>     f n v = (show n,v)
> desugar (VariantSel nm fs) = IVariantSel nm <$> mapM f fs
>   where
>     f (n,v) = (n,) <$> desugar v
> desugar x@(RecordSel {}) = desugar $ throwDesugar ("records not supported: " ++ show x)
> desugar (Iden i) = pure $ IIden i

> desugar (Parens e) = desugar e
> desugar (BinOp e0 f e1) = desugar $ App (Iden f) [e0,e1]
> desugar (UnaryMinus e) = desugar $ App (Iden "-") [e]

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
>            ,lam ["a"] $ (Iden "a" `eq` App (Iden "tostring") [e1])])
>   where
>     eq a b = App (Iden "==") [a,b]

> desugar x@(App (Iden "raises-satisfies") [e0,e1]) =
>   desugar =<< desugarRaises  (Pr.prettyExpr x) e0 e1

> desugar (App (Iden "or") [a,b]) =
>     desugar (If [(a, Iden "true")] (Just b))


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

  
> desugar (If bs e) =
>     IIf <$> mapM f bs <*> case e of
>                              Nothing -> pure Nothing
>                              Just e1 -> Just <$> desugar e1
>   where
>     f (c,t) = (,) <$> desugar c <*> desugar t

> desugar (DotExpr e f) = do
>     desugar (App (Iden "variant-field-get") [Text f, e])

> desugar (Construct (Iden "list") es) = desugar $ f es
>   where
>     f [] = Iden "empty"
>     f (v:vs) = App (Iden "link") [v,f vs]


> desugar (Cases _ty t bs els) = do
>     tv <- makeUniqueVar "casest"
>     bs' <- mapM (f tv) bs
>     desugar $ Let [(patName tv, t)] $ If bs' els
>   where
>     f tv (VariantP _q vnm fnms, e) | Just fnms' <- mapM getPn fnms =
>         let tst = appI "safe-variant-name" [Iden tv] `equals` Text vnm
>             thn = zipWith (\fnm n -> LetDecl (patName fnm) (appI "variant-field-get-ord" [Num $ fromIntegral n, Iden tv]))
>                           fnms' [(0::Int)..]
>         in pure (tst, Block (thn ++ [StExpr e]))
>     f tv (IdenP (PatName NoShadow vnm), e) =
>         let tst = appI "safe-variant-name" [Iden tv] `equals` (Text vnm)
>         in pure (tst, Block [StExpr e])
>     f _ x = lift $ throwE $ "unsupported pattern in cases: " ++ show x
>     getPn (IdenP (PatName _ x)) = Just x
>     getPn _ = Nothing
>     appI i as = App (Iden i) as
>     equals a b = App (Iden "==") [a,b]


> desugar x = desugar $ throwDesugar ("syntax not supported " ++ show x)

> letDecl :: String -> Expr -> Stmt
> letDecl n v = LetDecl (PatName NoShadow n) v

> patName :: String -> PatName
> patName x = PatName NoShadow x

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

> desugarStmts [] = desugar nothingSyntaxHack
> desugarStmts [StExpr e] = desugar e

> desugarStmts (LetDecl (PatName _ n) v : es) = do
>     v' <- desugar v      
>     ILet [(n,v')] <$> desugarStmts es

> desugarStmts (StExpr e : es) = ISeq <$> desugar e <*> desugarStmts es

todo: it ignores ref on fields, instead of giving an error
  
> desugarStmts (DataDecl nm varnts w : es) = do
>     x <- (\a b -> a ++ [b]) <$> mapM makeIsVar varntNms <*> makeIsDat
>     y <- mapM makeVarnt varnts
>     let w' = case w of
>                 Nothing -> []
>                 Just b -> [Check (Just nm) b]

>     desugarStmts (x ++ y ++ w' ++ es)
>  where
>    varntNms = map (\(VariantDecl vnm _) -> vnm) varnts
>    makeIsVar vnm = do
>        arg <- makeUniqueVar "is-x"
>        pure $ LetDecl (patName $ "is-" ++ vnm)
>               (lam [arg] (appI "safe-variant-name" [Iden arg] `equals` Text vnm))
>    makeIsDat = do
>        arg <- makeUniqueVar "is-dat"
>        let varChecks = map (\vnm -> appI ("is-" ++ vnm) [Iden arg]) varntNms
>            f [] = lift $ throwE "make is dat with no variants"  
>            f [a] = pure a
>            f (a:as) = (a `orf`) <$> f as
>        bdy <- f varChecks
>        pure $ LetDecl (patName $ "is-" ++ nm) (lam [arg] bdy)

if there are no args to the variant, it's a binding to a non lambda
value, not a lambda

>    makeVarnt (VariantDecl vnm []) =
>        pure $ LetDecl (patName vnm) (appI "make-variant-0" [Text vnm])

  | pt(x,y) ->
pt = lam (x,y): I.App "make-variant" ["pt",[list: {"x";x},{"y";y}]]

special case to bootstrap the variant constructor for list link

>    makeVarnt (VariantDecl vnm [f0,f1]) =
>        pure $ LetDecl (patName vnm)
>        (lam [snd f0, snd f1] $ appI "make-variant-2"
>            [Text vnm, makeVField $ snd f0, makeVField $ snd f1])


>    makeVarnt (VariantDecl vnm fs) =
>        let fields = listSel $ map (makeVField . snd) fs
>        in pure $ LetDecl (patName vnm)
>                  (lam (map snd fs) $ appI "make-variant" [Text vnm, fields])
>    makeVField f = TupleSel [Text f, Iden f]
>    listSel xs = Construct (Iden "list") xs
>    appI i as = App (Iden i) as
>    equals a b = App (Iden "==") [a,b]
>    orf a b = App (Iden "or") [a,b]


> desugarStmts (x : xs) = desugarStmts (StExpr (throwDesugar ("unsupported statement: " ++ show x)) : xs)

> nothingSyntaxHack :: Expr
> nothingSyntaxHack = VariantSel "nothing" []

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
> nothingWrapper f = modify f *> pure nothingValueHack

> nothingValueHack :: Value
> nothingValueHack = VariantV "nothing" []


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
> _unwrapTuple = ("Tuple", \case
>                           VariantV "tuple" fs -> pure $ map snd fs
>                           x -> throwInterp $ "type: expected tuple, got " ++ show x)

> unwrapNum :: (String, Value -> Interpreter Scientific)
> unwrapNum = ("Number", \case
>                           NumV n -> pure n
>                           x -> throwInterp $ "type: expected number, got " ++ show x)

> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n



> _unwrapBool :: (String, Value -> Interpreter Bool)
> _unwrapBool = ("Boolean", \case
>                           BoolV n -> pure n
>                           x -> throwInterp $ "type: expected boolean, got " ++ show x)

> wrapBool :: Bool -> Interpreter Value
> wrapBool n = pure $ BoolV n


> unwrapText :: (String, Value -> Interpreter String)
> unwrapText = ("Text", \case
>                           TextV n -> pure n
>                           x -> throwInterp $ "type: expected text, got " ++ show x)

> wrapText :: String -> Interpreter Value
> wrapText n = pure $ TextV n

> variantIn :: (String, Value -> Interpreter Value)
> variantIn = ("Variant", \case
>                           x@(VariantV {}) -> pure x
>                           x -> throwInterp $ "type: expected variant, got " ++ show x)

  
> unwrapList :: (String, Value -> Interpreter [Value])
> unwrapList = ("Variant", listToHaskellI)

> listToHaskellI :: Value -> Interpreter [Value]
> listToHaskellI x =
>         maybe (throwInterp $ "bad list value " ++ show x) pure $ listToHaskell x

> listToHaskell :: Value -> Maybe [Value]
>     -- hardcoded understanding of the list data type
>     -- to improve this, find some way to link the ffi boilerplate
>     -- to the list source code
>     -- can look into this again once do the error handling
>     -- or even wait until have types
> listToHaskell (VariantV "empty" []) = pure []
> listToHaskell (VariantV "link" [("first", v), ("rest", vs)]) = (v:) <$> listToHaskell vs
> listToHaskell _ = Nothing


> anyIn :: (String, Value -> Interpreter Value)
> anyIn = ("Any", pure)

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
> unconvI (IVariantSel nm fs) = App (Iden nm) (map f fs)
>   where
>     f (n,v) = TupleSel $ map unconvI [IText n, v]

> unconvI (IIden s) = Iden s
> unconvI (IApp (IIden e) [a,b]) | isOp e = BinOp (unconvI a) e (unconvI b)
>   where isOp x = not $ any (\z -> isAlphaNum z || z `elem` "_") x
> 
> unconvI (IApp e fs) = App (unconvI e) $ map unconvI fs
> unconvI (ILam ns e) = Lam (map unconvIPatName ns) $ unconvI e
> unconvI (ILet bs e) = Let (map (uncurry unconvIBinding) bs) (unconvI e)
> unconvI (ISeq a b) = Block $ map (StExpr . unconvI) [a, b]
> unconvI (IIf bs e) = If (map f bs) (fmap unconvI e)
>   where
>     f (c,t) = (unconvI c, unconvI t)

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

check "simple anomaly":
  #raise(my-check-a("test")) raises-satisfies _ == my-check-a("testa")
  b == 1 raises-satisfies _ == unbound-identifier("b")
  5(1) raises-satisfies _ == not-function-value('5')

  # wrong number of args
  # wrong arg types
  tostring(2,3) raises-satisfies _ == function-wrong-num-args(1,2)
  f = lam(a,b,c): a + 1 end
  f(2,3) raises-satisfies _ == function-wrong-num-args(3,2)

  test-string-ffi(2) raises-satisfies _ == function-wrong-types([list: 'Text'],[list: 'Number'])


  # lam: arg names repeated
  lam(a,b,a): a + b end(3,4,2) raises-satisfies _ == duplicate-name('a')
  # let: arg names repeated
  let a = 3, b = 4, a = 5: a + b end raises-satisfies _ == duplicate-name('a')

  # if -> check type isn't bool
  if 'x': 1 else: 2 end raises-satisfies _ == expected-type('Boolean', 'Text')
  # needs some more work to get the types of variants right
  if false: 1 else if nothing: 2 else: 3 end raises-satisfies _ == expected-type('Boolean', 'Variant')
  # if -> falls through with no else
  if 1 == 2: 1 else if 1 == 3: 2 end raises-satisfies _ == no-branches-satisfied
  # if with only one branch
  if 1 == 1: 1 end raises-satisfies _ == only-one-branch

  # block: an empty block gives an error
  block: end raises-satisfies _ == empty-block

  # sanity check
  block: 3 end is 3

  # let at the end of a block
  block: a = 3 end raises-satisfies _ == block-ends-with-let

  # tests for curried:
  badc = lam(a,b): a + b end
  # wrong number of args
  let g = badc(_): g(3) end raises-satisfies _ == function-wrong-num-args(2,1)

end

\end{code}
>    |]


      
> tests :: T.TestTree
> tests = T.makeTestsIO "anomaly1" $ (Right <$> evaluateWithChecks simpleTestScript)
