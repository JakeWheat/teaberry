
Simplest import with file loader extended to support a repl



> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE MultiWayIf #-}

> module Import4Repl (tests
>                    ,TeaberryHandle
>                    ,newTeaberryHandle
>                    ,runScript
>                    ,runFunction
>                    ,Value
>                    ,valueToString
>                    ) where

> import Text.RawString.QQ

> import qualified Parse as P
> import qualified Syntax as S
> import qualified Pretty as Pr


> import Control.Monad.Trans.Class (lift)
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, get, gets, state, put, modify)
> import Control.Exception.Safe (Exception, throwM, catch)

> import Control.Monad (when)
> import Data.Maybe (isJust, catMaybes)
>
> import Data.Char (isAlphaNum)

> import Scientific (Scientific, extractInt, divideScientific)
> import Data.List (intercalate, sortBy, nubBy)
> import Data.Ord (comparing)
>

> --import Debug.Trace (trace)
> import qualified TestUtils as T

> import Data.IORef (IORef, newIORef, readIORef, writeIORef)

------------------------------------------------------------------------------

"fileloader"
------------


wrapper around evaluate which can follow the imports and load them

has a numpty little mock system, so you can write a set of files in
one place here in the testing, as well as load from disk

> data FileSystemWrapper = FileSystemWrapper {
>     loadFile :: FilePath -> IO String
> }

> makeFileSystemMock :: [(FilePath, String)] -> FileSystemWrapper
> makeFileSystemMock lkp =
>     FileSystemWrapper
>     {loadFile = \fn -> maybe (error $ "file not found in mock: " ++ fn)
>                        pure $ lookup fn lkp}

> makeRealFilesystemReader :: FileSystemWrapper
> makeRealFilesystemReader = FileSystemWrapper {loadFile = readFile}
  
algo:
read script
parse source
pull out the import names: from import statements in the prelude
recursively load each of these
when there is a module that's already been loaded
insert it in the collected list again
then when done: reverse the entire list
  and remove duplicates keeping the first occurance of each
this should be ready now

  
todo: write a version which only reads enough of each file to get the imports
and only loads one file at a time and desugars it?
is this worth it? wait until it becomes a performance issue

todo: pass the top level name as a maybe, because at least some of the
time, the caller of this code has a name for it
and/or create a variation which is passed a filename instead of the source
and reads the starting source from disk here too
  
> loadSourceFiles :: FileSystemWrapper -> String -> Interpreter [(String,Module)]
> loadSourceFiles fsw src = do
>     -- todo: memoize so loading a module twice doesn't read the
>     -- file and parse it twice
>     x <- f "toplevel" src
>     -- reverse the list to put dependencies first
>     -- nub it so it only includes each dependency once
>     pure $ nubBy (\a b -> fst a == fst b) $ reverse x
>   where
>     -- parse the file, get the imports
>     -- recurse on these imports, returning the filename and the
>     -- file contents
>     f nm s = do
>              let ast@(Module ps _) = either error id $ parse s
>                  rs = catMaybes $ map getImp ps
>              ((nm,ast):) <$> (concat <$> mapM g rs)
>     g fn = do
>         x <- liftIO $ (loadFile fsw) fn
>         f fn x
>         
>     getImp (Import fn _) = Just fn
>     getImp _ = Nothing
>         

> evaluate :: Env -> String -> IO (Either String (Value, Env, [T.CheckResult]))
> evaluate = evaluate' makeRealFilesystemReader

 
> evaluate' :: FileSystemWrapper -> Env -> String -> IO (Either String (Value, Env, [T.CheckResult]))
> evaluate' fsw env src =
>     ((Right . fst) <$> evalRWST f env emptyInterpreterState)
>     `catch` (\e -> pure $ Left $ interpreterExceptionToString e)
>   where
>     f :: Interpreter (Value, Env, [T.CheckResult])
>     f = do
>         srcs <- loadSourceFiles fsw src
>         y <- either throwInterp pure $ runDesugar srcs
>         v <- {-trace (pretty y) $ -}interp y
>         t <- runAddedTests
>         case v of
>              TupleV [v', VariantV "record" e] -> pure (v',env {envEnv = e}, t)
>              _ -> throwInterp $ "expected 2 element tuple, second one record, got " ++ torepr' v

todo: this record is for preserving the env between calls from the embedded api
combine it with the implementation for provides?

------------------------------------------------------------------------------

syntax
------

  
> data Module = Module [PreludeStmt] [Stmt]
>             deriving (Eq, Show)

> data PreludeStmt = Import String String
>                  | Include String [(String, String)]
>                  deriving (Eq, Show)

import a as b
include from X: a as a, b as b end

> data Stmt = StExpr Expr
>           | LetDecl String Expr
>           | Check (Maybe String) [Stmt]
>           | VarDecl String Expr
>           | SetVar String Expr
>           | DataDecl String [VariantDecl]
>           | SetRef Expr [(String,Expr)]
>           deriving (Eq, Show)

> data VariantDecl = VariantDecl String [(Ref,String)]
>                  deriving (Eq,Show) 

> data Ref = Ref | Con
>          deriving (Eq,Show) 

  
> data Expr = -- selectors
>             Num Scientific
>           | Text String
>           | TupleSel [Expr]
>           | ListSel [Expr]
>           | RecordSel [(String,Expr)]
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
>           | Box Expr
>           | SetBox Expr Expr
>           | UnboxRef Expr String
>           | Unbox Expr
>           deriving (Eq, Show)

------------------------------------------------------------------------------

embedded api
------------

> data TeaberryHandle = TeaberryHandle
>     {henv :: IORef Env}
  
> newTeaberryHandle :: IO TeaberryHandle
> newTeaberryHandle = do
>     x <- newIORef defaultEnv
>     pure $ TeaberryHandle x

> runScript :: TeaberryHandle -> [(String,Value)] -> String -> IO (Either String Value)
> runScript h lenv src = do
>     enx <- readIORef (henv h)
>     let en = extendEnv lenv enx
>     rs <- evaluate en src
>     case rs of
>         Left e -> pure $ Left e
>         Right (v,en',t) -> do
>             case t of
>                 [] -> pure ()
>                 _ -> putStrLn $ T.renderCheckResults t
>             writeIORef (henv h) en'
>             pure $ Right v

> runFunction :: TeaberryHandle -> String -> [Value] -> IO (Either String Value)
> runFunction h f as = do
>     -- how to give the args unique names? or just use shadow?
>     -- (and the function)
>     v <- runScript h [] f
>     case v of
>         Left e -> pure $ Left e
>         Right v' -> do
>             let as' = zipWith (\i x -> ("aaa-" ++ show i, x)) [(0::Int)..] as
>             runScript h (("fff", v'):as') $ "fff(" ++ intercalate "," (map fst as') ++ ")"

> valueToString :: Value -> Maybe String
> valueToString v = case v of
>     NothingV -> Nothing
>     _ -> Just $ torepr' v

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

> runDesugar :: [(String,Module)] -> Either String Expr
> runDesugar srcs = fst <$> runExcept (evalRWST (f [] [] =<< addv srcs) () startingDesugarState)
>   where
>     addv [(nm,Module p stmts)] = do
>         stmts' <- addReturnEnv stmts
>         pure [(nm,Module p stmts')]
>     addv [] = pure []
>     addv (m:ms) = (m:) <$> addv ms
>     f _ desugaredModules [] = g $ reverse desugaredModules
>     -- should really learn how to use folds better
>     -- I think it would make the code more regular and
>     -- quicker to understand/review
>     f mns desugaredModules ((n,m):ms) = do
>         (mns', dsm) <- desugarModule mns n m
>         f (mns' ++ mns) (dsm:desugaredModules) ms
>     g :: [(String,Expr)] -> Desugarer Expr          
>     g [] = lift $ throwE $ "empty list of modules"
>     g [(_,e)] = pure e
>     g ((n,e):es) = Let [(n,e)] <$> g es

> addReturnEnv :: [Stmt] -> Desugarer [Stmt]
> addReturnEnv [] = pure [StExpr $ TupleSel [Iden "nothing", App (Iden "env-to-record") []]]
> addReturnEnv [StExpr x] = do
>     z <- makeUniqueVar "z"
>     pure [LetDecl z x
>          ,StExpr $ TupleSel [Iden z, App (Iden "env-to-record") []]]
> addReturnEnv (x:xs) = do
>     (x:) <$> addReturnEnv xs

  
> desugarModule :: [(String,String)] -> String -> Module -> Desugarer ([(String,String)], (String,Expr))
> desugarModule otherModuleNames moduleName (Module ps xs) = do
>     uniqueModuleName <- makeUniqueVar moduleName
>     ps' <- concat <$> mapM (desugarPreludeStmt otherModuleNames) ps
>     -- make a block with the last statement converting the env into a record
>     -- I can't believe this cheapo trick works
>     -- hack on a hack - if this is the 'last' module, it will
>     -- already have something here with env-to-record, don't replace it
>     let xss = ps' ++ case xs of
>                 (_:_) | StExpr (TupleSel [_, App (Iden "env-to-record") _]) <- last xs -> xs
>                 _ -> xs ++ [StExpr $ App (Iden "env-to-record") []]
          
>     xs' <- desugarStmts xss
>     pure ([(moduleName,uniqueModuleName)], (uniqueModuleName, xs'))

> desugarPreludeStmt :: [(String,String)] -> PreludeStmt -> Desugarer [Stmt]
> desugarPreludeStmt mp (Import a b) = do
>     srcName <- maybe (lift $ throwE $ "module not found in module name list: " ++ a ++ "\n" ++ show mp)
>                pure $ lookup a mp
>     pure [LetDecl b (Iden srcName)]

> desugarPreludeStmt mp (Include nm is) = do
>     srcName <- maybe (lift $ throwE $ "module not found in module name list: " ++ nm ++ "\n" ++ show mp)
>                pure $ lookup nm mp
>     pure $ flip map is $ \(n1,n2) -> LetDecl n2 (DotExpr (Iden srcName) n1)

> desugar :: Expr -> Desugarer Expr

> desugar (Block sts) = desugarStmts sts
> desugar (Num i) = pure $ Num i
> desugar (Text i) = pure $ Text i
> desugar (TupleSel fs) = TupleSel <$> mapM desugar fs
> desugar (RecordSel fs) = RecordSel <$> mapM f fs
>   where
>     f (a,b) = (a,) <$> desugar b
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

> desugar (ListSel vs) = ListSel <$> mapM desugar vs

> desugar (Box v) = Box <$> desugar v

no idea if this is good or not

> desugar (SetBox i@(Iden {}) v) = SetBox i <$> desugar v
> desugar (SetBox b v) = SetBox <$> desugar b <*> desugar v

> desugar (Cases _ty t bs els) = do
>     tv <- makeUniqueVar "casest"
>     bs' <- mapM (f tv) bs
>     desugar $ Let [(tv, t)] $ If bs' els
>   where
>     f tv (vnm, fnms, e) =
>         let tst = appI "safe-variant-name" [Iden tv] `equals` Text (dropQualifiers vnm)
>             thn = zipWith (\fnm n -> LetDecl fnm (appI "variant-field-get-ord" [Num $ fromIntegral n, Iden tv]))
>                           fnms [(0::Int)..]
>         in pure (tst, Block (thn ++ [StExpr e]))
>     appI i as = App (Iden i) as
>     equals a b = App (Iden "==") [a,b]

> desugar (UnboxRef e n) = desugar (Unbox (DotExpr e n))

> desugar (Unbox x) = Unbox <$> desugar x

  
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

> desugarStmts (VarDecl n v : es) = do
>     v' <- Box <$> desugar v
>     desugarStmts (LetDecl n v' : es)


> desugarStmts (StExpr e : es) =
>     Seq <$> desugar e <*> desugarStmts es

> desugarStmts (SetVar n v : es) = do
>     desugarStmts (StExpr (SetBox (Iden n) v) : es)


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

>    makeVarnt (VariantDecl vnm []) = pure $ LetDecl vnm (appI "make-variant" [Text vnm, ListSel []])

  | pt(x,y) ->
pt = lam (x,y): I.App "make-variant" ["pt",[list: {false, "x";x},{false, "y";y}]]

  | pt(ref x,y) ->
pt = lam (x,y): I.App "make-variant" ["pt",[list: {true, "x";x},{false, "y";y}]]
  

>    makeVarnt (VariantDecl vnm fs) =
>        let fields = ListSel $ map makeVField fs
>        in pure $ LetDecl vnm
>                  (Lam (map snd fs) $ appI "make-variant" [Text vnm, fields])
>    makeVField (x,f) = TupleSel [case x of
>                                     Ref -> Iden "true"
>                                     Con -> Iden "false"
>                                ,Text f, Iden f]
>    appI i as = App (Iden i) as
>    equals a b = App (Iden "==") [a,b]
>    orf a b = App (Iden "or") [a,b]

SetRef Expr [(String,Expr)]

a!{a:1, b:2}
->
setbox(a.a, 1)
setbox(a.b, 2)
  
> desugarStmts (SetRef e fs : es) = do
>     enm <- makeUniqueVar "setref"
>     let sts = LetDecl enm e
>         refSets = flip map fs $ \(n,v) -> SetBox (DotExpr (Iden enm) n) v
>     desugarStmts (sts : (map StExpr refSets ++ es))

  
------------------------------------------------------------------------------

values
------

> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | TupleV [Value]
>            | ListV [Value]
>            | NothingV
>            | VariantV String -- variant name
>                       [(String,Value)] -- fields
>            | BoxV Int
>            | FunV [String] Expr Env
>            | ForeignFunV String

value type name is used for looking up overloaded foreign functions

> valueTypeName :: Value -> String
> valueTypeName (NumV {}) = "number"
> valueTypeName (TextV {}) = "text"
> valueTypeName (BoolV {}) = "boolean"
> valueTypeName (TupleV {}) = "tuple"
> valueTypeName (ListV {}) = "list"
> valueTypeName (VariantV {}) = "variant" -- or should it be the variant's type name?
> valueTypeName (NothingV) = "nothing"
> valueTypeName (BoxV {}) = "box"
> valueTypeName (FunV {}) = "function"
> valueTypeName (ForeignFunV {}) = "foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (TupleV fs) = "TupleV [" ++ intercalate "," (map show fs) ++ "]"
>   show (ListV fs) = "[list: " ++ intercalate "," (map show fs) ++ "]"
>   show (VariantV nm fs) = nm ++ "(" ++ intercalate "," (map show fs) ++ ")"
>   show NothingV = "NothingV"
>   show (BoxV n) = "BoxV " ++ show n
>   show (FunV {}) = "FunV stuff"
>   show (ForeignFunV n) = "ForeignFunV " ++ show n

> instance Eq Value where
>     NumV a == NumV b = a == b
>     BoolV a == BoolV b = a == b
>     TextV a == TextV b = a == b
>     TupleV fs == TupleV gs = fs == gs
>     VariantV nm fs == VariantV lm gs = (nm,fs) == (lm,gs)
>     NothingV == NothingV = True
>     _ == _ = False

------------------------------------------------------------------------------

store
-----

holds the values of variables

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
> fetchStore i (Store xs) = maybe (throwInterp $ "invalid fetch on store: " ++ show i) pure
>                           $ lookup i xs

> updateISStore :: (Store -> Store) -> (InterpreterState -> InterpreterState)
> updateISStore f i = i {isStore = f (isStore i)}

------------------------------------------------------------------------------

> data InterpreterState =
>     InterpreterState
>     {addedTests :: [Value]
>     ,testResultLog :: [T.TestResultLog]
>     ,isStore :: Store
>     }

> emptyInterpreterState :: InterpreterState
> emptyInterpreterState = InterpreterState [] [] emptyStore

> data InterpreterException = InterpreterException String
>                           deriving Show

> instance Exception InterpreterException

> interpreterExceptionToString :: InterpreterException -> String
> interpreterExceptionToString (InterpreterException s) = s

  
> type Interpreter = RWST Env () InterpreterState IO

> throwInterp :: String -> Interpreter a
> throwInterp e = throwM $ InterpreterException e
  
> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Text t) = pure (TextV t)
> interp (TupleSel es) = TupleV <$> mapM interp es
> interp (ListSel es) = ListV <$> mapM interp es
> interp (RecordSel es) = VariantV "record" <$> mapM f (sortBy (comparing fst) es)
>   where
>       f (a,b) = (a,) <$> interp b

> interp (Iden a) = do
>     env <- ask
>     x <- envLookup a env
>     case x of
>         BoxV i -> do
>                   st <- get
>                   fetchStore i (isStore st)
>         _ -> pure x

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

> interp (Box e) = do
>     v <- interp e
>     box v

> interp (SetBox (Iden b) v) = do
>     env <- ask
>     b' <- envLookup b env
>     v' <- interp v
>     i <- case b' of
>              BoxV i -> pure i
>              _ -> throwInterp $ "attemped to setbox non box value: " ++ torepr' b'
>     modify $ \s -> (updateISStore (extendStore i v') s)
>     pure v'

> interp (SetBox b v) = do
>     b' <- interp b
>     v' <- interp v
>     i <- case b' of
>              BoxV i -> pure i
>              _ -> throwInterp $ "attemped to setbox non box value: " ++ torepr' b'
>     modify $ \s -> (updateISStore (extendStore i v') s)
>     pure v'
      
> interp (Unbox b) = do
>     b' <- interp b
>     case b' of
>         BoxV i -> do
>                   st <- get
>                   fetchStore i (isStore st)
>         _ -> throwInterp $ "attemped to unbox non box value: " ++ torepr' b'

> interp (UnboxRef {}) = throwInterp "undesugared unboxref"

  
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

> box :: Value -> Interpreter Value
> box v = do
>     i <- state $ \s ->
>          let i = newStoreLoc (isStore s)
>          in (i, updateISStore (extendStore i v) s)
>     pure $ BoxV i

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


> defaultEnv :: Env
> defaultEnv = either error id $ addForeignFuns' (
>    [("+", binaryOp unwrapNum unwrapNum wrapNum (+))
>    ,("*", binaryOp unwrapNum unwrapNum wrapNum (*))
>    ,("/", binaryOp unwrapNum unwrapNum wrapNum divideScientific)
>    ,("-", binaryOp unwrapNum unwrapNum wrapNum (-))
>    ,("+", binaryOp unwrapText unwrapText wrapText (++))
>    ,("==", binaryOp anyIn anyIn wrapBool (==))

>    ,("torepr", unaryOp anyIn pure torepr)
>    ,("tostring", unaryOp anyIn pure tostring)

>    ,("variant-field-get", binaryOp unwrapText variantIn id variantFieldGet)
>    ,("variant-field-get-ord", binaryOp unwrapNum variantIn id variantFieldGetOrd)
>    ,("safe-variant-name", unaryOp variantIn pure safeVariantName)
>    -- hack to make it work for any data type     
>    ,("safe-variant-name", unaryOp anyIn pure (const $ NothingV))
>    ,("make-variant", binaryOp unwrapText unwrapList id makeVariant)
>    ,("env-to-record", nullaryOp id envToRecord)


>    ,("add-tests", unaryOp functionIn id addTests)
>    ,("log-check-block", binaryOp unwrapNum unwrapText id logCheckBlock)
>    ,("log-test-pass", binaryOp unwrapNum unwrapText id logTestPass)
>    ,("log-test-fail", ternaryOp unwrapNum unwrapText unwrapText id logTestFail)

>    ]
>     ++ [])
>    $ emptyEnv {envEnv = [("true", BoolV True)
>                         ,("false", BoolV False)
>                         ,("nothing", NothingV)
>                         ,("empty", VariantV "empty" [])]}

> envToRecord :: Interpreter Value
> envToRecord = do
>     rd <- ask
>     pure $ VariantV "record" $ envEnv rd

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
> torepr' (BoxV {}) = "<Box>"
> torepr' (TupleV fs) =
>     "{" ++ intercalate ";" (map torepr' fs) ++ "}"
> torepr' (VariantV "record" fs) =
>     "{" ++ intercalate "," (map (\(a,b) -> a ++ " = " ++  torepr' b) fs) ++ "}"
> torepr' (VariantV nm fs) =
>     nm ++ "(" ++ intercalate "," (map (torepr' . snd) fs) ++ ")"
> torepr' (ListV fs) =
>     "[list: " ++ intercalate "," (map torepr' fs) ++ "]"

 
> torepr' NothingV = "nothing"


> tostring :: Value -> Value
> tostring x@(TextV {}) = x
> tostring x = torepr x




> safeVariantName :: Value -> Value
> safeVariantName (VariantV x _) = TextV $ dropQualifiers x
> safeVariantName _ = NothingV

> dropQualifiers :: String -> String
> dropQualifiers = reverse . takeWhile (/='.') . reverse

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
>     unpackTuple (TupleV [BoolV isRef, TextV nm, v]) =
>         if isRef
>         then do
>             v' <- box v
>             pure (nm,v')
>         else pure (nm,v)
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
> unwrapList =
>     ("list", \case
>                  ListV vs -> pure vs
>                  x -> throwInterp $ "type: expected list, got: " ++ torepr' x)
  
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
>     ,\as -> case as of
>                 [a] -> do
>                     ax <- (snd unwrap0) a
>                     wrap (f ax)
>                 _ -> throwInterp $ "wrong number of args to function, expected 1, got " ++ show (length as))

> nullaryOp :: (a -> Interpreter Value)
>           -> a
>           -> ([String], ([Value] -> Interpreter Value))
> nullaryOp wrap f =
>     ([] ,\as ->
>             case as of
>                 [] -> wrap f
>                 _ -> throwInterp $ "wrong number of args to function, expected 0, got " ++ show (length as))




> binaryOp :: (String, Value -> Interpreter a)
>          -> (String, Value -> Interpreter b)
>          -> (c -> Interpreter Value)
>          -> (a -> b -> c)
>          -> ([String], ([Value] -> Interpreter Value))
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


> parse :: String -> Either String Module
> parse src =
>     case P.parseProgram "" src of
>       Right (S.Program ps sts) -> Module <$> mapM convPrelude ps <*> convStmts sts
>       Left e -> Left e

> convPrelude :: S.PreludeItem -> Either String PreludeStmt

> convPrelude (S.Import (S.ImportName x) y) = pure $ Import x y

> convPrelude (S.IncludeFrom x as) | Just as' <- mapM f as =
>     pure $ Include x as'
>   where
>     f (S.ProvideAlias a b) = Just (a,b)
>     f _ = Nothing          
  
> convPrelude x = Left $ "unsupported prelude statement " ++ show x


  
> convStmts :: [S.Stmt] -> Either String [Stmt]
> convStmts = mapM convSt

> convExpr :: S.Expr -> Either String Expr
> convExpr (S.Sel (S.Num x)) = Right $ Num x
> convExpr (S.Sel (S.Str x)) = Right $ Text x
> convExpr (S.Sel (S.Tuple fs)) = TupleSel <$> mapM convExpr fs

> convExpr (S.Sel (S.Record fs)) = RecordSel <$> mapM f fs
>   where
>     f (a,b) = (a,) <$> convExpr b

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
>     f (S.VariantP vnm ps, ve)
>         | Right ps' <- mapM unpat ps = (convPatName vnm, ps',) <$> convExpr ve
>     f (S.IdenP _ vnm, ve) = (convPatName vnm, [],) <$> convExpr ve
>     f x = Left $ "parse: unsupported pattern: " ++ show x
>     convPatName (S.PatName x) = x
>     convPatName (S.QPatName x y) = x ++ "." ++ y
>     unpat (S.IdenP _ (S.PatName x)) = pure x
>     unpat x = Left $ "parse: unsupported pattern: " ++ show x

> convExpr (S.Unbox e n) = flip UnboxRef n <$> convExpr e

> convExpr x = Left $ "parse: unsupported expression " ++ show x

> convSt :: S.Stmt -> Either String Stmt
> convSt (S.StExpr e) = StExpr <$> convExpr e
> convSt (S.LetDecl (S.Binding (S.IdenP _ (S.PatName nm)) v)) = LetDecl nm <$> convExpr v
> convSt (S.Check nm bdy) = Check nm <$> mapM convSt bdy
> convSt (S.VarDecl (S.Binding (S.IdenP _ (S.PatName nm)) v)) = VarDecl nm <$> convExpr v
> convSt (S.SetVar n e) = SetVar n <$> convExpr e
> convSt (S.SetRef e ss) = SetRef <$> convExpr e <*> mapM f ss
>    where
>      f (a,b) = (a,) <$> convExpr b


> convSt (S.DataDecl nm fs Nothing) =
>     DataDecl nm <$> mapM convVarDecl fs

> convSt x = Left $ "parse: unsupported statement " ++ show x

> convVarDecl :: S.VariantDecl -> Either String VariantDecl
> convVarDecl (S.VariantDecl nm fs) = pure $ VariantDecl nm $ flip map fs $ \(a,b) -> (f a,b)
>    where
>      f S.Con = Con
>      f S.Ref = Ref

------------------------------------------------------------------------------

pretty
------

> pretty :: Expr -> String
> pretty x = Pr.prettyExpr $ unconv x


> unconv :: Expr -> S.Expr
> unconv (Num n) = S.Sel (S.Num n)
> unconv (Text n) = S.Sel (S.Str n)
> unconv (TupleSel fs) = S.Sel (S.Tuple $ map unconv fs)
> unconv (ListSel fs) = S.Construct (S.Iden "list") (map unconv fs)
> unconv (RecordSel fs) = S.Sel (S.Record (map f fs))
>   where
>     f (a,b) = (a,unconv b)
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
> unconv (Box e) = S.App (S.Iden "box") [unconv e]
> unconv (SetBox e f) = S.App (S.Iden "setbox") [unconv e, unconv f]

> unconv (UnboxRef e n) = S.Unbox (unconv e) n
> unconv (Unbox e) = S.App (S.Iden "unbox") [unconv e]

  
> --unconv x = error $ "unconv: " ++ show x

> unconvStmt :: Stmt -> S.Stmt
> unconvStmt (LetDecl n e) = S.LetDecl (unconvBinding n e)
> unconvStmt (StExpr e) = S.StExpr (unconv e)
> unconvStmt (Check nm bs) = S.Check nm $ map unconvStmt bs
> unconvStmt (VarDecl n e) = S.VarDecl (unconvBinding n e)
> unconvStmt (SetVar n e) = S.SetVar n (unconv e)
> unconvStmt (SetRef e fs) = S.SetRef (unconv e) $ map f fs
>   where
>     f (a,b) = (a,unconv b)

> unconvStmt (DataDecl nm vs) =
>      S.DataDecl nm (map f vs) Nothing
>    where
>      f (VariantDecl vnm fs) = S.VariantDecl vnm $ map uf fs
>      uf (Ref,x) = (S.Ref,x)
>      uf (Con,x) = (S.Con,x)

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

check "variables":
  var x = 10
  x is 10
  x := 15
  x is 15

  var a = 5
  a := a + 1
  a is 6

end

mk-counter = lam(): block:
  var ctr = 0
  lam(): block:
      ctr := ctr + 1
      ctr
    end
  end
end end

check "var in closure":
  x = mk-counter()
  y = mk-counter()
  y() is 1
  y() is 2
  x() is 1
  y() is 3
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


data MutX:
  | mut-x(ref x, y)
end

ex1 = mut-x(1, 2)


check:
  ex1!x is 1      # this access the value inside the reference
  #ex1.x is-not 1  # this does not
end

ex1a = mut-x(1,2)

ex1a!{x: 42}

check:
  ex1a!x is 42
end

data MutX1:
  | mut-x1(ref x, ref y)
end

ex2 = mut-x1(1, 2)

# multiple updates
ex2!{x: 42, y:43}

check:
  ex2!x is 42
  ex2!y is 43
end

data Box:
  | box(ref v)
end
#where:
check: 
  n1 = box(1)
  n2 = box(2)
  n1!{v : 3}
  n2!{v : 4}
  n1!v is 3
  n2!v is 4
end

check:
  adder = lam(a): block:
    a!{x: a!x + 1}
  end end
  ex1 = mut-x(1, 2)
  ex1!x is 1
  adder(ex1)
  ex1!x is 2
end

check:
  x = 33
  a = {a: "one", b : 2, c : x }
  a.a is "one"
  a.b is 2
  a.c is 33

  my-obj = {s: "Hello", b: true, n: 42}
  g = lam (thing):
    thing.n
  end

  g(my-obj) is 42
  # check non ordering of fields in equality

  {a:1, b:2} is { b:2, a:1}
  
end

\end{code}
>    |]


 
> importsTestScripts :: [String]
> importsTestScripts =
>     [[r|
\begin{code} 

xmodule: my-module
  
a = 1
b = lam(x): x + 1 end
check:
  a is 1
  b(1) is 2
end

xmodule: main1

import my-module as my-module

check:
  my-module.a is 1
  my-module.b(2) is 3
end

xmodule: main1-5

import my-module as my-module1

check:
  my-module1.a is 1
  my-module1.b(2) is 3
end

xmodule: main2

import my-module as my-module
include from my-module: a as a, b as b end

check:
  a is 1
  b(3) is 4
end


xmodule: main3

import my-module as my-2
check:
  my-2.a is 1
  my-2.b(5) is 6
end

xmodule: my-2

a = 2
b = lam(x): x + 2 end
check:
  a is 2
  b(1) is 3
end

xmodule: main3

import my-2 as my-module
import my-module as my-2

check:
  my-2.a is 1
  my-2.b(5) is 6
end

check:
  my-module.a is 2
  my-module.b(2) is 4
end

\end{code}
>  |], [r|
\begin{code}

xmodule: main1

# regular data type + cases

data Point:
  | pt(x, y)
end

f = lam(x): cases(Point) x:
      | pt(x,y) => x
    end end

a = pt(1,2)

check:
  f(a) is 1
end

xmodule: main2

# include data type + cases demo

import main1 as main1
include from main1: pt as pt, f as f, a as a end

check:
  f(a) is 1
end

b = pt(2,3)

check:
  f(b) is 2
end

g = lam(x): cases(Point) x:
      | pt(x,y) => x + 1
    end end
 
check:
  g(a) is 2
  g(b) is 3
end


xmodule: main3

# import data type + cases use under qualifier demo

import main1 as main1

check:
  main1.f(main1.a) is 1
end

b = main1.pt(2,3)

check:
  main1.f(b) is 2
end

g = lam(x): cases(main1.Point) x:
      | main1.pt(x,y) => x + 1
    end end
 
check:
  g(main1.a) is 2
  g(b) is 3
end


xmodule: main3

# shadow imported data type with local data type with same name and
# same variant names, use both

import main1 as main1

data Point:
  | pt(x, y)
end

f = lam(x): cases(Point) x:
      | pt(x,y) => x + 2
    end end

a = pt(1,2)

b = main1.pt(3,4)

g = lam(x): cases(main1.Point) x:
      | main1.pt(x,y) => x + 1
    end end


check:
  main1.f(main1.a) is 1
  f(a) is 3
  g(b) is 4
end

xmodule: main3

# import data type under different alias

import main1 as xxx

check:
  xxx.f(xxx.a) is 1
end

b = xxx.pt(2,3)

check:
  xxx.f(b) is 2
end

g = lam(x): cases(xxx.Point) x:
      | xxx.pt(x,y) => x + 1
    end end
 
check:
  g(xxx.a) is 2
  g(b) is 3
end

# import data type under different alias
# import another data type under previous one's module name

xmodule: main1alt

data Point:
  | pt(x, y)
end

f = lam(x): cases(Point) x:
      | pt(x,y) => x + 5
    end end

a = pt(3,4)

check:
  f(a) is 8
end

xmodule: xmain4

import main1 as main1alt
import main1alt as main1

data Point:
  | pt(x, y)
end

f = lam(x): cases(Point) x:
      | pt(x,y) => x + 2
    end end

a = pt(1,2)

b = main1.pt(3,4)

g = lam(x): cases(main1.Point) x:
      | main1.pt(x,y) => x + 1
    end end

c = main1alt.pt(5,6)

h = lam(x): cases(main1alt.Point) x:
      | main1alt.pt(x,y) => x + 7
    end end



check:
  main1alt.f(main1alt.a) is 1
  main1.f(main1.a) is 8
  f(a) is 3
  g(b) is 4
  h(c) is 12
end




\end{code}
>  |]
             
>      ]


> {-simpleTestScript33 :: String
> simpleTestScript33 = [r|
\begin{code} 

import a-module as a-module

check "basic tests":
  a-module.f(1) is 2
end
\end{code}
>   |]-}

> tests :: T.TestTree
> tests = T.testGroup "imports4repl" [tests1,tests2,tests4]

> {-tests3 :: T.TestTree
> tests3 = T.makeTestsIO "imports4c" $ evaluate simpleTestScript33-}

     
> tests1 :: T.TestTree
> tests1 = T.makeTestsIO "imports4repla" $ (fmap (\(_,_,x) -> x) <$> evaluate defaultEnv simpleTestScript)

> tests2 :: T.TestTree
> tests2 = T.testGroup "imports4replb" $ map makeTest importsTestScripts
>   where
>     makeTest s =
>       let ((_,src):ts) = reverse $ either error id $ T.parseModules s
>           crs = evaluate' (makeFileSystemMock ts) defaultEnv src
>       in T.makeTestsIO "imports4replb" (fmap (\(_,_,x) -> x) <$> crs)


> tests4 :: T.TestTree
> tests4 = T.testGroup "imports4repl4"
>     [testSanityArith
>     ,testEnvKept
>     ,testEnvOverridden
>     ,testAScript
>     ,testRunScriptWithValues
>     ,testRunFunctionSimple
>     --,testRunFunctionPartialApp
>     ]

> testSanityArith :: T.TestTree
> testSanityArith = T.testCase "testSanityArith" $ do
>     h <- newTeaberryHandle
>     v <- runScript h [] "1 + 2"
>     T.assertEqual "" (Right (NumV 3)) v


> testEnvKept :: T.TestTree
> testEnvKept = T.testCase "testEnvKept" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h [] "a = 1"
>     v <- runScript h [] "a"
>     T.assertEqual "" (Right (NumV 1)) v

> testEnvOverridden :: T.TestTree
> testEnvOverridden = T.testCase "testEnvOverridden" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h [] "a = 1"
>     _ <- runScript h [] "a = 4"
>     v <- runScript h [] "a"
>     T.assertEqual "" (Right (NumV 4)) v

*****
TODO: test running something with tests and there's a test failure


> testAScript :: T.TestTree
> testAScript = T.testCase "testAScript" $ do
>     let script = [r|
\begin{code}
a = 3
b = 4
a + b
\end{code}
>         |]
>     h <- newTeaberryHandle
>     v <- runScript h [] script
>     T.assertEqual "" (Right (NumV 7)) v

todo: lots of boilerplate and abstraction details to work out

> {-testFFI :: T.TestTree
> testFFI = T.testCase "testFFI" $ do
>     let myFFI :: [Value] -> Interpreter Value
>         myFFI = \case
>             [NumV a] = pure $ NumV $ a + 1
>             _ = lift $ throwE $ "expected num"
>     h <- newTeaberryHandle
>     addFFI h ("myfun", myFFI)
>     v <- runScript h "myfun(3)"
>     T.assertEqual "" (Right (NumV 4)) v-}

> testRunScriptWithValues :: T.TestTree
> testRunScriptWithValues = T.testCase "testRunScriptWithValues" $ do
>     h <- newTeaberryHandle
>     v <- runScript h [("a", NumV 3)
>                      ,("b", NumV 11)] "a + b"
>     T.assertEqual "" (Right (NumV 14)) v

how to implement this without it staying in the env?
todo: fix this, for now, they will stay in the env

when should stuff stay in the env generally?
"a = 1"
a should stay in the env
"a = 1
a + 2"
should a stay in the env? not sure


> testRunFunctionSimple :: T.TestTree
> testRunFunctionSimple = T.testCase "testRunFunctionSimple" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h [] "f = lam(x,y): x + y end"
>     v <- runFunction h "f" [NumV 5, NumV 11]
>     T.assertEqual "" (Right (NumV 16)) v

the next test should work as soon as partial application is
implemented

> {-testRunFunctionPartialApp :: T.TestTree
> testRunFunctionPartialApp = T.testCase "testRunFunctionPartialApp" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h [] "f = lam(x,y): x + y end"
>     v <- runFunction h "f(_,4)" [NumV 5]
>     T.assertEqual "" (Right (NumV 9)) v-}
