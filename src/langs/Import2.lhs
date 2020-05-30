

Implementation of import demo 2:

simple implementation desugaring the most basic import and
 include statements
all modules are assumed to provide *

you pass [(String,String)] to the evaluate
these are the module names, module sources
parse now has to parse to a module with a prelude section
the executor needs to run checks from multiple files
these have to be in a good order, so no module imports a module later
in the list

overview:
desugar each module by wrapping it in a block,
  which ends with a record of the top level values (implicit provide *)
then bind each module to a unique name
  the name has to be unique because e.g. you can import module
  'my-module' as something else, then import another module as 'my-module'
use these names to bind to the import alias
use the import alias to use members - it's regular record field access
plus to bind unaliased includes


a module of
stmts
->
  block:
    stmts
    env-to-record() # {a : a, b : b,  ... }
  end

env-to-record takes the runtime environment at that point and turns it
into a record value to return from the block

then to import this:
module-name-unique-suffix = block ...

these need to be unique names generated each compile to work in this
system

import file('xxx') as X
->
X = lookup the module name of file 'xxx' in the module-name-unique-suffix
    binding for the desugaring of that module
these need to get the module unique name lookup table to be desugared

include from X:
  n1 as n2, ...
end
->
n2 = X.n1
...
the X has to be looked up here too

one by one:
desugar a module
  this uses the module name lookup to desugar import and include
  as well as wrapping in a block and adding the tuple
add the module-name-unique suffix
add this to the module name lookup


> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE MultiWayIf #-}

> module Import2 (tests
>                ) where

> import Text.RawString.QQ

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T


> import qualified Parse as P
> import qualified Syntax as S
> import qualified Pretty as Pr


> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, get, gets, state, put, modify)

> import Control.Monad (when, forM)
> import Data.Maybe (isJust, isNothing)
>
> import Data.Char (isAlphaNum)

> import Scientific (Scientific, extractInt)
> import Data.List (intercalate, partition, sortBy)
> import Data.Ord (comparing)
>
> import Debug.Trace (trace)

> import Text.Megaparsec (Parsec
>                        ,errorBundlePretty
>                        --,many
>                        ,eof
>                        ,takeWhileP
>                        ,takeWhile1P
>                        ,choice
>                        --,notFollowedBy
>                        ,try
>                        ,anySingle
>                        ,(<|>)
>                        ,manyTill
>                        )
> import qualified Text.Megaparsec as Q (parse)

> import Data.Void (Void)
> import Text.Megaparsec.Char (space
>                             --,char
>                             ,string
>                             )
> import Control.Monad (void)
  
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
> runDesugar srcs = fst <$> runExcept (evalRWST (f [] [] srcs) () startingDesugarState)
>   where
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

> desugarModule :: [(String,String)] -> String -> Module -> Desugarer ([(String,String)], (String,Expr))
> desugarModule otherModuleNames moduleName (Module ps xs) = do
>     uniqueModuleName <- makeUniqueVar moduleName
>     ps' <- concat <$> mapM (desugarPreludeStmt otherModuleNames) ps
>     -- make a block with the last statement converting the env into a record
>     -- I can't believe this cheapo trick works
>     xs' <- desugar (Block (ps' ++ xs ++ [StExpr $ App (Iden "env-to-record") []]))
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
>         let tst = appI "safe-variant-name" [Iden tv] `equals` Text vnm
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
> desugarStmts [LetDecl {}] = lift $ throwE $ "block ends with let"

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
> fetchStore i (Store xs) = maybe (lift $ throwE $ "invalid fetch on store: " ++ show i) pure
>                           $ lookup i xs

> updateISStore :: (Store -> Store) -> (InterpreterState -> InterpreterState)
> updateISStore f i = i {isStore = f (isStore i)}

------------------------------------------------------------------------------

> data InterpreterState =
>     InterpreterState
>     {addedTests :: [Value]
>     ,testResultLog :: [TestResultLog]
>     ,isStore :: Store
>     }

> emptyInterpreterState :: InterpreterState
> emptyInterpreterState = InterpreterState [] [] emptyStore

> type Interpreter = RWST Env () InterpreterState (Except String)

> runInterp :: Env -> Expr -> Either String [CheckResult]
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

> interp (Block {}) = lift $ throwE $ "undesugared block passed to interpreter"
> interp (Seq a b) = interp a *> interp b

> interp z@(If bs e) = do
>     let f ((c,t):bs') = do
>             c' <- interp c
>             case c' of
>                 BoolV True -> interp t
>                 BoolV False -> f bs'
>                 _ -> lift $ throwE $ "expected bool in if test, got " ++ show c'
>         f [] = case e of
>                    Just x -> interp x
>                    Nothing -> lift $ throwE $ "no if branches matched and no else:\n"
>                               ++ pretty z
>     f bs

> interp e@(DotExpr {}) = lift $ throwE $ "interp: undesugared dotexpr " ++ show e
> interp e@(Cases {}) = lift $ throwE $ "interp: undesugared cases " ++ show e

> interp (Box e) = do
>     v <- interp e
>     box v

> interp (SetBox (Iden b) v) = do
>     env <- ask
>     b' <- envLookup b env
>     v' <- interp v
>     i <- case b' of
>              BoxV i -> pure i
>              _ -> lift $ throwE $ "attemped to setbox non box value: " ++ torepr' b'
>     modify $ \s -> (updateISStore (extendStore i v') s)
>     pure v'

> interp (SetBox b v) = do
>     b' <- interp b
>     v' <- interp v
>     i <- case b' of
>              BoxV i -> pure i
>              _ -> lift $ throwE $ "attemped to setbox non box value: " ++ torepr' b'
>     modify $ \s -> (updateISStore (extendStore i v') s)
>     pure v'
      
> interp (Unbox b) = do
>     b' <- interp b
>     case b' of
>         BoxV i -> do
>                   st <- get
>                   fetchStore i (isStore st)
>         _ -> lift $ throwE $ "attemped to unbox non box value: " ++ torepr' b'

> interp (UnboxRef {}) = lift $ throwE "undesugared unboxref"

  
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
>         _ -> lift $ throwE "non function value in app position"
>   where
>     safeZip ps xs | length xs == length ps = pure $ zip ps xs
>                   | otherwise = lift $ throwE $ "wrong number of args to function"

> box :: Value -> Interpreter Value
> box v = do
>     i <- state $ \s ->
>          let i = newStoreLoc (isStore s)
>          in (i, updateISStore (extendStore i v) s)
>     pure $ BoxV i

  

> evaluateWithChecks :: [(String,String)] -> Either String [CheckResult]
> evaluateWithChecks s =  do
>     ast <- forM s $ \(n,src) -> (n,) <$> parse src
>     ast' <- runDesugar ast
>     {-trace (pretty ast') $-}
>     runInterp testEnv ast'

------------------------------------------------------------------------------

haskell side testing infrastructure
-----------------------------------

checkresult:

this is the user api. Is this as simple and direct as possible?

todo: will extend the name with the package and module as well
want to be able to do a filter on which tests to run
maybe the filter is just a predicate on the package, module and check
block name

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
>                  deriving Show
> -- the second is Nothing if it's a pass
> -- if it is Just, it's a fail, and contains the failure message

--------------------------------------

ffi functions used by the desugared test code

TODO: use/write helpers for working with state monad, etc.
create a wrapper to convert an Interpreter () into NothingV

> data TestResultLog = TestPass Scientific String -- check block id, test source
>                    | TestFail Scientific String String -- check block id, test source, failure message
>                    | TestBlock Scientific String

add-tests(fn)
takes a function value
these functions are run at the end of the script to do the testing

> addTests :: Value -> Interpreter Value
> addTests v = nothingWrapper $ \s -> s {addedTests = v : addedTests s}

log-check-block(unique-block-id, name)
says there is a new test block with a unique id and it's name

> logCheckBlock :: Scientific -> String -> Interpreter Value
> logCheckBlock n nm =
>     nothingWrapper $ \s -> s {testResultLog = TestBlock n nm : testResultLog s}

log-test-pass(blockid, text of test)

> logTestPass :: Scientific -> String -> Interpreter Value
> logTestPass n msg =
>     nothingWrapper $ \s -> s {testResultLog = TestPass n msg : testResultLog s}

log-test-fail(block,id, text of test, fail message)

> logTestFail :: Scientific -> String -> String -> Interpreter Value
> logTestFail n msg failmsg = nothingWrapper $ \s ->
>     s {testResultLog = TestFail n msg failmsg : testResultLog s}

> nothingWrapper :: (InterpreterState -> InterpreterState) -> Interpreter Value
> nothingWrapper f = modify f *> pure NothingV

--------------------------------------

this is run at the end of the script to run all the saved tests

> runAddedTests :: Interpreter [CheckResult]
> runAddedTests = do
>     ts <- reverse <$> gets addedTests
>     mapM_ (\v -> app v []) ts
>     testLog <- reverse <$> gets testResultLog
>     either (lift . throwE) pure $ testLogToCheckResults testLog

this is run after that, to convert the log of test events, into a
organised data structure to view the results

> testLogToCheckResults :: [TestResultLog] -> Either String [CheckResult]
> testLogToCheckResults trls = do
>     -- 'streaming' algorithm, very efficient
>     -- but the reason to do it is because it's an easy
>     -- way to catch a test result without a matching check block
>     -- relies on the test results being in the order you write
>     -- the corresponding tests in the source file
>     let f :: [(Scientific, CheckResult)] -> [TestResultLog] -> Either String [CheckResult]
>         -- finished, reverse the check results
>         f res [] = pure $ map reverseResults $ map snd $ reverse res
>         -- new check block, make sure it's id hasn't been seen already
>         f res ((TestBlock cid nm) : xs) = do
>             when (any (== cid) $ map fst res)
>                 $ Left $ "multiple check result name for check block with id " ++ show cid
>             f ((cid, CheckResult nm []) : res) xs
>         -- if we see a pass or a fail without having see a testblock
>         -- then it's a bug
>         f [] (TestPass {} : _) = Left $ "unmatched check block id in test result"
>         f [] (TestFail {} : _) = Left $ "unmatched check block id in test result"
>         -- pass or fail, the id should match the id of the last testblock entry
>         -- not sure if this extra checking is really needed
>         -- maybe it will be useful if try to run tests in multiple threads
>         -- in the future?
>         f ((cid, CheckResult cnm ts):cs) ((TestPass tid tnm) : xs)
>             | cid == tid =
>               f ((cid,CheckResult cnm ((tnm, Nothing):ts)):cs) xs
>             | otherwise = Left $ "unmatched check block id in test result"
>         f ((cid, CheckResult cnm ts):cs) ((TestFail tid tnm fmsg) : xs)
>             | cid == tid =
>               f ((cid,CheckResult cnm ((tnm, Just fmsg):ts)):cs) xs
>             | otherwise = Left $ "unmatched check block id in test result"
>     f [] trls
>   where
>     reverseResults (CheckResult nm ts) = CheckResult nm $ reverse ts

function to convert the check result data structure into a nicely
formatted string

> renderCheckResults :: [CheckResult] -> String
> renderCheckResults cs =
>     let bs = map renderCheck cs
>         totalPasses = sum $ map (\(n,_,_) -> n) bs
>         totalFails = sum $ map (\(_,n,_) -> n) bs
>         msgs = map (\(_,_,m) -> m) bs
>     in intercalate "\n\n" msgs
>        ++ "\n\n" ++ (show totalPasses) ++ "/" ++ show (totalPasses + totalFails)
>        ++ " tests passed in all check blocks"
>   where
>     renderCheck (CheckResult nm ts) =
>         let (ps,fs) = partition (isNothing . snd) ts
>             msgs = map renderTest ts
>         in (length ps
>            ,length fs
>            ,"Check block: " ++ nm ++ "\n"
>            ++ intercalate "\n" (map indent msgs)
>            ++ "\n  " ++ show (length ps) ++ "/" ++ show (length ts) ++ " tests passed in check block: " ++ nm
>            )
>     renderTest (a,b) =
>         "test (" ++ a ++ "): "
>         ++ case b of
>                Nothing -> "OK"
>                Just msg -> "failed, reason:\n" ++ indent msg
>     indent [] = []
>     indent x = "  " ++ indent' x
>     indent' [] = []
>     indent' x@[_] = x
>     indent' ('\n':y@(_:_)) = "\n  " ++ indent' y
>     indent' (x:y) = x : indent' y

------------------------------------------------------------------------------

ffi catalog


> testEnv :: Env
> testEnv = either error id $ addForeignFuns' (
>    [("+", binaryOp unwrapNum unwrapNum wrapNum (+))
>    ,("*", binaryOp unwrapNum unwrapNum wrapNum (*))
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
> torepr' (VariantV nm fs) =
>     nm ++ "(" ++ intercalate "," (map (torepr' . snd) fs) ++ ")"
> torepr' (ListV fs) =
>     "[list: " ++ intercalate "," (map torepr' fs) ++ "]"

 
> torepr' NothingV = "nothing"


> tostring :: Value -> Value
> tostring x@(TextV {}) = x
> tostring x = torepr x


> safeVariantName :: Value -> Value
> safeVariantName (VariantV x _) = TextV x
> safeVariantName _ = NothingV

> variantFieldGet :: String -> Value -> Interpreter Value
> variantFieldGet fieldNm v@(VariantV _ fs) =
>     maybe (lift $ throwE $ "variant field not found " ++ fieldNm ++ ": " ++ torepr' v)
>           pure $ lookup fieldNm fs
> variantFieldGet _ x =
>     lift $ throwE $ "variant field get called on " ++ torepr' x

> variantFieldGetOrd :: Scientific -> Value -> Interpreter Value
> variantFieldGetOrd fieldIndex v@(VariantV _ fs) =
>     case extractInt fieldIndex of
>         Nothing -> lift $ throwE $ "variant field get ord passed non integer: " ++ show fieldIndex
>         Just i -> maybe (lift $ throwE $ "variant field # not found " ++ show fieldIndex ++ ": " ++ torepr' v)
>                   pure (safeIndex (map snd fs) i)
>   where
>     safeIndex [] _ = Nothing
>     safeIndex _ n | n < 0 = Nothing
>     safeIndex (x:_) 0 = Just x
>     safeIndex (_:xs) n = safeIndex xs (n - 1)
     
       
> variantFieldGetOrd _ x =
>     lift $ throwE $ "variant field get ord called on " ++ torepr' x


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
>     unpackTuple (VariantV "tuple" x) = lift $ throwE $ "value in list in make-variant, expected tuple of is-ref, name and val, got " ++ show (map (\(_,b) -> torepr' b) x)
>     unpackTuple x = lift $ throwE $ "expected tuple in make-variant, got " ++ torepr' x

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
>     maybe (lift $ throwE $ "Identifier not found " ++ nm) pure
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
>        | otherwise -> lift $ throwE $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")"



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
>                           x -> lift $ throwE $ "type: expected tuple, got " ++ show x)

> unwrapNum :: (String, Value -> Interpreter Scientific)
> unwrapNum = ("number", \case
>                           NumV n -> pure n
>                           x -> lift $ throwE $ "type: expected number, got " ++ show x)


> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n



> _unwrapBool :: (String, Value -> Interpreter Bool)
> _unwrapBool = ("boolean", \case
>                           BoolV n -> pure n
>                           x -> lift $ throwE $ "type: expected boolean, got " ++ show x)

> wrapBool :: Bool -> Interpreter Value
> wrapBool n = pure $ BoolV n


> unwrapText :: (String, Value -> Interpreter String)
> unwrapText = ("text", \case
>                           TextV n -> pure n
>                           x -> lift $ throwE $ "type: expected text, got " ++ show x)

> wrapText :: String -> Interpreter Value
> wrapText n = pure $ TextV n

> variantIn :: (String, Value -> Interpreter Value)
> variantIn = ("variant", \case
>                           x@(VariantV {}) -> pure x
>                           x -> lift $ throwE $ "type: expected variant, got " ++ show x)

> unwrapList :: (String, Value -> Interpreter [Value])
> unwrapList =
>     ("list", \case
>                  ListV vs -> pure vs
>                  x -> lift $ throwE $ "type: expected list, got: " ++ torepr' x)
  
> anyIn :: (String, Value -> Interpreter Value)
> anyIn = ("any", pure)

> functionIn :: (String, Value -> Interpreter Value)
> functionIn = ("function", \case
>                           x@(FunV {}) -> pure x
>                           x@(ForeignFunV {}) -> pure x
>                           x -> lift $ throwE $ "type: expected function, got " ++ show x)

  
  
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
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 1, got " ++ show (length as))

> nullaryOp :: (a -> Interpreter Value)
>           -> a
>           -> ([String], ([Value] -> Interpreter Value))
> nullaryOp wrap f =
>     ([] ,\as ->
>             case as of
>                 [] -> wrap f
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 0, got " ++ show (length as))




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
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 2, got " ++ show (length as))

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
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 4, got " ++ show (length as))


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
>     f (S.VariantP (S.PatName vnm) ps, ve)
>         | Right ps' <- mapM unpat ps = (vnm, ps',) <$> convExpr ve
>     f (S.IdenP _ (S.PatName vnm), ve) = (vnm, [],) <$> convExpr ve
>     f x = Left $ "parse: unsupported pattern: " ++ show x
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
> unconv (Box {}) = error "unsupported internal box syntax in pretty"
> unconv (SetBox {}) = error "unsupported internal setbox syntax in pretty"

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

> simpleTestScript :: [(String,String)]
> simpleTestScript = [("main", [r|
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
>    |])]


 
> importsTestScript :: String
> importsTestScript = [r|
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
>  |]

      
> tests :: T.TestTree
> tests = T.testGroup "imports1" [tests1,tests2]
      
> tests1 :: T.TestTree
> tests1 =
>     let crs = either error id $ evaluateWithChecks simpleTestScript
>         f (CheckResult nm res) =
>             T.testGroup nm $ map g res
>         g (nm, fm) = T.testCase nm $
>             case fm of
>                 Nothing -> T.assertBool "" True
>                 Just fmx -> T.assertFailure fmx
>     in trace (renderCheckResults crs)
>        $ T.testGroup "imports1a" $ map f crs


> tests2 :: T.TestTree
> tests2 = --trace (either show (intercalate "\n" . map show) $ parseModules importsTestScript2) $
>     let ts = either error id $ parseModules importsTestScript
>         crs = --trace (intercalate "\n" $ map show ts) $ 
>               either error id $ evaluateWithChecks ts
>         f (CheckResult nm res) =
>             T.testGroup nm $ map g res
>         g (nm, fm) = T.testCase nm $
>             case fm of
>                 Nothing -> T.assertBool "" True
>                 Just fmx -> T.assertFailure fmx
>     in trace (renderCheckResults crs)
>        $ T.testGroup "imports1b" $ map f crs


---------------

quick hack to simulate a bunch of separate files to be able to test the module system

the motivation is to put a bunch of files in one string in order to cut
down on boilerplate in the testing, and make the tests more readable
  
> parseModules :: String -> Either String [(String,String)]
> parseModules src = either (Left . errorBundlePretty) Right $
>                    Q.parse (whiteSpace *> pModules) "" src

> type Parser = Parsec Void String
  
> pModules :: Parser [(String,String)]
> pModules = do
>     _ <- string "xmodule:" <* whiteSpace
>     pbody
>   where
>     pbody :: Parser [(String,String)]        
>     pbody = do
>       moduleName <- takeWhile1P Nothing (\a -> (isAlphaNum a || a `elem` "?-+_"))
>       body <- manyTill anySingle (void (try (string "xmodule:") <* whiteSpace) <|> eof)
>       ((moduleName, body):) <$> choice [pbody <|> ([] <$ eof)]

> whiteSpace :: Parser ()
> whiteSpace = space *> choice [blockComment *> whiteSpace
>                              ,lineComment *> whiteSpace
>                              ,pure ()]


> lineComment :: Parser ()
> lineComment = () <$ try (string "#") <* takeWhileP Nothing (/='\n')

> blockComment :: Parser ()
> blockComment = startComment *> ctu
>   where
>     startComment = void (try (string "#|"))
>     endComment = void $ try (string "|#")
>     ctu = endComment <|> ((blockComment <|> void anySingle) *> ctu)
