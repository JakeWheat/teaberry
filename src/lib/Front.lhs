

Represents the combination of all the ideas that made it

cleanups:
parser extra:
  make an explicit module syntax to put multiple modules in one file
  change the set syntax to be in language

think about how to split this file:
syntax, interpreter, desugarer, tests


> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE MultiWayIf #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module Front (tests
>              ,TeaberryHandle
>              ,newTeaberryHandle
>              ,runScript
>              ,runFunction
>              ,Value
>              ,valueToString) where



> import qualified Parse as P
> import qualified Syntax as S
> import qualified Pretty as Pr

> import qualified ParserExtra as Px

> import Control.Monad.Trans.Class (lift)
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, get, gets, state, put, modify)
> import Control.Exception.Safe (Exception, throwM, catch)
> import Control.Concurrent (threadDelay)

> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Data (Data)

> import Control.Monad (when)
> import Data.Maybe (isJust, mapMaybe)
>
> import Data.Char (isAlphaNum)

> import Scientific (Scientific, extractInt, divideScientific)
> import Data.List (intercalate, nubBy, sortOn)

> --import Debug.Trace (trace, traceStack)
> import qualified TestUtils as T

> import Data.IORef (IORef, newIORef, readIORef, writeIORef)

> import Paths_teaberry
> import System.FilePath ((</>))


------------------------------------------------------------------------------

syntax
======

> data Module = Module [PreludeStmt] [Stmt]
>             deriving (Eq, Show, Data)

> data PreludeStmt = Import ImportSource String
>                  | IncludeFrom String [ProvideItem]
>                  deriving (Eq, Show, Data)

> data ProvideItem = ProvideAlias String String
>                  deriving (Eq,Show,Data) 


> data ImportSource = ImportSpecial String [String]
>                   | ImportName String
>                   deriving (Eq,Show, Data) 

> data Stmt = StExpr Expr
>           | LetDecl String Expr
>           | LetSplatDecl Expr
>           | Check (Maybe String) [Stmt]
>           | VarDecl String Expr
>           | RecDecl String Expr
>           | FunDecl String [String] Expr (Maybe [Stmt])
>           | SetVar String Expr
>           | DataDecl String [VariantDecl] (Maybe [Stmt])
>           | SetRef Expr [(String,Expr)]
>           | When Expr Expr
>           deriving (Eq, Show, Data)

> data VariantDecl = VariantDecl String [(Ref,String)]
>                  deriving (Eq,Show, Data)

> data Ref = Ref | Con
>          deriving (Eq,Show, Data)

  
> data Expr = -- selectors
>             Num Scientific
>           | Text String
>           | TupleSel [Expr]
>           | RecordSel [(String,Expr)]
>           -- todo: see if can remove this
>           | VariantSel String [(String,Expr)]
>           | Construct Expr [Expr]
>             -- other things
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>           | LetRec [(String,Expr)] Expr
>           | Block [Stmt]
>           | Seq Expr Expr
>           | If [(Expr,Expr)] (Maybe Expr)
>           | Ask [(Expr,Expr)] (Maybe Expr)
>           | DotExpr Expr String
>           | TupleGet Expr Int
>           | Cases String Expr [(String, [String], Expr)] (Maybe Expr)
>           -- todo: see if can remove the following
>           | Box Expr
>           | SetBox Expr Expr
>           | UnboxRef Expr String
>           | Unbox Expr
>           | Catch Expr Expr
>           deriving (Eq, Show, Data)


interpreter syntax

> data IExpr
>     = INum Scientific
>     | IText String
>      -- why does this exist? it's just app isn't it?
>      -- currently used to bootstrap tuple, record, nothing
>     | IVariantSel String [(String,IExpr)]
>     | IIden String
>     | IApp IExpr [IExpr]
>     | ILam [String] IExpr
>     | ILet [(String,IExpr)] IExpr
>     | ILetSplat IExpr IExpr
>     | ISeq IExpr IExpr
>     | IIf [(IExpr,IExpr)] (Maybe IExpr)
>     | IBox IExpr
>     | ISetBox IExpr IExpr
>     | IUnbox IExpr
>     | ICatch IExpr IExpr
>     deriving (Eq, Show, Data)



------------------------------------------------------------------------------

interpreter

embedded api
============

> newtype TeaberryHandle = TeaberryHandle
>     {tbh :: IORef RuntimeState}

> data RuntimeState = RuntimeState {
>      henv :: Env
>     ,executionStage :: ExecutionStage}

> defaultRuntimeState :: RuntimeState
> defaultRuntimeState = RuntimeState {henv = defaultEnv
>                                    ,executionStage = FullExecution}
  
> data ExecutionStage = FullExecution
>                     | DumpDesugar
>                     deriving (Eq,Show)
  
> newTeaberryHandle :: IO TeaberryHandle
> newTeaberryHandle = do
>     x <- newIORef defaultRuntimeState
>     pure $ TeaberryHandle x
  
> runScript :: TeaberryHandle -> Maybe String -> [(String,Value)] -> String -> IO Value
> runScript h fnm lenv src = do
>     ebs <- readIORef (tbh h)
>     let ebs' = ebs {henv = extendEnv lenv $ henv ebs}
>     (v,ebs'',t) <- evaluate fnm ebs' src
>     case t of
>         [] -> pure ()
>         _ -> putStrLn $ T.renderCheckResults t
>     -- if you press ctrl-c in between the start of
>     -- writeioref and if finishing, or even at another time,
>     -- can this write become corrupted?
>     writeIORef (tbh h) ebs''
>     pure v

> runFunction :: TeaberryHandle -> String -> [Value] -> IO Value
> runFunction h f as = do
>     -- how to give the args unique names? or just use shadow?
>     -- (and the function)
>     v <- runScript h Nothing [] f
>     let as' = zipWith (\i x -> ("aaa-" ++ show i, x)) [(0::Int)..] as
>     runScript h Nothing (("fff", v):as') $ "fff(" ++ intercalate "," (map fst as') ++ ")"

> valueToString :: Value -> Maybe String
> valueToString v = case v of
>     VariantV "nothing" [] -> Nothing
>     _ -> Just $ torepr' v


---------------------------------------

evaluate
========

hacky top level evaluate. not really sure if it will pay its way once
 the kruft is refactored, since it isn't part of the user api either

> evaluate :: Maybe String
>           -> RuntimeState
>           -> String
>           -> IO (Value, RuntimeState, [T.CheckResult])
> evaluate fnm ebs src = do
>     d <- getBuiltInModulesDir
>     fst <$> evalRWST (f d) (makeInterpreterEnv $ henv ebs) emptyInterpreterState
>   where
>     f :: FilePath -> Interpreter (Value, RuntimeState, [T.CheckResult])
>     f d =
>         -- quick hack for set
>         case Px.parseSet src of
>             Right (k,v)
>               | k == "dump-desugar" -> do
>                 case v of
>                      "t" -> pure (TextV "set dump desugar on", ebs {executionStage = DumpDesugar }, [])
>                      "f" -> pure (TextV "set full execution on", ebs {executionStage = FullExecution }, [])
>                      _ -> throwInterp $ "bad value for dump-desugar, expected t or f, got " ++ v
>               | otherwise -> throwInterp $ "unrecognised set key: " ++ k
>             Left _ -> do
>                 (ast,srcs) <- loadSourceFiles d src
>                 y <- either throwInterp pure $ runDesugar (maybe "toplevel.x" id fnm) ast srcs
>                 let z = simplify y
>                 case executionStage ebs of
>                     DumpDesugar -> pure (TextV $ "\n" ++ prettyIExpr z ++ "\n", ebs, [])
>                     FullExecution -> do
>                         v <- {-trace (pretty z) $-} interp z
>                         t <- runAddedTests
>                         case v of
>                             VariantV "tuple" [("0", v'), ("1", VariantV "record" e)] ->
>                                 let ne = (henv ebs) {envEnv = e}
>                                 in pure (v',ebs {henv = ne}, t)
>                             _ -> throwInterp $ "expected 2 element tuple, second one record, got " ++ torepr' v

recursively load all the referenced modules in the source given

> loadSourceFiles :: String -> String -> Interpreter (Module, [(String,Module)])
> loadSourceFiles buildInModDir src = do
>     -- todo: memoize so loading a module twice doesn't read the
>     -- file and parse it twice
>     (ast,rs) <- parseMod src
>     -- reverse the list to put dependencies first
>     -- nub it so it only includes each dependency once
>     x <- (nubBy (\a b -> fst a == fst b) . reverse . concat)
>          <$> mapM loadAndRecurse rs
>     builtins <- liftIO $ readFile (buildInModDir </> "built-ins.tea")
>     (bast,_) <- parseMod builtins
>     -- todo: assert the _ above is actually []
>     pure (ast, ("built-ins", bast) : x)
>   where
>     -- parse the file, get the imports
>     -- recurse on these imports, returning the filename and the
>     -- file contents
>     parseMod s = either throwInterp pure $ do
>                  ast@(Module ps _) <- parse s
>                  let rs = mapMaybe getImp ps
>                  pure (ast, rs)
>     f nm s = do
>              (ast,rs) <- parseMod s
>              ((nm,ast):) . concat <$> mapM loadAndRecurse rs
>     -- todo: a bit confused
>     -- what is the canonical name of a module?
>     -- it's not exactly the same as the file path
>     -- and it's definitely not the filepath of built in modules
>     -- try to find a single place to have this resolution
>     -- it's also used in the desugaring of prelude statements
>     -- before packages, hierarchical names and builtins being
>     -- in different namespaces, it should be the name of the built in
>     -- used by the user (no .tea, no path)
>     -- or the path used in import file(x), with any .tea removed
>     -- and the directory removed if there is one      
>     loadAndRecurse is = do
>         (fn,mn) <- case is of
>                   ImportSpecial "file" [n] -> pure (n,n)
>                   ImportSpecial x _ -> throwInterp $ "import special with " ++ x ++ " not supported"
>                   ImportName n -> pure (buildInModDir </> n ++ ".tea", n)
>         x <- liftIO $ readFile fn
>         f mn x
>     getImp (Import fn _) = Just fn
>     getImp _ = Nothing

> getBuiltInModulesDir :: IO FilePath
> getBuiltInModulesDir = getDataFileName "built-in-modules"

---------------------------------------

simplify

> simplify :: IExpr -> IExpr
> simplify = transformBi $ \case
>     ILet bs (ILet bs' x) -> ILet (bs ++ bs') x
>     x1 -> x1


---------------------------------------

values
======

> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | VariantV String -- variant name
>                       [(String,Value)] -- fields
>            | BoxV Int
>            | FunV [String] IExpr Env
>            | ForeignFunV String

value type name is used for looking up overloaded foreign functions

> valueTypeName :: Value -> String
> valueTypeName NumV {} = "number"
> valueTypeName TextV {} = "text"
> valueTypeName BoolV {} = "boolean"
> valueTypeName VariantV {} = "variant" -- or should it be the variant's type name?
> valueTypeName BoxV {} = "box"
> valueTypeName FunV {} = "function"
> valueTypeName ForeignFunV {} = "foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (VariantV nm fs) = "VariantV " ++ nm ++ "[" ++ intercalate "," (map show fs) ++ "]"
>   show (BoxV n) = "BoxV " ++ show n
>   show FunV {} = "FunV stuff"
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

store holds the values of variables

> newtype Store = Store [(Int, Value)]

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

> data InterpreterState =
>     InterpreterState
>     {addedTests :: [Value]
>     ,testResultLog :: [T.TestResultLog]
>     ,isStore :: Store
>     }

> emptyInterpreterState :: InterpreterState
> emptyInterpreterState = InterpreterState [] [] emptyStore

> data InterpreterException = InterpreterException String
>                           | ValueException Value
>                           deriving Show

> instance Exception InterpreterException where
>   --  displayException (InterpreterException msg) = msg


> data Env = Env
>     {envEnv :: [(String,Value)]
>     ,envForeignFuns :: [((String,[String]), [Value] -> Interpreter Value)]}

> _showEnvNames :: Env -> String
> _showEnvNames e = unlines (map fst (envEnv e)
>                          ++ map (show . fst) (envForeignFuns e))

> emptyEnv :: Env
> emptyEnv = Env
>     {envEnv = []
>     ,envForeignFuns = []}

> extendEnv :: [(String,Value)] -> Env -> Env
> extendEnv bs env = env {envEnv = bs ++ envEnv env}
>
> envLookup :: String -> Env -> Interpreter Value
> envLookup nm env =
>     maybe
>       (--traceStack  "" -- \n----------\n" ++ _showEnvNames env ++"\n-----------\n"
>       throwInterp $ "Identifier not found: " ++ nm {-++ "\n" ++ showEnvNames env-})
>       pure $ lookup nm (envEnv env)

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



> addForeignFun' :: String -> ([String], [Value] -> Interpreter Value) -> Env -> Either String Env
> addForeignFun' nm (tys, f) env = addForeignFun nm tys f env

> addForeignFuns' :: [(String, ([String], [Value] -> Interpreter Value))] -> Env -> Either String Env
> addForeignFuns' [] env = pure env
> addForeignFuns' ((x,y):xs) env = do
>     env' <- addForeignFun' x y env
>     addForeignFuns' xs env'

> data InterpreterEnv = InterpreterEnv
>     {ieEnv :: Env
>     ,traceLevel :: Int
>     }

not sure if it should do this, or keep the rest of the env in the state

> makeInterpreterEnv :: Env -> InterpreterEnv
> makeInterpreterEnv x = InterpreterEnv x 0
  
> type Interpreter = RWST InterpreterEnv () InterpreterState IO


trace the string if the tracel level is >= i

> traceIt :: Int -> String -> Interpreter ()
> traceIt i msg = do
>     l <- traceLevel <$> ask
>     when (l >= i) $ liftIO $ putStrLn msg


---------------------------------------

ffi catalog
===========

> defaultEnv :: Env
> defaultEnv = either error id $ addForeignFuns' (
>    [("+", binaryOp unwrapNum unwrapNum wrapNum (+))
>    ,("*", binaryOp unwrapNum unwrapNum wrapNum (*))
>    ,("/", binaryOp unwrapNum unwrapNum wrapNum divideScientific)
>    ,("-", binaryOp unwrapNum unwrapNum wrapNum (-))
>    ,("-", unaryOp unwrapNum wrapNum (\a -> -a))
>    ,("+", binaryOp unwrapText unwrapText wrapText (++))
>    ,("==", binaryOp anyIn anyIn wrapBool (==))
>    ,("<", binaryOp anyIn anyIn id lt)
>    ,(">", binaryOp anyIn anyIn id gt)
>    ,("<=", binaryOp anyIn anyIn id lte)
>    ,(">=", binaryOp anyIn anyIn id gte)
>    ,("<>", binaryOp anyIn anyIn id neq)

>    -- todo: why doesn't this work?
>    --,("^", binaryOp anyIn functionIn id (\a f -> app f [a]))
>    ,("^", binaryOp anyIn anyIn id (\a f -> app f [a]))
>    ,("|>", binaryOp anyIn anyIn id (\f a -> app f [a]))


>    ,("not", unaryOp unwrapBool wrapBool not)

>    ,("torepr", unaryOp anyIn pure torepr)
>    ,("to-repr", unaryOp anyIn pure torepr)
>    ,("tostring", unaryOp anyIn pure tostring)
>    ,("to-string", unaryOp anyIn pure tostring)
>    ,("print", unaryOp anyIn id ffiprint)
>    ,("sleep", unaryOp unwrapNum id ffisleep)

>    ,("variant-field-get", binaryOp unwrapText variantIn id variantFieldGet)
>    ,("variant-field-get-ord", binaryOp unwrapNum variantIn id variantFieldGetOrd)
>    ,("safe-variant-name", unaryOp variantIn id safeVariantName)
>    -- hack to make it work for any data type     
>    ,("safe-variant-name", unaryOp anyIn pure (const $ nothingLiteralHack))
>    ,("make-variant", binaryOp unwrapText unwrapList id makeVariant)
>    ,("env-to-record", nullaryOp id envToRecord)
>    ,("link", binaryOp anyIn anyIn pure listLink)
>    ,("call-construct-make", binaryOp variantIn variantIn id callConstructMake)

>    -- another mystery
>    --,("tostring-equals", binaryOp unwrapText anyIn wrapBool tostringEquals)
>    ,("tostring-equals", binaryOp anyIn anyIn wrapBool tostringEqualsx)

>    ,("raise", unaryOp anyIn id raise)

>    ,("is-boolean", unaryOp anyIn wrapBool isBoolean)
>    ,("is-number", unaryOp anyIn wrapBool isNumber)
>    ,("is-string", unaryOp anyIn wrapBool isString)
>    ,("is-function", unaryOp anyIn wrapBool isFunction)
>    ,("is-tuple", unaryOp anyIn wrapBool isTuple)
>    ,("is-record", unaryOp anyIn wrapBool isRecord)

>    ,("add-tests", unaryOp functionIn id addTests)
>    ,("log-check-block", binaryOp unwrapNum unwrapText id logCheckBlock)
>    ,("log-test-pass", binaryOp unwrapNum unwrapText id logTestPass)
>    ,("log-test-fail", ternaryOp unwrapNum unwrapText unwrapText id logTestFail)

>    ])
>    $ emptyEnv {envEnv = [("true", BoolV True)
>                         ,("false", BoolV False)
>                         ,("empty", VariantV "empty" [])]}

hardcoded nothing. There is also a syntax version. even though nothing
is defined in built in, it needs bootstrapping like list because a
bunch of the language machinery needs nothing before it's been
created

> nothingLiteralHack :: Value
> nothingLiteralHack = VariantV "nothing" []

> envToRecord :: Interpreter Value
> envToRecord = do
>     rd <- ask
>           
>     pure $ VariantV "record" $ envEnv $ ieEnv rd

> raise :: Value -> Interpreter Value
> raise v = throwM $ ValueException v

 > tostringEquals :: String -> Value -> Bool
 > tostringEquals e0 e1 = TextV e0 == tostring e1

> tostringEqualsx :: Value -> Value -> Bool
> tostringEqualsx e0 e1 = e0 == tostring e1

> isBoolean :: Value -> Bool
> isBoolean (BoolV {}) = True
> isBoolean _ = False

> isNumber :: Value -> Bool
> isNumber (NumV {}) = True
> isNumber _ = False

> isString :: Value -> Bool
> isString (TextV {}) = True
> isString _ = False

> isFunction :: Value -> Bool
> isFunction (FunV {}) = True
> isFunction (ForeignFunV {}) = True
> isFunction _ = False

> isTuple :: Value -> Bool
> isTuple (VariantV "tuple" _) = True
> isTuple _ = False

> isRecord :: Value -> Bool
> isRecord (VariantV "record" _) = True
> isRecord _ = False


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

> torepr' (VariantV "tuple" fs) =
>     "{" ++ intercalate ";" (map (torepr' . snd) fs) ++ "}"
> torepr' (VariantV "record" fs) =
>     "{" ++ intercalate "," (map f fs) ++ "}"
>   where
>     f (a,b) = a ++ " = " ++ torepr' b
> torepr' (VariantV nm []) = nm
> torepr' (VariantV nm fs) =
>     nm ++ "(" ++ intercalate "," (map (torepr' . snd) fs) ++ ")"
 

> tostring :: Value -> Value
> tostring x@(TextV {}) = x
> tostring x = torepr x

> lt :: Value -> Value -> Interpreter Value
> lt (NumV a) (NumV b) = pure $ BoolV $ a < b
> lt (TextV a) (TextV b) = pure $ BoolV $ a < b
> lt (BoolV a) (BoolV b) = pure $ BoolV $ a < b
> lt a b = throwInterp $ "cannot compare " ++ show a ++ " and " ++ show b

> gt :: Value -> Value -> Interpreter Value
> gt (NumV a) (NumV b) = pure $ BoolV $ a > b
> gt (TextV a) (TextV b) = pure $ BoolV $ a > b
> gt (BoolV a) (BoolV b) = pure $ BoolV $ a > b
> gt a b = throwInterp $ "cannot compare " ++ show a ++ " and " ++ show b

> lte :: Value -> Value -> Interpreter Value
> lte (NumV a) (NumV b) = pure $ BoolV $ a <= b
> lte (TextV a) (TextV b) = pure $ BoolV $ a <= b
> lte (BoolV a) (BoolV b) = pure $ BoolV $ a <= b
> lte a b = throwInterp $ "cannot compare " ++ show a ++ " and " ++ show b

> gte :: Value -> Value -> Interpreter Value
> gte (NumV a) (NumV b) = pure $ BoolV $ a >= b
> gte (TextV a) (TextV b) = pure $ BoolV $ a >= b
> gte (BoolV a) (BoolV b) = pure $ BoolV $ a >= b
> gte a b = throwInterp $ "cannot compare " ++ show a ++ " and " ++ show b

> neq :: Value -> Value -> Interpreter Value
> neq (NumV a) (NumV b) = pure $ BoolV $ a /= b
> neq (TextV a) (TextV b) = pure $ BoolV $ a /= b
> neq (BoolV a) (BoolV b) = pure $ BoolV $ a /= b
> neq a b = throwInterp $ "cannot compare " ++ show a ++ " and " ++ show b

> ffiprint :: Value -> Interpreter Value
> ffiprint v = do
>     liftIO $ putStrLn $ case v of
>                             TextV u -> u
>                             _ -> torepr' v
>     pure nothingLiteralHack


> ffisleep :: Scientific -> Interpreter Value
> ffisleep v = do
>     liftIO $ threadDelay (floor (v * 1000 * 1000))
>     pure nothingLiteralHack

> listLink :: Value -> Value -> Value
> listLink a b = VariantV "link" [("first",a),("rest",b)]

  
> safeVariantName :: Value -> Interpreter Value
> safeVariantName (VariantV x _) = pure $ TextV $ dropQualifiers x
> safeVariantName _ = pure $ nothingLiteralHack

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
>     unpackTuple (VariantV "tuple" [("0",BoolV isRef), ("1", TextV nm), ("2", v)]) =
>         if isRef
>         then do
>             v' <- box v
>             pure (nm,v')
>         else pure (nm,v)
>     unpackTuple (VariantV "tuple" x) = throwInterp $ "value in list in make-variant, expected tuple of is-ref, name and val, got " ++ show (map (\(_,b) -> torepr' b) x)
>     unpackTuple x = throwInterp $ "expected tuple in make-variant, got " ++ torepr' x

algo: if there is a matching makeN in the record, use it, else if
 there is a make, use that, else error
todo: can do this in the desugaring once there is a type checker

> callConstructMake :: Value -> Value -> Interpreter Value
> callConstructMake f as = case f of
>     VariantV "record" ts | Just args' <- listToHaskell as -> do
>           let makeN = "make" ++ show (length args')
>           case () of
>               _ | Just g <- lookup makeN ts -> app g args'
>                 | Just g <- lookup "make" ts -> app g [as]
>                 | otherwise -> throwInterp "The left side was not a defined convenience constructor."
>         | otherwise -> throwInterp $ "construct make called on non list: " ++ torepr' as
>     _ -> throwInterp "The left side was not a defined convenience constructor."


---------------------------------------

interp
======

> throwInterp :: String -> Interpreter a
> throwInterp e = throwM $ InterpreterException e
  
> interp :: IExpr -> Interpreter Value
> interp (INum n) = pure (NumV n)
> interp (IText t) = pure (TextV t)
> interp (IVariantSel nm fs) = VariantV nm <$> mapM f fs
>   where
>     f (n,v) = (n,) <$> interp v


> interp (IIden a) = do
>     env <- ask
>     x <- envLookup a $ ieEnv env
>     case x of
>         BoxV i -> do
>                   st <- get
>                   fetchStore i (isStore st)
>         _ -> pure x

> interp (IApp f es) = do
>     fv <- interp f
>     vs <- mapM interp es
>     app fv vs
> interp (ILam ps e) = do
>     env <- ask
>     pure $ FunV ps e $ ieEnv env
> interp (ILet bs e) = do
>     let newEnv [] = interp e
>         newEnv ((b,ex):bs') = do
>             traceIt 1 $ "let " ++ b ++ " = " ++ prettyIExpr ex
>             v <- interp ex
>             traceIt 1 $ "let " ++ b ++ " = " ++ torepr' v
>             local (\x -> x {ieEnv = extendEnv [(b,v)] $ ieEnv x}) $ newEnv bs'
>     newEnv bs

take the fields in the record, and bind them to the current env:
e.g.
r = {a:1,b:2}
expand-record(r)
  ->
a = r.a
b = r.b

> interp (ILetSplat re e) = do
>     x <- interp re
>     case x of
>         VariantV "record" bs ->
>             local (\y -> y {ieEnv = extendEnv bs $ ieEnv y}) $ interp e
>         _ -> throwInterp $ "expected record in letsplat, got " ++ show x
>     -- get value for r in env
>     -- make sure it's a record
>     -- extent the env with the record bindings

  
> interp (ISeq a b) = interp a *> interp b

> interp z@(IIf bs e) = do
>     let f ((c,t):bs') = do
>             c' <- interp c
>             case c' of
>                 BoolV True -> interp t
>                 BoolV False -> f bs'
>                 _ -> throwInterp $ "expected bool in if test, got " ++ show c'
>         f [] = case e of
>                    Just x -> interp x
>                    Nothing -> throwInterp $ "no if branches matched and no else:\n"
>                               ++ prettyIExpr z
>     f bs

> interp (IBox e) = do
>     v <- interp e
>     box v

todo: combine

> interp (ISetBox (IIden b) v) = do
>     env <- ask
>     b' <- envLookup b $ ieEnv env
>     v' <- interp v
>     i <- case b' of
>              BoxV i -> pure i
>              _ -> throwInterp $ "attemped to setbox non box value: " ++ torepr' b'
>     modify $ \s -> updateISStore (extendStore i v') s
>     pure v'

> interp (ISetBox b v) = do
>     b' <- interp b
>     v' <- interp v
>     i <- case b' of
>              BoxV i -> pure i
>              _ -> throwInterp $ "attemped to setbox non box value: " ++ torepr' b'
>     modify $ \s -> updateISStore (extendStore i v') s
>     pure v'
      
> interp (IUnbox b) = do
>     b' <- interp b
>     case b' of
>         BoxV i -> do
>                   st <- get
>                   fetchStore i (isStore st)
>         _ -> throwInterp $ "attemped to unbox non box value: " ++ torepr' b'

> interp (ICatch e c) = interp e `catch` (\case
>     ValueException v -> do
>         cf <- interp c
>         app cf [v]
>     -- is rethrowing an exception like this bad in haskell?
>     s -> throwM s)

  
> app :: Value -> [Value] -> Interpreter Value
> app fv vs = do
>     case fv of
>         FunV ps bdy env' -> do
>             as <- safeZip ps vs
>             let env'' = extendEnv as env'
>             local (\x -> x {ieEnv = env''}) $ interp bdy
>         ForeignFunV nm -> do
>             let tys = map valueTypeName vs
>             env <- ask
>             hf <- lookupForeignFun nm tys $ ieEnv env
>             hf vs
>         _ -> throwInterp "non function value in app position"
>   where
>     safeZip ps xs | length xs == length ps = pure $ zip ps xs
>                   | otherwise = throwInterp "wrong number of args to function"

> box :: Value -> Interpreter Value
> box v = do
>     i <- state $ \s ->
>          let i = newStoreLoc (isStore s)
>          in (i, updateISStore (extendStore i v) s)
>     pure $ BoxV i


------------------------------------------------------------------------------

desugaring
==========

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

> runDesugar :: String -> Module -> [(String,Module)] -> Either String IExpr
> runDesugar nm ast srcs = fst <$> runExcept (evalRWST go () startingDesugarState)
>   where
>     go = do
>          srcs' <- desugarEm [] (srcs ++ [(nm,ast)])
>          -- for repl support, splat out the contents of the last module
>          -- and return the last value of this module plus the env           
>          sf <- desugarStmts [LetSplatDecl (tg 1)
>                             ,StExpr $ TupleSel [tg 0, tg 1]]
>          let y = (combineEm srcs') sf
>          pure y
>     tg i = TupleGet (Iden ("module." ++ nm)) i
>     desugarEm desugaredModules [] = pure $ reverse desugaredModules
>     -- should learn how to use folds better
>     -- would make the code more regular and
>     -- quicker to understand/review?
>     desugarEm desugaredModules ((n,m):ms) = do
>         dsm <- desugarModule (n == "built-ins") m
>         desugarEm (("module." ++ n, dsm):desugaredModules) ms
>     combineEm [] = id
>     combineEm ((n,e):es) = ILet [(n,e)] <$> combineEm es

desugar module:
desugar the prelude

add the last statement which returns the last value and the env, for
  repl/embedded and for imports

> nothingSyntaxHack :: Expr
> nothingSyntaxHack = VariantSel "nothing" []

> desugarModule :: Bool -> Module -> Desugarer IExpr
> desugarModule skipBuiltins (Module ps stmts) = do
>     ps' <- concat <$> mapM desugarPreludeStmt (builtins ++ ps)
>     -- add the final value for repl/embedded and imported module support
>     stmts' <- addTopRet stmts
>     desugarStmts (ps' ++ stmts')
>   where
>     mk x = StExpr $ TupleSel [x, App (Iden "env-to-record") []]
>     addTopRet [] = pure [mk nothingSyntaxHack]
>     addTopRet [StExpr x] = do
>         z <- makeUniqueVar "z"
>         pure [LetDecl z x, mk $ Iden z]
>     addTopRet (x:xs) = (x:) <$> addTopRet xs
>     builtins = if skipBuiltins
>                then []
>                else [Import (ImportName "built-ins") "built-ins"
>                     ,IncludeFrom "built-ins" (map (\a -> ProvideAlias a a) bs)]
>     -- temp before * is supported
>     bs = ["is-empty","is-link", "empty", "link", "is-List", "nothing", "is-Nothing", "is-nothing"]

  
> desugarPreludeStmt :: PreludeStmt -> Desugarer [Stmt]
> desugarPreludeStmt (Import is b) = do
>     n <- importSourceName is
>     pure [LetDecl b (TupleGet (Iden n) 1)]

> desugarPreludeStmt (IncludeFrom nm is) =
>     pure $ flip map is $ \(ProvideAlias n1 n2) ->
>         LetDecl n2 (DotExpr (TupleGet (Iden ("module." ++ nm)) 1) n1)

> importSourceName :: ImportSource -> Desugarer String
> importSourceName (ImportSpecial "file" [s]) = pure $ "module." ++ s
> importSourceName (ImportSpecial x _ ) = lift $ throwE $ "import special with " ++ x ++ " not supported"
> importSourceName (ImportName n) = pure $ "module." ++ n  
  
> desugar :: Expr -> Desugarer IExpr

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

> desugar (RecordSel fs) = desugar (VariantSel "record" fs)

> desugar (Iden i) = pure $ IIden i

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

> desugar (App (Iden "raises") [e0, e1]) = do
>   desugar (App (Iden "raises-satisfies") [e0, Lam ["a"] $ App (Iden "tostring-equals") [e1, Iden "a"]])

> desugar x@(App (Iden "raises-satisfies") [e0,e1]) = do
>   let p = pretty x
>   y <- desugarRaises p e0 e1
>   desugar y


> desugar (App (Iden "or") [a,b]) =
>     desugar (If [(a, Iden "true")] (Just b))

> desugar (App (Iden "and") [a,b]) = do
>     desugar (If [(a, b)] (Just $ Iden "false"))

> desugar (App (Iden "catch") [a,b]) = ICatch <$> desugar a <*> desugar b

> desugar (Catch a b) = ICatch <$> desugar a <*> desugar b


> desugar (App f as) = IApp <$> desugar f <*> mapM desugar as
> desugar (Lam ns e) = ILam ns <$> desugar e
> desugar (Let bs e) = do
>     let f (n,v) = (n,) <$> desugar v
>     ILet <$> mapM f bs <*> desugar e

> desugar (LetRec bs e) = do
>     desugar (Let (mapMaybe convLam bs ++ map createBind bs) e)
>   where
>     newName = (++"'")
>     bindNames = map fst bs
>     bindNames' = map newName bindNames
>     -- fX = lam (asX): bdyX end -> fX' = lam (f0,f1,...,*asX): bdyX' end
>     convLam (n,Lam as bdy) =
>         Just (newName n, Lam (bindNames ++ as) $ patchBody bdy)
>     -- fX = bdyX (something not a lam) -> fX = bdyX'
>     -- not sure about this one
>     convLam _ = Nothing --(newName n, patchBody x)
>     -- fX = lam (asX): bdyX end -> fX = lam(asX): fX'(f0',f1',...,*asX) end
>     createBind (n,Lam as _bdy) =
>         (n, Lam as $ App (Iden $ newName n) (map Iden (bindNames' ++ as)))
>     createBind (n,x) = (n, patchBody x)
>     --bdyN with fN(as) replaced with fN(f0,...fX,*as)
>     patchBody = transformBi $ \case
>         App (Iden f) args | f `elem` bindNames -> App (Iden f) (map Iden bindNames ++ args)
>         x -> x

> {-desugar (LetSplat re e) =
>     ILetSplat <$> desugar re <*> desugar e-}
  
> desugar (Seq a b) =
>     ISeq <$> desugar a <*> desugar b


> desugar (Ask bs e) = desugar (If bs e)

> desugar (If bs e) =
>     IIf <$> mapM f bs <*> case e of
>                              Nothing -> pure Nothing
>                              Just e1 -> Just <$> desugar e1
>   where
>     f (c,t) = (,) <$> desugar c <*> desugar t
  
> desugar (DotExpr e f) = do
>     desugar (App (Iden "variant-field-get") [Text f, e])

> desugar (Box v) = IBox <$> desugar v

no idea if this is good or not

> desugar (SetBox b v) = ISetBox <$> desugar b <*> desugar v

hardcoded list constructor. It isn't quite right since it matches
on the literal list, instead of working for any list value here

> desugar (Construct (Iden "list") es) = desugar $ f es
>   where
>     f [] = Iden "empty"
>     f (v:vs) = App (Iden "link") [v,f vs]

general construct:

all constructs desugar to the same thing:

[xxx: <elements>]
->
xxx.make([list: <elements>])


> desugar (Construct rc es) =
>     desugar (App (Iden "call-construct-make")
>                 [rc,Construct (Iden "list") es])


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

> desugar (Unbox x) = IUnbox <$> desugar x
> desugar (TupleGet e i) = desugar (DotExpr e (show i))

  
> desugarStmts :: [Stmt] -> Desugarer IExpr

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
>         _ -> ISeq desugaredCheck <$> desugarStmts es
>   where
>     appI i as = App (Iden i) as

> desugarStmts [] = lift $ throwE $ "empty block"

testing hack
  
> desugarStmts (StExpr (App (Iden "letsplat") [re]) : xs) =
>     desugarStmts (LetSplatDecl re : xs)
  
> desugarStmts [StExpr e] = desugar e
> desugarStmts [x@(LetDecl {})] = desugarStmts [x, StExpr nothingSyntaxHack]
> desugarStmts [x@(LetSplatDecl {})] = desugarStmts [x, StExpr nothingSyntaxHack]
> desugarStmts [x@(FunDecl {})] = desugarStmts [x, StExpr nothingSyntaxHack]
> desugarStmts [x@(RecDecl {})] = desugarStmts [x, StExpr nothingSyntaxHack]

> desugarStmts (LetDecl n v : es) = desugar (Let [(n,v)] (Block es))


> desugarStmts (LetSplatDecl re : es) = do
>     ILetSplat <$> desugar re <*> desugarStmts es

> desugarStmts es@(st : _) | isRecOrFun st = do
>     (recs,es') <- getRecs es [] []
>     desugar (LetRec recs (Block es'))
>   where
>     getRecs :: [Stmt] -> [(String,Expr)] -> [Stmt] -> Desugarer ([(String,Expr)], [Stmt])
>     getRecs (RecDecl nm bdy : es') recs whrs =
>         getRecs es' ((nm,bdy) : recs) whrs
>     getRecs (FunDecl nm ps bdy whr : es') recs whrs =
>         let bnd = (nm, (Lam ps bdy))
>             whrs' = case whr of
>                   Nothing -> whrs
>                   Just b -> Check (Just nm) b : whrs
>         in getRecs es' (bnd : recs) whrs'
>     getRecs sts recs whrs  = pure (reverse recs,reverse whrs ++ sts)
>     isRecOrFun RecDecl {} = True
>     isRecOrFun FunDecl {} = True
>     isRecOrFun _ = False      

> desugarStmts (FunDecl {}: _) = lift $ throwE $ "Internal: I had a bet with ghc that fundecl pattern was always matched in desugarstmts, and now ghc has won that bet"
> desugarStmts (RecDecl {}: _) = lift $ throwE $ "Internal: I had a bet with ghc that recdecl pattern was always matched in desugarstmts, and now ghc has won that bet"

> desugarStmts (VarDecl n v : es) = desugarStmts (LetDecl n (Box v) : es)


> desugarStmts (StExpr e : es) =
>     ISeq <$> desugar e <*> desugarStmts es

> desugarStmts (When c t : es) =
>     let e = (If [(c, Block [StExpr t
>                            ,StExpr nothingSyntaxHack])]
>                     (Just nothingSyntaxHack))
>     in ISeq <$> desugar e <*> desugarStmts es


> desugarStmts (SetVar n v : es) = do
>     desugarStmts (StExpr (SetBox (Iden n) v) : es)


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
>        pure $ LetDecl ("is-" ++ vnm)
>               (Lam [arg] (appI "safe-variant-name" [Iden arg] `equals` Text vnm))
>    makeIsDat = do
>        arg <- makeUniqueVar "is-dat"
>        let varChecks = map (\vnm -> appI ("is-" ++ vnm) [Iden arg]) varntNms
>            f [] = lift $ throwE "make is dat with no variants"  
>            f [a] = pure a
>            f (a:as) = (a `orf`) <$> f as
>        bdy <- f varChecks
>        pure $ LetDecl ("is-" ++ nm) (Lam [arg] bdy)

if there are no args to the variant, it's a binding to a non lambda
value, not a lambda

>    makeVarnt (VariantDecl vnm []) = pure $ LetDecl vnm (appI "make-variant" [Text vnm, listSel []])

  | pt(x,y) ->
pt = lam (x,y): I.App "make-variant" ["pt",[list: {false, "x";x},{false, "y";y}]]

  | pt(ref x,y) ->
pt = lam (x,y): I.App "make-variant" ["pt",[list: {true, "x";x},{false, "y";y}]]
  

>    makeVarnt (VariantDecl vnm fs) =
>        let fields = listSel $ map makeVField fs
>        in pure $ LetDecl vnm
>                  (Lam (map snd fs) $ appI "make-variant" [Text vnm, fields])
>    makeVField (x,f) = TupleSel [case x of
>                                     Ref -> Iden "true"
>                                     Con -> Iden "false"
>                                ,Text f, Iden f]
>    listSel xs = Construct (Iden "list") xs
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



aexpr raises-satisfies bexpr
desugars to

haskell:
failmsg = "The test operation raise-satisfies failed for the test " ++ pretty bexpr

it could show the value it actually got?

catch(
  block:
    aexpr
    log-test-fail(checkblockid, name, "No exception raised: " + $src)
  end,
  lam(shadow a):
    shadow v1 = bexpr
    if v1(a):
      log-test-pass(checkblockid, name)
    else:
      shadow src = pretty something
      log-test-fail(checkblockid, name, $failmsg)
    end
  end)

> desugarRaises :: String -> Expr -> Expr -> Desugarer Expr
> desugarRaises syn e e1 = do
>     let failMsg = "The test operation raises-satisfies failed for the test "
>                   ++ pretty e1
>     checkBlockIDName <- (maybe (lift $ throwE "'is' test outside check block") pure)
>                         =<< (gets currentCheckBlockIDName)
>     
>     nameit <- makeUniqueVar "isname"
>     v1 <- makeUniqueVar "isv1"
>     pure $ Catch
>         (Block [StExpr e
>                ,LetDecl nameit $ Text syn
>                ,StExpr $ appx "log-test-fail" [Iden checkBlockIDName
>                                              ,Iden nameit
>                                              ,Text "No exception raised"]])
>         (Lam ["a"] $ Block
>          [LetDecl v1 e1
>          ,LetDecl nameit $ Text syn
>          ,StExpr $ If [(appx v1 [Iden "a"]
>                        ,appx "log-test-pass" [Iden checkBlockIDName, Iden nameit])]
>                    $ Just $ appx "log-test-fail"
>                             [Iden checkBlockIDName
>                             ,Iden nameit
>                             ,Text failMsg `plus`
>                              Text ", value was " `plus`
>                              appx "torepr" [Iden "a"]]])
>   where
>       plus a b = appx "+" [a,b]
>       appx nm es = App (Iden nm) es


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
>     pure (nm,Num $ fromIntegral newID)

> exitCheckBlock :: Desugarer ()
> exitCheckBlock = modify $ \s -> s {currentCheckBlockIDName = Nothing}

> getAnonBlockName :: Desugarer String
> getAnonBlockName = state $ \s ->
>     let blockNo = nextAnonymousBlockNumber s
>     in ("check-block-" ++ show blockNo
>        ,s {nextAnonymousBlockNumber = blockNo + 1})


------------------------------------------------------------------------------


test infra
==========

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

this is run at the end of the script to run all the saved tests

> runAddedTests :: Interpreter [T.CheckResult]
> runAddedTests = do
>     ts <- reverse <$> gets addedTests
>     mapM_ (`app` []) ts
>     testLog <- reverse <$> gets testResultLog
>     either throwInterp pure $ T.testLogToCheckResults testLog


------------------------------------------------------------------------------

ffi boilerplate
===============



> nothingWrapper :: (InterpreterState -> InterpreterState) -> Interpreter Value
> nothingWrapper f = modify f *> pure nothingLiteralHack

> _unwrapTuple :: (String, Value -> Interpreter [Value])
> _unwrapTuple = ("tuple", \case
>                           VariantV "tuple" fs -> pure $ map snd fs
>                           x -> throwInterp $ "type: expected tuple, got " ++ show x)

> unwrapNum :: (String, Value -> Interpreter Scientific)
> unwrapNum = ("number", \case
>                           NumV n -> pure n
>                           x -> throwInterp $ "type: expected number, got " ++ show x)


> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n



> unwrapBool :: (String, Value -> Interpreter Bool)
> unwrapBool = ("boolean", \case
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
> anyIn = ("any", pure)

> functionIn :: (String, Value -> Interpreter Value)
> functionIn = ("function", \case
>                           x@(FunV {}) -> pure x
>                           x@(ForeignFunV {}) -> pure x
>                           x -> throwInterp $ "type: expected function, got " ++ show x)

  
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

> nullaryOp :: (a -> Interpreter Value)
>           -> a
>           -> ([String], [Value] -> Interpreter Value)
> nullaryOp wrap f =
>     ([] ,\as ->
>             case as of
>                 [] -> wrap f
>                 _ -> throwInterp $ "wrong number of args to function, expected 0, got " ++ show (length as))




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

parse
=====

> parse :: String -> Either String Module
> parse src =
>     case P.parseModule "" src of
>       Right (S.Module ps sts) -> Module <$> mapM convPrelude ps <*> convStmts sts
>       Left e -> Left e

> convPrelude :: S.PreludeStmt -> Either String PreludeStmt

> convPrelude (S.Import (S.ImportName x) y) = pure $ Import (ImportName x) y
> convPrelude (S.Import (S.ImportSpecial nm as) y) =
>     pure $ Import (ImportSpecial nm as) y

> convPrelude (S.IncludeFrom x as) | Just as' <- mapM f as =
>     pure $ IncludeFrom x as'
>   where
>     f (S.ProvideAlias a b) = Just (ProvideAlias a b)
>     f _ = Nothing          
  
> convPrelude x = Left $ "unsupported prelude statement " ++ show x


  
> convStmts :: [S.Stmt] -> Either String [Stmt]
> convStmts = mapM convSt

> convExpr :: S.Expr -> Either String Expr
> convExpr (S.Sel (S.Num x)) = Right $ Num x
> convExpr (S.Sel (S.Text x)) = Right $ Text x
> convExpr (S.Sel (S.TupleSel fs)) = TupleSel <$> mapM convExpr fs

> convExpr (S.Sel (S.RecordSel fs)) = RecordSel <$> mapM f fs
>   where
>     f (a,b) = (a,) <$> convExpr b

> convExpr (S.Iden s) = Right $ Iden s
> convExpr (S.Parens e) = convExpr e
> convExpr (S.App f es) = App <$> convExpr f <*> mapM convExpr es
> convExpr (S.BinOp e f e1) = do
>     a <- convExpr e
>     b <- convExpr e1
>     pure $ App (Iden f) [a,b]
> convExpr (S.UnaryMinus e) = do
>     a <- convExpr e
>     pure $ App (Iden "-") [a]
> convExpr (S.Lam ps e) = do
>         ps' <- mapM pf ps
>         e' <- convExpr e
>         Right $ Lam ps' e'
>       where
>         pf (S.IdenP (S.PatName _ x)) = Right x
>         pf x = Left $ "parse: unsupported pattern " ++ show x
> convExpr (S.Let bs e) = do
>         bs' <- mapM bf bs
>         e' <- convExpr e
>         Right $ Let bs' e'
>       where
>         bf (S.Binding (S.IdenP (S.PatName _ x)) ex) =
>             (x,) <$> convExpr ex
>         bf x = Left $ "parse: unsupported binding " ++ show x
> convExpr (S.LetRec bs e) = do
>         bs' <- mapM bf bs
>         e' <- convExpr e
>         Right $ LetRec bs' e'
>       where
>         bf (S.Binding (S.IdenP (S.PatName _ x)) ex) =
>             (x,) <$> convExpr ex
>         bf x = Left $ "unsupported binding " ++ show x

> convExpr (S.Block sts) = Block <$> mapM convSt sts

> convExpr (S.If bs e) = do
>     If <$> mapM f bs <*> maybe (pure Nothing) ((Just <$>) . convExpr) e
>   where
>     f (c,t) = (,) <$> convExpr c <*> convExpr t

> convExpr (S.Ask bs e) = do
>     Ask <$> mapM f bs <*> maybe (pure Nothing) ((Just <$>) . convExpr) e
>   where
>     f (c,t) = (,) <$> convExpr c <*> convExpr t


> convExpr (S.DotExpr e f) =
>     flip DotExpr f <$> convExpr e
> convExpr (S.TupleGet e i) = TupleGet <$> convExpr e <*> pure i

> convExpr (S.Cases ty e bs els) =
>     Cases ty <$> convExpr e <*> mapM f bs <*> maybe (pure Nothing) ((Just <$>) .  convExpr) els
>   where
>     f (S.VariantP x vnm ps, ve)
>         | Right ps' <- mapM unpat ps =
>               let vnm' = maybe vnm (\y -> y ++ "." ++ vnm) x
>               in (vnm', ps',) <$> convExpr ve
>     f (S.IdenP (S.PatName _ vnm), ve) = (vnm, [],) <$> convExpr ve
>     f x = Left $ "parse: unsupported pattern: " ++ show x
>     unpat (S.IdenP (S.PatName _ x)) = pure x
>     unpat x = Left $ "parse: unsupported pattern: " ++ show x

> convExpr (S.Unbox e n) = flip UnboxRef n <$> convExpr e

> convExpr (S.Construct e es) = Construct <$> convExpr e <*> mapM convExpr es


> -- convExpr x = Left $ "parse: unsupported expression " ++ show x

> convSt :: S.Stmt -> Either String Stmt
> convSt (S.StExpr e) = StExpr <$> convExpr e
> convSt (S.When c t) = When <$> convExpr c <*> convExpr t
> convSt (S.LetDecl (S.Binding (S.IdenP (S.PatName _ nm)) v)) = LetDecl nm <$> convExpr v
> convSt (S.RecDecl (S.Binding (S.IdenP (S.PatName _ nm)) v)) = RecDecl nm <$> convExpr v
> convSt (S.FunDecl (S.PatName _ nm) ps bdy whr) = do
>     whr' <- case whr of
>                 Nothing -> pure Nothing
>                 Just w -> Just <$> mapM convSt w
>     FunDecl nm <$> mapM pf ps <*> convExpr bdy <*> pure whr'
>   where 
>     pf (S.IdenP (S.PatName _ x)) = Right x
>     pf x = Left $ "unsupported pattern " ++ show x
> convSt (S.Check nm bdy) = Check nm <$> mapM convSt bdy
> convSt (S.VarDecl (S.Binding (S.IdenP (S.PatName _ nm)) v)) = VarDecl nm <$> convExpr v
> convSt (S.SetVar n e) = SetVar n <$> convExpr e
> convSt (S.SetRef e ss) = SetRef <$> convExpr e <*> mapM f ss
>    where
>      f (a,b) = (a,) <$> convExpr b


> convSt (S.DataDecl nm fs whr) = do
>     whr' <- case whr of
>                 Nothing -> pure Nothing
>                 Just w -> Just <$> mapM convSt w
>     DataDecl nm <$> mapM convVarDecl fs <*> pure whr'

> convSt x = Left $ "parse: unsupported statement " ++ show x

> convVarDecl :: S.VariantDecl -> Either String VariantDecl
> convVarDecl (S.VariantDecl nm fs) = pure $ VariantDecl nm $ flip map fs $ \(a,b) -> (f a,b)
>    where
>      f S.Con = Con
>      f S.Ref = Ref


------------------------------------------------------------------------------

pretty
======

> pretty :: Expr -> String
> pretty x = Pr.prettyExpr $ unconv x

> unconv :: Expr -> S.Expr
> unconv (Num n) = S.Sel (S.Num n)
> unconv (Text n) = S.Sel (S.Text n)
> unconv (TupleSel fs) = S.Sel (S.TupleSel $ map unconv fs)
> unconv (VariantSel nm fs) = S.App (S.Iden nm) (map f fs)
>   where
>     f (n,v) = unconv (TupleSel [Text n, v])
> unconv (Construct e es) = S.Construct (unconv e) (map unconv es)  

> unconv (RecordSel fs) = S.Sel (S.RecordSel (map f fs))
>   where
>     f (a,b) = (a,unconv b)
> unconv (Iden s) = S.Iden s
> unconv (Catch e t) = unconv (App (Iden "catch") [e,t])
> unconv (App (Iden e) [a,b]) | isOp e = S.BinOp (unconv a) e (unconv b)
>   where isOp x = not $ any (\z -> isAlphaNum z || z `elem` "_") x
> 
> unconv (App e fs) = S.App (unconv e) $ map unconv fs
> unconv (Lam ns e) = S.Lam (map unconvPattern ns) $ unconv e
> unconv (Let bs e) = S.Let (map (uncurry unconvBinding) bs) (unconv e)
> unconv (LetRec bs e) = S.LetRec (map (uncurry unconvBinding) bs) (unconv e)
> unconv (Block sts) = S.Block $ map unconvStmt sts
> unconv (Seq a b) = S.Block $ map unconvStmt [StExpr a, StExpr b]
> unconv (If bs e) = S.If (map f bs) (fmap unconv e)
>   where
>     f (c,t) = (unconv c, unconv t)
> unconv (Ask bs e) = S.Ask (map f bs) (fmap unconv e)
>   where
>     f (c,t) = (unconv c, unconv t)
> unconv (DotExpr e f) = S.DotExpr (unconv e) f
> unconv (TupleGet t i) = S.TupleGet (unconv t) i
> unconv (Cases ty t bs els) =
>     S.Cases ty (unconv t) (map f bs) (fmap unconv els)
>   where
>     f (n,fs,e) = (S.VariantP Nothing n (map unconvPattern fs), unconv e)
>
> unconv (Box e) = S.App (S.Iden "box") [unconv e]
> unconv (SetBox e f) = S.App (S.Iden "setbox") [unconv e, unconv f]

> unconv (UnboxRef e n) = S.Unbox (unconv e) n
> unconv (Unbox e) = S.App (S.Iden "unbox") [unconv e]

  
> --unconv x = error $ "unconv: " ++ show x

> unconvStmt :: Stmt -> S.Stmt
> unconvStmt (LetDecl n e) = S.LetDecl (unconvBinding n e)
> unconvStmt (RecDecl n e) = S.RecDecl (unconvBinding n e)
> unconvStmt (LetSplatDecl re) = S.StExpr (S.App (S.Iden "letsplatdecl") [unconv re])
> unconvStmt (FunDecl nm ps bdy w) =
>     S.FunDecl (S.PatName S.NoShadow nm) (map unconvPattern ps) (unconv bdy) (fmap (map unconvStmt) w)
> unconvStmt (StExpr e) = S.StExpr (unconv e)
> unconvStmt (When c t) = S.When (unconv c) (unconv t)
> unconvStmt (Check nm bs) = S.Check nm $ map unconvStmt bs
> unconvStmt (VarDecl n e) = S.VarDecl (unconvBinding n e)
> unconvStmt (SetVar n e) = S.SetVar n (unconv e)
> unconvStmt (SetRef e fs) = S.SetRef (unconv e) $ map f fs
>   where
>     f (a,b) = (a,unconv b)

> unconvStmt (DataDecl nm vs w) =
>      S.DataDecl nm (map f vs) (fmap (map unconvStmt) w)
>    where
>      f (VariantDecl vnm fs) = S.VariantDecl vnm $ map uf fs
>      uf (Ref,x) = (S.Ref,x)
>      uf (Con,x) = (S.Con,x)

> unconvBinding :: String -> Expr -> S.Binding
> unconvBinding n v = S.Binding (unconvPattern n) (unconv v)

> unconvPattern :: String -> S.Pat
> unconvPattern n = S.IdenP (S.PatName S.NoShadow n)


> prettyIExpr :: IExpr -> String
> prettyIExpr x = Pr.prettyExpr $ unconvI x

> unconvI :: IExpr -> S.Expr

> unconvI (INum n) = S.Sel (S.Num n)
> unconvI (IText n) = S.Sel (S.Text n)
> unconvI (IVariantSel nm fs) = S.App (S.Iden nm) (map f fs)
>   where
>     f (n,v) = S.Sel (S.TupleSel $ map unconvI [IText n, v])

> unconvI (IIden s) = S.Iden s
> unconvI (ICatch e t) = unconvI (IApp (IIden "catch") [e,t])
> unconvI (IApp (IIden e) [a,b]) | isOp e = S.BinOp (unconvI a) e (unconvI b)
>   where isOp x = not $ any (\z -> isAlphaNum z || z `elem` "_") x
> 
> unconvI (IApp e fs) = S.App (unconvI e) $ map unconvI fs
> unconvI (ILam ns e) = S.Lam (map unconvIPattern ns) $ unconvI e
> unconvI (ILet bs e) = S.Let (map (uncurry unconvIBinding) bs) (unconvI e)
> unconvI (ISeq a b) = S.Block $ map (S.StExpr . unconvI) [a, b]
> unconvI (IIf bs e) = S.If (map f bs) (fmap unconvI e)
>   where
>     f (c,t) = (unconvI c, unconvI t)
> unconvI (IBox e) = S.App (S.Iden "box") [unconvI e]
> unconvI (ISetBox e f) = S.App (S.Iden "setbox") [unconvI e, unconvI f]
> unconvI (IUnbox e) = S.App (S.Iden "unbox") [unconvI e]
> unconvI (ILetSplat re e) = S.App (S.Iden "letsplat") [unconvI re, unconvI e]

> unconvIBinding :: String -> IExpr -> S.Binding
> unconvIBinding n v = S.Binding (unconvIPattern n) (unconvI v)

> unconvIPattern :: String -> S.Pat
> unconvIPattern n = S.IdenP (S.PatName S.NoShadow n)

------------------------------------------------------------------------------

tests
=====

> tests :: T.TestTree
> tests = T.testGroup "front" [tests1, tests4]

> testFiles :: [FilePath]
> testFiles =
>     ["agdt.tea"
>     ,"ahoy.tea"
>     ,"arithmetic.tea"
>     --,"binding.tea"  tuple binding
>     ,"blocks.tea"
>     ,"boolean.tea"
>     ,"built-in-functions.tea"
>     ,"catch.tea"
>
>     ,"check_block_closures.tea"
>     ,"check_closure.tea"
>     ,"check.tea"
>     ,"comparisons.tea"
>     ,"construct.tea"
>     ,"empty_check.tea"
>     ,"empty.tea"
>     ,"functions.tea"
>     ,"identifiers.tea"
>     ,"if_ask.tea"
>     ,"let.tea"
>     ,"lists-basics.tea"
>     --,"lists.tea" provides
>     --,"my-module-provide-all.tea"
>     --,"my-module-provide-x.tea"
>     --,"my-module.tea"
>     --,"my-module-two-provides.tea"
>     ,"nested_comment.tea"
>     --,"prelude-built-in-module.tea"
>     --,"prelude-combine-provides.tea"
>     --,"prelude-import-from.tea"
>     --,"prelude-include-module.tea"
>     --,"prelude-local-module.tea"
>     --,"prelude-provide-all.tea"
>     --,"prelude-provide-x.tea"
>     --,"random.tea" -- change in letrec desugaring, possibly bug
>     ,"records.tea"
>     ,"recursive.tea"
>     ,"ref.tea"
>     --,"tour.tea" -- next issue is provide
>     ,"trivial_is.tea"
>     --,"tuples.tea" tuple binding
>     ,"two_same_name_check.tea"
>     ,"vars.tea"
>     ,"when.tea"
>     --,"where.tea" issue with where desugaring and nested functions
>     ]

> tests1 :: T.TestTree
> tests1 = T.testGroup "front1" $
>     map (\f -> T.makeTestsIO ("front" ++ f)
>             $ runFileTests ("examples/tests/fulltests" </> f))
>     testFiles

> runFileTests :: FilePath -> IO (Either String [T.CheckResult])
> runFileTests fp = do
>     src <- readFile fp
>     (_,_,ts) <- evaluate (Just fp) defaultRuntimeState src
>     pure $ Right ts


> tests4 :: T.TestTree
> tests4 = T.testGroup "front4"
>     [testSanityArith
>     ,testEnvKept
>     ,testEnvOverridden
>     ,testAScript
>     ,testRunScriptWithValues
>     ,testRunFunctionSimple
>     --,testRunFunctionPartialApp
>     ]

> testSanityArith :: T.TestTree
> testSanityArith = T.testCase "testSanityArith4" $ do
>     h <- newTeaberryHandle
>     v <- runScript h Nothing [] "1 + 2"
>     T.assertEqual "" (NumV 3) v


> testEnvKept :: T.TestTree
> testEnvKept = T.testCase "testEnvKept" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h Nothing [] "a = 1"
>     v <- runScript h Nothing [] "a"
>     T.assertEqual "" (NumV 1) v

> testEnvOverridden :: T.TestTree
> testEnvOverridden = T.testCase "testEnvOverridden" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h Nothing [] "a = 1"
>     _ <- runScript h Nothing [] "a = 4"
>     v <- runScript h Nothing [] "a"
>     T.assertEqual "" (NumV 4) v

*****
TODO: test running something with tests and there's a test failure


> testAScript :: T.TestTree
> testAScript = T.testCase "testAScript" $ do
>     let script = "a = 3\nb = 4\na + b"
>     h <- newTeaberryHandle
>     v <- runScript h Nothing [] script
>     T.assertEqual "" (NumV 7) v

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
>     v <- runScript h Nothing
>                      [("a", NumV 3)
>                      ,("b", NumV 11)] "a + b"
>     T.assertEqual "" (NumV 14) v

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
>     _ <- runScript h Nothing [] "f = lam(x,y): x + y end"
>     v <- runFunction h "f" [NumV 5, NumV 11]
>     T.assertEqual "" (NumV 16) v

the next test should work as soon as partial application is
implemented

> {-testRunFunctionPartialApp :: T.TestTree
> testRunFunctionPartialApp = T.testCase "testRunFunctionPartialApp" $ do
>     h <- newTeaberryHandle
>     _ <- runScript h [] "f = lam(x,y): x + y end"
>     v <- runFunction h "f(_,4)" [NumV 5]
>     T.assertEqual "" (Right (NumV 9)) v-}


