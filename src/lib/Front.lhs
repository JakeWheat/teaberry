

Represents the combination of all the ideas that made it

think about how to split this file:
syntax, interpreter, desugarer, tests

=============================================================================

imports

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE MultiWayIf #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module Front (TeaberryHandle
>              ,newTeaberryHandle
>              ,runScript
>              ,runScriptWithTests
>              ,runFunction
>              ,Value(..)
>              ,valueToString
>              ,CheckResult(..)
>              ) where

> import Prelude hiding ((<>))
> import Text.PrettyPrint (render, text, (<>), (<+>), -- empty, parens,
>                          nest, Doc, punctuate, {-comma,-} sep {-,quotes-}
>                          {-,doubleQuotes-}
>                          {-,braces, ($$), ($+$),-} {-vcat-})

> import Text.Read (readMaybe)

> import qualified Parse as P
> import Syntax
> import qualified Pretty as Pr

> -- import qualified ParserExtra as Px

> import Control.Monad.Trans.Class (lift)
> --import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST
>                                --,evalRWST
>                                ,runRWST
>                                ,ask
>                                ,local
>                                --,get
>                                ,gets
>                                ,state
>                                --,put
>                                ,modify
>                                --,asks
>                                )
> import Control.Exception.Safe (Exception, throwM, catch)
> import Control.Concurrent (threadDelay)

> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Data (Data)

> import Control.Monad (when,void)
> import Data.Maybe (mapMaybe)
>
> import Data.Char (isAlphaNum)

> import Scientific (Scientific, divideScientific, showScientific)
> import Data.List (intercalate, nubBy, sortOn, findIndex, isPrefixOf, tails, find, nub, (\\))

> -- import Debug.Trace (trace, traceStack)
> --import qualified TestUtils as T

> import Data.IORef (IORef, newIORef, readIORef, writeIORef)

> import Paths_teaberry
> import System.FilePath ((</>), takeDirectory)

> import Data.Dynamic (Dynamic, toDyn, fromDynamic, Typeable)

> import Text.Show.Pretty (ppShow)


------------------------------------------------------------------------------

interpreter syntax
==================

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

> data RuntimeState = RuntimeState
>     {henv :: InterpreterEnv
>     ,hstate :: InterpreterState
>     }

> defaultRuntimeState :: RuntimeState
> defaultRuntimeState = RuntimeState {henv = makeInterpreterEnv defaultEnv
>                                    ,hstate = emptyInterpreterState}
  
> newTeaberryHandle :: IO TeaberryHandle
> newTeaberryHandle = do
>     th <- TeaberryHandle <$> newIORef defaultRuntimeState
>     _ <- runScript th (Just "initialize") [] "include built-ins"
>     pure th

runs a script then runs a callback on the result from running the script

> additionalLoadTracing :: Bool
> additionalLoadTracing = False

todo: plenty of duplication and refactoring possibilities here

> runScriptInternal :: TeaberryHandle
>                   -> Maybe String
>                   -> [(String,Value)]
>                   -> String
>                   -> (Value -> Interpreter ([(String,Value)],a))
>                   -> IO a
> runScriptInternal th mfn lenv src cb = do
>     -- loadfiles - this loads and parses the files following the dependencies
>     -- it only parses them because it needs to do this to load the dependencies
>     -- you get back a list of module name/ module sources
>     -- plus the script
>     -- then, in a loop do:
>     --   desugar the module, run it, save it to the environment
>     -- then finally run the script
>     let fn = maybe "" id mfn
>     evaluateWithHandle th $ interpWithAdditionalEnv lenv $ do
>         when additionalLoadTracing $ lift $ putStrLn $ "load script" 
>         (sast,mods) <- loadScript fn src
>         let f [] = do
>                 when additionalLoadTracing $ lift $ putStrLn $ "compile script"
>                 compiled <- doDesugar $ desugarScript False sast
>                 when additionalLoadTracing $ traceExtra compiled
>                 interp compiled
>             f ((is,ast):srcs) = do
>                 when additionalLoadTracing $ lift $ putStrLn $ "compile " ++ show is
>                 (nm,compiled) <- doDesugar $ desugarModule is ast
>                 when additionalLoadTracing $ traceExtra compiled
>                 v <- interp compiled
>                 lift $ putStrLn $ "loaded " ++
>                     (case is of
>                          ImportName x -> x
>                          ImportSpecial "file" [ifn] -> "\"" ++ ifn ++ "\""
>                          _ -> show is)
>                 interpWithAdditionalEnv [(nm,v)] $ f srcs
>         cb =<< f mods
>  where
>    traceExtra compiled = do
>        rd <- ask
>        lift $ putStrLn $ "ENV:\n" ++ torepr' (VariantV "record" $ envEnv $ ieEnv rd)
>        lift $ putStrLn $ "COMPILED:\n" ++ prettyIExpr compiled
>        lift $ putStrLn $ ppShow compiled
>        lift $ putStrLn $ "run"
> 

> runScript :: TeaberryHandle -> Maybe String -> [(String,Value)] -> String -> IO Value
> runScript th mfn lenv src =
>     runScriptInternal th mfn lenv src $ \x -> do
>         case x of
>             VariantV "tuple" [("0", v)
>                              ,("1", VariantV "record" e)] -> pure (e, v)
>             _ -> throwInterp $ "expected 2 element tuple with second element being a record, got this: "
>                     ++ torepr' x

> runScriptWithTests :: TeaberryHandle
>                    -> Maybe String
>                    -> [(String,Value)]
>                    -> String
>                    -> IO [CheckResult]
> runScriptWithTests th mfn lenv src =
>     runScriptInternal th mfn lenv src $ \x -> do
>         case x of
>             VariantV "tuple" [("0", _v)
>                              ,("1", e'@(VariantV "record" e))] -> do
>                 tv <- interpFunction "lam(x): test-results-to-haskell(run-tests(x._module_tests)) end" [e']
>                 (e,) <$> case tv of
>                     FFIVal cs' ->
>                         case fromDynamic cs' of
>                             Just trs -> pure (trs :: [CheckResult])
>                             Nothing -> throwInterp $ "expected [checkresult], got " ++ show cs'
>                     _ -> throwInterp $ "expected FFIVal, got " ++ show tv
>             _ -> throwInterp $ "expected 2 element tuple with second element being a record, got this: "
>                     ++ torepr' x

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

this is the helper function which enters the interpreter monad using
runRWST, and also handles saving the updated interpreter state into
the handle

> evaluateWithHandle :: TeaberryHandle
>                    -> Interpreter ([(String,Value)], a)
>                    -> IO a
> evaluateWithHandle h f = do
>     rts <- readIORef (tbh h)
>     (x,s,_) <- runRWST f (henv rts) (hstate rts)
>     let ie = let y = (henv rts)
>              in y {ieEnv = (ieEnv y) {envEnv = fst x}} 
>     writeIORef (tbh h) $ RuntimeState ie s
>     pure (snd x)

-------------------------------------

load script
-------------

Take a script which is a list of statements. It can have imports at
the top and variations, but cannot have provides. Then regular
teaberry statements follow

This function reads the imports and recurses through them all to get
all the dependencies, then returns them in an order you can load them
one by one and it will work.

Since it has to parse the files, it also returns a 'cache' of all the
files loaded and parsed. In the future, it could try to only read
enough of each file to get the imports, and then later the system can
load each file one at a time and flexibly cache them, in case this
improves the performance or allows huge programs to be loaded that use
too much memory otherwise (although this seems unlikely on modern
hardware).

But it might give a better ux to not parse every single file in it's
entirety before running the modules one by one - faster to get to the
first error, and you are closer to getting any errors from the first
module in the list completely instead of this having errors, but
instead you see parse errors from a later module first.

Two main entry points:
load a script from a file
load a script that's already in a string

The auxiliary function is:
load a module - you pass the module name and it finds the right path

it's possible that a useful function in the future would be load a
module from a string instead of from a file, e.g. to help with a
plugin system that can support import from urls or databases or
something

If you don't have a filename, then pass in a name which doesn't look
like path, or if you pass something that looks like a path with / in
it, then you want this to be used as the working dir for relative path
imports in the script. If there's no path, this code will use the cwd
of the process

> loadScript :: FilePath
>            -> String
>            -> Interpreter (Module, [(ImportSource,Module)])
> loadScript fnm src = do
>     -- parse the source
>     -- recurse on each referenced module
>     -- reverse the final list and nub it
>     d <- lift $ getBuiltInModulesDir
>     -- todo: memoize so loading a module twice doesn't read the
>     -- file and parse it twice
>     (ast,rs) <- parseAndGetImports src
>     --lift $ putStrLn $ "got top imports: " ++ ppShow rs
>     -- reverse the list to put dependencies first
>     -- nub it so it only includes each dependency once
>     x <- (nubBy (\a b -> fst a == fst b) . reverse . concat)
>          <$> mapM (loadAndRecurse d (takeDirectory fnm)) rs
>     --lift $ putStrLn $ "all imports: " ++ ppShow (map fst x)
>     pure (ast, x)
>   where
>     -- parse the file, get the imports
>     -- recurse on these imports, returning the filename and the
>     -- file contents
>     -- the swd is the script working directory, so that imports
>     -- are relative to the path of the file that the import is in
>     parseAndGetImports s = either throwInterp pure $ do
>         ast@(Module ps _) <- P.parseModule fnm s -- todo: get the filename in here
>         let rs = mapMaybe getImportSource ps
>         pure (ast, rs)
>     loadAndRecurse d swd is = do
>         (fn,_mn) <- importSourceToFileNameModName d swd is
>         src' <- lift $ readFile fn
>         (ast,rs) <- parseAndGetImports src'
>         ((is,ast):) . concat <$> mapM (loadAndRecurse d (takeDirectory fn)) rs
>     getImportSource (Import is _) = Just is
>     getImportSource (Include is) = Just is
>     getImportSource _ = Nothing

> getBuiltInModulesDir :: IO FilePath
> getBuiltInModulesDir = getDataFileName "built-in-modules-snapshot4"

> importSourceToFileNameModName :: FilePath -> FilePath -> ImportSource -> Interpreter (FilePath, String)
> importSourceToFileNameModName _ cwd (ImportSpecial "file" [n]) =
>     pure (if cwd == ""
>           then n
>           else cwd </> n, n)
> importSourceToFileNameModName _ _ (ImportSpecial x _) = throwInterp $ "import special with " ++ x ++ " not supported"
> importSourceToFileNameModName builtInModDir _ (ImportName n) = pure (builtInModDir </> n ++ ".tea", n)


---------------------------------------

values
======

> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | VariantV String -- variant name
>                       [(String,Value)] -- fields
>            | BoxV (IORef Value)
>            | FunV [String] IExpr Env
>            | ForeignFunV String
>            | FFIVal Dynamic

value type name is used for looking up overloaded foreign functions

> valueTypeName :: Value -> String
> valueTypeName NumV {} = "Number"
> valueTypeName TextV {} = "Text"
> valueTypeName BoolV {} = "Boolean"
> valueTypeName VariantV {} = "Variant" -- or should it be the variant's type name?
> valueTypeName BoxV {} = "Box"
> valueTypeName FunV {} = "Function"
> valueTypeName ForeignFunV {} = "Foreign-function"
> valueTypeName FFIVal {} = "FFI-val"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (VariantV nm fs) = "VariantV " ++ nm ++ "[" ++ intercalate "," (map show fs) ++ "]"
>   show (BoxV _n) = "BoxV XX" -- ++ show n
>   show (FunV as bdy _env) = "FunV " ++ show as ++ "\n" ++ prettyIExpr bdy
>   show (ForeignFunV n) = "ForeignFunV " ++ show n
>   show (FFIVal n) = "FFIVal " ++ show n

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
>     {desugarState :: DesugarState
>     }

> emptyInterpreterState :: InterpreterState
> emptyInterpreterState = InterpreterState startingDesugarState

> data InterpreterException = InterpreterException String
>                           | ValueException Value


> instance Show InterpreterException where
>     show (InterpreterException s) = s
>     show (ValueException v) = torepr' v

> instance Exception InterpreterException where
>   --  displayException (InterpreterException msg) = msg

-------------------------------------

language exceptions

> raiseValue :: Expr -> Interpreter a
> raiseValue e = do
>     _ <- interpAst $ Module [] [StExpr $ eapp "raise" [e]]
>     throwInterp "internal error: should have raised"

> eapp :: String -> [Expr] -> Expr
> eapp f as = App (Iden f) as

> throwUnboundIdentifier :: String -> Interpreter a
> throwUnboundIdentifier "empty" = error $ "unbound identifier empty - this happens when the code is completely broken"
> throwUnboundIdentifier i =
>     if (i == "unbound-identifier")
>     then error ("unbound loop")
>     else raiseValue $ eapp "unbound-identifier" [Text i]

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

-------------------------------------

env

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
> extendEnv bs env = env {envEnv = nubBy (\a b -> fst a == fst b) (bs ++ envEnv env)}
>
> envLookup :: String -> Env -> Interpreter Value
> envLookup nm env =
>     maybe
>       --traceStack  "" -- \n----------\n" ++ _showEnvNames env ++"\n-----------\n"
>       (throwUnboundIdentifier nm) pure $ lookup nm (envEnv env)
>       -- throwInterp $ "Identifier not found: " ++ nm {-++ "\n" ++ showEnvNames env-})
>       -- pure $ lookup nm (envEnv env)

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
>     when (l >= i) $ lift $ putStrLn msg


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

>    ,("^", binaryOp anyIn functionIn id (\a f -> app f [a]))
>    ,("|>", binaryOp anyIn anyIn id (\f a -> app f [a]))


>    ,("not", unaryOp unwrapBool wrapBool not)

>    ,("torepr", unaryOp anyIn pure torepr)
>    ,("to-repr", unaryOp anyIn pure torepr)
>    ,("tostring", unaryOp anyIn pure tostring)
>    ,("to-string", unaryOp anyIn pure tostring)
>    ,("print", unaryOp anyIn id ffiprint)
>    ,("sleep", unaryOp unwrapNum id ffisleep)
>    ,("debug-print", unaryOp anyIn id debugPrint)

>    ,("variant-field-get", binaryOp unwrapText variantIn id variantFieldGet)
>    ,("variant-field-get-ord", binaryOp unwrapNum variantIn id variantFieldGetOrd)
>    ,("safe-variant-name", unaryOp anyIn id safeVariantName)
>    ,("make-variant", binaryOp unwrapText unwrapList id makeVariant)
>    ,("make-variant-0", unaryOp unwrapText id makeVariant0)
>    ,("make-variant-2", ternaryOp anyIn anyIn anyIn id makeVariant2)
>    ,("env-to-record", nullaryOp id envToRecord)
>    ,("call-construct-make", binaryOp variantIn variantIn id callConstructMake)

>    ,("raise", unaryOp anyIn id raise)

>    ,("is-boolean", unaryOp anyIn wrapBool isBoolean)
>    ,("is-number", unaryOp anyIn wrapBool isNumber)
>    ,("is-string", unaryOp anyIn wrapBool isString)
>    ,("is-function", unaryOp anyIn wrapBool isFunction)
>    ,("is-tuple", unaryOp anyIn wrapBool isTuple)
>    ,("is-record", unaryOp anyIn wrapBool isRecord)

>    ,("make-check-result-list", nullaryOp pure makeCheckResultList)
>    ,("haskell-cons-cr", binaryOp unwrapDyn unwrapDyn wrapDyn haskellConsCr)
>    ,("make-cr", binaryOp unwrapText unwrapList id makeCr)

>    ,("provides", unaryOp anyIn id providesImpl)

>    ,("mhs", unaryOp unwrapText pure makeHaskellString)
>    ,("ghs", unaryOp anyIn id getHaskellString)

>    ,("text-length", unaryOp unwrapText wrapInt length)
>    ,("text-char-at", binaryOp unwrapText unwrapInt wrapText textCharAt)
>    ,("text-starts-with", binaryOp unwrapText unwrapText wrapBool isPrefixOf)
>    ,("text-drop-prefix", binaryOp unwrapText unwrapInt wrapText (flip drop))
>    ,("text-index-of", binaryOp unwrapText unwrapText wrapInt textIndexOf)
>    ,("text-length", unaryOp unwrapText wrapInt length)
>    -- "string" is too weird, let's use text instead ...
>    ,("text-substring", ternaryOp unwrapText unwrapInt unwrapInt wrapText textSubstring)

>    ,("string-to-number", unaryOp unwrapText id stringToNumber)
>    ,("string-index-of", binaryOp unwrapText unwrapText wrapNum stringIndexOf)
>    ,("string-append", binaryOp unwrapText unwrapText wrapText (++))
>    ,("num-to-string", unaryOp unwrapNum wrapText showScientific)


>    ,("show-desugar-script", unaryOp unwrapText id showDesugar)
>    ,("show-desugar-file", unaryOp unwrapText id showDesugarScript)
>    ,("show-desugar-module", unaryOp unwrapText id showDesugarModule)
>    ,("show-desugar-builtin-module", unaryOp unwrapText id showDesugarBuiltInModule)


>    ])
>    $ emptyEnv {envEnv = [("true", BoolV True)
>                         ,("false", BoolV False)]}



> makeCheckResultList :: Value
> makeCheckResultList = FFIVal $ toDyn ([] :: [CheckResult])

written out in full to add the type easily

> haskellConsCr :: CheckResult -> [CheckResult] -> [CheckResult]
> haskellConsCr = (:)


> showDesugar :: String -> Interpreter Value
> showDesugar src = do
>     md <- either throwInterp pure $ P.parseModule "" src
>     compiled <- doDesugar $ desugarScript False md
>     pure $ TextV $ prettyIExpr compiled

> showDesugarModule :: FilePath -> Interpreter Value
> showDesugarModule fn = showDesugarImportSource $ ImportSpecial "file" [fn]

> showDesugarBuiltInModule :: String -> Interpreter Value
> showDesugarBuiltInModule mn = showDesugarImportSource $ ImportName mn

> showDesugarImportSource :: ImportSource -> Interpreter Value
> showDesugarImportSource is = do
>     d <- lift $ getBuiltInModulesDir
>     fn' <- fst <$> importSourceToFileNameModName d "" is
>     src <- lift $ readFile fn'
>     ast <- either throwInterp pure $ P.parseModule fn' src
>     (nm,compiled) <- doDesugar $ desugarModule is ast
>     -- todo: just print let nm = and then indent the content so
>     -- it looks like a let statement
>     pure $ TextV $ prettyIExpr (ILet [(nm,compiled)] (IIden "handle environment"))

> showDesugarScript :: String -> Interpreter Value
> showDesugarScript fn = do
>     src <- lift $ readFile fn
>     showDesugar src




> textSubstring :: String -> Int -> Int -> String
> textSubstring str start end = take (end - start) (drop start str)

> data CheckResult = CheckResult String -- the test block name
>                               [(String, Maybe String)]
>                  deriving Show

> makeCr :: String -> [Value] -> Interpreter Value
> makeCr nm vs = do
>     vs' <- mapM f vs
>     pure $ FFIVal $ toDyn $ CheckResult nm vs'
>   where
>     f (VariantV "tuple" [("0", TextV tnm)
>                         ,("1", TextV msg)]) = pure (tnm, if msg == "OK"
>                                                          then Nothing
>                                                          else Just msg)
>     f x = throwInterp $ "expected tuple<text,text>, got " ++ show x

> textIndexOf :: String -> String -> Int
> textIndexOf str i = case findIndex (isPrefixOf i) (tails str) of
>     Nothing -> -1
>     Just n -> n

> providesImpl :: Value -> Interpreter Value
> providesImpl x = do
>     pis <- teaToPIS x
>     rd <- ask
>     let r = envEnv $ ieEnv rd
>     newR <- mapM (apPi r) pis
>     pure $ VariantV "record" $ concat newR
>   where
>     apPi r IProvideAll = pure r
>     apPi r (IProvide i a) = case lookup i r of
>         Just v -> pure [(a,v)]
>         Nothing -> throwInterp $ "provide item not found: " ++ i
>         

> stringIndexOf :: String -> String -> Scientific
> stringIndexOf str substr = case findIndex (isPrefixOf substr) (tails str) of
>     Just n -> fromIntegral n
>     Nothing -> -1

Returns a String containing the character at the string index n from String n.
dodgy: returns an empty string if index is out of range

> textCharAt :: String -> Int -> String
> textCharAt s i =
>     if i < 0
>     then ""
>     else go s i
>   where
>     go [] _ = ""
>     go (x:_) 0 = [x]
>     go (_:xs) j = go xs (j -1)



> stringToNumber :: String -> Interpreter Value
> stringToNumber s = case readMaybe s of
>     Just x -> interpFunction "some" [NumV x]
>     Nothing -> interpSrc "none"


hardcoded nothing. There is also a syntax version. even though nothing
is defined in built in, it needs bootstrapping like list because a
bunch of the language machinery needs nothing before it's been
created

> nothingValueHack :: Value
> nothingValueHack = VariantV "nothing" []


> makeHaskellString :: String -> Value
> makeHaskellString s = FFIVal $ toDyn s

> getHaskellString :: Value -> Interpreter Value
> getHaskellString (FFIVal v) = case fromDynamic v of
>     Just x -> pure $ TextV x
>     Nothing -> throwInterp $ "expected string in dynamic val, got " ++ show v

> getHaskellString x = throwInterp $ "expected FFIVal in ghs, got " ++ show x

> envToRecord :: Interpreter Value
> envToRecord = do
>     rd <- ask
>     pure $ VariantV "record" $ envEnv $ ieEnv rd

> raise :: Value -> Interpreter Value
> raise v = throwM $ ValueException v

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
> torepr' = render . toreprx
> 
> toreprx :: Value -> Doc
> toreprx (NumV n) = case extractInt n of
>                              Just x -> text $ show x
>                              Nothing ->  text $ show n
> toreprx (BoolV n) = text $ if n then "true" else "false"
> toreprx (FunV {}) = text "<Function>"
> toreprx (ForeignFunV {}) = text "<Function>"
> toreprx (TextV s) = text $ "\"" ++ s ++ "\""
> toreprx (BoxV {}) = text "<Box>"

> toreprx (VariantV "tuple" fs) =
>     text "{" <> nest 2 (xSep ";" $ map (toreprx . snd) fs) <> text "}"
> toreprx (VariantV "record" fs) =
>     text "{" <> nest 2 (xSep "," $ map f fs) <> text "}"
>   where
>     f (a,b) = text a <+> text "=" <+> toreprx b
> toreprx (VariantV nm []) = text nm
> toreprx (VariantV nm fs) =
>     text nm <> text "(" <> nest 2 (xSep "," $ map (toreprx . snd) fs) <> text ")"
> toreprx (FFIVal f) = text "ffi-val(" <> text (show f) <> text ")"

> xSep :: String -> [Doc] -> Doc
> xSep x ds = sep $ punctuate (text x) ds


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
> neq a b = pure $ BoolV $ not $ a == b

> ffiprint :: Value -> Interpreter Value
> ffiprint v = do
>     lift $ putStrLn $ case v of
>                             TextV u -> u
>                             _ -> torepr' v
>     pure nothingValueHack

> debugPrint :: Value -> Interpreter Value
> debugPrint v = do
>     lift $ putStrLn $ show v
>     pure nothingValueHack


> ffisleep :: Scientific -> Interpreter Value
> ffisleep v = do
>     lift $ threadDelay (floor (v * 1000 * 1000))
>     pure nothingValueHack

  
> safeVariantName :: Value -> Interpreter Value
> safeVariantName (VariantV x _) = pure $ TextV $ dropQualifiers x
> safeVariantName _ = pure $ nothingValueHack

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
>         BoxV i -> lift $ readIORef i
>         _ -> pure x

> interp (IApp f es) = do
>     fv <- interp f
>     vs <- mapM interp es
>     app fv vs
> interp (ILam ps e) = do
>     case ps \\ nub ps of
>         (x:_) -> throwDuplicateName x        
>         [] -> pure ()
>     env <- ask
>     pure $ FunV ps e $ ieEnv env
> interp (ILet bs e) = do
>     let ps = map fst bs      
>     case ps \\ nub ps of
>         (x:_) -> throwDuplicateName x        
>         [] -> pure ()
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
let-splat(r)
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

  
> interp (ISeq a b) = do
>     traceIt 1 $ prettyIExpr a
>     void $ interp a
>     traceIt 1 $ prettyIExpr b
>     interp b

> interp (IIf [_] Nothing) = throwOnlyOneBranch

> interp (IIf bs e) = do
>     let f ((c,t):bs') = do
>             c' <- interp c
>             case c' of
>                 BoolV True -> interp t
>                 BoolV False -> f bs'
>                 _ -> throwExpectedType "Boolean" (valueTypeName c')
>         f [] = case e of
>                    Just x -> interp x
>                    Nothing -> throwNoBranchesSatisfied
>     f bs

> interp (IBox e) = do
>     v <- interp e
>     box v

todo: combine

> interp (ISetBox (IIden b) v) = do
>     env <- ask
>     b' <- envLookup b $ ieEnv env
>     v' <- interp v
>     case b' of
>              BoxV i -> lift $ writeIORef i v'
>              _ -> throwInterp $ "attemped to setbox non box value: " ++ torepr' b'
>     pure v'

> interp (ISetBox b v) = do
>     b' <- interp b
>     v' <- interp v
>     case b' of
>              BoxV i -> lift $ writeIORef i v'
>              _ -> throwInterp $ "attemped to setbox non box value: " ++ torepr' b'
>     pure v'
      
> interp (IUnbox b) = do
>     b' <- interp b
>     case b' of
>         BoxV i -> lift $ readIORef i
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
>         _ -> throwNotFunctionValue $ torepr' fv
>   where
>     safeZip ps xs | length ps == length xs  = pure $ zip ps xs
>                   | otherwise = throwWrongNumberOfArgs (length ps) (length xs)

> box :: Value -> Interpreter Value
> box v = lift (BoxV <$> newIORef v)

--------------------------------------

interpreter helper functions

run a function with args passed as values

> interpFunction :: String -> [Value] -> Interpreter Value
> interpFunction f as = do
>     v <- interpSrc f
>     let as' = zipWith (\i x -> ("aaa-" ++ show i, x)) [(0::Int)..] as
>     interpWithAdditionalEnv (("fff", v):as') $
>         interpSrc $ "fff(" ++ intercalate "," (map fst as') ++ ")"

> interpSrc :: String -> Interpreter Value
> interpSrc src = do
>     ast <- either throwInterp pure $ P.parseModule "" src
>     interpAst ast

> interpAst :: Module -> Interpreter Value
> interpAst m = do
>     ib <- doDesugar $ desugarScript False m
>     x <- interp ib
>     case x of
>             VariantV "tuple" [("0", v)
>                              ,("1", VariantV "record" _e)] -> pure v
>             _ -> throwInterp $ "expected 2 element tuple with second element being a record, got this: "
>                     ++ torepr' x

run a script with additional bindings

> interpWithAdditionalEnv :: [(String,Value)] -> Interpreter a -> Interpreter a
> interpWithAdditionalEnv lenv f = local (\y -> y {ieEnv = extendEnv lenv $ ieEnv y}) f



------------------------------------------------------------------------------

desugaring
==========

> type Desugarer = RWST DesugarReader () DesugarState (Except String)

> data DesugarReader = DesugarReader {}

> data DesugarState = DesugarState {uniqueCtr :: Int
>                                  ,nextAnonymousCheckBlockNumber :: Int}

> startingDesugarState :: DesugarState
> startingDesugarState = DesugarState 0 0

> makeUniqueVar :: String -> Desugarer String
> makeUniqueVar pref = state $ \s ->
>     let suff = uniqueCtr s
>     in (pref ++ "-" ++ show suff
>        ,s {uniqueCtr = suff + 1})

> getAnonCheckBlockName :: Desugarer String
> getAnonCheckBlockName = state $ \s ->
>     let blockNo = nextAnonymousCheckBlockNumber s
>     in ("check-block-" ++ show blockNo
>        ,s {nextAnonymousCheckBlockNumber = blockNo + 1})

> throwDesugarV :: Expr ->  Expr
> throwDesugarV v = App (Iden "raise") [v]

--------------------------------------

desugaring code

> doDesugar :: Desugarer a -> Interpreter a
> doDesugar go = do
>    ds <- gets desugarState
>    let f (v,ds',_) = do
>            modify $ \s -> s {desugarState = ds'}
>            pure v
>    either throwInterp f $ runExcept (runRWST go (DesugarReader {}) ds)


slightly lazy - a script can contain import statements (but not
provides), so just use a Module ast to represent this for now


> desugarScript :: Bool -> Module -> Desugarer IExpr
> desugarScript tempIsBuiltIn (Module ps stmts) = do
>         ps' <- concat <$> mapM desugarPreludeInStmt ps
>         let testsVar = if tempIsBuiltIn
>                        then []
>                        else [VarDecl (PatName NoShadow "_module_tests") (Iden "empty")]
>         stmts' <- addTopRet (testsVar ++ stmts)
>         simplify <$> desugarStmts (ps' ++ stmts')
>   where
>     -- to be able to bind something in a script
>     -- then refer to this binding in a later call to the embedded api
>     -- rewrite the script to return the last value that the user wants
>     -- and the env at this point that the system uses to update the
>     -- saved env in the embedded handle
>     mk x = StExpr $ TupleSel [x
>                              ,App (Iden "env-to-record") []]
>     addTopRet [] = pure ([mk nothingSyntaxHack])
>     addTopRet [StExpr x] = do
>         z <- makeUniqueVar "z"
>         pure ([LetDecl (patName z) x]
>               ++ [mk $ Iden z])
>     addTopRet (x:xs) = (x:) <$> addTopRet xs


> desugarModule :: ImportSource ->  Module -> Desugarer (String,IExpr)
> desugarModule moduleName (Module ps stmts) = do
>     internalModName <- importSourceInternalModuleName moduleName
>     ps' <- concat <$> mapM desugarPreludeInStmt ps
>     (internalModName,) <$> desugarStmts
>         (ps' ++ testsVar ++ stmts ++ [StExpr $ App (Iden "provides") [pis]])
>   where
>     pis = pisToTea (ProvideName "_module_tests" : getProvides ps)
>     testsVar = -- hack to avoid referring to empty before it exists in the builtins
>                if moduleName == ImportName "built-ins"
>                then []
>                else [VarDecl (PatName NoShadow "_module_tests") (Iden "empty")]

--------------------------------------

desugar module:
desugar the prelude

add the last statement which returns the last value and the env, for
  repl/embedded and for imports

> nothingSyntaxHack :: Expr
> nothingSyntaxHack = VariantSel "nothing" []

> pisToTea :: [ProvideItem] -> Expr
> pisToTea [] = Iden "empty"
> pisToTea (x:xs) =
>     let x' = case x of
>                  ProvideAlias i a -> App (Iden "provide-alias") [Text i, Text a]
>                  ProvideName n -> App (Iden "provide-alias") [Text n, Text n]
>                  ProvideAll -> Iden "provide-all"
>     in App (Iden "link") [x', pisToTea xs]

> data IProvideItem = IProvideAll
>                   | IProvide String String

> teaToPIS :: Value -> Interpreter [IProvideItem]
> teaToPIS x = do
>     x' <- listToHaskellI x
>     getPis x'
>   where
>     getPis [] = pure []
>     getPis (VariantV "provide-all" [] : xs) = (IProvideAll:) <$> getPis xs
>     getPis (VariantV "provide-alias" [("i",TextV i),("a",TextV a)] : xs) =
>         (IProvide i a:) <$> getPis xs
>     getPis z = throwInterp $ " bad value in provides list: " ++ show z


> patName :: String -> PatName
> patName x = PatName NoShadow x

> unPatName :: PatName -> String
> unPatName (PatName _ x) = x

ignores provides statements, they are handled differently

> desugarPreludeInStmt :: PreludeStmt -> Desugarer [Stmt]
> desugarPreludeInStmt (Import is b) = do
>     n <- importSourceInternalModuleName is
>     pure [LetDecl (patName b) (Iden n)]

> desugarPreludeInStmt (IncludeFrom nm is) =
>     pure $ flip map is $ \case
>         ProvideAlias n1 n2 -> LetDecl (patName n2) (DotExpr (Iden nm) n1)
>         ProvideName n -> LetDecl (patName n) (DotExpr (Iden nm) n)
>         ProvideAll -> LetSplatDecl (Iden nm)

> desugarPreludeInStmt (Provide {}) = pure []

> desugarPreludeInStmt (Include is) = do
>     m <- makeUniqueVar "module"
>     concat <$> mapM desugarPreludeInStmt [Import is m, IncludeFrom m [ProvideAll]]

> getProvides :: [PreludeStmt] -> [ProvideItem]
> getProvides xs = case mapMaybe getPIs xs of
>     [] -> [ProvideAll]
>     ys -> concat ys
>   where
>     getPIs (Provide pis) = Just pis
>     getPIs _ = Nothing

> internalModuleNameFromFilename :: FilePath -> String
> internalModuleNameFromFilename s = "_internal_modules-" ++ replace '.' '_' s

> replace :: Eq a =>
>            a   -- ^ Value to look for
>         -> a   -- ^ Value to replace it with
>         -> [a] -- ^ Input list
>         -> [a] -- ^ Output list
> replace x y = map (\z -> if z == x then y else z)



> importSourceInternalModuleName :: ImportSource -> Desugarer String
> importSourceInternalModuleName (ImportSpecial "file" [s]) = pure $ internalModuleNameFromFilename s
> importSourceInternalModuleName (ImportSpecial x _ ) = lift $ throwE $ "import special with " ++ x ++ " not supported"
> importSourceInternalModuleName (ImportName n) = pure $ internalModuleNameFromFilename n -- todo: change to module.builtin

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

> desugar (RecordSel fs) = desugar (VariantSel "record" fs)

> desugar (Iden i) = pure $ IIden i

> desugar (Parens e) = desugar e
> desugar (BinOp e0 f e1) = desugar $ App (Iden f) [e0,e1]
> desugar (UnaryMinus e) = desugar $ App (Iden "-") [e]

a is b

->

test-results := link(test-is("a is b", lam(): a end, lam(): b end), test-results)

> desugar (App (Iden "is") [a,b]) =
>     desugarStmts [SetVar "test-results"
>        (App (Iden "link")
>         [App (Iden "test-is")
>          [Text $ Pr.prettyExpr a ++ " is " ++ Pr.prettyExpr b
>          ,Lam [] a
>          ,Lam [] b]
>         ,Iden "test-results"])]

> desugar (App (Iden "raises") [e0, e1]) =
>     desugarStmts [SetVar "test-results"
>        (App (Iden "link")
>         [App (Iden "test-raises")
>          [Text $ Pr.prettyExpr e0 ++ " raises " ++ Pr.prettyExpr e1
>          ,Lam [] e0
>          ,e1]
>         ,Iden "test-results"])]

> desugar (App (Iden "raises-satisfies") [e0,e1]) =
>     desugarStmts [SetVar "test-results"
>        (App (Iden "link")
>         [App (Iden "test-raises-satisfies")
>          [Text $ Pr.prettyExpr e0 ++ " raises-satisfies " ++ Pr.prettyExpr e1
>          ,Text $ Pr.prettyExpr e1
>          ,Lam [] e0
>          ,Lam [] e1]
>         ,Iden "test-results"])]

> desugar (App (Iden "or") [a,b]) =
>     desugar (If [(a, Iden "true")] (Just b))

> desugar (App (Iden "and") [a,b]) = do
>     desugar (If [(a, b)] (Just $ Iden "false"))

> desugar (App (Iden "catch") [a,b]) = ICatch <$> desugar a <*> desugar b

> desugar (Catch a b) = ICatch <$> desugar a <*> desugar b


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
> desugar (Lam ns e) = ILam (map b ns) <$> desugar e
>   where
>     b (PatName _ n) = n
> desugar (Let bs e) = do
>     let f (PatName _ n, v) = (n,) <$> desugar v
>     ILet <$> mapM f bs <*> desugar e

> desugar (LetRec bs e) = do
>     bsx <- mapM createBind bs
>     desugar (Let (mapMaybe convLam bs ++ bsx) e)
>   where
>     newName = (++"'")
>     bindNames = map (unPatName . fst) bs
>     bindNames' = map newName bindNames
>     -- fX = lam (asX): bdyX end -> fX' = lam (f0,f1,...,*asX): bdyX' end
>     convLam (n,Lam as bdy) =
>         Just (patName $ newName $ unPatName n, Lam (map patName bindNames ++ as) $ patchBody bdy)
>     -- fX = bdyX (something not a lam) -> fX = bdyX'
>     -- not sure about this one
>     convLam _ = Nothing --(newName n, patchBody x)
>     -- fX = lam (asX): bdyX end -> fX = lam(asX): fX'(f0',f1',...,*asX) end
>     createBind (n,Lam as _bdy) = do
>         let as' = map unPatName as
>         pure (n, Lam as $ App (Iden $ newName $ unPatName n) (map Iden (bindNames' ++ as')))
>     createBind (n,x) = pure (n, patchBody x)
>     --bdyN with fN(as) replaced with fN(f0,...fX,*as)
>     patchBody = transformBi $ \case
>         App (Iden f) args | f `elem` bindNames -> App (Iden f) (map Iden bindNames ++ args)
>         x -> x
  
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

> desugar (UnboxRef e n) = desugar (Unbox (DotExpr e n))

> desugar (Unbox x) = IUnbox <$> desugar x
> desugar (TupleGet e i) = desugar (DotExpr e (show i))


> lam :: [String] -> Expr -> Expr
> lam ps e = Lam (map f ps) e
>   where
>     f i = PatName NoShadow i

> registerCheckBlock :: Expr -> Stmt
> registerCheckBlock f =
>     SetVar "_module_tests"
>     (App (Iden "link") [f, Iden "_module_tests"])

> desugarStmts :: [Stmt] -> Desugarer IExpr

> desugarStmts (Check nm bdy : es) = do

check:
  stmt1
  ..
end

desugared to:

register-test-block(lam():
  var test-results = empty
  desugared stmt1
  ...

  # return:
  check-block-results(written-or-generated-test-block0name,reverse(test-results))

end

>     desugaredCheck <- do
>       blockName <- maybe getAnonCheckBlockName pure nm
>       desugarStmts [registerCheckBlock
>           (Lam [] (Block (
>                    [VarDecl (PatName NoShadow "test-results") (Iden "empty")]
>                    ++ bdy
>                    ++ [StExpr $ App (Iden "check-block-results")
>                                     [Text blockName
>                                     ,App (Iden "reverse") [(Iden "test-results")]]])))]
>     case es of
>         [] -> pure desugaredCheck
>         _ -> ISeq desugaredCheck <$> desugarStmts es

contracts are ignored

> desugarStmts (Contract {} : xs) = desugarStmts xs

> desugarStmts [] = desugar nothingSyntaxHack

testing hack
  
> desugarStmts (StExpr (App (Iden "letsplat") [re]) : xs) =
>     desugarStmts (LetSplatDecl re : xs)
  
> desugarStmts [StExpr e] = desugar e
> desugarStmts [x@(LetDecl {})] = desugarStmts [x, StExpr nothingSyntaxHack]
> desugarStmts [x@(LetSplatDecl {})] = desugarStmts [x, StExpr nothingSyntaxHack]
> desugarStmts [x@(FunDecl {})] = desugarStmts [x, StExpr nothingSyntaxHack]
> desugarStmts [x@(RecDecl {})] = desugarStmts [x, StExpr nothingSyntaxHack]

> desugarStmts (LetDecl (PatName _ n) v : es) = do
>     v' <- desugar v      
>     ILet [(n,v')] <$> desugarStmts es


> desugarStmts (LetSplatDecl re : es) = do
>     ILetSplat <$> desugar re <*> desugarStmts es

> desugarStmts es@(st : _) | isRecOrFun st = do
>     (recs,es') <- getRecs es [] []
>     desugar (LetRec recs (Block es'))
>   where
>     getRecs :: [Stmt] -> [(PatName,Expr)] -> [Stmt] -> Desugarer ([(PatName,Expr)], [Stmt])
>     getRecs (RecDecl nm bdy : es') recs whrs =
>         getRecs es' ((nm,bdy) : recs) whrs
>     getRecs (FunDecl nm ps bdy whr : es') recs whrs =
>         let bnd = (nm, (Lam ps bdy))
>             whrs' = case whr of
>                   Nothing -> whrs
>                   Just b -> Check (Just $ unPatName nm) b : whrs
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
pt = lam (x,y): I.App "make-variant" ["pt",[list: {false, "x";x},{false, "y";y}]]

  | pt(ref x,y) ->
pt = lam (x,y): I.App "make-variant" ["pt",[list: {true, "x";x},{false, "y";y}]]
  
special case to bootstrap the variant constructor for list link

>    makeVarnt (VariantDecl vnm [f0,f1]) =
>        pure $ LetDecl (patName vnm)
>        (lam [snd f0, snd f1] $ appI "make-variant-2"
>            [Text vnm, makeVField f0, makeVField f1])


>    makeVarnt (VariantDecl vnm fs) =
>        let fields = listSel $ map makeVField fs
>        in pure $ LetDecl (patName vnm)
>                  (lam (map snd fs) $ appI "make-variant" [Text vnm, fields])
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
>     let sts = LetDecl (patName enm) e
>         refSets = flip map fs $ \(n,v) -> SetBox (DotExpr (Iden enm) n) v
>     desugarStmts (sts : (map StExpr refSets ++ es))

> desugarStmts (e@(TPred {}) : _) = lift $ throwE $ "test preds are not supported " ++ show e
> desugarStmts (e@(TPostfixOp {}) : _) = lift $ throwE $ "test preds are not supported " ++ show e

---------------------------------------

simplify

> simplify :: IExpr -> IExpr
> simplify = transformBi $ \case
>     ILet bs (ILet bs' x) -> ILet (bs ++ bs') x
>     x1 -> x1

todo: put this in the regular desugar, it doesn't need generics

when can (Seq (Seq a b) c) be safely changed to (Seq a (Seq b c))?
this will make a lot of things more readable
if there are no decls, then is it always?
if there are, it's if the first statement doesn't introduce a name
  but this is only important if names aren't checked already and aren't
  made unique
there aren't any things that make seq unsafe to change this way in this ast

------------------------------------------------------------------------------

ffi boilerplate
===============

> _unwrapTuple :: (String, Value -> Interpreter [Value])
> _unwrapTuple = ("Tuple", \case
>                           VariantV "tuple" fs -> pure $ map snd fs
>                           x -> throwInterp $ "type: expected tuple, got " ++ show x)

> unwrapNum :: (String, Value -> Interpreter Scientific)
> unwrapNum = ("Number", \case
>                           NumV n -> pure n
>                           x -> throwInterp $ "type: expected number, got " ++ show x)

> unwrapInt :: (String, Value -> Interpreter Int)
> unwrapInt = ("Number", \case
>                           NumV n | Just i <- extractInt n -> pure i
>                                  | otherwise -> throwInterp $ "expected integer, got " ++ show n
>                           x -> throwInterp $ "type: expected number, got " ++ show x)



> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n

> wrapInt :: Int -> Interpreter Value
> wrapInt n = pure $ NumV $ fromIntegral n




> unwrapBool :: (String, Value -> Interpreter Bool)
> unwrapBool = ("Boolean", \case
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

> functionIn :: (String, Value -> Interpreter Value)
> functionIn = ("Function", \case
>                           x@(FunV {}) -> pure x
>                           x@(ForeignFunV {}) -> pure x
>                           x -> throwInterp $ "type: expected function, got " ++ show x)

> unwrapDyn :: Typeable a => (String, Value -> Interpreter a)
> unwrapDyn = ("FFI-val", \case
>                           FFIVal n -> case fromDynamic n of
>                               Just x -> pure x
>                               -- todo: terrible error message doesn't even say what was expected
>                               Nothing -> throwInterp $ "wrong dyn val, got " ++ show n
>                           x -> throwInterp $ "type: expected number, got " ++ show x)

> wrapDyn :: Typeable a => a -> Interpreter Value
> wrapDyn a = pure $ FFIVal $ toDyn a

  
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
> unconvI (ICatch e t) = unconvI (IApp (IIden "catch") [e,t])
> unconvI (IApp (IIden e) [a,b]) | isOp e = BinOp (unconvI a) e (unconvI b)
>   where isOp x = not $ any (\z -> isAlphaNum z || z `elem` "_") x
>
> unconvI (IApp (IIden "variant-field-get") [IText f, x]) = DotExpr (unconvI x) f
> unconvI (IApp e fs) = App (unconvI e) $ map unconvI fs
> unconvI (ILam ns e) = Lam (map unconvIPatName ns) $ unconvI e
> unconvI (ILet bs e) = Let (map (uncurry unconvIBinding) bs) (unconvI e)
> unconvI (ISeq a b) =
>     let sts = accumSeqs [a] b
>     in Block $ map (StExpr . unconvI) sts
>   where
>     accumSeqs xs (ISeq x y) = accumSeqs (x:xs) y
>     accumSeqs xs y = reverse (y:xs)
> 
> 
> unconvI (IIf bs e) = If (map f bs) (fmap unconvI e)
>   where
>     f (c,t) = (unconvI c, unconvI t)
> unconvI (IBox e) = App (Iden "box") [unconvI e]
> unconvI (ISetBox e f) = App (Iden "setbox") [unconvI e, unconvI f]
> unconvI (IUnbox e) = App (Iden "unbox") [unconvI e]
> unconvI (ILetSplat re e) = App (Iden "letsplat") [unconvI re, unconvI e]

> unconvIBinding :: String -> IExpr -> (PatName, Expr)
> unconvIBinding n v = (unconvIPatName n, unconvI v)

> unconvIPatName :: String -> PatName
> unconvIPatName n = PatName NoShadow n
