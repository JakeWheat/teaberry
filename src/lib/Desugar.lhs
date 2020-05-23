
Desugaring from the high level syntax to the interpreter syntax.

The non trivial parts are

the testing support (check:, example:, where:)
data decls
pattern desugaring
letrec
ref update
cases


> {-# LANGUAGE TupleSections, LambdaCase #-}
> module Desugar (loadProgramImports, desugarProgram,desugarExpr, getBuiltInModulesDir) where

> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Maybe (catMaybes, mapMaybe)
> import Data.Either (partitionEithers)

> import Control.Monad.RWS (RWST(..), runRWST, ask, local,state, get)
> import Control.Monad.Except (Except, runExcept, throwError)
> import Control.Monad (forM, zipWithM, forM_)

> import Control.Exception.Safe (Exception, throwM)


> import System.FilePath ((</>), takeBaseName, takeDirectory)
> import Paths_teaberry

> --import Debug.Trace (trace)
> import Text.Show.Pretty (ppShow)

> import qualified Syntax as S
> import qualified InterpreterSyntax as I
> import qualified Pretty as P
> import Parse (parseProgram)


------------------------------------------------------------------------------

= api

> desugarProgram :: [(S.ImportSource,S.Program)] -> Either String I.Program
> desugarProgram ps =
>     case runExcept (runRWST f defaultDesugarReader defaultDesugarStore) of
>         Left e -> Left e
>         Right (result, _store, _log) -> Right result
>   where

doing this weird create [I.Stmt], then seqify, doesn't seem like a
good way to do it

>     f :: RWST DesugarReader [()] DesugarStore (Except String) I.Program
>     f = do
>         let desugarPrograms _ _ [] = pure []
>             desugarPrograms mp mvnm ((impsrc,q):qs) = do
>                 -- the name for the local let name for the desugared module
>                 nmx <- case impsrc of
>                            S.ImportSpecial "file" [fn] -> pure $ takeBaseName fn
>                            _ -> throwError $ "unsupported import source " ++ show impsrc ++ "\n"
>                                       ++ ppShow mp
>                 localModuleName <- getUnique $ "module-" ++ nmx

>                 (mvnm', q') <- desugarProgramPreludeHighLevel mp mvnm localModuleName q
>                 sts <- desugarProgramPreludeLowLevel mp localModuleName q'
>                 let mp' = (impsrc, localModuleName) : mp
>                 (sts :) <$>  desugarPrograms mp' mvnm' qs
>         stmts <- concat <$> desugarPrograms [] [] ps
>         --trace ("-------------------\n" ++ P.prettyStmts stmts) $ pure ()
>         (I.Program . seqify) <$> desugarStmts stmts

> desugarExpr :: S.Expr -> Either String I.Expr
> desugarExpr e =
>     case runExcept (runRWST (desugarExpr' e) defaultDesugarReader defaultDesugarStore) of
>         Left x -> Left x
>         Right (result, _store, _log) -> Right result

------------------------------------------------------------------------------

= types for the desugarer monad

> type DesugarStack = RWST DesugarReader [()] DesugarStore (Except String)

> data DesugarReader = DesugarReader
>     {inCheckBlockID :: Maybe Int -- just iff in a check block
>     ,variablesEnv :: [String] -- store the names of variables in scope
>                               -- so these can be desugared differently
>     } deriving (Eq,Show)

> defaultDesugarReader :: DesugarReader
> defaultDesugarReader = DesugarReader Nothing []

> addVariableName :: String -> DesugarReader -> DesugarReader
> addVariableName v r = r {variablesEnv = v:variablesEnv r}

> data DesugarStore = DesugarStore
>     {nextCheckBlockNameID :: Int -- the id for the next anonymous check block name
>     ,nextCheckBlockID :: Int
>     ,cheapoUnique :: Int
>     ,variantFields :: [(String,[String])] -- variant name, variant field names
>     }
>     deriving (Eq,Show)

> defaultDesugarStore :: DesugarStore
> defaultDesugarStore = DesugarStore 1 0 0
>     -- hardcoded list data type
>     [("empty", [])
>     ,("link", ["first", "rest"])]

> getVariantFields :: String -> DesugarStack [String]
> getVariantFields vnt = do
>     x <- variantFields <$> get
>     maybe (throwError $ "variant not found: " ++ vnt)
>          pure $ lookup vnt x

> getVariantNames :: DesugarStack [String]
> getVariantNames = map fst <$> variantFields <$> get

> addVariantFields :: String -> [String] -> DesugarStack ()
> addVariantFields vnt fs = do
>     state $ \s ->
>         (()
>         ,s {variantFields = (vnt,fs) : variantFields s})


a slightly improved hack to keep moving without getting stuck doing
 this properly yet:

> getUnique :: String -> DesugarStack String
> getUnique prefix = do
>     i <- state $ \s ->
>             let i = cheapoUnique s
>             in (i, s {cheapoUnique = cheapoUnique s + 1})
>     return $ prefix ++ "-" ++ show i ++ "-XX" -- ++ "-deffoUnique"
>                   

the full plan is to find a pseudo random generator that can put in the
pure monad state, and also track envs like in the interpreter (without values),
and then can implement this and other things properly

------------------------------------------------------------------------------

loading imports

before the (pure) desugaring happens, we have to follow all the
imports and load them.

This takes a program, and returns all the imported programs with their
filenames. They are returned in an order such that no module
refers to a later module in the list, and the one you pass
in is last.

TODO: think about how separate compilation could work

> data MyException = MyException String
>     deriving Show

> instance Exception MyException


> getBuiltInModulesDir :: IO FilePath
> getBuiltInModulesDir = getDataFileName "built-in-modules"

> loadProgramImports :: FilePath -> S.Program -> IO [(S.ImportSource, S.Program)]
> loadProgramImports pfn (S.Program prs stmts) = do
>     -- todo: use transformer to support io either
>     -- get the list of imports
>     let imsrc = S.ImportSpecial "file" [pfn]
>         cwd = takeDirectory pfn
>     builtInModulesDir <- getBuiltInModulesDir
>
>     -- run through the prelude statements
>     -- each one that has an import source
>     -- make sure it has the full path
>     -- if it's a built in, convert it to a path
>     let convertIS (S.ImportSpecial "file" [fn]) =
>             let nfn = cwd </> fn
>             in (S.ImportSpecial "file" [nfn], Just nfn)
>         convertIS (S.ImportName x) =
>             let nfn = builtInModulesDir </> x ++ ".tea"
>             in (S.ImportSpecial "file" [nfn], Just nfn)
>         convertIS x = (x, Nothing)
> 
>         getImports fns' prs' [] = (fns',prs')
>         getImports fns' prs' (i@(S.Import is a) : xs) =
>             let (is', fn) = convertIS is
>             in case fn of
>                 Nothing -> getImports fns' (i:prs') xs
>                 Just fn' -> getImports (fn':fns')
>                                 (S.Import is' a : prs')
>                                 xs
>         getImports fns' prs' (i@(S.ImportNames nms is) : xs) =
>             let (is', fn) = convertIS is
>             in case fn of
>                 Nothing -> getImports fns' (i:prs') xs
>                 Just fn' -> getImports (fn':fns')
>                                 (S.ImportNames nms is' : prs')
>                                 xs
>         getImports fns' prs' (i@(S.Include is) : xs) =
>             let (is', fn) = convertIS is
>             in case fn of
>                 Nothing -> getImports fns' (i:prs') xs
>                 Just fn' -> getImports (fn':fns')
>                                 (S.Include is' : prs')
>                                 xs
>         getImports fns' prs' (x:xs) =
>             getImports fns' (x:prs') xs
>         (fns, prsnew) = getImports [] [] prs
>         p' = S.Program (reverse prsnew) stmts
>
>     --   load the files
>     let recurseOnModule fn = do
>             src <- readFile fn
>             ast <- either (throwM . MyException) pure $ parseProgram fn src
>             loadProgramImports fn ast
>     allModules <- ((imsrc,p'):) <$> concat <$> mapM recurseOnModule fns
>     -- only have files appear once in the list
>     let mkUnique _ [] = []
>         mkUnique seen (x@(mnm,_):xs) =
>             if mnm `elem` seen
>             then mkUnique seen xs
>             else x : mkUnique (mnm : seen) xs
>     let allModulesUnique = mkUnique [] allModules
>     -- reverse lists only at the top level
>     --trace ("-----------loadstuff--------\n " ++ ppShow (map fst allModulesUnique) ++ "\n------------------\n") $ pure ()
>     pure $ reverse $ allModulesUnique
>     -- todo: detect cycles
>     -- how to memoize and not load files more than once?
>     -- canonicalize the import sources so we don't load a module twice
>     -- because it's referred to in two different ways


------------------------------------------------------------------------------

desugaring programs

This code desugars programs to just be lists of statements -
converting the import, include and provides into regular
statements. The desugaring method for these is based on representing
modules as record values. In the future, it will could use a renamer
instead (at least, representing as record values isn't going to work
for types).


High level desugaring takes the full range of supported import,
include and provide statements, and desugars them to the base set of
import, include and provide statements.

High level desugaring:

all provides and includes are explicit lists with aliases (expand * as
well)
all imports and includes which refer to modules directly are desugared
to "import file as alias" + extras
all provides are combined, and if there isn't one, provides: end is
added

do the prelude items need to be reordered so they work in any order?
can you write:

include from SD: * end
import string-dict as SD

I think it's ok to demand these are in the right order (unless confirm
 that pyret doesn't require this)

provides can be in any order because of how the desugaring works, and
pyret supports this also, e.g.:

provide from T: * end
provide from C: * end
provide from I: * end
import tables as T
import chart as C
import image as I

> desugarProgramPreludeHighLevel :: [(S.ImportSource, String)]
>                                -> [(String, [String])]
>                                -> String -- the desugared let name for this module
>                                -> S.Program
>                                -> DesugarStack ([(String,[String])], S.Program)
> desugarProgramPreludeHighLevel moduleNameMap moduleValueNameMap nm p = do
>     p0 <- desugarIncludeModule p
>     p1 <- desugarImportFrom p0
>     p2 <- desugarIncludeAll moduleNameMap moduleValueNameMap p1
>     (modNames,p3) <- desugarProvideAll p2
>     p4 <- desugarProvideIncludeAliases p3
>     p5 <- desugarToOneProvide p4
>     pure ((nm, modNames) : moduleValueNameMap, p5)

include

includeall
provideall
provideincludealiases

include <file or built-in>
->
import file(xxx) as temp-name
include from temp-name: * end

> desugarIncludeModule :: S.Program -> DesugarStack S.Program
> desugarIncludeModule (S.Program prs stmts) = do
>     prs' <- concat <$> mapM dim prs
>     pure (S.Program prs' stmts)
>   where
>     dim (S.Include is) = do
>         nm <- getUnique "include-module"
>         pure [S.Import is nm
>              ,S.IncludeFrom nm [S.ProvideAll]]
>     dim x = pure [x]



import name1, ... from <some-module>
->
import <some-module> as temp-name
include from temp-name:
  name1, ...
end

> desugarImportFrom :: S.Program -> DesugarStack S.Program
> desugarImportFrom (S.Program prs stmts) = do
>     prs' <- concat <$> mapM f prs
>     pure $ S.Program prs' stmts
>   where
>     f (S.ImportNames nms is) = do
>         nm <- getUnique "import-from"
>         pure [S.Import is nm
>              ,S.IncludeFrom nm $ map S.ProvideName nms]
>     f x = pure [x]


include from X: * end
->
include from X: all the elements of X listed explicitly

to support include all:
1. go from the alias to the import filename
   this can be done locally
2. go from the import filename to the internal modulename
   this is the map that is created
3. from the internal module name, can look up the exposed names
   this is the other map that is created

> desugarIncludeAll :: [(S.ImportSource, String)]
>                   -> [(String,[String])]
>                   -> S.Program
>                   -> DesugarStack S.Program
> desugarIncludeAll moduleNameMap moduleValueNameMap (S.Program prs stmts) = do
>     -- create the map from aliases to import sources
>     let aliasMap = flip mapMaybe prs $ \case
>                        (S.Import is alias) -> Just (alias,is)
>                        _ -> Nothing
>     prs' <- mapM (f aliasMap) prs
>     pure (S.Program prs' stmts)
>   where
>     f aliasMap (S.IncludeFrom alias is) = do
>         is' <- mapM (g aliasMap alias) is
>         pure $ S.IncludeFrom alias $ concat is'
>     f _ x = pure x
>     g :: [(String,S.ImportSource)] -> String -> S.ProvideItem -> DesugarStack [S.ProvideItem]
>     g aliasMap alias S.ProvideAll = do
>         is <- maybe (throwError $ "desugarincludeall:include from alias not found: " ++ alias
>                                    ++ "\n" ++ show aliasMap)
>               pure $ lookup alias aliasMap
>         xis <- maybe (throwError $ "desugarincludeall: internal error module not found: " ++ show is
>                                    ++ "\n" ++ show moduleNameMap)
>                pure $ lookup is moduleNameMap
>         nms <- maybe (throwError $ "desugarincludeall: internal error module names not found: " ++ show xis
>                                    ++ "\n" ++ show moduleValueNameMap)
>                pure $ lookup xis moduleValueNameMap
>         pure $ map S.ProvideName nms
>     g _ _ x = pure [x]
> 


provide: * end
->
provide: a,b, ... end

> desugarProvideAll :: S.Program
>                   -> DesugarStack ([String], S.Program)
> desugarProvideAll (S.Program prs stmts) = do
>     provideAllNames <- expandProvideAll stmts
>     let prs' = map (epa provideAllNames) prs
>     pure (provideAllNames, S.Program prs' stmts)
>   where
>     epa nms (S.Provide xs) = S.Provide $ concatMap (ep nms) xs
>     epa _ p = p
>     ep nms (S.ProvideAll) = map S.ProvideName nms
>     ep _ x = [x]


provide: a,b end
->
provide: a as a, b as b end

include from X: a,b end
->
include from X: a as a,b as b end

> desugarProvideIncludeAliases :: S.Program -> DesugarStack S.Program
> desugarProvideIncludeAliases = pure . transformBi ( \case
>     S.ProvideName s -> S.ProvideAlias s s
>     x -> x)

> desugarToOneProvide :: S.Program -> DesugarStack S.Program
> desugarToOneProvide (S.Program prs stmts) =
>     let fs = flip map prs $ \case
>                  S.Provide is -> Left is
>                  x -> Right x
>         (pr, os) = partitionEithers fs
>         pr1 = case concat pr of
>                   [] -> S.Provide []
>                   xs -> S.Provide xs
>     in pure $ S.Program (pr1:os) stmts


---------------------------------------

Low level desugaring:

a module of
provide: a as b, c as d, ... end
stmts
->
module-the-module-name-or-something =
  block:
    stmts
    {b : a, d : c,  ...}
  end
if there is no provide, then the last statement is the empty record value: {}

import file('xxx') as X
->
X = lookup the module name of file 'xxx' in the module-the-module-name-or-something
    binding for the desugaring of that module

include from X:
  n1 as n2, ...
end
->
n2 = X.n1
...


> desugarProgramPreludeLowLevel :: [(S.ImportSource, String)]
>                               -> String
>                               -> S.Program
>                               -> DesugarStack [S.Stmt]
> desugarProgramPreludeLowLevel moduleNameMap localModuleName (S.Program prels stmts) = do
>     (prels', is) <- desugarImports moduleNameMap prels
>     (prels'', rv') <- desugarProvides prels'
>     case prels'' of
>         x:_ -> throwError $ "unsupported prelude statement " ++ show x
>         [] -> pure ()
>     rv <- case rv' of
>               [_] -> pure rv'
>               [] -> throwError $ "internal error: no provides in desugared module"
>               _ -> throwError $ "internal error: multiple provides in desugared module"
>     let stmts' = is ++
>                  [S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName localModuleName))
>                              (S.Block (stmts ++ rv)))
>                  ,S.StExpr $ S.Iden "nothing"]
>     {-trace ("-----------------------\n" ++ P.prettyStmts stmts') $ -}
>     pure stmts'

> desugarProvides :: [S.PreludeItem] -> DesugarStack ([S.PreludeItem], [S.Stmt])
> desugarProvides prs =
>     (\(a,b) -> (a,concat b)) <$> partitionEithers <$> mapM desugarProvide prs
>   where
>     desugarProvide :: S.PreludeItem -> DesugarStack (Either S.PreludeItem [S.Stmt])
>     desugarProvide (S.Provide pis) | Just x <- mapM f pis =
>         pure $ Right [S.StExpr $ S.Sel $ S.Record x]
>     desugarProvide x = pure $ Left x
>     f (S.ProvideAlias n m) = Just (m,S.Iden n)
>     f _ = Nothing


> desugarImports :: [(S.ImportSource, String)] -> [S.PreludeItem] -> DesugarStack ([S.PreludeItem], [S.Stmt])
> desugarImports moduleNameMap prs =
>     (\(a,b) -> (a,concat b)) <$> partitionEithers <$> mapM desugarImport prs
>   where
>     desugarImport :: S.PreludeItem -> DesugarStack (Either S.PreludeItem [S.Stmt])
>     desugarImport (S.Import impsrc x) =
>         case lookup impsrc moduleNameMap of
>              Just inm -> pure $ Right $ [S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName x))
>                                                    (S.Iden inm))]
>              Nothing -> throwError $ "module not found: " ++ show impsrc ++ "\n" ++ show moduleNameMap
>     desugarImport (S.IncludeFrom nm pis) | Just x <- mapM (f nm) pis =
>         pure $ Right $ map (\(n,e) -> S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName n)) e)) x
>     desugarImport x = pure $ Left x
>     f nm (S.ProvideAlias n m) = Just (m,S.DotExpr (S.Iden nm) n)
>     f _ _ = Nothing


---------------------------------------

provide * desugaring

this collects all the top level binding names (no values or types) it
includes the names of the functions generated from data declarations
the data types themselves plus the details of the patterns for the
variants are handled separately (the variants also have a
corresponding function value here too for creating variant values)

> expandProvideAll :: [S.Stmt] -> DesugarStack [String]
> expandProvideAll [] = pure []
> expandProvideAll (S.LetDecl (S.Binding p _) : xs) =
>     (getBindingNames p ++) <$> expandProvideAll xs
> expandProvideAll (S.RecDecl (S.Binding p _) : xs) =
>     (getBindingNames p ++) <$> expandProvideAll xs
> expandProvideAll (S.VarDecl (S.Binding p _) : xs) =
>     (getBindingNames p ++) <$> expandProvideAll xs
> expandProvideAll (S.FunDecl nm _ _ _ : xs) =
>     (nm:) <$> expandProvideAll xs
> expandProvideAll (S.DataDecl nm vs _ : xs) =
>     let vnms = map (\(S.VariantDecl vnm _) -> vnm) vs
>     in ((vnms
>          ++ map ("is-"++) (nm:vnms))
>         ++) <$>
>        expandProvideAll xs
> expandProvideAll (_ : xs) = expandProvideAll xs

> getBindingNames :: S.Pat -> [String]
> getBindingNames (S.IdenP _ (S.PatName s)) = [s]
> getBindingNames (S.VariantP _ ps) = concatMap getBindingNames ps
> getBindingNames (S.TupleP ps) = concatMap getBindingNames ps
> getBindingNames (S.AsP p _ nm) = nm : getBindingNames p

------------------------------------------------------------------------------

desugar a block

doesn't yet do the top level the way it is supposed to, pending
research on a better letrec than the usual scheme one

> desugarStmts :: [S.Stmt] -> DesugarStack [I.Expr]
> desugarStmts (s:ss) | Just s' <- convRec s = do

rules for fun and rec:
when a fun or rec is seen, it will collect subsequent funs and recs
(both) and then desugar them all together as one letrec

>     -- get all the immediately following recursive defs
>     let (addDecls',ss') = spanMaybe convRec ss
>         (addDecls, whrs') = unzip (s' : addDecls')
>         whrs :: [(Maybe String, [S.Stmt])]
>         whrs = map (\(a,b) -> (Just a, b)) $ catMaybes whrs'
>     -- desugar them to regular let together
>     defs <- desugarRecs addDecls
>     -- do the check blocks, not sure it's right to not interleave them
>     chks <- desugarStmts $ map (uncurry S.Check) whrs
>     -- desugar to interpreter syntax
>     idecls <- desugarStmts $ map S.LetDecl defs
>     (++) (idecls ++ chks) <$> desugarStmts ss'
>   where
>     convRec (S.FunDecl nm as bdy whr) =
>         Just (S.Binding
>               (S.IdenP S.NoShadow (S.PatName nm))
>               (S.Lam as bdy)
>              ,fmap (nm,) whr)
>     convRec (S.RecDecl b) = Just (b, Nothing)
>     convRec _ = Nothing

> desugarStmts (s:ss) = do
>     -- if it's a variable, add to the env
>     xs <- desugarStmt s
>     ys <- case s of
>               S.VarDecl (S.Binding (S.IdenP _ (S.PatName n)) _) -> do
>                   local (addVariableName n) $ desugarStmts ss
>               _ -> desugarStmts ss
>     pure (xs ++ ys)

> desugarStmts [] = pure []


> desugarStmt :: S.Stmt -> DesugarStack [I.Expr]

> desugarStmt (S.StExpr x@(S.BinOp e0 "is" e1)) = do
>   let p = P.prettyExpr x
>   y <- desugarIs p e0 e1 
>   desugarStmt y


> desugarStmt (S.StExpr (S.BinOp e0 "raises" e1)) = do
>   desugarStmt(S.StExpr (S.BinOp e0 "raises-satisfies" (S.App (S.Iden "tostring-equals") [e1])))

> desugarStmt (S.StExpr x@(S.BinOp e0 "raises-satisfies" e1)) = do
>   let p = P.prettyExpr x
>   y <- desugarRaises p e0 e1
>   desugarStmt y

> 
> desugarStmt (S.StExpr e) = (:[]) <$> desugarExpr' e
> desugarStmt (S.When c t) = (:[]) <$>
>     desugarExpr' (S.If [(c, S.Block [S.StExpr t
>                                    ,S.StExpr $ S.Iden "nothing"])]
>                     (Just (S.Iden "nothing")))

> desugarStmt (S.LetDecl b) = do
>     bs <- desugarPatternBinding b
>     forM bs $ \(_,nm,e) -> I.LetDecl nm <$> desugarExpr' e

> desugarStmt (S.FunDecl nm as bdy whr) = do
>     a <- desugarStmt (S.RecDecl (S.Binding (S.IdenP S.NoShadow (S.PatName nm)) (S.Lam as bdy)))
>     case whr of
>         Nothing -> pure a
>         Just w -> (++) a <$> desugarStmt (S.Check (Just nm) w)
>     
> desugarStmt (S.RecDecl b) = do
>    defs <- desugarRecs [b]
>    desugarStmts $ map S.LetDecl defs


the is- functions use safe-variant-name since they are supposed to
work without error on any value

> desugarStmt (S.DataDecl typenm vs whr) = do
>     -- is-typenm
>     let variantNames = map (\(S.VariantDecl nm _) -> nm) vs
>         vnofx = S.App (S.Iden "safe-variant-name") [S.Iden "x"]
>         eqVariants = map (\n -> S.App (S.Iden "==") [S.Iden "vn", S.Sel $ S.Str n]) variantNames
>         
>         isT = S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName $ "is-" ++ typenm))
>               $ S.Lam [S.IdenP S.Shadow (S.PatName "x")] (S.Let [S.Binding (S.IdenP S.Shadow (S.PatName "vn")) vnofx]
>                        (foldr orEm (S.Iden "false") eqVariants)))
>     -- variant selectors
>         mkV :: String -> [(S.Ref,String)] -> S.Stmt
>         -- 0 args
>         mkV ctnm [] = S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName ctnm))
>                                  $ S.App
>              (S.Iden "make-variant")
>              [S.Sel (S.Str ctnm)
>              ,mkListSel []])
>         mkV ctnm fs = S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName ctnm))
>                                  $ S.Lam (map (S.IdenP S.Shadow . S.PatName) $ map snd fs) $ S.App
>              (S.Iden "make-variant")
>              [S.Sel (S.Str ctnm)
>              ,mkListSel $ map (\(m,a) -> S.Sel $ S.Tuple [case m of
>                                                               S.Ref -> S.Iden "true"
>                                                               S.Con -> S.Iden "false"
>                                                          ,S.Sel $ S.Str a, S.Iden a]) fs])
>         vnts = map (\(S.VariantDecl nm fs) -> mkV nm fs) vs
>     -- is-variant fns
>         mkIsVnt ctnm = S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName $ "is-" ++ ctnm))
>                                $ S.Lam [S.IdenP S.Shadow (S.PatName "x")] $ S.BinOp (S.App (S.Iden "safe-variant-name") [S.Iden "x"])
>                                "==" (S.Sel $ S.Str ctnm))
>         isVnts = map (\(S.VariantDecl nm _) -> mkIsVnt nm) vs
>     -- where block
>         w = case whr of
>              Nothing -> []
>              Just w' -> [S.Check (Just typenm) w']
>     x <- desugarStmts $ [isT] ++ vnts ++ isVnts ++ w
>     -- add the variant fields to the env so pattern bindings
>     -- can be desugared
>     -- desugar the statements first, otherwise the defintion of the variant
>     -- desugarers gets desugared to pattern binding
>     forM_ vs $ \(S.VariantDecl n fs) -> addVariantFields n $ map snd fs
>     pure x
>   where
>     orEm a b = S.BinOp a "or" b
>     mkListSel = S.Construct (S.Iden "list")

desugaring a data decl:

data Point:
  | pt(x, y)
end

->

is-Point = lam(x): let vn = I.App "variant-name" [x]
                   in vn == "pt" (or vn == ..)
pt = lam (x,y): I.App "make-variant" ["pt",[list: {"x",x},{"y",y}]]
is-pt = lam(x): I.App "variant-name" [x] == "pt"


> desugarStmt (S.VarDecl (S.Binding (S.IdenP _ (S.PatName n)) e)) =
>     (:[]) <$> (I.LetDecl n . I.Box) <$> desugarExpr' e
> desugarStmt (S.VarDecl (S.Binding p _)) = throwError $ "var binding must be name, got " ++ show p
> desugarStmt (S.SetVar n e) = (:[]) <$> I.SetBox (I.Iden n) <$> desugarExpr' e

> desugarStmt (S.SetRef e as) = do
>     enm <- getUnique "setref"
>     bs <- desugarPatternBinding (S.Binding (S.IdenP S.NoShadow (S.PatName enm)) e)
>     sts <- forM bs $ \(_,nm,e') -> I.LetDecl nm <$> desugarExpr' e'
>     stsets <- forM as $ \(nm, bdy) -> do
>         nm' <- desugarExpr' (S.DotExpr (S.Iden enm) nm)
>         bdy' <- desugarExpr' bdy
>         pure $ I.SetBox nm' bdy'
>     pure $ sts ++ stsets

a!{a:1, b:2}
->
setbox(a.a, 1)
setbox(a.b, 2)


------------------------------------------------------------------------------

desugaring check blocks:

a check block is desugared to this:

add-tests(lam():
  checkblockid = n # unique number, and the variable name checkblock id should be unique (todo)
  log-check-block(checkblockid, "blockname")
  {desuged statements in the check block}
  nothing # a top level check block has no value
end

todo: a non top level check block can use something like this:

expr
check:
...
end
->
shadow tmpvar = expr
add-tests(lam():
   ...
   # instead of ending with nothing:
   tmpvar
end

this isn't needed for nested where:, but is for a nested
check/examples if they are at the end of a block

surprising behaviour in pyret:

block:
  y = 2
  y
  check "nested":
    y is 2
  end
end

runs the test and returns 2

block:
  y = 2
  check "nested":
    y is 2
  end
end

this succeeds, runs the test, and outputs nothing

block:
  y = 2
  #check "nested":
  #  y is 2
  #end
end

this fails with 'a block cannot end with a binding'


> desugarStmt (S.Check nm sts) = do
>     nm' <- case nm of
>                   Just x -> pure x
>                   Nothing -> do
>                       i <- state (\st -> (nextCheckBlockNameID st
>                                          ,st {nextCheckBlockNameID = nextCheckBlockNameID st + 1}))
>                       pure $ "check-block-" ++ show i
>     checkblockid <- state (\st -> (nextCheckBlockID st
>                                   ,st {nextCheckBlockID  = nextCheckBlockID st + 1}))
>     checkblockidnm <- getUnique "checkblockid"
>     let blockNameVal = S.Sel $ S.Str nm'
>         checkblockidval = S.Sel $ S.Num $ fromIntegral checkblockid
>         blk = [S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName checkblockidnm)) checkblockidval)
>                 ,S.StExpr $ S.App (S.Iden "log-check-block") [S.Iden checkblockidnm
>                                                              ,blockNameVal]
>               ] ++ sts ++ [S.StExpr $ S.Sel S.NothingS]
>         blockWrap =
>             [S.StExpr $ S.App (S.Iden "add-tests") $ [S.Lam [] $ S.Block blk]]
>     sts' <- local (\x -> x {inCheckBlockID = Just checkblockid}) $ desugarStmts blockWrap
>     pure [seqify sts']
>     

> desugarStmt x = throwError $ "Desugar desugarStmt " ++ show x

an "is" test desugars to

block:
  shadow v0 = aexpr
  shadow v1 = bexpr # todo: change to tuple bind to avoid bexpr
                    # capturing the shadowed v0
  shadow name = "aexpr is bexpr"
  if v0 == v1:
    log-test-pass(checkblockid, name)
  else:
    shadow failmsg = "Values not equal:\n" + torepr(v0) + "\n" + torepr(v1)
    log-test-fail(checkblockid, name, failmsg)
  end
end

> desugarIs :: String -> S.Expr -> S.Expr -> DesugarStack S.Stmt
> desugarIs syn e e1 = do
>     x <- inCheckBlockID <$> ask
>     checkBlockID <- maybe (throwError $ "'is' outside of checkblock")
>                     (pure . S.Sel . S.Num . fromIntegral) x
>     v0 <- getUnique "isv0"
>     v1 <- getUnique "isv1"
>     nameit <- getUnique "isname"
>     failmsg <- getUnique "isfailmsg"
>     let mys = S.StExpr $ S.Block
>                    [{-S.StExpr $ S.App (S.Iden "print") [S.Sel $ S.Str $ "Desugar test enter"]
>                    ,-}S.LetDecl (S.Binding (S.IdenP S.Shadow (S.PatName v0)) e)
>                    ,S.LetDecl (S.Binding (S.IdenP S.Shadow (S.PatName v1)) e1)
>                    ,S.LetDecl (S.Binding (S.IdenP S.Shadow (S.PatName nameit)) $ S.Sel $ S.Str syn)
>                    ,S.StExpr $ S.If [(S.App (S.Iden "==") [S.Iden v0, S.Iden v1]
>                                    ,S.App (S.Iden "log-test-pass") [checkBlockID, S.Iden nameit])]
>                        (Just $ S.Block
>                         [S.LetDecl (S.Binding (S.IdenP S.Shadow (S.PatName failmsg))
>                                     (str "Values not equal:\n" `plus` app "torepr" [S.Iden v0]
>                                               `plus` str "\n" `plus` app "torepr" [S.Iden v1]))
>                         ,S.StExpr $ S.App (S.Iden "log-test-fail")
>                          [checkBlockID, S.Iden nameit, S.Iden failmsg]])]
>     pure mys
>   where
>       plus a b = S.BinOp a "+" b
>       str = S.Sel . S.Str
>       app nm es = S.App (S.Iden nm) es


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


> desugarRaises :: String -> S.Expr -> S.Expr -> DesugarStack S.Stmt
> desugarRaises syn e e1 = do
>     x <- inCheckBlockID <$> ask
>     checkBlockID <- maybe (throwError $ "'raises' outside of checkblock")
>                     (pure . S.Sel . S.Num . fromIntegral) x
>     nameit <- getUnique "isname"
>     v1 <- getUnique "isv1"
>     let failMsg = "The test operation raises-satisfies failed for the test "
>                   ++ P.prettyExpr e1
>         arg0 = S.Block [S.StExpr e
>                        ,S.LetDecl (S.Binding (S.IdenP S.Shadow (S.PatName nameit)) $ S.Sel $ S.Str syn)
>                        ,S.StExpr $ app "log-test-fail"
>                          [checkBlockID, S.Iden nameit, str "No exception raised"]]
>         arg1 = S.Lam [S.IdenP S.Shadow (S.PatName "a")]
>                $ S.Block [S.LetDecl (S.Binding (S.IdenP S.Shadow (S.PatName v1)) e1)
>                          ,S.LetDecl (S.Binding (S.IdenP S.Shadow (S.PatName nameit)) $ S.Sel $ S.Str syn)
>                          ,S.StExpr (S.If [(app v1 [S.Iden "a"]
>                                           ,app "log-test-pass" [checkBlockID, S.Iden nameit])]
>                                     $ Just $ app "log-test-fail"
>                                     [checkBlockID, S.Iden nameit
>                                     ,str failMsg `plus`
>                                     str ", value was "
>                                     `plus` app "torepr" [S.Iden "a"]
>                                     ]
>                                     )
>                           ]
>     pure $ S.StExpr $ app "catch" [arg0,arg1]
>   where
>       plus a b = S.BinOp a "+" b
>       str = S.Sel . S.Str
>       app nm es = S.App (S.Iden nm) es


todo: add a hook so that raises can desugar to this but have different
 error messages:
No exception raised, expected "hello"
Got unexpected exception "bye", expected "hello"



--------------------------------------


> desugarExpr' :: S.Expr -> DesugarStack I.Expr
> desugarExpr' (S.Sel (S.Num n)) = pure $ I.Sel (I.Num n)
> desugarExpr' (S.Sel (S.Str s)) = pure $ I.Sel (I.Str s)
> desugarExpr' (S.Sel S.NothingS) = pure $ I.Sel I.NothingS
> desugarExpr' (S.Iden "_") = throwError "'_' in expression context"

> desugarExpr' (S.Iden i) = do
>     -- see if it's a variable
>     vs <- variablesEnv <$> ask
>     if i `elem` vs
>       then pure $ I.Unbox (I.Iden i)
>       else pure $ I.Iden i
> 
> desugarExpr' (S.Parens e) = desugarExpr' e
> desugarExpr' (S.Ask b e) = desugarExpr' (S.If b e)

> desugarExpr' (S.If [] Nothing) = pure $ I.App (I.Iden "raise") (I.Sel $ I.Str "no branches matched")
> desugarExpr' (S.If [] (Just e)) = desugarExpr' e
> desugarExpr' (S.If ((c,t):xs) els) = I.If <$> desugarExpr' c <*> desugarExpr' t <*> desugarExpr' (S.If xs els)

bit of a hack, catch is like special syntax, not a regular function
 value
it exists for testing and development of the language only

> desugarExpr' (S.App (S.Iden "catch") [e0,e1]) = do
>   a <- desugarExpr' e0
>   b <- desugarExpr' e1
>   pure $ I.Catch a b


> desugarExpr' (S.App f xs) = do
>     f' <- desugarExpr' f
>     xs' <- mapM desugarExpr' xs
>     let r g [] = pure $ I.App g (I.Sel $ I.NothingS)
>         r g [y] = pure $ I.App g y
>         r g (y:ys) = r (I.App g y) ys
>     r f' xs'

> desugarExpr' (S.UnaryMinus e) = desugarExpr' (S.App (S.Iden "*") [S.Sel $ S.Num (-1), e])

"short circuiting" and and or

> desugarExpr' (S.BinOp a "and" b) =
>     desugarExpr' (S.If [(a, b)] (Just $ S.Iden "false"))

> desugarExpr' (S.BinOp a "or" b) =
>     desugarExpr' (S.If [(a, S.Iden "true")] (Just b))

> desugarExpr' (S.BinOp a op b) = desugarExpr' (S.App (S.Iden op) [a,b])

> desugarExpr' (S.Lam [] bdy) = I.LamVoid <$> desugarExpr' bdy
> desugarExpr' (S.Lam [S.IdenP _ (S.PatName x)] bdy) = I.Lam x <$> desugarExpr' bdy
> desugarExpr' (S.Lam (S.IdenP _ (S.PatName x):xs) bdy) = I.Lam x <$> desugarExpr' (S.Lam xs bdy)

> desugarExpr' (S.Let ps bdy) = do
>     ps' <- concat <$> mapM desugarPatternBinding ps
>     bdy' <- desugarExpr' bdy
>     desugarBindingsToLet bdy' ps'


> desugarExpr' (S.LetRec fs ex) = do
>    defs <- desugarRecs fs
>    {-trace (prettyExpr (S.Let defs ex)) -}
>    desugarExpr' (S.Let defs ex)

> desugarExpr' (S.Block ss) = do
>     ss' <- desugarStmts ss
>     pure $ seqify ss'

> desugarExpr' (S.Sel (S.Tuple ts)) = do
>     ts' <- mapM desugarExpr' ts
>     pure $ I.Sel $ I.Variant "tuple" (zip nms ts')
>   where
>     nms = map show [(0::Int)..]

> desugarExpr' (S.Sel (S.Record ts)) = do
>     ts' <- mapM (\(a,b) -> (a,) <$> desugarExpr' b) ts
>     pure $ I.Sel $ I.Variant "record" ts'


> desugarExpr' (S.TupleGet e i) = do
>     desugarExpr' (S.App (S.Iden "variant-field-get") [e, S.Sel $ S.Str $ show i])

> desugarExpr' (S.DotExpr e f) = do
>     desugarExpr' (S.App (S.Iden "variant-field-get") [e, S.Sel $ S.Str f])

special case for lists and bootstrapping, the desugar knows the
implementation of list "constructor"

> desugarExpr' (S.Construct (S.Iden "list") vs) =
>     desugarExpr' $ f vs
>   where
>     f [] = S.Iden "empty"
>     f (v:vs') = S.App (S.Iden "link") [v,f vs']

general construct:

all constructs desugar to the same thing:

[xxx: <elements>]
->
xxx.make([list: <elements>])

if the construction name isn't found, it's regular 'Identifier not found'
if the make function isn't there, you should get:
The left side was not a defined convenience constructor.
(TODO)

the plan is to call these 'selectors' after ttm, which I think is a
better term than construct. It reflects the heavy value oriented
interpretation of the language, which is a little different to pyret
which I think has a more java style 'hidden reference' interpretation

todo: rename the syntax node to Selector or ConvenienceSelector or
something

> desugarExpr' (S.Construct rc es) =
>     desugarExpr' (S.App (S.Iden "call-construct-make")
>                   [rc,S.Construct (S.Iden "list") es])
>     {-let x = (\f -> desugarExpr' (S.App (S.DotExpr rc f) es))
>     in case es of
>         [] -> x "make0"
>         [_] -> x "make1"
>         [_,_] -> x "make2"
>         [_,_,_] -> x "make3"
>         [_,_,_,_] -> x "make4"
>         [_,_,_,_,_] -> x "make5"
>         _ -> desugarExpr' (S.App (S.DotExpr rc "make") [S.Construct (S.Iden "list") es])-}



desugaring cases:
  cases(List) l:
-> assert typeof l is List
   will need to be able to find the type from the variant name in the runtime
    | empty => true
    -> if variant-name == empty:
         rhs
    | link(f, r) => 1 + length(r)
    ->
      if variantname  == link
      link(f,r) = l
      rhs

Cases String Expr [(Pat, Expr)] (Maybe Expr)

> desugarExpr' (S.Cases _ty e branches els) = do
>     -- enm = e
>     enm <- getUnique "cases-e"
>     -- todo: assert typeof e is ty
>     let a0 = S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName enm)) e)
>     -- vnm = variant-name(e)
>     vnm <- getUnique "cases-vnm"
>     let a1 = S.LetDecl (S.Binding (S.IdenP S.NoShadow (S.PatName vnm)) $ S.App (S.Iden "variant-name") [S.Iden enm])
>         --a15 = S.StExpr $ S.App (S.Iden "print") [S.Iden vnm]
>     -- fn to get the variant name from a pattern
>     let makeVB pat expr = (S.BinOp (S.Iden vnm) "==" (S.Sel $ S.Str $ patternVariantName pat)
>                           ,S.Let [S.Binding pat (S.Iden enm)]  expr)
>         a2 = map (uncurry makeVB) branches
>         aif = S.StExpr $ S.If a2 els
>     desugarExpr' (S.Block [a0, a1, aif])
>   where
>     patternVariantName (S.VariantP (S.PatName x) _) = x
>     patternVariantName (S.IdenP _ (S.PatName x)) = x

> desugarExpr' (S.Unbox e f) = do
>     x <- desugarExpr' (S.DotExpr e f)
>     pure $ I.Unbox x

> desugarExpr' x = throwError $ "desugarExpr': " ++ show x

turn a list of expressions into a nested seq value

> seqify :: [I.Expr] -> I.Expr
> seqify [] = I.Sel I.NothingS
> seqify [e] = e
> seqify (e:es) = I.Seq e $ seqify es

> desugarBindingsToLet :: I.Expr -> [(S.Shadow, String, S.Expr)] -> DesugarStack I.Expr
> desugarBindingsToLet bdy [] = pure bdy
> desugarBindingsToLet bdy ((_, n, lbdy) : ls) = do
>     lbdy' <- desugarExpr' lbdy
>     I.Let n lbdy' <$> desugarBindingsToLet bdy ls
>   


------------------------------------------------------------------------------

desugar pattern binding

> desugarPatternBinding :: S.Binding -> DesugarStack [(S.Shadow, String, S.Expr)]
> desugarPatternBinding (S.Binding (S.IdenP s (S.PatName nm)) e) = do
>     -- check if nm is a variant name
>     -- todo: see if we can use shadow to shadow a variant
>     x <- getVariantNames
>     if nm `elem` x
>     then desugarPatternBinding (S.Binding (S.VariantP (S.PatName nm) []) e)
>     else pure [(s,nm,e)]

{x; y} = {1; 2}
->
tmp = {1; 2}
x = tmp.{0}
y = tmp.{1}

> desugarPatternBinding (S.Binding (S.AsP (S.TupleP ps) s anm) e) = do
>     -- todo: desugar the tuple to a variant binding instead
>     -- and recurse
>     let tmp = (s, anm, e)
>     let expandIt p f =
>             desugarPatternBinding
>                 (S.Binding p (S.TupleGet (S.Iden anm) f))
>     fs <- zipWithM expandIt ps [0..]
>     pure (tmp : concat fs)

link(a,b) as x
->
x = link(a,b)
a = x.first
b = x.rest

cnm = link : assert this
ps = a,b
x = anm
where to get the names first, rest from?
these have to be saved into the desugar monad
  the reason we can find this in a dynamic language
  is that the variant name uniquely identifies the field names
  (there is also the type name in the cases syntax, but this is
   more like an assertion, since it isn't really needed in correct code,
   and doesn't give enough information on it's own either)

desugar tuples to the same thing instead of via tupleget, to combine
 the code paths?

problem: this function only returns deconstructed bindings, not a list
 of statements
when -> use a hack

anm = e
_ = block:
      when variant-name(anm) <> cnm:
        raise
      nothing
    end

> desugarPatternBinding (S.Binding (S.AsP (S.VariantP (S.PatName cnm) ps) s anm) e) = do
>     let a0 = (s, anm, e)
>     -- assert e.variantname == cnm
>         msg = S.Sel (S.Str "desugarPatternBinding: pattern match failure in, expected ")
>               `plus` S.Sel (S.Str cnm) `plus` S.Sel (S.Str ", got ")
>               `plus` S.App (S.Iden "variant-name") [S.Iden anm]
>         a1 = (S.NoShadow, "_", S.Block [
>               S.When (S.BinOp (S.App (S.Iden "variant-name") [S.Iden anm]) "<>" (S.Sel $ S.Str cnm))
>                    $ S.App (S.Iden "raise") [msg]])
>     vfs <- getVariantFields cnm
>     let expandIt p f =
>             desugarPatternBinding
>                 (S.Binding p (S.DotExpr (S.Iden anm) f))
>     fs <- zipWithM expandIt ps vfs
>     pure (a0:a1:concat fs)
>   where
>     plus a b = S.BinOp a "+" b


every variant/tuple/"compound type" without an 'as' gets one added
automatically

> desugarPatternBinding (S.Binding x e) | wantsAs x = do
>     tmpNm <- getUnique "tmpAs"
>     desugarPatternBinding (S.Binding (S.AsP x S.NoShadow tmpNm) e)
>   where
>     wantsAs (S.AsP {}) = False
>     wantsAs (S.IdenP {}) = False
>     wantsAs _ = True



> desugarPatternBinding x = throwError $ "desugar: unsupported type of pattern binding: " ++ show x




------------------------------------------------------------------------------

rec in blocks:

This is the canonical rewrite from scheme which I think says what
works and what doesn't work in vanilla pyret:

(letrec ([v e] ...) b...)
->
(let ([v "some dummy value"] ...)
  (set! v e)
  ...
  b)


a non lambda binding doesn't do 'rec', it can only
  refer to bindings before it in the list
a lambda binding can refer to later defined bindings, including non
  lambda ones


what about doubling up the non-lambda bindings to 0 arg lambdas
and seeing if this can make it work more nicely

letrec
  x = y,
  y = 1:
  x + y
end

->

letrec
  x' = lam() : y',
  y' = lam() : 1,
  x = x'(),
  y = y'():
  x + y
end
then desugar using the usual algo, making sure the x and y bindings
 come at the end

here is a case to cover, a recursive value:
rec tr = node("rec", lam(): tr end, lam(): tr end)

without the lambdas, is this only possible in a lazy language?
check the graphs section of papl

other todo for letrec:
  generate unique names
  replace only the correct calls when fixing the bodies:
    don't descend when shadowed

letrec f(a) = ...
->
let f'(g,a) = ... with f replaced with g
    f(a) = f'(f',a)

letrec f = lam (a): bdy end
->
let fz = lam (f,a): bdy' end
    f = lam(a): f'(f',a) end


letrec f0 = lam (as0): bdy0
       ...
       fn = lam (asn): bdyn
in ex ->
  [bdyn' = bdyn replace(fn(x) with fn(f0,...,fN,x)]
  let [fn' = lam(f0,...,fn,asn) bdyn']
      [fn = lam (asn) : fn'(f0',...,fn',asn)]
  in ex

the current rewrite is like this, it has limitations compared to pyret:
letrec
  fact = lam(n): if n == 1: 1 else: n * fact(n - 1) end end,
  abc = fact(5):
  abc
end
->
let
  factXXX = lam(fact, n): if n == 1.0: 1.0
                            else: n * fact(fact, n - 1.0)
                            end end,
  fact = lam(n): factXXX(factXXX, n) end,
  abc = factXXX(factXXX, 5)
  abc
end


> desugarRecs :: [S.Binding] -> DesugarStack [S.Binding]
> desugarRecs rs = do
>     let
>         recnms = flip mapMaybe rs $ \case
>                                         S.Binding (S.IdenP _ (S.PatName f)) (S.Lam {}) -> Just f
>                                         _ -> Nothing
>         recmap = zip recnms recnms
>     recnms' <- mapM getUnique recnms
>     let recmap' = zip recnms recnms'
>         mkrec :: S.Binding -> DesugarStack (Maybe S.Binding, Maybe S.Binding)
>         mkrec (S.Binding (S.IdenP s (S.PatName f)) (S.Lam as bdy)) = do
>             f' <- maybe (throwError $ "Desugarer: internal error, didn't find just generated matching unique id " ++ f)
>                      pure $ lookup f recmap'
>             let bdy' = patchCalls recmap recnms bdy
>             pure (Just (S.Binding (S.IdenP S.NoShadow (S.PatName f')) (S.Lam (map (S.IdenP S.Shadow . S.PatName) recnms ++ as) bdy'))
>                  ,Just (S.Binding (S.IdenP s (S.PatName f)) (S.Lam as (S.App (S.Iden f') (map S.Iden (recnms' ++ map (\(S.IdenP _ (S.PatName x)) -> x) as))))))
>         mkrec (S.Binding f e) = pure (Nothing
>                                      ,Just (S.Binding f (patchCalls recmap' recnms' e)))
>     (a,b) <- unzip <$> mapM mkrec rs
>     pure $ catMaybes (a ++ b)
>  where
>      --showit :: [(String, S.Expr)] -> String
>      --showit ss = intercalate "\n" $ map (\(a,b) -> a ++ " = " ++ prettyExpr b) ss
>      patchCalls mp ids = transformBi $ \case
>          S.App (S.Iden fx) args | Just fx' <- lookup fx mp -> S.App (S.Iden fx') (map S.Iden ids ++ args)
>          x -> x

------------------------------------------------------------------------------

utils

> spanMaybe :: (t -> Maybe a) -> [t] -> ([a], [t])
> spanMaybe _ xs@[] =  ([], xs)
> spanMaybe p xs@(x:xs') = case p x of
>     Just y  -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
>     Nothing -> ([], xs)



------------------------------------------------------------------------------


Top level design notes

The top level will be desugared to a let rec
this isn't like other implicit let recs in blocks, it doesn't need
definitions to be next to each other - all definitions and statements
in the entire top level will 'participate' in the same letrec

rec a = ...

b = ...

rec c = ...

a and b and c will be part of the same letrec

check blocks 'participate' in the letrec, along with statements - have
 to figure out how to make this work

shadow doesn't work in a letrec for another var in the letrec:

rec a = 5
shadow rec a = 6
or any other variation is an error
_except_ at the top level

this is to support this at the top level:

a = 5

...

shadow a = 6

are bindings the only kind of redefinition that can happen?

a way to support this is roughly:
if idens are shadowed, rename to remove the shadowing
then, move the declarations first and the statements second
then it should all work
problem is, if a closure refers to a var and it gets updated
  this is ok for check blocks, but what about other things?
  will it work out ok?
  if var updates get moved to the bottom
  and all the non decl statements keep their relative order
  will everything work out?


this is only important for learning, an industrial only language
_could_ disallow this too, but if we want the top level to double
time as a script/implicit block and a top level for a more structured
program, this is a viable compromise (another one would be to have a
flag to switch which - e.g top level desugaring works like blocks,
shadow allowed for top level scripts, or top level works like a
regular letrec for not script-y programs, and shadow is not allowed
at the top level at all
