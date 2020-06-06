
Simple expressions with recursive functions

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module Expr1Letrecstar (tests) where

> import qualified Parse as P
> import qualified Syntax as S
> import SimpleExpr (simpleInterpreterExamples
>                   ,TestTree
>                   ,makeSimpleTests
>                   )
> import qualified Pretty as Pr

> import Data.Generics.Uniplate.Data (transformBi)
> import Data.Data (Data)

> --import qualified SimpleExpr (Expr(..), parse)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
> import Scientific (Scientific)
> import Data.List (intercalate)
> import Debug.Trace (trace)
> import Data.Maybe (mapMaybe)

------------------------------------------------------------------------------

interpreter
-----------

> data Expr = Num Scientific
>           | Text String
>           | TupleSel [Expr]
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>           | LetRec [(String,Expr)] Expr
>           | If Expr Expr Expr
>            deriving (Eq, Show, Data)


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
>     -- todo: error message can list functions with the same name but different types
>     -- it can also do a fuzzy match on the function name
>     maybe (lift $ throwE $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")")
>       pure $ lookup (nm,tys) $ envForeignFuns env

------------------------------------------------------------------------------

values


> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | FunV [String] Expr Env
>            | ForeignFunV String
>            | TupleV [Value]

> valueTypeName :: Value -> String
> valueTypeName (NumV {}) = "number"
> valueTypeName (BoolV {}) = "boolean"
> valueTypeName (TextV {}) = "text"
> valueTypeName (TupleV {}) = "tuple"
> valueTypeName (FunV {}) = "function"
> valueTypeName (ForeignFunV {}) = "foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (TupleV fs) = "{" ++ intercalate ";" (map show fs) ++ "}"
>   show (FunV {}) = "FunV stuff"
>   show (ForeignFunV n) = "ForeignFunV " ++ show n

> instance Eq Value where
>     NumV a == NumV b = a == b
>     TextV a == TextV b = a == b
>     BoolV a == BoolV b = a == b
>     TupleV a == TupleV b = a == b
>     _ == _ = False

------------------------------------------------------------------------------

desugar

only thing it does is convert letrec to let

letrec f0 = lam (as0): bdy0 end
       f1 = lam (as1): bdy1 end
       ...
->

bdyN' = bdyN with fN(as) replaced with fN'(f0',...fX',*as)

let f0' = lam (f0,f1,...,*as): bdy0' end
    f1' = lam (f0,f1,...,*as): bdy1' end
    ...
    f0 = lam(as): f0'(f0',f1',...,*as) end
    f1 = lam(as): f1'(f0',f1',...,*as) end
    ...

there is also a tuple method to try - put all the functions in a
 tuple, is this really different? less names to generate
and also try the fix method

> desugar :: Expr -> Either String Expr
> desugar (LetRec bs e) = do
>     desugar (Let (map convLam bs ++ mapMaybe createBind bs) e)
>   where
>     newName = (++"'")
>     bindNames = map fst bs
>     bindNames' = map newName bindNames
>     --nmMap = zip bindNames bindNames'
>     -- fX = lam (asX): bdyX end
>     -- ->
>     -- fX' = lam (f0,f1,...,*asX): bdyX' end
>     convLam (n,Lam as bdy) =
>         (newName n, Lam (bindNames ++ as) $ patchBody bdy)
>     -- fX = bdyX (something not a lam)
>     -- ->
>     -- fX = bdyX'
>     -- not sure about this one
>     convLam (n,x) = (newName n, patchBody x)
>     -- fX = lam (asX): bdyX end
>     -- ->
>     -- fX = lam(asX): fX'(f0',f1',...,*asX) end
>     createBind (n,Lam as _bdy) =
>         Just (n, Lam as $ App (Iden $ newName n) (map Iden (bindNames' ++ as)))
>     createBind _ = Nothing -- undefined
>     --bdyN' = bdyN with fN(as) replaced with fN(f0,...fX,*as)
>     patchBody = transformBi $ \case
>         App (Iden f) args | f `elem` bindNames -> App (Iden f) (map Iden bindNames ++ args)
>         x -> x

> desugar (TupleSel fs) = TupleSel <$> mapM desugar fs
> desugar x@(Num {}) = pure x
> desugar x@(Text {}) = pure x
> desugar x@(Iden {}) = pure x
> desugar (App f es) = App <$> desugar f <*> mapM desugar es
> desugar (Lam as e) = Lam as <$> desugar e
> desugar (Let bs e) = Let <$> mapM f bs <*> desugar e
>   where
>     f (n,v) = (n,) <$> desugar v

> desugar (If c t e) = If <$> desugar c <*> desugar t <*> desugar e


------------------------------------------------------------------------------

interpreter

> type Interpreter = ReaderT Env (Except String)

> runInterp :: Env -> Expr -> Either String Value
> runInterp env expr = runExcept (runReaderT (interp expr) env)

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Text n) = pure (TextV n)
> interp (TupleSel es) = TupleV <$> mapM interp es
> interp (Iden i) = do
>     env <- ask
>     envLookup i env
> interp (App f es) = do
>     fv <- interp f
>     vs <- mapM interp es
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
>     safeZip ps vs | length vs == length ps = pure $ zip ps vs
>                   | otherwise = lift $ throwE $ "wrong number of args to function"
> interp (Lam ps e) = do
>     env <- ask
>     pure $ FunV ps e env
> interp (Let bs e) = do
>     let newEnv [] = interp e
>         newEnv ((b,ex):bs') = do
>             v <- interp ex
>             local (extendEnv [(b,v)]) $ newEnv bs'
>     newEnv bs

> interp (If c t e) = do
>     c' <- interp c
>     case c' of
>         BoolV True -> interp t
>         BoolV False -> interp e
>         _ -> lift $ throwE $ "expected bool in if test, got " ++ show c'


> interp x@(LetRec {}) = lift $ throwE $ "undesugared letrec in interp " ++ show x


> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     ast' <- desugar ast
>     runInterp testEnv (trace (pretty ast') ast')

------------------------------------------------------------------------------

ffi catalog

> testEnv :: Env
> testEnv = either error id $ addForeignFuns'
>    [("+", binaryOp unwrapNum unwrapNum wrapNum plusNum)
>    ,("*", binaryOp unwrapNum unwrapNum wrapNum (*))
>    ,("-", binaryOp unwrapNum unwrapNum wrapNum (-))
>    ,("==", binaryOp unwrapNum unwrapNum wrapBool (==))
>    ,("+", binaryOp unwrapText unwrapText wrapText plusText)]
>    $ emptyEnv {envEnv = [("true", BoolV True)
>                         ,("false", BoolV False)]}

> plusNum :: Scientific -> Scientific -> Scientific
> plusNum = (+)

> plusText :: String -> String -> String
> plusText = (++)

> addForeignFun' :: String -> ([String], ([Value] -> Interpreter Value)) -> Env -> Either String Env
> addForeignFun' nm (tys, f) env = addForeignFun nm tys f env

> addForeignFuns' :: [(String, ([String], ([Value] -> Interpreter Value)))] -> Env -> Either String Env
> addForeignFuns' [] env = pure env
> addForeignFuns' ((x,y):xs) env = do
>     env' <- addForeignFun' x y env
>     addForeignFuns' xs env'
>     


> unwrapNum :: (String, Value -> Interpreter Scientific)
> unwrapNum = ("number", \case
>                           NumV n -> pure n
>                           x -> lift $ throwE $ "type: expected number, got " ++ show x)

> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n

> wrapBool :: Bool -> Interpreter Value
> wrapBool b = pure $ BoolV b


> unwrapText :: (String, Value -> Interpreter String)
> unwrapText = ("text", \case
>                           TextV n -> pure n
>                           x -> lift $ throwE $ "type: expected text, got " ++ show x)

> wrapText :: String -> Interpreter Value
> wrapText n = pure $ TextV n


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
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 2, got " ++ show (length as))



TODO:

want to make it easy to give much better errors

what does it look like to use 'dynamic type classes'

eventually want a standard set of functions with the ability to add
your own haskell functions when running something, and the ability to
have a minimal built in set instead of the usual full one, or no built
ins at all

the next step after that is to implement haskell data types ffi
so you can pass haskell values around in language opaquely
and then add something that allows you to use them as conveniently as
in language values

it would be nice to make the Value type abstract for users of this code
too, to insulate at API level from implementation changes.

------------------------------------------------------------------------------

parser

> parse :: String -> Either String Expr
> parse src =
>     case P.parseExpr "" src of
>       Right e -> convExpr e
>       Left e -> Left e
>
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

> convExpr (S.If [(c,t)] (Just e)) = do
>     If <$> convExpr c <*> convExpr t <*> convExpr e


> convExpr x = Left $ "unsupported syntax " ++ show x

------------------------------------------------------------------------------

pretty printer
--------------

for error messages, etc.

> pretty :: Expr -> String
> pretty x = Pr.prettyExpr $ unconv x

> unconv :: Expr -> S.Expr
> unconv (Num n) = S.Sel (S.Num n)
> unconv (Iden s) = S.Iden s
> unconv (Text s) = S.Sel (S.Str s)
> unconv (TupleSel fs) = S.Sel (S.Tuple (map unconv fs))

> unconv (App e fs) = S.App (unconv e) $ map unconv fs
> unconv (Lam ns e) = S.Lam (map unconvPattern ns) $ unconv e
> unconv (Let bs e) = S.Let (map (uncurry unconvBinding) bs) (unconv e)
> unconv (LetRec bs e) = S.LetRec (map (uncurry unconvBinding) bs) (unconv e)
> unconv (If c t e) = S.If [(unconv c, unconv t)] (Just $ unconv e)

> unconvBinding :: String -> Expr -> S.Binding
> unconvBinding n v = S.Binding (unconvPattern n) (unconv v)

> unconvPattern ::String -> S.Pat
> unconvPattern n = S.IdenP S.NoShadow (S.PatName n)


------------------------------------------------------------------------------

tests
-----

> interpreterExamples :: [(String, String)]
> interpreterExamples =
>     simpleInterpreterExamples ++
>     [("letrec fac = lam(x): if x == 0: 1 else: x * fac(x - 1) end end:\n\
>      \{fac(4);fac(5);fac(1)} end", "{24;120;1}")

>     ,("letrec\n\
>       \    addeven = lam(x): if x == 0: 0 else: x + addodd(x - 1) end end,\n\
>       \    addodd = lam(x):  if x == 0: 0 else: x + addeven(x - 1) end end:\n\
>       \    {addeven(2);addodd(2);addodd(5)}\n\
>       \end"
>      ,"{3;3;15}")
>     ]

> tests :: TestTree
> tests = makeSimpleTests "expr1letrecstar" interpreterExamples evaluate
