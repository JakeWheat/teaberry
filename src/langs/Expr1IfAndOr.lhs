
extent the simpleexprif to add 'and' and 'or'

they can't be regular functions if we want short circuiting. here,
they are desugared to 'if' to implement short circuiting

todo: import almost everything from simpleexprifandor

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}

> module Expr1IfAndOr (tests) where


> import qualified Parse1 as P
> import qualified Syntax1 as S
> import SimpleExpr (simpleInterpreterExamples
>                   ,TestTree
>                   ,makeSimpleTests
>                   )

> --import qualified SimpleExpr (Expr(..), parse)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
> import Scientific1 (Scientific)
> import Data.List (intercalate)

------------------------------------------------------------------------------

interpreter
-----------

> data Expr = Num Scientific
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>           | If Expr Expr Expr
>            deriving (Eq, Show)

todo: import this from the ffi example

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
>     maybe (lift $ throwE $ "Identifier not found " ++ nm) pure
>     $ lookup nm (envEnv env)

> addForeignFun :: String -> [String] -> ([Value] -> Interpreter Value) -> Env -> Either String Env
> addForeignFun nm tys f env =
>     pure ((extendEnv [(nm, ForeignFunV nm)] env)
>          {envForeignFuns = ((nm,tys), f) : envForeignFuns env})

> lookupForeignFun :: String -> [String] -> Env -> Interpreter ([Value] -> Interpreter Value)
> lookupForeignFun nm tys env =
>     maybe (lift $ throwE $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")")
>       pure $ lookup (nm,tys) $ envForeignFuns env

------------------------------------------------------------------------------

values


> data Value = NumV Scientific
>            | BoolV Bool
>            | FunV [String] Expr Env
>            | ForeignFunV String

> valueTypeName :: Value -> String
> valueTypeName (NumV {}) = "number"
> valueTypeName (BoolV {}) = "boolean"
> valueTypeName (FunV {}) = "function"
> valueTypeName (ForeignFunV {}) = "foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (FunV {}) = "FunV stuff"
>   show (ForeignFunV n) = "ForeignFunV " ++ show n

> instance Eq Value where
>     NumV a == NumV b = a == b
>     BoolV a == BoolV b = a == b
>     _ == _ = False

------------------------------------------------------------------------------

desugar

> desugar :: Expr -> Either String Expr
> desugar (Num i) = pure $ Num i
> desugar (Iden i) = pure $ Iden i

> desugar (App (Iden "and") [a,b]) = do
>     a' <- desugar a
>     b' <- desugar b
>     pure $ If a' b' (Iden "false")
> 
> desugar (App (Iden "or") [a,b]) = do
>     a' <- desugar a
>     b' <- desugar b
>     pure $ If a' (Iden "true") b'


a and b
->
if a: b else: false end
if not(a): false else: b end

are these the same? if so, option 1 is better because there's less to
it

a or b
->
if a: true else: b end

something isn't quite right: what if you make and into a value
suddenly, it has the wrong behaviour?
does it need to apply the short circuiting in the interpreter?

try to break the behaviour using this implementation to demonstrate
it's definitely flawed

how to test short circuiting? only by side effects? maybe it needs to
wait for vars to be trivially testable? or do a hack with print/writer
or something? prefer a nice simple hack to waiting for a big feature

regular functions + some way to show they're flawed
naive desugar to if + some way to show it's flawed
a better solution, maybe 2:
  do in in the interpreter to make it work
  prevent and and or from being used as values?


> desugar (App f as) = App <$> desugar f <*> mapM desugar as
> desugar (Lam ns e) = Lam ns <$> desugar e
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

'if' becomes the primary place to implement "functions" which don't
always evaluate all their arguements. e.g. we use this to implement
short circuiting 'and' and 'or'.

> interp (If c t e) = do
>     c' <- interp c
>     case c' of
>         BoolV True -> interp t
>         BoolV False -> interp e
>         _ -> lift $ throwE $ "expected bool in if test, got " ++ show c'

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     ast' <- desugar ast
>     runInterp testEnv ast'

------------------------------------------------------------------------------

ffi catalog

> testEnv :: Env
> testEnv = either error id $ addForeignFuns'
>    [("+", binaryOp unwrapNum unwrapNum wrapNum (+))
>    ,("not", unaryOp unwrapBool wrapBool not)
>    ,("==", binaryOp unwrapNum unwrapNum wrapBool (==))
>    ]
>    emptyEnv

--------------------------------------

boilerplate

todo: import from the ffi example

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


> unwrapBool :: (String, Value -> Interpreter Bool)
> unwrapBool = ("boolean", \case
>                           BoolV n -> pure n
>                           x -> lift $ throwE $ "type: expected boolean, got " ++ show x)

> wrapBool :: Bool -> Interpreter Value
> wrapBool n = pure $ BoolV n


> {-unwrapText :: (String, Value -> Interpreter String)
> unwrapText = ("text", \case
>                           TextV n -> pure n
>                           x -> lift $ throwE $ "type: expected text, got " ++ show x)

> wrapText :: String -> Interpreter Value
> wrapText n = pure $ TextV n-}

> unaryOp :: (String, Value -> Interpreter a)
>         -> (a -> Interpreter Value)
>         -> (a -> a)
>         -> ([String], ([Value] -> Interpreter Value))
> unaryOp unwrap0 wrap f =
>     ([fst unwrap0]
>     ,\as -> do
>             case as of
>                 [a] -> do
>                     ax <- (snd unwrap0) a
>                     wrap (f ax)
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 1, got " ++ show (length as))



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
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 2, got " ++ show (length as))

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
> convExpr (S.If [(c,t)] (Just e)) = do
>     If <$> convExpr c <*> convExpr t <*> convExpr e

>     
> convExpr x = Left $ "unsupported syntax " ++ show x

------------------------------------------------------------------------------

tests
-----

> interpreterExamples :: [(String, String)]
> interpreterExamples =
>     simpleInterpreterExamples ++
>     [("true or false", "true")
>     ,("false or false", "false")
>     ,("true and false", "false")
>     ,("not(true)", "false")
>     ,("not(false)", "true")
>     ]

todo: what's the earliest point where can check the short circuiting
 is working?

> tests :: TestTree
> tests = makeSimpleTests "expr1ifandor" interpreterExamples evaluate
