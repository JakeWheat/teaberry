
simple expressions with catch:

           | Catch Expr -- eval this, return the result if it doesn't raise
                   Expr -- if it does, pass the raised value to the
                        -- function value of this and return the value of that

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE MultiWayIf #-}

> module Expr1Catch (tests) where

> import qualified Parse as P
> import qualified Syntax as S
> import SimpleExpr (simpleInterpreterExamples
>                   ,TestTree
>                   ,makeSimpleTests
>                   )

> --import qualified SimpleExpr (Expr(..), parse)
> --import Control.Monad.Trans.Class (lift)
> --import Control.Monad.Trans.Except (Except, runExcept, throwE, catchE)
> --import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)

> --import Control.Monad.Trans.Class (lift)
> --import Control.Monad.IO.Class (liftIO)
> --import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.RWS (RWST, evalRWST, ask, local, {-get, gets, state, put, modify-})
> import Control.Exception.Safe (Exception, throwM, catch)

> import System.IO.Unsafe (unsafePerformIO)
> 
> import Scientific (Scientific)
> import Data.List (intercalate)

------------------------------------------------------------------------------

interpreter
-----------

> data Expr = Num Scientific
>           | Text String
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>           --  | Catch Expr -- eval this, return the result if it doesn't raise
>           --        Expr -- if it does, pass the raised value to the
>           --             -- function value of this and return the value of that
>            deriving (Eq, Show)


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
>     maybe (throwInterp $ UserException $ "Identifier not found " ++ nm) pure
>     $ lookup nm (envEnv env)

> addForeignFun :: String -> [String] -> ([Value] -> Interpreter Value) -> Env -> Either String Env
> addForeignFun nm tys f env =
>     pure ((extendEnv [(nm, ForeignFunV nm)] env)
>          {envForeignFuns = ((nm,tys), f) : envForeignFuns env})

> lookupForeignFun :: String -> [String] -> Env -> Interpreter ([Value] -> Interpreter Value)
> lookupForeignFun nm tys env =
>     if | Just f <- lookup (nm,tys) $ envForeignFuns env -> pure f
>        | Just f <- lookup (nm, map (const "any") tys) $ envForeignFuns env -> pure f
>        | otherwise -> throwInterp $ UserException $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")"

------------------------------------------------------------------------------

values


> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | FunV [String] Expr Env
>            | ForeignFunV String

> valueTypeName :: Value -> String
> valueTypeName (NumV {}) = "number"
> valueTypeName (BoolV {}) = "boolean"
> valueTypeName (TextV {}) = "text"
> valueTypeName (FunV {}) = "function"
> valueTypeName (ForeignFunV {}) = "foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (FunV {}) = "FunV stuff"
>   show (ForeignFunV n) = "ForeignFunV " ++ show n

> instance Eq Value where
>     NumV a == NumV b = a == b
>     TextV a == TextV b = a == b
>     BoolV a == BoolV b = a == b
>     _ == _ = False


------------------------------------------------------------------------------

interpreter

> -- type Interpreter = ReaderT Env (Except InterpreterExcept)

> type Interpreter = RWST Env () () IO


> data InterpreterException = ValueException Value
>                           | UserException String
>                           deriving Show

> instance Exception InterpreterException where

> throwInterp :: InterpreterException -> Interpreter a
> throwInterp e = throwM  e


> runInterp :: Env -> Expr -> IO Value
> runInterp env expr =
>     fst <$> evalRWST (interp expr) env ()

> --either (Left . show) Right $ runExcept (runReaderT (interp expr) env)

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Text n) = pure (TextV n)
> interp (Iden i) = do
>     env <- ask
>     envLookup i env

> interp (App (Iden "catch") [e, c]) = interp e `catch` (\case
>     ValueException v -> do
>         cf <- interp c
>         app cf [v]
>     -- is rethrowing an exception like this bad in haskell?
>     s -> throwInterp s)

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
>         _ -> throwInterp $ UserException "non function value in app position"
>   where
>     safeZip ps xs | length xs == length ps = pure $ zip ps xs
>                   | otherwise = throwInterp $ UserException "wrong number of args to function"



> evaluate :: String -> IO Value
> evaluate s =  do
>     let ast = either error id $ parse s
>     runInterp testEnv ast

------------------------------------------------------------------------------

ffi catalog

> testEnv :: Env
> testEnv = either error id $ addForeignFuns'
>    [("+", binaryOp unwrapNum unwrapNum wrapNum plusNum)
>    ,("+", binaryOp unwrapText unwrapText wrapText plusText)
>    ,("raise", unaryOp anyIn id raise)
>    ]
>    $ emptyEnv {envEnv = [("true", BoolV True)
>                         ,("false", BoolV False)]}

> plusNum :: Scientific -> Scientific -> Scientific
> plusNum = (+)

> plusText :: String -> String -> String
> plusText = (++)

> raise :: Value -> Interpreter Value
> raise v = throwInterp $ ValueException v

--------------------------------------

boilerplate

I don't think it's perfect, but I think it's maintainable enough.
Maybe good do better with some clever code or generics or something.


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
>                           x -> throwInterp $ UserException $ "type: expected number, got " ++ show x)

> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n

> unwrapText :: (String, Value -> Interpreter String)
> unwrapText = ("text", \case
>                           TextV n -> pure n
>                           x -> throwInterp $ UserException $ "type: expected text, got " ++ show x)

> wrapText :: String -> Interpreter Value
> wrapText n = pure $ TextV n

> anyIn :: (String, Value -> Interpreter Value)
> anyIn = ("any", pure)


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
>                 _ -> throwInterp $ UserException $ "wrong number of args to function, expected 2, got " ++ show (length as))


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
>                 _ -> throwInterp $ UserException $ "wrong number of args to function, expected 1, got " ++ show (length as))


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
> convExpr x = Left $ "unsupported syntax " ++ show x

------------------------------------------------------------------------------

tests
-----

> interpreterExamples :: [(String, String)]
> interpreterExamples =
>     simpleInterpreterExamples ++
>       [("let add1 = lam(a): a + 1 end: catch(1, add1) end", "1")
>       ,("let add1 = lam(a): a + 1 end: catch(raise(2), add1) end", "3")
>       ]

> tests :: TestTree
> tests = makeSimpleTests "expr1catch" interpreterExamples f
>   where
>     f src = unsafePerformIO $ evaluate src
