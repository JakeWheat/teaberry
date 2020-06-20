
sketch of implementing if on top of expr1ffi using splices

> $(splice Expr1FFI.language_pragmas)

->

 > {-# LANGUAGE TupleSections #-}
 > {-# LANGUAGE LambdaCase #-}

> module Expr1If (tests) where

> $(splice Expr1FFI.imports)

-> 

 > import qualified OldParse as P
 > import qualified OldSyntax as S
 > import SimpleExpr (simpleInterpreterExamples
 >                   ,TestTree
 >                   ,makeSimpleTests
 >                   )

 > --import qualified SimpleExpr (Expr(..), parse)
 > import Control.Monad.Trans.Class (lift)
 > import Control.Monad.Trans.Except (Except, runExcept, throwE)
 > import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
 > import Scientific1 (Scientific, extractInt)

 > import Data.List (intercalate)

------------------------------------------------------------------------------

interpreter
-----------

> $(splice Expr1FFI.data where name = Expr and variant `notelem` [text]
> add variant
>     | If Expr Expr Expr
> )



-> 

 > data Expr = Num Scientific
 >           | Iden String

- Text

 >           | App Expr [Expr]
 >           | Lam [String] Expr
 >           | Let [(String,Expr)] Expr

  + 

 >           | If Expr Expr Expr

 >            deriving (Eq, Show)

> $(splice Expr1FFI.data where name = Env)

->

 > data Env = Env
 >     {envEnv :: [(String,Value)]
 >     ,envForeignFuns :: [((String,[String]), [Value] -> Interpreter Value)]}

> $(splice Expr1FFI.values where name in [emptyEnv, extendEnv, envLookup, addForeignFunction, lookupForeignFunction])
todo: create a name for a set of things that go together, so you can import
then with one name (and still filter this)

->

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
 >     maybe (lift $ throwE $ "ffi function not found: " ++ nm ++ "(" ++ intercalate "," tys ++")")
 >       pure $ lookup (nm,tys) $ envForeignFuns env

------------------------------------------------------------------------------

values

> $(splice data Value)

 > data Value = NumV Scientific
 >            | BoolV Bool
 >            | FunV [String] Expr Env
 >            | ForeignFunV String


want to say 'take this function, but exclude any branches
where the first arg pattern matches on this variant in some way
(only matches that variant literally written there)

> $(splice Expr1FFI.values where name == valueTypeName
>  and first_arg not matches(Text*))


 > valueTypeName :: Value -> String 
 > valueTypeName (NumV {}) = "number"
 > valueTypeName (BoolV {}) = "boolean"
 > valueTypeName (FunV {}) = "function"
 > valueTypeName (ForeignFunV {}) = "foreign-function"

> $(splice Expr1FFI.instances where name == Show Value
>  and first_arg not matches(Text*))

->

 > instance Show Value where
 >   show (NumV n) = "NumV " ++ show n
 >   show (BoolV n) = "BoolV " ++ show n
 >   show (FunV {}) = "FunV stuff"
 >   show (ForeignFunV n) = "ForeignFunV " ++ show n


> $(splice instance eq value)

 > instance Eq Value where
 >     NumV a == NumV b = a == b
 >     BoolV a == BoolV b = a == b
 >     _ == _ = False


------------------------------------------------------------------------------

interpreter

> type Interpreter = ReaderT Env (Except String)

> runInterp :: Env -> Expr -> Either String Value
> runInterp env expr = runExcept (runReaderT (interp expr) env)

splice value interp exclude firstarg matches text

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



> interp (If c t e) = do
>     c' <- interp c
>     case c' of
>         BoolV True -> interp t
>         BoolV False -> interp e
>         _ -> lift $ throwE $ "expected bool in if test, got " ++ show c'

splice value evaluate

 > evaluate :: String -> Either String Value
 > evaluate s =  do
 >     ast <- parse s
 >     runInterp testEnv ast

------------------------------------------------------------------------------

ffi catalog

> splice value testEnv

in the source, refactor:

 > testEnv :: Env
 > testEnv = either error id $ addForeignFuns'
 >    [("+", binaryOp unwrapNum unwrapNum wrapNum plusNum)
 >    ,("+", binaryOp unwrapText unwrapText wrapText plusText)]
 >    $ emptyEnv {envEnv = [("true", BoolV True)
 >                         ,("false", BoolV False)]}

to this:

 > testEnv :: Env
 > testEnv = either error id $ addForeignFuns'
 >    [("+", binaryOp unwrapNum unwrapNum wrapNum plusNum)]
 >    ++ localFileFFIEnv
 >    $ emptyEnv {envEnv = [("true", BoolV True)
 >                         ,("false", BoolV False)]}

 > localFileFFIEnv = [("+", binaryOp unwrapText unwrapText wrapText plusText)]

then replace the local file ffi env here, to give a different list
so to splice in something like this but different, you have to explicitly
structure the haskell code in this way to make it possible

> localFileFFIEnv = [("==", binaryOp unwrapNum unwrapNum wrapBool (==))]

we want to get this:

 > testEnv :: Env
 > testEnv = either error id $ addForeignFuns'
 >    [("+", binaryOp unwrapNum unwrapNum wrapNum (+))
 >    ,("==", binaryOp unwrapNum unwrapNum wrapBool (==))
 >    ]
 >    $ emptyEnv {envEnv = [("true", BoolV True)
 >                          ,("false", BoolV False)]}

so it spliced in the testEnv, but with a different value

--------------------------------------

boilerplate

> $(splice addForeignFun' addForeignFuns' unwrapNum , wrapNum wrapBool binaryOp)

 > addForeignFun' :: String -> ([String], ([Value] -> Interpreter Value)) -> Env -> Either String Env
 > addForeignFun' nm (tys, f) env = addForeignFun nm tys f env

 > addForeignFuns' :: [(String, ([String], ([Value] -> Interpreter Value)))] -> Env -> Either String Env
 > addForeignFuns' [] env = pure env
 > addForeignFuns' ((x,y):xs) env = do
 >     env' <- addForeignFun' x y env
 >     addForeignFuns' xs env'

 > unwrapNum :: (String, Value -> Interpreter Scientific)
 > unwrapNum = ("number", \case
 >                           NumV n -> pure n
 >                           x -> lift $ throwE $ "type: expected number, got " ++ show x)

 > wrapNum :: Scientific -> Interpreter Value
 > wrapNum n = pure $ NumV n


 > wrapBool :: Bool -> Interpreter Value
 > wrapBool n = pure $ BoolV n

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

$(splice parse)
$(splice convExpr filter not matches (sel . Str))


> convExpr (S.If [(c,t)] (Just e)) = do
>     If <$> convExpr c <*> convExpr t <*> convExpr e

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

> $(splice simpleInterpreterexamples)

> interpreterExamples :: [(String, String)]
> interpreterExamples =
>     simpleInterpreterExamples ++
>     [("if true: 1 else: 2 end", "1")
>     ,("if false: 1 else: 2 end", "2")
>     ,("if 1 == 1: 1 else: 2 end", "1")
>     ,("if 1 == 2: 1 else: 2 end", "2")
>     ]

> tests :: TestTree
> tests = makeSimpleTests "expr1if" interpreterExamples evaluate
