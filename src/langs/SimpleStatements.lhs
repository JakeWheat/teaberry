
simple expression, extended with statements supported directly in the
interpreter

the only kinds of statement are let statement or expression
the variation is always desugar let statement to expression
it also has block expression
two successive expressions in a block desugar to a binary seq ctor

todo:
keep blocks in the interpreter without introducing seq
  see how different it makes it
then keep let statements explicitly in the interpreter
without desugaring to let expressions
  (is it possible to desugar all let expressions to let statements?)

smaller todos:
demonstrate combining successive let statements into a single
let expression and write why this is interesting - to make debugging
and stuff easier. this can be done with let expressions directly also

are there passes that can be done to remove redundant seqs?
does supporting block in the interpreter instead of seq make things
simpler all things considered?



the surface syntax has:
block expression
statements:
  let statement
  stexpr

write tests for all the variations which try to break them, to try to
make sure the implementations are complete

see if there's a reason to prefer one or the other


a question:
what is the top level?
is it an expression?
do you have to write block explicitly first if you want more than just
a single expression?
is the top level a list of statements
if so, does it add the block implicitly or not?
later might want to distinguish, since the top level
  might become a special letrec
  but non top level blocks are not letrec

what's the easiest thing to do?
parse a list of statements
add the block in the ast automatically to wrap them
this keeps syntax compatibility with the check version

what's the ultimate thing to do? maybe come back to this after doing
programs and imports and stuff


> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}

> module SimpleStatements (tests
>                         ) where

> import SimpleExpr (TestTree
>                   ,makeSimpleTests
>                   ,simpleInterpreterExamples)

> import qualified Parse as P
> import qualified Syntax as S


> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)

> import Data.Scientific (Scientific)
> import Data.List (intercalate)

------------------------------------------------------------------------------

syntax
------

> data Stmt = StExpr Expr
>           | LetDecl String Expr
>           deriving (Eq, Show)

> data Expr = Num Scientific
>           | Text String
>           | TupleSel [Expr]
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
>           | Block [Stmt]
>           | Seq Expr Expr
>           deriving (Eq, Show)

------------------------------------------------------------------------------

desugar
-------

lists of statements are desugared to Seq
block is removed completely
so the desugaring removes statement completely, and blocks
  and adds seq which does one expression then another

what are the pros and cons of desugaring to the same ast?

> desugar :: Expr -> Either String Expr
> desugar (Block []) = Left $ "empty block"
> desugar (Block [StExpr e]) = desugar e
> desugar (Block [LetDecl {}]) = Left $ "block ends with let"

> desugar (Block (LetDecl n v : es)) = do
>     v' <- desugar v
>     Let [(n,v')] <$> desugar (Block es)


> desugar (Block (StExpr e : es)) =
>     Seq <$> desugar e <*> desugar (Block es)

> desugar (Num i) = pure $ Num i
> desugar (Text i) = pure $ Text i
> desugar (TupleSel fs) = TupleSel <$> mapM desugar fs
> desugar (Iden i) = pure $ Iden i
> desugar (App f as) = App <$> desugar f <*> mapM desugar as
> desugar (Lam ns e) = Lam ns <$> desugar e
> desugar (Let bs e) = do
>     let f (n,v) = (n,) <$> desugar v
>     Let <$> mapM f bs <*> desugar e

shouldn't be hit

> desugar (Seq a b) =
>     Seq <$> desugar a <*> desugar b


------------------------------------------------------------------------------

values
------

> data Value = NumV Scientific
>            | BoolV Bool
>            | TextV String
>            | TupleV [Value]
>            | FunV [String] Expr Env
>            | ForeignFunV String

> valueTypeName :: Value -> String
> valueTypeName (NumV {}) = "number"
> valueTypeName (TextV {}) = "text"
> valueTypeName (BoolV {}) = "boolean"
> valueTypeName (TupleV {}) = "tuple"
> valueTypeName (FunV {}) = "function"
> valueTypeName (ForeignFunV {}) = "foreign-function"

> instance Show Value where
>   show (NumV n) = "NumV " ++ show n
>   show (TextV n) = "TextV " ++ show n
>   show (BoolV n) = "BoolV " ++ show n
>   show (TupleV fs) = "TupleV [" ++ intercalate "," (map show fs) ++ "]"
>   show (FunV {}) = "FunV stuff"
>   show (ForeignFunV n) = "ForeignFunV " ++ show n

> instance Eq Value where
>     NumV a == NumV b = a == b
>     TextV a == TextV b = a == b
>     BoolV a == BoolV b = a == b
>     TupleV fs == TupleV gs = fs == gs
>     _ == _ = False

------------------------------------------------------------------------------

> type Interpreter = ReaderT Env (Except String)

> runInterp :: Env -> Expr -> Either String Value
> runInterp env expr = runExcept (runReaderT (interp expr) env)

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Text t) = pure (TextV t)
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

> interp (Block {}) = lift $ throwE $ "undesugared block passed to interpreter"
> interp (Seq a b) = interp a *> interp b



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

------------------------------------------------------------------------------

env, ffi boilerplate
--------------------

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

parse
-----

> parse :: String -> Either String Expr
> parse src =
>     case P.parseProgram "" src of
>       Right (S.Program [] sts) -> convExpr (S.Block sts)
>       Right (S.Program x _) -> Left $ "prelude not supported " ++ show x
>       Left e -> Left e


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

> convExpr (S.Block sts) = Block <$> mapM convSt sts

> convExpr x = Left $ "unsupported expression " ++ show x

> convSt :: S.Stmt -> Either String Stmt
> convSt (S.StExpr e) = StExpr <$> convExpr e
> convSt (S.LetDecl (S.Binding (S.IdenP _ (S.PatName nm)) v)) = LetDecl nm <$> convExpr v

> convSt x = Left $ "unsupported statement " ++ show x


------------------------------------------------------------------------------

tests
-----

todo:

figure out how doing the simplest check thing will work here
maybe extend the simplest check concept
and sketch out the tests for this code
then work backwards from it
not sure if it can be used without doing a basic desugaring of check
and tests
want to do a stripped down proper check implementation
this will form the basis of most examples, the only ones
  are the ones building the bits which need this?
  or does it still add a lot of complexity to avoid in experiments?



check basics
check that the block scoping works by shadowing a variable

the tests are cheating slightly, because testing the code properly is
deferred until more features are added. not sure if this is legit, an
issue, good or bad for maintenance or for tutorial purposes

> additionalTests :: [(String,String)]
> additionalTests = [("x = 3\n\
>                     \x", "3")
>                   ,("3 + 4\n\
>                     \4 + 6", "10")
>                   ,("x = block:\n\
>                     \      3 + 4\n\
>                     \      4 + 6\n\
>                     \    end\n\
>                     \x + 2", "12")
>                   ]

> tests :: TestTree
> tests = makeSimpleTests "simplestatements" (simpleInterpreterExamples ++ additionalTests) evaluate
