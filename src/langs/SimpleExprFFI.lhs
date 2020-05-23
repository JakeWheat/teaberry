
First pass on a proper range of data types. It's a bit trivial, but
the base for some other things.

update: this is daft, it doesn't do anything, what's the motivation

bools: if, ask?
text: ?
binops
start by doing an ffi immediately
copy the existing one
then try to refactor it



start with
numbers
booleans
text
functions

todo later:
variants
boxes
foreign types


> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}

> module SimpleExprFFI (tests) where

> import qualified Parse as P
> import qualified Syntax as S

> --import qualified SimpleExpr (Expr(..), parse)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Except (Except, runExcept, throwE)
> import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
> import Data.Scientific (Scientific)
> import Data.List (intercalate)

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

------------------------------------------------------------------------------

interpreter
-----------

> data Expr = Num Scientific
>           | Text String
>           | Iden String
>           | App Expr [Expr]
>           | Lam [String] Expr
>           | Let [(String,Expr)] Expr
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

> type Interpreter = ReaderT Env (Except String)

> runInterp :: Env -> Expr -> Either String Value
> runInterp env expr = runExcept (runReaderT (interp expr) env)

> interp :: Expr -> Interpreter Value
> interp (Num n) = pure (NumV n)
> interp (Text n) = pure (TextV n)
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

------------------------------------------------------------------------------

function ffi: call haskell functions from in language

creating the standard set of functions:

> testEnv :: Env
> testEnv = either error id $ addForeignFuns'
>    [("+", ternAOp unwrapNum wrapNum plusNum)
>    ,("+", ternAOp unwrapText wrapText plusText)]
>    emptyEnv

> plusNum :: Scientific -> Scientific -> Scientific
> plusNum = (+)

> plusText :: String -> String -> String
> plusText = (++)

todo:

what's the best way to be able to add a new version of (+)?
if functions are registered with their types, then can use a poor man's
overloading system and it can help with error messages for the time being
so the lookup for haskell functions only, uses the types as well
this means we can do overloading with ffi functions, but not native in
language functions

the other option is to create these function by creating a function
that wraps all the variations. this isn't very extensible, but for a
while the only functions like this will be the fixed set in the base
built in functions

either way, eventually at some point need to dispatch on the name of
the function and the types of the args, so they aren't really that
different. the second one doesn't change much except split doing the
lookups into two different places

try both out in separate files?

(the eventual approach might be to take the implementation of types and
type classes, then do an untyped version of this. this needs the
types work to progress a lot first)

what if there was a function in language, that called a haskell
function in the ffi list? this function itself would be a special case
the possible advantage is that every function you can call that's
ships with the language would be in language itself, instead of
hidden, but maybe this isn't sensible if you don't want to force library
writers to take this style, if they don't then you lose that
consistency anyway

once have this working, do an ad hoc error message check
then try to implement the full catalog from the existing main implementation
to see how it looks

then think about a usage api -

how to call scripts from a haskell program with custom haskell ffi
functions

how call scripts from the command line with ad hoc haskell ffi
functions - bit trickier

then figure out what other ffi stuff
-> opaque haskell data types being passed around in language

The above is probably good enough for a long time

-> see if there are options right now to do non opaque data types
     without explicit boilerplate

embedded haskell? is it possible?
want to call haskell functions from in language, without any bespoke
haskell code anywhere else



--------------------------------------

boilerplate

>
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
>
> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n

> unwrapText :: (String, Value -> Interpreter String)
> unwrapText = ("text", \case
>                           TextV n -> pure n
>                           x -> lift $ throwE $ "type: expected text, got " ++ show x)
>
> wrapText :: String -> Interpreter Value
> wrapText n = pure $ TextV n


> ternAOp :: (String, Value -> Interpreter a)
>         -> (a -> Interpreter Value)
>         -> (a -> a -> a)
>         -> ([String], ([Value] -> Interpreter Value))
> ternAOp unwrap wrap f =
>     ([fst unwrap, fst unwrap]
>     ,\as -> do
>             case as of
>                 [a,b] -> do
>                     ax <- (snd unwrap) a
>                     bx <- (snd unwrap) b
>                     wrap (f ax bx)
>                 _ -> lift $ throwE $ "wrong number of args to function, expected 2, got " ++ show (length as))



TODO:

haskellFunctions is hardcoded in the interpreter
want to pass it as a value

want to write something like:
add function "+", plus
there's tons of boilerplate
want to make it easy to give much better errors
is there a way to automatically create the unwrapping?

what does it look like to use 'dynamic type classes'
so you create + as a typeclass function
then you can add a + for some particular types independently

eventually want a standard set of functions

with the ability to add your own haskell functions when running
something

and the ability to have a minimal built in set, or none at all

the next step after that is to implement haskell data types ffi
so you can pass haskell values around in language opaquely
and then add something that allows you to use them as conveniently as
in language values

it would be nice to make the Value type abstract for users of the code
too, to insulate at API level from implementation changes.

------------------------------------------------------------------------------

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

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     runInterp testEnv ast

------------------------------------------------------------------------------

tests
-----

> interpreterExamples :: [(String, String)]
> interpreterExamples =
>     [("1", "1")
>     ,("1 + 2", "3")
>     ,("let x = 3: x end", "3")
>     ,("lam(x,y): x + y end(1,2)", "3")
>     ,("let f = lam(x,y): x + y end: f(1,2) end", "3")
>     ,("'three'", "'three'")
>     ,("'three' + 'four'", "'threefour'")
>     ]

> runTest :: String -> String -> T.TestTree
> runTest s v = T.testCase s $ do
>     let res = either error id $ evaluate s
>         expected = either error id $ evaluate v
>     T.assertEqual "" expected res

> tests :: T.TestTree
> tests = T.testGroup "simpleexprreader"
>            $ map (uncurry runTest) interpreterExamples
