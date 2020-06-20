
Showing a function ffi for implementing functions in haskell

TODO:

is this too much:
create the first pass file separately
then show the boilerplate remove
then show the overloading hack
then do one with the full range of functions from the current main code?

start by doing later experiments that need the ffi, and then refactor
backwards


what's a complete set of functions for a demo right now?

list stuff
tuple stuff
agdt support

constants
nothing - which is a variant?

test support

*
/
+
-
==
<>
<
 >
^
|>
and
or
not
raise?
print
torepr
to-repr
tostring
to-string
is-boolean
is-string
is-function
is-nothing
is-tuple
is-record
is-number




> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE LambdaCase #-}

> module Expr1FFI (tests) where

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
> import Scientific1 (Scientific)
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

> evaluate :: String -> Either String Value
> evaluate s =  do
>     ast <- parse s
>     runInterp testEnv ast

------------------------------------------------------------------------------

ffi catalog

> testEnv :: Env
> testEnv = either error id $ addForeignFuns'
>    [("+", binaryOp unwrapNum unwrapNum wrapNum plusNum)
>    ,("+", binaryOp unwrapText unwrapText wrapText plusText)]
>    $ emptyEnv {envEnv = [("true", BoolV True)
>                         ,("false", BoolV False)]}

> plusNum :: Scientific -> Scientific -> Scientific
> plusNum = (+)

> plusText :: String -> String -> String
> plusText = (++)

Eventually, every ffi function must say what types it accepts for the
numpty overloading system (that will be replaced later), and have an
signature which is [Value] -> Interpreter Value.

Create a bunch of helpers which can lift functions that accept
specific types, or aren't already in the interpreter monad, so each
ffi has the natural signature you want: unwrapped types, types
wrapped in Value already, in or not in the interpreter monad, takes a
[Value] or not, etc.

Then these helpers are a bunch of annoying boilerplate to write and
maintain, but implementing each ffi function is direct and simple,
and adding them to the catalog is simple too.

TODO: bring in the other helper functions and ideas from later
versions into a extended ffi version which has all the different
helpers that are used.


--
old notes:

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
>                           x -> lift $ throwE $ "type: expected number, got " ++ show x)

> wrapNum :: Scientific -> Interpreter Value
> wrapNum n = pure $ NumV n

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
> convExpr (S.Sel (S.Text x)) = Right $ Text x
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
>     [("'three'", "'three'")
>     ,("'three' + 'four'", "'threefour'")
>     ,("true", "true")
>     ]

> tests :: TestTree
> tests = makeSimpleTests "expr1ffi" interpreterExamples evaluate
