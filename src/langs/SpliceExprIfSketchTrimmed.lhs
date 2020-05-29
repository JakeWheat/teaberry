
sketch of implementing if on top of expr1ffi using splices
trimmed to remove comments

> $(splice Expr1FFI.language_pragmas)

> module Expr1If (tests) where

> $(splice Expr1FFI.imports)

------------------------------------------------------------------------------

interpreter
-----------

> $(splice Expr1FFI.data where name = Expr and variant `notelem` [text]
> add variant
>     | If Expr Expr Expr
> )

> $(splice Expr1FFI.data where name = Env)

> $(splice Expr1FFI.values where name in [emptyEnv, extendEnv, envLookup, addForeignFunction, lookupForeignFunction])

------------------------------------------------------------------------------

values

> $(splice data Value)

> $(splice Expr1FFI.values where name == valueTypeName
>  and first_arg not matches(Text*))

> $(splice Expr1FFI.instances where name == Show Value
>  and first_arg not matches(Text*))

> $(splice instance eq value)


------------------------------------------------------------------------------

interpreter

> $(splice type interpreter, runinterp)


> $(splice value interp exclude firstarg matches text)

> interp (If c t e) = do
>     c' <- interp c
>     case c' of
>         BoolV True -> interp t
>         BoolV False -> interp e
>         _ -> lift $ throwE $ "expected bool in if test, got " ++ show c'

> $(splice value evaluate)

------------------------------------------------------------------------------

ffi catalog

> $(splice value testEnv)

> localFileFFIEnv = [("==", binaryOp unwrapNum unwrapNum wrapBool (==))]

> $(splice addForeignFun' addForeignFuns' unwrapNum , wrapNum wrapBool binaryOp)

------------------------------------------------------------------------------

parser

$(splice parse)
$(splice convExpr filter not matches (sel . Str))

question: if the last line in convExpr is something like
convExpr x = Left $ "syntax not supported: " ++ show x

we don't want to append after this line
I don't want the code to be rearranged implicitly
so the splice above must either say move this line below
or there should be two splices, one without this line above
and one just for this line below

maybe with the named groups of splicable stuff, can start naming
splice points, and do something high level, but which is still
explicit and isn't something that tries to be implicitly aware of
the meaning of the code

> convExpr (S.If [(c,t)] (Just e)) = do
>     If <$> convExpr c <*> convExpr t <*> convExpr e

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
