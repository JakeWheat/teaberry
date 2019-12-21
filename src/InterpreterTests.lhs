
> {-# LANGUAGE TupleSections #-}
> module InterpreterTests (interpreterExamples, testInterpreter) where
>
> import Parse
> import Desugar
> import Interpreter
> --import qualified InterpreterSyntax as I
>
> import Test.Tasty
> import Test.Tasty.HUnit

> interpreterExamples :: [(String, Env, [(String,String)], Value)]
> interpreterExamples =
>     [("2", defaultHaskellFFIEnv, [], NumV 2)
>     ,("myVar", defaultHaskellFFIEnv, defs, NumV 1)
>     ,("true", defaultHaskellFFIEnv, [], BoolV True) 
>     ,("false", defaultHaskellFFIEnv, [],  BoolV False)
>     ,("\"hello\"", defaultHaskellFFIEnv, [], StrV "hello")
>     ,("1 + 2", defaultHaskellFFIEnv, defs, NumV 3)
>     ,("3 - 2", defaultHaskellFFIEnv, defs, NumV 1)
>     ,("if true: 1 else: 2 end", defaultHaskellFFIEnv, defs, NumV 1)
>     ,("if false: 1 else: 2 end", defaultHaskellFFIEnv, defs, NumV 2)
>     ,("add2(1)", defaultHaskellFFIEnv, defs, NumV 3)
>     ,("5 + quad(3)", defaultHaskellFFIEnv, defs, NumV 17)
>     ,("const5(3) * 4", defaultHaskellFFIEnv, defs, NumV 20)
>     ,("10 + const5(10)", defaultHaskellFFIEnv, defs, NumV (10 + 5))
>     ,("10 + double(1 + 2)", defaultHaskellFFIEnv, defs, NumV (10 + 3 + 3))
>     ,("10 + quad(1 + 2)", defaultHaskellFFIEnv, defs, NumV (10 + 3 + 3 + 3 + 3))
>     ,("lam(x): x + 5 end(10)", defaultHaskellFFIEnv, defs, NumV 15)
>     ,("let x=4,y=5: x + y end", defaultHaskellFFIEnv, defs, NumV 9)
>     ,("let x=4,y=5: x - y end", defaultHaskellFFIEnv, defs, NumV (-1))
>     ,("let myF=lam(x): x + 5 end: myF(20) end", defaultHaskellFFIEnv, defs, NumV 25)
>     ,("block: 1 + 2\n  3 + 4\nend", defaultHaskellFFIEnv, defs, NumV 7)
>
>     ,("letrec fact = lam(n):\n\
>       \    if n == 1: 1 else: n * fact(n - 1) end\n\
>       \end: fact(5) end", defaultHaskellFFIEnv, defs, NumV 120)

>     ,("block:\n\
>       \  a = 4\n\
>       \  a + 3\n\
>       \end", defaultHaskellFFIEnv, defs, NumV 7)

>     ,("block:\n\
>       \  a = 4\n\
>       \  b = 5\n\
>       \  a + b\n\
>       \end", defaultHaskellFFIEnv, defs, NumV 9)

>     ,("block:\n\
>       \  #fact = lam(x): x + 1 end\n\
>       \  #fact = lam(f): lam(x): if x == 0: 1 else: x * f(x - 1) end end end\n\
>       \  f1 = lam(factx,x): if x == 0: 1 else: x * factx(factx,x - 1) end end\n\
>       \  fact = lam(y): f1(f1,y) end\n\
>       \  fact(1)\n\
>       \  #fact = fix(lam(fact): lam(x): if x == 0: 1 else: x * fact(x - 1) end end\n\
>       \  #fact(0)\n\
>       \end", defaultHaskellFFIEnv, [], NumV 1)

Fix (Lam (\f -> Lam (\y -> Lift ( if eval y == 0 then 1 else eval y * (eval f) (eval y - 1)))))
let rec x = e1 in e2    =    let x = fix (\x. e1) in e2


>     --,("let f = lam(x) block:\n
>     -- \
>     ]
>  where
>    defs = [("double", "lam (x): x + x end")
>           ,("quad", "lam (x): double (double (x)) end")
>           ,("const5", "lam (_): 5 end")
>           ,("f4", "lam (x): if x: 1 else: 0 end end")
>           ,("add2", "lam (a): a + 2 end")
>           ,("myVar", "1")]

> --s2p2d :: String -> Either String I.Expr
> --s2p2d s = desugar =<< parseStmt "" s

> 

> {-addFn :: String -> String -> Env -> IO Env
> addFn nm src env = do
>     let ast = either error id $ s2p2d src
>     x <- interp env ast
>     case x of
>         ClosV {} -> either error id $ extendEnv nm x env
>         _ -> error "expected closure in addFn, got " ++ show x-}
>         
>         


> testInterpreter :: (String,Env,[(String,String)],Value) -> TestTree
> testInterpreter (src, env, defs, ex) = testCase ("interp " ++ src) $ do
>     --x <- case desugar =<< parseStmt "" src of
>     --    Left e -> return $ Left e
>     let defsP = map (\(n,s) -> (n,s2p2de s)) defs
>     x <- interp env defsP (s2p2de src)
>     either error (assertEqual "" ex) x
>  where
>    s2p2de s = either error id (desugar =<< parseStmt "" s)
