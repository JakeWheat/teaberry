
TODO:
move most of these tests to language based tests

> {-# LANGUAGE TupleSections #-}
> module InterpreterTests (interpreterExamples, testInterpreter) where


> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as T

> import Parse (parseStmts)
> import Desugar (desugarStmts)
> import Interpreter (Value(..))
> import qualified Interpreter as I
> --import qualified InterpreterSyntax as I
>

> interpreterExamples :: [(String, String, Value)]
> interpreterExamples =
>     [("2", [], NumV 2)
>     ,("myVar", defs, NumV 1)
>     ,("true", [], BoolV True) 
>     ,("false", [],  BoolV False)
>     ,("\"hello\"", [], StrV "hello")
>     ,("1 + 2", defs, NumV 3)
>     ,("3 - 2", defs, NumV 1)
>     ,("if true: 1 else: 2 end", defs, NumV 1)
>     ,("if false: 1 else: 2 end", defs, NumV 2)
>     ,("add2(1)", defs, NumV 3)
>     ,("5 + quad(3)", defs, NumV 17)
>     ,("const5(3) * 4", defs, NumV 20)
>     ,("10 + const5(10)", defs, NumV (10 + 5))
>     ,("10 + double(1 + 2)", defs, NumV (10 + 3 + 3))
>     ,("10 + quad(1 + 2)", defs, NumV (10 + 3 + 3 + 3 + 3))
>     ,("lam(x): x + 5 end(10)", defs, NumV 15)
>     ,("let x=4,y=5: x + y end", defs, NumV 9)
>     ,("let x=4,y=5: x - y end", defs, NumV (-1))
>     ,("let myF=lam(x): x + 5 end: myF(20) end", defs, NumV 25)
>     ,("block: 1 + 2\n  3 + 4\nend", defs, NumV 7)
>
>     ,("letrec fact = lam(n):\n\
>       \    if n == 1: 1 else: n * fact(n - 1) end\n\
>       \end: fact(5) end", defs, NumV 120)

>     ,("lam(): 7 end()", defs, NumV 7)

>     ,("letrec\n\
>       \  fact = lam(n): if n == 1: 1 else: n * fact(n - 1) end end,\n\
>       \  abc = 5:\n\
>       \  fact(abc)\n\
>       \end", defs, NumV 120)

>     ,("letrec\n\
>       \  fact = lam(n): if n == 1: 1 else: n * fact(n - 1) end end,\n\
>       \  abc = fact(5):\n\
>       \  abc\n\
>       \end", defs, NumV 120)

>     ,("letrec\n\
>       \  abc = fact(5),\n\
>       \  fact = lam(n): if n == 1: 1 else: n * fact(n - 1) end end:\n\
>       \  abc\n\
>       \end", defs, NumV 120)

>     {-"letrec\n\  BAD: abc is before fact and refers to it
>       \  one = 1,\n\
>       \  abc = fact(5),\n\
>       \  fact = lam(n): if n == one: one else: n * fact(n - one) end end:\n\
>       \  abc\n\
>       \end", defs, NumV 120)

>     ,("letrec\n\  BAD: abc is before fact and refers to it
>       \  abc = fact(5),\n\
>       \  fact = lam(n): if n == one: one else: n * fact(n - one) end end:\n\
>       \  one = 1,\n\
>       \  abc\n\
>       \end", defs, NumV 120)-}


>     ,("letrec\n\
>       \  zero = 0,\n\
>       \  one = zero + 1\n\
>       \  : one\n\
>       \end", defs, NumV 1)


>     {-,("letrec \n\ BAD: needs reordering support for one
>       \  zero = 0,\n\
>       \  addeven = lam(x) : if x == 0: 0 else: x + addodd(x - one) end end,\n\
>       \  addodd = lam(x) : if x == 0: 0 else: x + addeven(x - 1) end end,\n\
>       \  one = #|addeven(0) +|# addodd(0) + 1 + zero,\n\
>       \  #zero = 0,\n\
>       \  a = 1\n\
>       \  :\n\
>       \  addeven(6)\n\
>       \end", defs, NumV 21)-}

>     {-,("letrec \n\ BAD: needs reordering support for one
>       \  zero = 0,\n\
>       \  addodd = lam(x) : if x == 0: 0 else: x + addeven(x - 1) end end,\n\
>       \  addeven = lam(x) : if x == 0: 0 else: x + addodd(x - one) end end, \n\
>       \  one = #|addeven(0) +|# addodd(0) + 1 + zero,\n\
>       \  a = 1\n\
>       \  :\n\
>       \  addeven(6)\n\
>       \end", defs, NumV 21)-}


>     {-,("letrec \n\ not sure how this is supposed to work - check if it does in pyret
>       \  addeven = lam(x) : if x == 0: 0 else: x + addodd(x - one) end end,\n\
>       \  addodd = lam(x) : if x == 0: 0 else: x + addeven(x - 1) end end,\n\
>       \  one = addeven(0) + addodd(0) + 1,\n\
>       \  a = 1\n\
>       \  :\n\
>       \  addeven(6)\n\
>       \end", defs, NumV 21)-}

>     {-,("letrec \n\ #can't find the 0 - reorder?
>       \  addodd = lam(x) : if x == 0: zero else: x + addeven(x - 1) end end,\n\
>       \  addeven = lam(x) : if x == 0: 0 else: x + addodd(x - one) end end, \n\
>       \  zero = 0,\n\
>       \  one = #|addeven(0) +|# addodd(0) + 1 + zero,\n\
>       \  a = 1\n\
>       \  :\n\
>       \  addeven(6)\n\
>       \end", defs, NumV 21)-}


>     ,("letrec\n\ 
>       \  addeven = lam(x) : if x == 0: 0 else: x + addodd(x - 1) end end,\n\
>       \  addodd = lam(x) : if x == 0: 0 else: x + addeven(x - 1) end end:\n\
>       \  addeven(6)\n\
>       \end", defs, NumV 21)

>     ,("block:\n\
>       \fun addeven1(x): if x == 0: 0 else: x + addodd1(x - 1) end end\n\
>       \fun addodd1(x): if x == 0: 0 else: x + addeven1(x - 1) end end\n\
>       \addeven1(6)\n\
>       \end", defs, NumV 21)

>     ,("block:\n\
>       \rec addeven2 = lam(x) : if x == 0: 0 else: x + addodd2(x - 1) end end\n\
>       \rec addodd2 = lam(x) : if x == 0: 0 else: x + addeven2(x - 1) end end\n\
>       \addeven2(6)\n\
>       \end", defs, NumV 21)

 #addeven3 = lam(x) : if x == 0: 0 else: x + addodd3(x - 1) end end
 #addodd3 = lam(x) : if x == 0: 0 else: x + addeven3(x - 1) end end
 #addeven3(6)

>     ,("block:\n\
>       \fun addeven4(x): if x == 0: 0 else: x + addodd4(x - 1) end end\n\
>       \rec addodd4 = lam(x) : if x == 0: 0 else: x + addeven4(x - 1) end end\n\
>       \addeven4(6)\n\
>       \end", defs, NumV 21)

>     ,("block:\n\
>       \rec addeven4 = lam(x) : if x == 0: 0 else: x + addodd4(x - 1) end end\n\
>       \fun addodd4(x): if x == 0: 0 else: x + addeven4(x - 1) end end\n\
>       \addeven4(6)\n\
>       \end", defs, NumV 21)



>     ,("block:\n\
>       \  a = 4\n\
>       \  a + 3\n\
>       \end", defs, NumV 7)

>     ,("block:\n\
>       \  a = 4\n\
>       \  b = 5\n\
>       \  a + b\n\
>       \end", defs, NumV 9)

>     ,("block:\n\
>       \  #fact = lam(x): x + 1 end\n\
>       \  #fact = lam(f): lam(x): if x == 0: 1 else: x * f(x - 1) end end end\n\
>       \  f1 = lam(factx,x): if x == 0: 1 else: x * factx(factx,x - 1) end end\n\
>       \  fact = lam(y): f1(f1,y) end\n\
>       \  fact(1)\n\
>       \  #fact = fix(lam(fact): lam(x): if x == 0: 1 else: x * fact(x - 1) end end\n\
>       \  #fact(0)\n\
>       \end", [], NumV 1)

>     ,("block:\n\
>       \  var a = 5\n\
>       \  a\n\
>       \end", [], NumV 5)

>     ,("block:\n\
>       \  var a = 5\n\
>       \  a := 6\n\
>       \  a\n\
>       \end", [], NumV 6)

>     ,("block:\n\
>       \  var a = 5\n\
>       \  a := a + 1\n\
>       \  a\n\
>       \end", [], NumV 6)


>     ,("block:\n\
>       \  mk-counter = lam(a): block:\n\
>       \      var ctr = 0\n\
>       \      lam(a): block:\n\
>       \          ctr := ctr + 1\n\
>       \          ctr\n\
>       \        end\n\
>       \      end\n\
>       \    end\n\
>       \  end\n\
>       \  x = mk-counter(0)\n\
>       \  x(1)\n\
>       \  x(1)\n\
>       \end", [], NumV 2)

>     ,("block:\n\
>       \  mk-counter = lam(a): block:\n\
>       \      var ctr = 0\n\
>       \      lam(a): block:\n\
>       \          ctr := ctr + 1\n\
>       \          ctr\n\
>       \        end\n\
>       \      end\n\
>       \    end\n\
>       \  end\n\
>       \  x = mk-counter(0)\n\
>       \  y = mk-counter(0)\n\
>       \  y(1)\n\
>       \  y(1)\n\
>       \  x(1)\n\
>       \end", [], NumV 1)

>     ,("block:\n\
>       \  fun mk-counter():\n\
>       \    var ctr = 0\n\
>       \    lam(): block:\n\
>       \        ctr := ctr + 1\n\
>       \        ctr\n\
>       \      end\n\
>       \    end\n\
>       \  end\n\
>       \  x = mk-counter()\n\
>       \  y = mk-counter()\n\
>       \  y()\n\
>       \  y()\n\
>       \  x()\n\
>       \end", [], NumV 1)

>     ,("block:\n\
>       \  fun fact(x):\n\
>       \    if x == 0: 1\n\
>       \    else: x * fact(x - 1)\n\
>       \    end\n\
>       \  end\n\
>       \  fact(5)\n\
>       \end", [], NumV 120)

>     ,("block:\n\
>       \  rec fact = lam (x):\n\
>       \    if x == 0: 1\n\
>       \    else: x * fact(x - 1)\n\
>       \    end\n\
>       \  end\n\
>       \  fact(5)\n\
>       \end", [], NumV 120)

>     ,("letrec\n\ 
>       \  addeven = lam(x) : if x == 0: 0 else: x + addodd(x - 1) end end,\n\
>       \  addodd = lam(x) : if x == 0: 0 else: x + addeven(x - 1) end end:\n\
>       \  addeven(6)\n\
>       \end", [], NumV 21)


>     ]
>  where
>    defs = "double = lam (x): x + x end\n\
>           \quad = lam (x): double (double (x)) end\n\
>           \const5 = lam (_): 5 end\n\
>           \f4 = lam (x): if x: 1 else: 0 end end\n\
>           \add2 = lam (a): a + 2 end\n\
>           \myVar = 1"

> testInterpreter :: (String, String, Value) -> T.TestTree
> testInterpreter (src, defs, ex) = T.testCase ("interp " ++ src) $ do
>     let s = "block:\n" ++ defs ++ "\n" ++ src ++ "\nend"
>         ast = either error id $ parseStmts "" s
>         iast = either error id $ desugarStmts ast
>     Right (Just x) <- I.interp iast
>     T.assertEqual "" ex x
