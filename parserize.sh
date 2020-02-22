#!/bin/bash
#set -ex


ASK=$(cat <<-END
ask:
  | x == 0 then: 1
  | x > 0 then: x
  | otherwise: x * -1
end
END
)

CASES=$(cat <<-END
cases (Two) a:
  | pt1(x,y) => if n == 0: x else: y end
  | pt2(x,y) => if n == 0: x else: y end
end
END
)

CONSTR=$(cat <<-END
example2 = {
  make: lam(args): link(42,args) end,
  make8: lam(a,b): [list: 8,a,b] end
}
END
)

FUNDECL=$(cat <<-END
fun apply-twice(f, x):
  f(f(x))
where:
  apply-twice(square, 3) is 81
end
END
)


CHECK=$(cat <<-END
check "x":
  f(1) is true
  raise("hello") raises "hello"
  raise(1) raises-satisfies is_one
end
END
)

CURLYTHINGS=$(cat <<-END
{a:1,b:2}
{a,b}
END
)

# todo: add prelude statements




cabal run -v0 teaberry --disable-optimization -- --parserize-c "$ASK"
cabal run -v0 teaberry --disable-optimization -- --parserize-c "$CASES"
cabal run -v0 teaberry --disable-optimization -- --parserize-c "$CONSTR"
cabal run -v0 teaberry --disable-optimization -- --parserize-c "$FUNDECL"
cabal run -v0 teaberry --disable-optimization -- --parserize-c "$CHECK"
cabal run -v0 teaberry --disable-optimization -- --parserize-c "$CURLYTHINGS"



