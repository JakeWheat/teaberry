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
  make8: lam(a,b,c,d,e,f,g,h): [list: 8,a,b,c,d,e,f,g,h] end
}
END
)


cabal run -v0 teaberry --disable-optimization -- --parserize-c "$ASK"
cabal run -v0 teaberry --disable-optimization -- --parserize-c "$CASES"
cabal run -v0 teaberry --disable-optimization -- --parserize-c "$CONSTR"


