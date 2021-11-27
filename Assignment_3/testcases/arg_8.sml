val a = ALL(VAR("x"), ATOM("p", [VAR("x")]))
val b = OR(NOT(ATOM("p", [CONST "a"])), NOT(ATOM("p", [CONST "b"])))

val arg = HENCE([a], NOT(b))

