val a = ALL(VAR("x"), ATOM("p", [VAR("x")]))
val b = EX(VAR("x"), NOT(ATOM("p", [FUN("f", [FUN("g", [VAR("x")])])])))

val arg = HENCE([a], NOT(b))

