

val a = ALL(VAR("x"), ATOM("p", [VAR("x")]))
val b = EX(VAR("x"), NOT(ATOM("p", [FUN("f", [VAR("x")])])))

val arg = HENCE([a], NOT(b))
