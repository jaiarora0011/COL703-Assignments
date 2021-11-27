val a = ALL(VAR("x"), COND(ATOM("p", [VAR("x")]), ATOM("q", [VAR("x")])))
val b = COND(ALL(VAR("x"), ATOM("p", [VAR("x")])), ALL(VAR("x"), ATOM("q", [VAR("x")])))

val arg = HENCE([a], b)

