val p1 = ALL(VAR("x"), COND(ATOM("p", [VAR("x")]), EX( VAR("y"), AND(ATOM("p", [VAR("y")]), ATOM("q", [VAR("y"), VAR("x")]) ) )))

val p2 = ALL(VAR("x"), COND(ATOM("F", [VAR("x")]), ATOM("r", [VAR("x"), CONST("a")]) ))

val px = COND(ATOM("p", [VAR("x")]), COND(ATOM("p", [VAR("y")]), COND(ATOM("r", [VAR("y"), VAR("x")]), NOT(ATOM("q", [VAR("x"), VAR("y")])))));

val p3 = ALL(VAR("x"), ALL(VAR("y"), px));


val arg = HENCE([p1, p2, p3], NOT(ATOM("p", [CONST("a")])));
