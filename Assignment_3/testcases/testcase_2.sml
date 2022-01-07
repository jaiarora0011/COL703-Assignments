val arg = HENCE([ALL(VAR("x"), COND(ATOM("F", [VAR("x")]), ATOM("G", [VAR("x")]))), ALL(VAR("x"), ATOM("G", [VAR("x")]))],ATOM("F", [CONST("a")]));
