val arg = HENCE([], COND(EX(VAR("x"), AND(ATOM("P", [VAR("x")]), ATOM("Q", [VAR("x")]))), AND(EX(VAR("x"), ATOM("P", [VAR("x")])), EX(VAR("x"), ATOM("Q", [VAR("x")])) )) );
