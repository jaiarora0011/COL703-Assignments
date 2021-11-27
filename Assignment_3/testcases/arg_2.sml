val a = ATOM("p", [])
val b = ATOM("q", [])
val c = ATOM("r", [])
val d = COND(a, COND(b, c))
val e = COND(COND(a, b), COND(a, c))

val arg = HENCE([], COND(d, e))
