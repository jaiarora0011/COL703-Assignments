val a = ATOM("p", [])
val b = ATOM("q", [])
val c = ATOM("r", [])
val d = COND(a, b)
val e = COND(b, c)

val arg = HENCE([d, e], COND(a, c))
