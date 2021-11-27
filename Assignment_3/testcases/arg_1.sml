val a = ATOM("p", [])
val b = ATOM("q", [])

val arg = HENCE([], COND(a, COND(b, a)))
