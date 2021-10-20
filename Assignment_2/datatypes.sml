structure Propositions =
struct
  datatype Prop = ATOM of string
              | NOT of Prop
              | AND of Prop * Prop
              | OR of Prop * Prop
              | COND of Prop * Prop
              | BIC of Prop * Prop
              | ITE of Prop * Prop * Prop

datatype Argument = HENCE of Prop list * Prop

  fun preOrder(ATOM(s)) = "ATOM(\"" ^ s ^ "\")"
    | preOrder(NOT(p)) = "NOT(" ^ (preOrder p) ^ ")"
    | preOrder(AND(p1, p2)) = "AND(" ^ (preOrder p1) ^ ", " ^ (preOrder p2) ^ ")"
    | preOrder(OR(p1, p2)) = "OR(" ^ (preOrder p1) ^ ", " ^ (preOrder p2) ^ ")"
    | preOrder(COND(p1, p2)) = "COND(" ^ (preOrder p1) ^ ", " ^ (preOrder p2) ^ ")"
    | preOrder(BIC(p1, p2)) = "BIC(" ^ (preOrder p1) ^ ", " ^ (preOrder p2) ^ ")"
    | preOrder(ITE(p1, p2, p3)) = "ITE(" ^ (preOrder p1) ^ ", " ^ (preOrder p2) ^ ", " ^ (preOrder p3) ^ ")"

  fun ListRepr [] = ""
    | ListRepr [p] = preOrder p
    | ListRepr (p :: ps) = (preOrder p) ^ ", " ^ (ListRepr ps)

  fun ArgumentRepr (HENCE(l, p)) = "HENCE([" ^ (ListRepr l) ^ "], " ^ (preOrder p) ^ ")"
end


