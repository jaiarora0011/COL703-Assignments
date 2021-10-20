use "datatypes.sml";
open Propositions;

fun rewriteITEprop(prop: Prop) =
  case prop of
      ITE(a, b, c) => AND(COND(rewriteITEprop a, rewriteITEprop b), COND(NOT(rewriteITEprop a), rewriteITEprop c))
    | NOT(a) => NOT(rewriteITEprop a)
    | AND(a, b) => AND(rewriteITEprop a, rewriteITEprop b)
    | OR(a, b) => OR(rewriteITEprop a, rewriteITEprop b)
    | COND(a, b) => COND(rewriteITEprop a, rewriteITEprop b)
    | BIC(a, b) => BIC(rewriteITEprop a, rewriteITEprop b)
    | _ => prop

fun rewriteITE(arg) =
  let
    val HENCE(l, p) = arg
  in
    HENCE(map rewriteITEprop l, rewriteITEprop p)
  end
