use "datatypes.sml";
open Propositions;
open List;
type FormulaSet = Prop list
type TruthAssignment = (Prop * bool) list

fun rewriteITEprop(prop: Prop) =
  case prop of
      ITE(a, b, c) => AND(COND(rewriteITEprop a, rewriteITEprop b), COND(NOT(rewriteITEprop a), rewriteITEprop c))
    | NOT(a) => NOT(rewriteITEprop a)
    | AND(a, b) => AND(rewriteITEprop a, rewriteITEprop b)
    | OR(a, b) => OR(rewriteITEprop a, rewriteITEprop b)
    | COND(a, b) => COND(rewriteITEprop a, rewriteITEprop b)
    | BIC(a, b) => BIC(rewriteITEprop a, rewriteITEprop b)
    | _ => prop

fun rewriteITE(arg: Argument) =
  let
    val HENCE(l, p) = arg
  in
    HENCE(map rewriteITEprop l, rewriteITEprop p)
  end


fun convertArgumentToFormulaSet(arg: Argument) : FormulaSet =
  let
    val HENCE(l, p) = arg
  in
    l @ [NOT p]
  end

fun checkMembership l elem =
  exists (fn x => x = elem) l

fun checkComplimentaryPair (fs: FormulaSet) =
  case fs of
    [] => false
  | [a] => false
  | NOT(p) :: xs => if checkMembership xs p then true else checkComplimentaryPair xs
  | p :: xs => if checkMembership xs (NOT(p)) then true else checkComplimentaryPair xs

val x = checkComplimentaryPair []
val y = checkComplimentaryPair [NOT(ATOM "ABC")]
val z = checkComplimentaryPair [NOT(NOT(ATOM "ABC")), NOT(ATOM "ABC")]
val a = checkComplimentaryPair [NOT(NOT(NOT(AND(ATOM "ABC", ATOM "DEF")))), NOT(NOT(AND(ATOM "ABC", ATOM "DEF")))]
val b = checkComplimentaryPair [ATOM "ABC", AND(ATOM "XYZ", ATOM "XYZ"), NOT(AND(ATOM "XYZ", ATOM "XYZ"))]
