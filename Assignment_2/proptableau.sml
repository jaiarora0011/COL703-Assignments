use "datatypes.sml";
open Propositions;
open List;
type FormulaSet = Prop list
type TruthAssignment = (Prop * bool) list

exception NotLiteralException

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
val y = checkComplimentaryPair [NOT(ATOM "ABC"), OR(COND(ATOM "BC", ATOM "AC"), ATOM "AB")]
val z = checkComplimentaryPair [NOT(NOT(ATOM "ABC")), NOT(ATOM "ABC")]
val a = checkComplimentaryPair [NOT(NOT(NOT(AND(ATOM "ABC", ATOM "DEF")))), NOT(NOT(AND(ATOM "ABC", ATOM "DEF")))]
val b = checkComplimentaryPair [ATOM "ABC", AND(ATOM "XYZ", ATOM "XYZ"), NOT(AND(ATOM "XYZ", ATOM "XYZ"))]


fun checkLiteral (p: Prop) =
  case p of
      ATOM(str) => true
    | NOT(ATOM(str)) => true
    | _ => false

fun checkElongation (p: Prop) =
  case p of
      NOT(NOT(x)) => true
    | AND(x, y) => true
    | NOT(OR(x, y)) => true
    | NOT(COND(x, y)) => true
    | _ => false

fun checkBranching (p: Prop) =
  case p of
      NOT(AND(x, y)) => true
    | OR(x, y) => true
    | COND(x, y) => true
    | BIC(x, y) => true
    | NOT(BIC(x, y)) => true
    | _ => false


fun getStats (fs: FormulaSet) : int * int * int =
  case fs of
      [] => (0, 0, 0)
    | x :: xs => (let
                    val (n1, n2, n3) = getStats xs
                  in
                    if checkLiteral x then (n1+1, n2, n3)
                    else if checkElongation x then (n1, n2+1, n3)
                    else (n1, n2, n3+1)
                  end)


fun applyElongation(fs: FormulaSet) =
  case fs of
      [] => []
    | NOT(NOT(x)) :: xs => x :: xs
    | AND(x, y) :: xs => x :: y :: xs
    | NOT(OR(x, y)) :: xs => (NOT(x)) :: (NOT(y)) :: xs
    | NOT(COND(x, y)) :: xs => x :: (NOT(y)) :: xs
    | x :: xs => x :: (applyElongation xs)

fun applyBranching(fs: FormulaSet): FormulaSet * FormulaSet =
  case fs of
      [] => ([], [])
    | NOT(AND(x, y)) :: xs => (NOT(x) :: xs, NOT(y) :: xs)
    | OR(x, y) :: xs => (x :: xs, y :: xs)
    | COND(x, y) :: xs => (NOT(x) :: xs, y :: xs)
    | BIC(x, y) :: xs => (AND(x, y) :: xs, AND(NOT(x), NOT(y)) :: xs)
    | NOT(BIC(x, y)) :: xs => (AND(x, NOT(y)) :: xs, AND(NOT(x), y) :: xs)
    | x :: xs => (let
                    val (l1, l2) = applyBranching xs
                  in
                    (x :: l1, x :: l2)
                  end)

fun getLiteralMapping (literal: Prop) =
  case literal of
      ATOM(str) => (ATOM(str), true)
    | NOT(ATOM(str)) => (ATOM(str), false)
    | _ => raise NotLiteralException

fun tableauMethod (fs: FormulaSet): TruthAssignment list =
  case fs of
      [] => []
    | x :: xs => (if checkComplimentaryPair fs then []
                    else (let
                            val (n1, n2, n3) = getStats fs
                          in
                            if n2 <> 0 then tableauMethod (applyElongation fs)
                              else if n3 <> 0 then (let
                                                      val (l1, l2) = applyBranching fs
                                                    in
                                                      (tableauMethod l1) @ (tableauMethod l2)
                                                    end)
                                    else [map getLiteralMapping fs]
                          end)
                  )

fun boolToString(false) = "False"
  | boolToString(true) = "True"

fun truthAssignmentToString(tau: TruthAssignment): string =
  case tau of
      [] => ""
    | (ATOM(str), b) :: xs => str ^ " <-- " ^ (boolToString b) ^ "\n" ^ (truthAssignmentToString xs)
    | _ => raise NotLiteralException

fun finalOutput(falsifying: TruthAssignment list): string =
  case falsifying of
      [] => "No Falsifying assignment found -- The input formula is valid"
    | [t] => "=Mapping Starts\n" ^ (truthAssignmentToString t) ^ "=Mapping Ends\n"
    | t :: xs => "=Mapping Starts\n" ^ (truthAssignmentToString t) ^ "=Mapping Ends\n" ^ (finalOutput xs)
