use "datatypes.sml";
open Propositions;
open List;
type FormulaSet = Prop list (* The formula set is just a list of formulas *)
type TruthAssignment = (Prop * bool) list

exception NotLiteralException


(* Need to rewrite the IF-THEN-ELSE construct as it has no corresponding tableau rules*)
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

(*
  Takes an Argument as input and converts it into a set of formulae by taking the NOT of the conclusion and adding it to the list of antecedents
*)
fun convertArgumentToFormulaSet(arg: Argument) : FormulaSet =
  let
    val HENCE(l, p) = arg
  in
    l @ [NOT p]
  end

(* Function for checking if an element exists in a list *)
fun checkMembership l elem =
  exists (fn x => x = elem) l

(*
  Checks if the given formula set has any complimentary formula pair. If yes, then return true, else false.
  If there is a complimentary pair, then the corresponding path in the tableau is closed.
*)
fun checkComplimentaryPair (fs: FormulaSet) =
  case fs of
    [] => false
  | [a] => false
  | NOT(p) :: xs => if checkMembership xs p then true else checkComplimentaryPair xs
  | p :: xs => if checkMembership xs (NOT(p)) then true else checkComplimentaryPair xs

(*val x = checkComplimentaryPair []
val y = checkComplimentaryPair [NOT(ATOM "ABC"), OR(COND(ATOM "BC", ATOM "AC"), ATOM "AB")]
val z = checkComplimentaryPair [NOT(NOT(ATOM "ABC")), NOT(ATOM "ABC")]
val a = checkComplimentaryPair [NOT(NOT(NOT(AND(ATOM "ABC", ATOM "DEF")))), NOT(NOT(AND(ATOM "ABC", ATOM "DEF")))]
val b = checkComplimentaryPair [ATOM "ABC", AND(ATOM "XYZ", ATOM "XYZ"), NOT(AND(ATOM "XYZ", ATOM "XYZ"))]*)

(* Checks if the given propositional formula is a literal *)
fun checkLiteral (p: Prop) =
  case p of
      ATOM(strr) => true
    | NOT(ATOM(strr)) => true
    | _ => false

(* Checks if an elongation rule can be applied to the given formula *)
fun checkElongation (p: Prop) =
  case p of
      NOT(NOT(x)) => true
    | AND(x, y) => true
    | NOT(OR(x, y)) => true
    | NOT(COND(x, y)) => true
    | _ => false

(* Checks if a branching rule can be applied to the given formula *)
fun checkBranching (p: Prop) =
  case p of
      NOT(AND(x, y)) => true
    | OR(x, y) => true
    | COND(x, y) => true
    | BIC(x, y) => true
    | NOT(BIC(x, y)) => true
    | _ => false

(*
  Returns a 3-tuple of integers given a formula set -- (x, y, z)
  x denotes the number of literals in the formula set
  y denotes the number of formulae where elongation can be applied
  z denotes the number of formulae where branching can be applied

  NOTE: x + y + z = sizeof(FormulaSet) always
*)
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

(* This function takes a formula set and applies an elongation rule to the first formula where it can be applied *)
fun applyElongation(fs: FormulaSet) =
  case fs of
      [] => []
    | NOT(NOT(x)) :: xs => x :: xs
    | AND(x, y) :: xs => x :: y :: xs
    | NOT(OR(x, y)) :: xs => (NOT(x)) :: (NOT(y)) :: xs
    | NOT(COND(x, y)) :: xs => x :: (NOT(y)) :: xs
    | x :: xs => x :: (applyElongation xs)

(* This function takes a formula set and applies a branching rule to the first formula where it can be applied, and returns a pair of formula sets *)
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

(* Given a literal at the leaf, this function returns the truth assignment mapping for that literal *)
fun getLiteralMapping (literal: Prop) =
  case literal of
      ATOM(strr) => (ATOM(strr), true)
    | NOT(ATOM(strr)) => (ATOM(strr), false)
    | _ => raise NotLiteralException

(*
  This function implements the actual tableau.
  It takes a formula set as input and returns a list of all possible truth assignments for the formula set.
  First, we check if there is a complementary pair in the set. If yes, then the empty list is returned.
  If no, then we check if there is any formula where we can apply any elongation rule and recurse.
  The heuristic is to apply elongation rules before branching rules.
  Then we check the possibility of applying a branching rule and recurse.
  At some point, we will be left with a set of only literals (without any complimentary pairs), at which point we can get a truth assignment.
  We collect all possible truth assignments we can get and return a list.
*)
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
    | (ATOM(strr), b) :: xs => strr ^ " <-- " ^ (boolToString b) ^ "\n" ^ (truthAssignmentToString xs)
    | _ => raise NotLiteralException

fun finalOutput(falsifying: TruthAssignment list): string =
  case falsifying of
      [] => "No Falsifying assignment found -- The input formula is valid (A tautology)"
    | [t] => "=Mapping Starts\n" ^ (truthAssignmentToString t) ^ "=Mapping Ends\n"
    | t :: xs => "=Mapping Starts\n" ^ (truthAssignmentToString t) ^ "=Mapping Ends\n" ^ (finalOutput xs)
