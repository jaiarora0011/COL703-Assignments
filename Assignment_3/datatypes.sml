(*structure Propositions =
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

  fun listRepr [] = ""
    | listRepr [p] = preOrder p
    | listRepr (p :: ps) = (preOrder p) ^ ", " ^ (listRepr ps)

  fun argumentRepr (HENCE(l, p)) = "HENCE([" ^ (listRepr l) ^ "], " ^ (preOrder p) ^ ")"

  fun prop2flasl(ATOM(s)) = "\"" ^ s ^ "\""
  | prop2flasl(NOT(p)) = "NOT " ^ (prop2flasl p)
  | prop2flasl(AND(p1, p2)) = "(" ^ (prop2flasl p1) ^ " AND " ^ (prop2flasl p2) ^ ")"
  | prop2flasl(OR(p1, p2)) = "(" ^ (prop2flasl p1) ^ " OR " ^ (prop2flasl p2) ^ ")"
  | prop2flasl(COND(p1, p2)) = "(IF " ^ (prop2flasl p1) ^ " THEN " ^ (prop2flasl p2) ^ ")"
  | prop2flasl(BIC(p1, p2)) = "(" ^ (prop2flasl p1) ^ " IFF " ^ (prop2flasl p2) ^ ")"
  | prop2flasl(ITE(p1, p2, p3)) = "(IF " ^ (prop2flasl p1) ^ " THEN " ^ (prop2flasl p2) ^ " ELSE " ^ (prop2flasl p3) ^ ")"

fun ast2flasl(HENCE(l, p)) =
  let
    val mapped = map prop2flasl l
    val periodProps = map (fn s => s ^ ".") mapped
    val hypo = foldr (fn (s, y) => s ^ " " ^ y) "" periodProps
  in
    hypo ^ "THEREFORE " ^ (prop2flasl p) ^ "."
  end

end
*)

signature FOL =
  sig
      datatype term = VAR of string
                    | FUN of string * term list
                    | CONST of string (* for generated constants only *)
      datatype Pred = FF (* special constant for closing a tableau path *)
                    | ATOM of string * term list
                    | NOT of Pred
                    | AND of Pred * Pred
                    | OR of Pred * Pred
                    | COND of Pred * Pred
                    | BIC of Pred * Pred
                    | ITE of Pred * Pred * Pred
                    | ALL of term * Pred
                    | EX of term * Pred
      datatype Argument =  HENCE of Pred list * Pred
      val mktableau: Pred list * Pred -> unit (* outputs file "tableau.dot" in dot format *)

      exception NotVAR (* Binding term in a quantified formula is not a variable *)
      exception NotWFT (* term is not well-formed *)
      exception NotWFP (* Predicate is not well-formed *)
      exception NotWFA (* Argument is not well-formed *)
      exception NotClosed (* a formula is not closed *)
  end

fun output (outfile: string) (res: string) =
  let
    val outstream = TextIO.openOut outfile
  in
    TextIO.output(outstream, res)
  end

fun boolAnd (b1: bool, b2: bool) = b1 andalso b2
