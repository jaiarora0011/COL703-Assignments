use "datatypes.sml";
open List;

structure MyFOL : FOL =
  struct
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


    exception NotVAR (* Binding term in a quantified formula is not a variable *)
    exception NotWFT (* term is not well-formed *)
    exception NotWFP (* Predicate is not well-formed *)
    exception NotWFA (* Argument is not well-formed *)
    exception NotClosed (* a formula is not closed *)

    fun checkNotVarPred (pd: Pred) : bool =
      case pd of
        FF => true
      | ATOM(_,_) => true
      | NOT(x) => checkNotVarPred x
      | AND(x, y) => (checkNotVarPred x) andalso (checkNotVarPred y)
      | OR(x, y) => (checkNotVarPred x) andalso (checkNotVarPred y)
      | COND(x, y) => (checkNotVarPred x) andalso (checkNotVarPred y)
      | BIC(x, y) => (checkNotVarPred x) andalso (checkNotVarPred y)
      | ITE(x, y, z) => (checkNotVarPred x) andalso (checkNotVarPred y) andalso (checkNotVarPred z)
      | ALL(VAR(x), p) => true
      | ALL(_,_) => raise NotVAR
      | EX(VAR(x), p) => true
      | EX(_,_) => raise NotVAR

    fun checkNotVar (arg: Argument) : bool =
      let
        val HENCE(l, p) = arg
        val predList = l @ [p]
      in
        foldl boolAnd true (map checkNotVarPred predList)
      end

    fun mktableau (l: Pred list, p: Pred) = () (* outputs file "tableau.dot" in dot format *)
  end
