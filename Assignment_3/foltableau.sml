use "datatypes.sml";
(*open HashConsMap;*)

structure MyFOL =
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

    type variable = string
    type symbol = string
    type substitution = (variable * term) list
    type signature_ = (symbol * int) list

    exception NotUnifiable
    exception SymbolNotFound

    fun convertArgToPred (arg: Argument) =
      let
        val HENCE(l, p) = arg
      in
        foldl AND (NOT p) l
      end

    fun getArity (s: signature_) (sym: symbol) : int =
      case s of
        [] => ~1
      | (x, n) :: xs => if (x = sym) then n else (getArity xs sym)

    fun termVars (t: term) : variable list =
      case t of
        VAR(v) => [v]
      | FUN(f, l) => foldl listUnion [] (map termVars l)
      | CONST(_) => []

    fun predVars (pd: Pred) : variable list =
      case pd of
        FF => []
      | ATOM(a, l) => foldl listUnion [] (map termVars l)
      | NOT(x) => predVars x
      | AND(x, y) => foldl listUnion [] (map predVars [x, y])
      | OR(x, y) => foldl listUnion [] (map predVars [x, y])
      | COND(x, y) => foldl listUnion [] (map predVars [x, y])
      | BIC(x, y) => foldl listUnion [] (map predVars [x, y])
      | ITE(x, y, z) => foldl listUnion [] (map predVars [x, y, z])
      | ALL(VAR(x), p) => listRemove (predVars p) x
      | EX(VAR(x), p) => listRemove (predVars p) x

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
        foldl boolAnd true (List.map checkNotVarPred predList)
      end

    fun checkNotWfpPred (pd: Pred, s: signature_) =
      case pd of
        FF => s
      | ATOM(a, l) => (let
                        val arity = getArity s a
                      in
                        if arity = ~1 then (a, length l) :: s else if arity = length l then s else raise NotWFP
                      end)
      | NOT(x) => checkNotWfpPred (x, s)
      | AND(x, y) => foldl checkNotWfpPred s [x, y]
      | OR(x, y) => foldl checkNotWfpPred s [x, y]
      | COND(x, y) => foldl checkNotWfpPred s [x, y]
      | BIC(x, y) => foldl checkNotWfpPred s [x, y]
      | ITE(x, y, z) => foldl checkNotWfpPred s [x, y, z]
      | ALL(x, p) => checkNotWfpPred (p, s)
      | EX(x, p) => checkNotWfpPred (p, s)

    fun checkNotWfp (arg: Argument) =
      let
        val pd = convertArgToPred arg
        val s = checkNotWfpPred (pd, [])
      in
        true
      end

    fun checkNotWftTerm (t: term, s: signature_) =
      case t of
        VAR(v) => s
      | FUN(f, l) => (let
                        val arity = getArity s f
                        val s_ = if arity = ~1 then (f, length l) :: s else if arity = length l then s else raise NotWFT
                      in
                        foldl checkNotWftTerm s_ l
                      end)
      | CONST(_) => s

    fun checkNotWftPred (pd: Pred, s: signature_) =
      case pd of
        FF => s
      | ATOM(a, l) => foldl checkNotWftTerm s l
      | NOT(x) => checkNotWftPred (x, s)
      | AND(x, y) => foldl checkNotWftPred s [x, y]
      | OR(x, y) => foldl checkNotWftPred s [x, y]
      | COND(x, y) => foldl checkNotWftPred s [x, y]
      | BIC(x, y) => foldl checkNotWftPred s [x, y]
      | ITE(x, y, z) => foldl checkNotWftPred s [x, y, z]
      | ALL(x, p) => checkNotWftPred (p, s)
      | EX(x, p) => checkNotWftPred (p, s)

    fun checkNotWft (arg: Argument) =
      let
        val pd = convertArgToPred arg
        val s = checkNotWftPred (pd, [])
      in
        true
      end

    fun checkNotClosed (arg: Argument) =
      let
        val HENCE(l, p) = arg
        val predList = l @ [p]
        val b = List.all (fn ll => ll = []) (map predVars l)
      in
        if b then b else raise NotClosed
      end

    fun argErrorChecks (arg: Argument) = (checkNotVar arg) andalso (checkNotWfp arg) andalso (checkNotWft arg) andalso (checkNotClosed arg)



    fun getSubstMapping (s: substitution) (var: variable) : term =
      case s of
        [] => VAR(var)
      | (v, t) :: xs => if (var = v) then t else (getSubstMapping xs var)

    fun removeSubstMapping (s: substitution) (elem: variable) : substitution = List.filter (fn (v, t) => v <> elem) s

    fun subst (s: substitution) (t: term) : term =
      case t of
        VAR(v) => getSubstMapping s v
      | FUN(f, l) => FUN(f, map (subst s) l)
      | CONST(_) => t

    fun differenceHelper (s1: substitution) (domS2: variable list) : substitution =
      case s1 of
        [] => []
      | (v, t) :: xs => if (checkMembership domS2 v) then (differenceHelper xs domS2) else (v, t) :: (differenceHelper xs domS2)

    fun difference (s1: substitution) (s2: substitution) : substitution =
      let
        val (domS2, ranS2) = splitList s2
      in
        differenceHelper s1 domS2
      end

    fun reduce (s:substitution) : substitution =
      case s of
        [] => []
      | (var, VAR(v)) :: xs => if(var = v) then (reduce xs) else (var, VAR(v)) :: (reduce xs)
      | x :: xs => x :: (reduce xs)

    fun composeHelper (s1:substitution) (s2:substitution) : substitution =
      case s1 of
        [] => []
      | (v, t) :: xs => (v, (subst s2 t)) :: (composeHelper xs s2)

    (* Returns the composition of s1 and s2, with s1 applied first *)
    fun compose (s2:substitution) (s1:substitution) : substitution = reduce ((composeHelper s1 s2) @ (difference s2 s1))

    fun mguHelper (l1: term list) (l2: term list) (mgu_acc: substitution) : substitution =
      case (l1, l2) of
        ([], []) => mgu_acc
      | (x :: xs, y :: ys) => (let
                              val s = mgu x y
                            in
                              mguHelper (map (subst s) xs) (map (subst s) ys) (compose s mgu_acc)
                            end)
      | (_,_) => raise NotUnifiable
    (*
      Returns the unifier of 2 terms. Patterm matching is done on both the terms.
    *)
    and mgu (t1: term) (t2: term) : substitution =
      case (t1, t2) of
        (VAR(v1), VAR(v2)) => if(v1 = v2) then [] else [(v1, VAR(v2))]
      | (VAR(var), FUN(f, l)) => if (checkMembership (termVars t2) var) then raise NotUnifiable  (*OCCURS check*)
                                  else [(var, t2)]
      | (FUN(f, l), VAR(var)) => mgu t2 t1
      | (FUN(f1, l1), FUN(f2, l2)) => if (f1 <> f2) then raise NotUnifiable else mguHelper l1 l2 []
      | (VAR(v), CONST(_)) => [(v, t2)]
      | (CONST(_), VAR(v)) => [(v, t1)]
      | (CONST(c1), CONST(c2)) => if (c1 <> c2) then raise NotUnifiable else []
      | (_,_) => raise NotUnifiable

    fun mktableau (l: Pred list, p: Pred) = () (* outputs file "tableau.dot" in dot format *)
  end
