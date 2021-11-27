use "util.sml";
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
    exception FatalError

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
      | _ => raise NotVAR

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
        val b = List.all (fn ll => (List.length ll) = 0) (map predVars l)
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

    fun substPred (s: substitution) (pd: Pred) : Pred =
      case pd of
        FF => pd
      | ATOM(a, l) => ATOM(a, map (subst s) l)
      | NOT(x) => NOT(substPred s x)
      | AND(x, y) => AND(substPred s x, substPred s y)
      | OR(x, y) => OR(substPred s x, substPred s y)
      | COND(x, y) => COND(substPred s x, substPred s y)
      | BIC(x, y) => BIC(substPred s x, substPred s y)
      | ITE(x, y, z) => ITE(substPred s x, substPred s y, substPred s z)
      | ALL(VAR(x), p) => ALL(VAR(x), substPred (removeSubstMapping s x) p)
      | EX(VAR(x), p) => EX(VAR(x), substPred (removeSubstMapping s x) p)
      | _ => raise NotVAR

    fun differenceHelper (s1: substitution) (domS2: variable list) : substitution =
      case s1 of
        [] => []
      | (v, t) :: xs => if (checkMembership domS2 v) then (differenceHelper xs domS2) else (v, t) :: (differenceHelper xs domS2)

    fun difference (s1: substitution) (s2: substitution) : substitution =
      let
        val (domS2, ranS2) = ListPair.unzip s2
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

    fun mguPred (p1: Pred) (p2: Pred) : substitution =
      case (p1, p2) of
        (FF, FF) => []
      | (ATOM(a1, l1), ATOM(a2, l2)) => if (a1 <> a2) then raise NotUnifiable else mguHelper l1 l2 []
      | (NOT(ATOM(a1, l1)), NOT(ATOM(a2, l2))) => if (a1 <> a2) then raise NotUnifiable else mguHelper l1 l2 []
      | (_, _) => raise NotUnifiable

    fun rewriteITEpred(pd: Pred) =
      case pd of
        ITE(a, b, c) => AND(COND(rewriteITEpred a, rewriteITEpred b), COND(NOT(rewriteITEpred a), rewriteITEpred c))
      | NOT(a) => NOT(rewriteITEpred a)
      | AND(a, b) => AND(rewriteITEpred a, rewriteITEpred b)
      | OR(a, b) => OR(rewriteITEpred a, rewriteITEpred b)
      | COND(a, b) => COND(rewriteITEpred a, rewriteITEpred b)
      | BIC(a, b) => BIC(rewriteITEpred a, rewriteITEpred b)
      | ALL(x, p) => ALL(x, rewriteITEpred p)
      | EX(x, p) => EX(x, rewriteITEpred p)
      | _ => pd

    fun rewriteITE(arg: Argument) =
      let
        val HENCE(l, p) = arg
      in
        HENCE(map rewriteITEpred l, rewriteITEpred p)
    end

    fun getPredRank (pd : Pred) : int =
      case pd of
        FF => 0
      | NOT(NOT(x)) => 1
      | AND(x, y) => 1
      | NOT(OR(x, y)) => 1
      | NOT(COND(x, y)) => 1
      | OR(x, y) => 2
      | NOT(AND(x, y)) => 2
      | COND(x, y) => 2
      | BIC(x, y) => 2
      | NOT(BIC(x, y)) => 2
      | EX(x, p) => 3
      | NOT(ALL(x, p)) => 3
      | ALL(x, p) => 4
      | NOT(EX(x, p)) => 4
      | ATOM(a, l) => 5
      | NOT(ATOM(a, l)) => 5
      | _ => raise FatalError

    fun sortPreds (pl: Pred list) = ListMergeSort.sort (fn (x, y) => (getPredRank x) > (getPredRank y)) pl

    type EdgeList = (int * int) list
    type VertexList = (int * Pred) list
    type FormulaSet = Pred list

    fun applyElongation (pd: Pred) =
      case pd of
        NOT(NOT(x)) => [x]
      | AND(x, y) => [x, y]
      | NOT(OR(x, y))=> [NOT(x), NOT(y)]
      | NOT(COND(x, y)) => [x, NOT(y)]
      | _ => raise FatalError


    fun applyBranching (pd: Pred) =
      case pd of
        NOT(AND(x, y)) => (NOT(x), NOT(y))
      | OR(x, y) => (x, y)
      | COND(x, y) => (NOT(x), y)
      | BIC(x, y) => (AND(x, y), AND(NOT(x), NOT(y)))
      | NOT(BIC(x, y)) => (AND(x, NOT(y)), AND(NOT(x), y))
      | _ => raise FatalError

    fun applyExistential (pd: Pred) (c: term) =
      case pd of
        EX(VAR(x), p) => substPred [(x, c)] p
      | NOT(ALL(VAR(x), p)) => substPred [(x, c)] (NOT(p))
      | _ => raise FatalError

    fun applyUniversal (pd: Pred) (x1: term) =
      case pd of
        ALL(VAR(x), p) => substPred [(x, x1)] p
      | NOT(EX(VAR(x), p)) => substPred [(x, x1)] (NOT(p))
      | _ => raise FatalError

    fun checkUnification (l: FormulaSet) (pd: Pred) =
      case l of
        [] => NONE
      | x :: xs => SOME((x, (mguPred x pd))) handle NotUnifiable => (checkUnification xs pd)

    fun checkComplimentaryPair (fs: FormulaSet) =
      case fs of
        [] => NONE
      | [a] => NONE
      | NOT(ATOM(a, l)) :: xs => (case checkUnification xs (ATOM(a, l)) of
                                  SOME((x, s)) => SOME((NOT(ATOM(a, l)), x, s))
                                | NONE => checkComplimentaryPair xs)
      | ATOM(a, l) :: xs => (case checkUnification xs (NOT(ATOM(a, l))) of
                                  SOME((x, s)) => SOME((ATOM(a, l), x, s))
                                | NONE => checkComplimentaryPair xs)
      | _ :: xs => checkComplimentaryPair xs

    fun checkComplimentaryPair_ (fs: FormulaSet) =
      case fs of
        [] => NONE
      | [a] => NONE
      | NOT(p) :: xs => if checkMembership xs p then SOME((NOT(p), p)) else checkComplimentaryPair_ xs
      | p :: xs => if checkMembership xs (NOT(p)) then SOME((p, NOT(p))) else checkComplimentaryPair_ xs

    fun getVertexID (vl_: VertexList) (pd: Pred) : int =
      case vl_ of
        [] => raise FatalError
      | (u, x) :: xs => if (x = pd) then u else (getVertexID xs pd)

    val vl : VertexList ref = ref []
    val dir : EdgeList ref = ref []
    val ancestor : EdgeList ref = ref []
    val undir : EdgeList ref = ref []
    val nextID : int ref = ref 0
    val acc_sub : substitution ref = ref []

    fun replacePred (vl_: VertexList) (pd: Pred) (id: int) =
      case vl_ of
        [] => []
      | (u, x) :: xs => if u = id then (u, pd) :: xs else (u, x) :: (replacePred xs pd id)

    fun listToString (l: string list) =
      case l of
        [] => ""
      | x :: nil => x
      | x :: xs => x ^ "," ^ (listToString xs)

    fun termToString (t: term) =
      case t of
        VAR(v) => v
      | FUN(f, l) => f ^ "(" ^ (listToString (map termToString l)) ^ ")"
      | CONST(c) => c

    fun predToString (pd: Pred) : string =
      case pd of
        FF => " \\bot "
      | ATOM(a, l) => a ^ "(" ^ (listToString (map termToString l)) ^ ")"
      | NOT(x) => "\\neg " ^ (predToString x)
      | AND(x, y) => "(" ^ (predToString x) ^ "\\wedge " ^ (predToString y) ^ ")"
      | OR(x, y) => "(" ^ (predToString x) ^ "\\vee " ^ (predToString y) ^ ")"
      | COND(x, y) => "(" ^ (predToString x) ^ "\\rightarrow " ^ (predToString y) ^ ")"
      | BIC(x, y) => "(" ^ (predToString x) ^ "\\leftrightarrow " ^ (predToString y) ^ ")"
      | ALL(VAR(x), p) => "\\forall " ^ x ^ "[" ^ (predToString p) ^ "]"
      | EX(VAR(x), p) => "\\exists " ^ x ^ "[" ^ (predToString p) ^ "]"
      | _ => raise FatalError

    fun replacePredsInVL (vl_: VertexList) (s: substitution) =
      case vl_ of
        [] => []
      | (u, x) :: xs => (u, (substPred s x)) :: (replacePredsInVL xs s)

    fun tableauMethod (*(vl: VertexList)*) (acf: FormulaSet) (*(dir: EdgeList) (ancestor: EdgeList) (undir: EdgeList)*)
        (currentLeafID: int) (*(nextID: int)*) : unit =
      let
        val acf_ = (map (substPred (!acc_sub)) acf)
      in
        case acf_ of
        [] => ()
      | FF :: xs => raise FatalError
      | x :: xs => (case (checkComplimentaryPair acf_) of
                      SOME((a, b, s)) => (let
                                        val u = getVertexID (!vl) a
                                        val v = getVertexID (!vl) b
                                        val _ = (vl := (replacePredsInVL (!vl) s))
                                        val _ = (acc_sub := s)
                                        val _ = (vl := (!nextID, FF) :: (!vl))
                                        val _ = (dir := (currentLeafID, !nextID) :: (!dir))
                                        val _ = (undir := (u, v) :: (!undir))
                                        val _ = (nextID := !nextID + 1)
                                      in
                                        ()
                                      end)
                    | NONE => if (getPredRank x) = 1 then (let
                                                            val u = getVertexID (!vl) x
                                                            val l = applyElongation x
                                                            val _ = (vl := (!nextID, (hd l))::(!vl))
                                                            val _ = (ancestor := (u, !nextID)::(!ancestor))
                                                            val _ = (dir := (currentLeafID, !nextID)::(!dir))
                                                            val _ = if (length l) = 2 then ( (vl := (!nextID+1, (hd (tl l)))::(!vl));
                                                                                            ((ancestor := (u, !nextID+1)::(!ancestor)));
                                                                                            (dir := (!nextID, !nextID+1)::(!dir))
                                                                                          ) else ()
                                                            val new = if (length l) = 1 then !nextID else !nextID+1
                                                            val _ = (nextID := !nextID + (length l))
                                                          in
                                                            tableauMethod (sortPreds (l@xs)) new
                                                          end)
                              else if (getPredRank x) = 2 then (let
                                                                  val u = getVertexID (!vl) x
                                                                  val (left, right) = applyBranching x
                                                                  val _ = (vl := (!nextID, left)::(!vl))
                                                                  val _ = (vl := (!nextID+1, right)::(!vl))
                                                                  val _ = (ancestor := (u, !nextID)::(!ancestor))
                                                                  val _ = (ancestor := (u, !nextID+1)::(!ancestor))
                                                                  val _ = (dir := (currentLeafID, !nextID)::(!dir))
                                                                  val _ = (dir := (currentLeafID, !nextID+1)::(!dir))
                                                                  val old = !nextID
                                                                  val _ = (nextID := !nextID + 2)
                                                                in
                                                                  tableauMethod (sortPreds (left::xs)) old;
                                                                  tableauMethod (sortPreds (right::xs)) (old+1)
                                                                end)
                              else if (getPredRank x) = 3 then (let
                                                                  val u = getVertexID (!vl) x
                                                                  val freshConst = CONST(Int.toString (!nextID))
                                                                  val inst = applyExistential x freshConst
                                                                  val _ = (vl := (!nextID, inst)::(!vl))
                                                                  val _ = (ancestor := (u, !nextID)::(!ancestor))
                                                                  val _ = (dir := (currentLeafID, !nextID)::(!dir))
                                                                  val old = !nextID
                                                                  val _ = (nextID := !nextID + 1)
                                                                in
                                                                  tableauMethod (sortPreds (inst::xs)) old
                                                                end)
                              else if (getPredRank x) = 4 then (let
                                                                  val u = getVertexID (!vl) x
                                                                  val newVar = VAR(Int.toString (!nextID))
                                                                  val inst = applyUniversal x newVar
                                                                  val _ = (vl := (!nextID, inst)::(!vl))
                                                                  val _ = (ancestor := (u, !nextID)::(!ancestor))
                                                                  val _ = (dir := (currentLeafID, !nextID)::(!dir))
                                                                  val old = !nextID
                                                                  val _ = (nextID := !nextID + 1)
                                                                  (*val _ = print ((listToString (map predToString acf_)) ^ "\n")*)
                                                                in
                                                                  tableauMethod (sortPreds ([inst] @ xs @ [x])) old
                                                                end)
                              else raise FatalError
                   )
      end


    fun assignIDs (acf: FormulaSet) (id: int) : unit =
      case acf of
        [] => ()
      | x :: xs => let
                    val _ = (vl := (id, x)::(!vl))
                  in
                    assignIDs xs (id-1)
                  end

    fun assignDir (id: int) : unit =
      case id of
        0 => ()
      | 1 => (dir := (0, 1) :: (!dir))
      | n => let
              val _ = (dir := (n-1, n) :: (!dir))
            in
              assignDir (id-1)
            end



    fun mktableau (l: Pred list, p: Pred, out: string) =
      let
        val acf = sortPreds (map rewriteITEpred (l @ [NOT(p)]))
        val _ = assignIDs acf ((length acf)-1)
        val _ = assignDir ((length acf)-1)
        val _ = (nextID := (length acf))
        val _ = tableauMethod acf ((length acf)-1)
        val (ids, preds) = ListPair.unzip (!vl)
        val predStrings = map (fn s => "$" ^ s ^ "$") (map predToString preds)
        val vls = ListPair.zip (ids, predStrings)
        val _ = (ancestor := listDifference (!ancestor) (!dir))
        val dot = dotFormatRepr vls (!dir) (!ancestor) (!undir)
      in
        output out dot
      end
  end
