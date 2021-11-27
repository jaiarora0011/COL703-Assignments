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

fun output (outfile: string) (res: string) =
  let
    val outstream = TextIO.openOut outfile
  in
    TextIO.output(outstream, res)
  end

fun boolAnd (b1: bool, b2: bool) = b1 andalso b2

fun checkMembership l elem = List.exists (fn x => x = elem) l

fun listRemove l elem = List.filter (fn x => x <> elem) l

fun listUnion (l1, l2) =
  case l1 of
    [] => l2
  | x :: xs => if (checkMembership l2 x) then listUnion (xs, l2) else x :: (listUnion (xs, l2))

fun listDifference l1 l2 =
  case l1 of
    [] => []
  | x :: xs => if (checkMembership l2 x) then (listDifference xs l2) else x :: (listDifference xs l2)

fun removeDuplicates l =
  case l of
    [] => []
  | [x] => l
  | x :: xs => if checkMembership xs x then xs else x :: (removeDuplicates xs)

fun edgeListRepr (el: (int * int) list) : string =
  case el of
    [] => ""
  | (u, v) :: xs => (Int.toString u) ^ " -> " ^ (Int.toString v) ^ ";\n" ^ (edgeListRepr xs)

fun vertexListRepr (vl: (int * string) list) =
  case vl of
    [] => ""
  | (u, s) :: xs => (Int.toString u) ^ " [texlbl=\"\\underline{" ^ (Int.toString u) ^ ". " ^ s ^ " }\"];\n" ^ (vertexListRepr xs)


fun dotFormatRepr vl dir ancestor undir =
  "digraph{\nnodesep = 0.5;\nranksep = 0.35;\nnode [shape=plaintext];\n" ^ (vertexListRepr vl)
    ^ "subgraph dir\n{\n" ^ (edgeListRepr dir) ^ "}\nsubgraph ancestor\n{\nedge [dir=back, color=blue, style=dashed]\n"
      ^ (edgeListRepr ancestor) ^ "}\nsubgraph undir\n{\nedge [dir=none, color=red]\n" ^ (edgeListRepr undir) ^ "}\n}\n"
