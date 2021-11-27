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
