use "datatypes.sml";
open Propositions;

Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;

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

val l = CommandLine.arguments()

val inp = if length l > 0 then hd l else "arg.sml";
val out = if length l > 1 then hd (tl l) else "arg-out.flasl";

fun run (arg: Argument, outFile: string) =
  let
    val outstream = TextIO.openOut outFile
  in
    TextIO.output(outstream, ast2flasl(arg))
  end




