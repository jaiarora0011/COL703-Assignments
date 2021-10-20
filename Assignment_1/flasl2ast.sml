CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
CM.make "sources.cm";

use "datatypes.sml";
use "grammar.grm.sig";
use "lexer.lex.sml";
use "grammar.grm.sml";
use "flaslParser.sml";

Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;

val l = CommandLine.arguments()

val inp = if length l > 0 then hd l else "arg-inp.flasl";
val out = if length l > 1 then hd (tl l) else "arg.sml";

val resOut = Propositions.ArgumentRepr (parse(makeLex inp));

val boilerplate = "use \"datatypes.sml\";\n" ^ "open Propositions;\n";

output(out, (*boilerplate ^ *)"val arg = " ^ resOut);


OS.Process.exit(OS.Process.success): unit;
