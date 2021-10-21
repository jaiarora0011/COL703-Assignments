CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
CM.make "sources.cm";

use "proptableau.sml";
use "grammar.grm.sig";
use "lexer.lex.sml";
use "grammar.grm.sml";
use "flaslParser.sml";

open Flasl;
open Propositions;

Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;

val l = CommandLine.arguments()

val inp = if length l > 0 then hd l else "arg-inp.flasl";
val out = if length l > 1 then hd (tl l) else "arg.out";

val parsedAST = parse (makeLex inp);

val rewrite = rewriteITE parsedAST;

val set = convertArgumentToFormulaSet rewrite;

val resOut = argumentRepr rewrite;

val str = ast2flasl rewrite;

OS.Process.exit(OS.Process.success): unit;
