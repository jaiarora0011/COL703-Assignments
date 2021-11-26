CM.make("$/basis.cm");
CM.make("$/hash-cons-lib.cm");
CM.make("$/smlnj-lib.cm");
use "foltableau.sml";
open MyFOL;
Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;

val l = CommandLine.arguments()

val inp = if length l > 0 then hd l else "testcases/arg.sml";
val out = if length l > 1 then hd (tl l) else "output/arg.dot";

use inp;
val res_ = Bool.toString (argErrorChecks arg)
  handle NotVAR => "NotVAR"
       | NotWFT => "NotWFT"
       | NotWFP => "NotWFP"
       | NotWFA => "NotWFA"
       | NotClosed => "NotClosed"

val vl = [(0, "$a$"), (1, "$b$"), (2, "$c$"), (3, "$d$"), (4, "$e$")]
val dir = [(0, 1), (1, 2), (2, 3), (3, 4)]
val ancestor = [(0, 2), (1, 3)]
val undir = [(2, 3)]

val res = dotFormatRepr vl dir ancestor undir;

val _ = output out res;

OS.Process.exit(OS.Process.success): unit;
