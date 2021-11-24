CM.make("$/basis.cm");
CM.make("$/hash-cons-lib.cm");
CM.make("$/smlnj-lib.cm");
use "foltableau.sml";
open MyFOL;
Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;

val l = CommandLine.arguments()

val inp = if length l > 0 then hd l else "testcases/arg.sml";
val out = if length l > 0 then hd (tl l) else "out";

use inp;

OS.Process.exit(OS.Process.success): unit;
