use "proptableau.sml";

Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;

val l = CommandLine.arguments()

val inp = if length l > 0 then hd l else "testcases/govt1.flasl";
val out = if length l > 1 then hd (tl l) else "out";

val resOut = "Hello\n"

val _ = output out resOut;

OS.Process.exit(OS.Process.success): unit;
