(*fun readToken () : unit =
  let
    val nextToken = lexer()
  in
    if nextToken = EOF then
      ()
    else
      (print (toString nextToken ^ " "); readToken())
  end*)

structure FlaslLrVals =
         FlaslLrValsFun(structure Token = LrParser.Token)

structure FlaslLex =
         FlaslLexFun(structure Tokens =
                            FlaslLrVals.Tokens)

structure FlaslParser =
           Join(structure Lex = FlaslLex
                structure ParserData = FlaslLrVals.ParserData
                structure LrParser = LrParser)

fun makeLex (inp: string) =
  let
    val instream = TextIO.openIn inp
  in
    FlaslParser.makeLexer (fn _ => TextIO.input instream)
  end

val invoke = fn lexstream =>
  let val print_error = fn (s,pos,col) =>
    (print ("At position = " ^ (Int.toString pos) ^ ", Column =  " ^ (Int.toString col) ^ ": " ^  s ^ "\n"))
  in FlaslParser.parse(0, lexstream, print_error, ())
  end

fun parse (lexer) =
  let val dummyEOF = FlaslLrVals.Tokens.EOF(0, 0)
    val (result,lexer) = invoke lexer
      val (nextToken,lexer) = FlaslParser.Stream.get lexer
  in
    if FlaslParser.sameToken(nextToken,dummyEOF) then result
    else raise FlaslParser.ParseError
  end

fun output (outfile: string, res: string) =
  let
    val outstream = TextIO.openOut outfile
  in
    TextIO.output(outstream, res)
  end
