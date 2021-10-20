exception ScanError
exception Atom_exception

structure Tokens = Tokens


type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue,pos) token

val pos = ref 1  (* Tracks the line number *)
val col = ref 1  (* Tracks the column number *)
val eof = fn () => Tokens.EOF(!pos, !pos)
val soFar = ref "" (* Lexer accumulates the words inside an atomic proposition *)

fun print_error (err: string, p: int, c: int, text: string) = print (err ^ " at position = " ^ (Int.toString p) ^ ", column = " ^ (Int.toString c) ^ ": " ^ text)

%%

%s s1;
printable=[^\(\)\.\"\ \t\n];
whitespace=[\ \t];
%header (functor FlaslLexFun(structure Tokens : Flasl_TOKENS));


%%

<s1>\n                        => (pos := (!pos) + 1; col := 1; soFar := !soFar ^ " "; lex());
\n                            => (pos := (!pos) + 1; col := 1; lex());
<s1>({whitespace}*)"\""       => (col := !col + (String.size yytext); YYBEGIN INITIAL; Tokens.APROP(!soFar, !pos, !col));
<s1>{whitespace}+             => (col := !col + (String.size yytext); soFar := !soFar ^ " "; lex());
{whitespace}+                 => (col := !col + (String.size yytext); lex());
<INITIAL>"\""({whitespace})*  => (col := !col + (String.size yytext); YYBEGIN s1; soFar := ""; lex());
<s1>"("                       => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"."                       => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>")"                       => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"NOT"                     => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"AND"                     => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"OR"                      => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"IF"                      => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"THEN"                    => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"ELSE"                    => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"IFF"                     => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
<s1>"THEREFORE"               => (print_error("Invalid string in ATOM", !pos, !col, yytext); raise Atom_exception);
"("                           => (col := !col + (String.size yytext); Tokens.LPAREN(!pos, !col));
"."                           => (col := !col + (String.size yytext); Tokens.PERIOD(!pos, !col));
")"                           => (col := !col + (String.size yytext); Tokens.RPAREN(!pos, !col));
"NOT"                         => (col := !col + (String.size yytext); Tokens.NOT(!pos, !col));
"AND"                         => (col := !col + (String.size yytext); Tokens.AND(!pos, !col));
"OR"                          => (col := !col + (String.size yytext); Tokens.OR(!pos, !col));
"IF"                          => (col := !col + (String.size yytext); Tokens.IF(!pos, !col));
"THEN"                        => (col := !col + (String.size yytext); Tokens.THEN(!pos, !col));
"ELSE"                        => (col := !col + (String.size yytext); Tokens.ELSE(!pos, !col));
"IFF"                         => (col := !col + (String.size yytext); Tokens.IFF(!pos, !col));
"THEREFORE"                   => (col := !col + (String.size yytext); Tokens.THEREFORE(!pos, !col));
<s1>{printable}+              => (col := !col + (String.size yytext); soFar := !soFar ^ yytext; lex());
{printable}+                  => (print_error("ATOM found outside quotes", !pos, !col, yytext); raise Atom_exception);
.                             => (print_error("Unindetified Char", !pos, !col, yytext); raise ScanError);
