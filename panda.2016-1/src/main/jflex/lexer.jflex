/* -*-Mode: java-*- */

package parse;

import java.io.Reader;

import errormsg.Loc;

import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;

%%

%public
%final
%class Lexer
%implements parse.Terminals
%line
%column
%cupsym Parse.Tokens
%cup

%eofval{
    return tok(EOF);
%eofval} 

%{
    private String unit;
    private ComplexSymbolFactory symbolFactory;
    private int commentLevel;
    private StringBuilder builder = new StringBuilder();
    private Location strLeft;

    public Lexer(String unit, Reader input) {
        this(input);
        this.unit = unit;
        this.symbolFactory = new ComplexSymbolFactory();
    }
    
    public String getUnit() {
        return unit;
    }

    private Location locLeft() {
      return new Location(yyline+1, yycolumn+1);
    }

    private Location locRight() {
      return new Location(yyline+1, yycolumn+1+yylength());
    }

    private java_cup.runtime.Symbol tok(int terminalcode) {
      return tok(terminalcode, null);
    }

    private java_cup.runtime.Symbol tok(int terminalcode, Object val) {
      return tok(terminalcode, val, locLeft(), locRight());
    }

    private java_cup.runtime.Symbol tok(int terminalcode, Object val, Location left, Location right) {
      return symbolFactory.newSymbol(
        terminalNames[terminalcode],
        terminalcode,
        left, right,
        val);
    }

    private void error(String message) {
        errormsg.Error.error(
          new Loc(new Location(unit, yyline+1, yycolumn+1),
                  new Location(unit, yyline+1, yycolumn+1+yylength())),
          "lexical error: " + message);
    }
%}

%state COMMENT
%state STR

%%

<YYINITIAL>{
[ \t\f\n\r]+          { /* skip */ }

/* palavras reservadas */
:                                           { return tok(COLON); }
=                                           { return tok(EQ); }
"("                                         { return tok(LPAREN); }
")"                                         { return tok(RPAREN); }
"["                                         { return tok(LBRACK); }
"]"                                         { return tok(RBRACK); }
"{"                                         { return tok(LBRACE); }
"}"                                         { return tok(RBRACE); }
,                                           { return tok(COMMA); }
;                                           { return tok(SEMICOLON); }
:=                                          { return tok(ASSIGN); }

"+"                                         { return tok(PLUS); }
"-"                                         { return tok(MINUS); }
"*"                                         { return tok(TIMES); }
"/"                                         { return tok(DIV); }
"%"                                         { return tok(MOD); }
"^"                                         { return tok(POW); }

"<>"                                        { return tok(NE); }
"<"                                         { return tok(LT); }
"<="                                        { return tok(LE); }
">"                                         { return tok(GT); }
">="                                        { return tok(GE); }
"&&"				                        { return tok(AND); }
"||"				                        { return tok(OR); }
"."				                            { return tok(DOT); }
@                                           { return tok(AT); }

nil                                         {return tok(NIL);}
if                                          {return tok(IF);}
then                                        {return tok(THEN);}
else                                        {return tok(ELSE);}
while                                       {return tok(WHILE);}
do                                          {return tok(DO);}
break                                       {return tok(BREAK);}
let                                         {return tok(LET);}
in                                          {return tok(IN);}
var                                         {return tok(VAR);}
function                                    {return tok(FUNCTION);}
type                                        {return tok(TYPE);}

/* booleanos */
true | false            { return tok(LITBOOL, new Boolean(yytext())); }

/* inteiros */
[+-]? [0-9]+            { return tok(LITINT, new Long(yytext())); }

/* reais */
([+-]?  [0-9]+ "."? [0-9]* | [0-9]* "." [0-9]+) ([eE] [+-]? [0-9]+)? { return tok(LITREAL, new Double(yytext())); }

/* strings */
\"                                          { yybegin(STR); builder.setLength(0); strLeft = locLeft(); }
//\"(\\.|[^\\\"])*\"                        { return tok(LITSTRING, yytext().substring(1,yylength()-1) ); }

/* caracteres */
\'[^\n\r\"\\]\'                             { return tok(LITCHAR, new Character(yytext().charAt(1))); }
\'"\\b"\'                                   { return tok(LITCHAR, '\b');}
\'"\\t"\'                                   { return tok(LITCHAR, '\t');}
\'"\\n"\'                                   { return tok(LITCHAR, '\n');}
\'"\\f"\'                                   { return tok(LITCHAR, '\f');}
\'"\\r"\'                                   { return tok(LITCHAR, '\r');}
\'"\\\""\'                                  { return tok(LITCHAR, '\"');}
\'"\\'"\'                                   { return tok(LITCHAR, '\'');}
\'"\\\\"\'                                  { return tok(LITCHAR, '\\');}
\'"\\^@"\'                                  { return tok(LITCHAR, (char) 0); }
\'"\\^I'"                                   { return tok(LITCHAR, (char) 9);}
\'"\\^['"                                   { return tok(LITCHAR, (char) 27);}
\'"\\^\\'"                                  { return tok(LITCHAR, (char) 28);}
\'"\\^]'"                                   { return tok(LITCHAR, (char) 29);}
\'"\\^^'"                                   { return tok(LITCHAR, (char) 30);}
\'"\\^_'"                                   { return tok(LITCHAR, (char) 31);}
\'"\\^?'"                                   { return tok(LITCHAR, (char) 127);}
\'\\[0-3]?[0-7]?[0-7]\'                     { return tok(LITCHAR, new Character((char) Integer.parseInt(yytext().substring(2,yylength()-1)))); }
\'\\[\^].\'                                 {  error("invalid control character in char literal");}
\'\\.\'                                     {  error("invalid escape sequence in char literal");}
\'.+\'                                      {  error("invalid char literal");}
\'                                          {  error("unclosed char literal"); }

/* comentários */
"#" .*                { /* skip */ }
"{#"                  { commentLevel++; yybegin(COMMENT);}

/* ids */
[a-zA-Z0-9]+ [(\_) | [0-9][a-zA-Z]]*        {return tok(ID, symbol.Symbol.symbol(yytext()));}
"_"                                         {error("invalid character: [" + yytext() + "]");}


}

<COMMENT> {
   "{#"     {   commentLevel++; }
   "#}"     {
                commentLevel--;
                if (commentLevel <= 0) {
                    yybegin(YYINITIAL);
                }
            }
    [^{#}\R]+    { /* skip */ }
    "#"          { /* skip */ }
    \R           { /* skip */ }
    <<EOF>>   { error("unclosed comment") ;}
}


<STR>{
    \"                                      { yybegin(YYINITIAL); return tok(LITSTRING, builder.toString(),strLeft,locRight()); }
    [^\r\n\"\\]+                            { builder.append( yytext() ); }
    "\\b"                                   { builder.append( '\b' ); }
    "\\t"                                   { builder.append( '\t' ); }
    "\\n"                                   { builder.append( '\n' ); }
    "\\f"                                   { builder.append( '\f' ); }
    "\\r"                                   { builder.append( '\r' ); }
    "\\\""                                  { builder.append( '\"' ); }
    "\\'"                                   { builder.append( '\'' ); }
    "\\\\"                                  { builder.append( '\\' ); }
    "\\^@"                                  { builder.append( (char) 0); }
    "\\^I"                                  { builder.append( (char) 9);}
    "\\^["                                  { builder.append( (char) 27);}
    "\\^\\"                                 { builder.append( (char) 28);}
    "\\^]"                                  { builder.append( (char) 29);}
    "\\^^"                                  { builder.append( (char) 30);}
    "\\^_"                                  { builder.append( (char) 31);}
    "\\^?"                                  { builder.append( (char) 127);}
    \\[0-3]?[0-7]?[0-7]                     { builder.append( (char) Integer.parseInt(yytext().substring(1)) ); }
    \\[\^].                                 { error("invalid control character in string literal"); }
    \\.                                     { error("invalid escape sequence in string literal"); }
    \r|\n|\r\n                              { error("invalid newline in string literal"); }
}

.           { error("invalid character: [" + yytext() + "]"); }
