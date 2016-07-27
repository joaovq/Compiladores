package Test;

import java_cup.runtime.Symbol;
import javaslang.collection.List;
import org.assertj.core.api.JUnitSoftAssertions;
import org.junit.Rule;
import org.junit.Test;
import parse.Lexer;
import parse.Terminals;
import parse.Tokens;

import java.io.IOException;
import java.io.StringReader;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertArrayEquals;

public class LexerTest {

  private String run(String input) throws IOException {
    Lexer lexer = new Lexer("unknown", new StringReader(input));
    Symbol token;
    StringBuilder builder = new StringBuilder();
    List<String> list = List.empty();
    do {
      token = lexer.next_token();
      builder.append(Tokens.dumpToken(token)).append('\n');
      list = list.append(Tokens.dumpToken(token));
    } while (token.sym != Terminals.EOF);
    return builder.toString();
    //return list;
  }

  private void trun(String input, String... output) throws IOException {
    StringBuilder builder = new StringBuilder();
    for (String x : output)
      builder.append(x).append('\n');
    softly.assertThat(run(input))
          .as("%s", input)
          .isEqualTo(builder.toString());
  }

  private void erun(String input, String message) throws IOException {
    softly.assertThatThrownBy(() -> run(input))
          .as("%s", input)
          .isInstanceOf(errormsg.Error.class)
          .hasToString(message);
  }

  @Rule
  public final JUnitSoftAssertions softly = new JUnitSoftAssertions();

  @Test
  public void lexerTest1() throws IOException {

    // whitespaces
    trun("    \t\n\n\n\t\r\n\r\n  ", "6:3-6:3 EOF");

    // comments
    trun("# a line comment\n"            , "2:1-2:1 EOF");
    trun("# a line comment"              , "1:17-1:17 EOF");
    trun("{# a block comment #}"         , "1:22-1:22 EOF");
    trun("{# a\nmultiline\ncomment #}"   , "3:11-3:11 EOF");
    trun("{# begin ### end #}"           , "1:20-1:20 EOF");
    trun("{# begin #### end #}"          , "1:21-1:21 EOF");
    trun("{# begin ####}"                , "1:15-1:15 EOF");
    trun("{# begin #####}"               , "1:16-1:16 EOF");
    trun("{# outer {# inner #} outer #}" , "1:30-1:30 EOF");
    erun("{# a {# ab {# abc #} ba"       , "1:24-1:24: lexical error: unclosed comment");

   // punctuation
   trun(":"  , "1:1-1:2 COLON"    , "1:2-1:2 EOF");
   trun("="  , "1:1-1:2 EQ"       , "1:2-1:2 EOF");
   trun("("  , "1:1-1:2 LPAREN"   , "1:2-1:2 EOF");
   trun(")"  , "1:1-1:2 RPAREN"   , "1:2-1:2 EOF");
   trun("["  , "1:1-1:2 LBRACK"   , "1:2-1:2 EOF");
   trun("]"  , "1:1-1:2 RBRACK"   , "1:2-1:2 EOF");
   trun("{"  , "1:1-1:2 LBRACE"   , "1:2-1:2 EOF");
   trun("}"  , "1:1-1:2 RBRACE"   , "1:2-1:2 EOF");
   trun(","  , "1:1-1:2 COMMA"    , "1:2-1:2 EOF");
   trun(";"  , "1:1-1:2 SEMICOLON", "1:2-1:2 EOF");
   trun(":=" , "1:1-1:3 ASSIGN"   , "1:3-1:3 EOF");

   // operators
   trun("+"  , "1:1-1:2 PLUS"  , "1:2-1:2 EOF");
   trun("-"  , "1:1-1:2 MINUS" , "1:2-1:2 EOF");
   trun("*"  , "1:1-1:2 TIMES" , "1:2-1:2 EOF");
   trun("/"  , "1:1-1:2 DIV"   , "1:2-1:2 EOF");
   trun("%"  , "1:1-1:2 MOD"   , "1:2-1:2 EOF");
   trun("^"  , "1:1-1:2 POW"   , "1:2-1:2 EOF");
   trun("<>" , "1:1-1:3 NE"    , "1:3-1:3 EOF");
   trun("<"  , "1:1-1:2 LT"    , "1:2-1:2 EOF");
   trun("<=" , "1:1-1:3 LE"    , "1:3-1:3 EOF");
   trun(">"  , "1:1-1:2 GT"    , "1:2-1:2 EOF");
   trun("&&" , "1:1-1:3 AND"   , "1:3-1:3 EOF");
   trun("||" , "1:1-1:3 OR"    , "1:3-1:3 EOF");
   trun("."  , "1:1-1:2 DOT"   , "1:2-1:2 EOF");
   trun("@"  , "1:1-1:2 AT"    , "1:2-1:2 EOF");

   // boolean literals
   trun("true"  , "1:1-1:5 LITBOOL(true)"  , "1:5-1:5 EOF");
   trun("false" , "1:1-1:6 LITBOOL(false)" , "1:6-1:6 EOF");

   // integer literals
   trun("26342"  , "1:1-1:6 LITINT(26342)" , "1:6-1:6 EOF");
   trun("0"      , "1:1-1:2 LITINT(0)"     , "1:2-1:2 EOF");
   trun("+75"    , "1:1-1:4 LITINT(75)"    , "1:4-1:4 EOF");
   trun("-75"    , "1:1-1:4 LITINT(-75)"   , "1:4-1:4 EOF");

   // real literals
   trun("12.345"   , "1:1-1:7 LITREAL(12.345)"    , "1:7-1:7 EOF");
   trun("1234."    , "1:1-1:6 LITREAL(1234.0)"    , "1:6-1:6 EOF");
   trun(".21"      , "1:1-1:4 LITREAL(0.21)"      , "1:4-1:4 EOF");
   trun("1.2e100"  , "1:1-1:8 LITREAL(1.2E100)"   , "1:8-1:8 EOF");
   trun("1.E+100"  , "1:1-1:8 LITREAL(1.0E100)"   , "1:8-1:8 EOF");
   trun(".55e-100" , "1:1-1:9 LITREAL(5.5E-101)"  , "1:9-1:9 EOF");
   trun("2106E2"   , "1:1-1:7 LITREAL(210600.0)"  , "1:7-1:7 EOF");
   trun("56e-234"  , "1:1-1:8 LITREAL(5.6E-233)"  , "1:8-1:8 EOF");
   trun("710E+18"  , "1:1-1:8 LITREAL(7.1E20)"    , "1:8-1:8 EOF");
   trun("+12.34"   , "1:1-1:7 LITREAL(12.34)"     , "1:7-1:7 EOF");
   trun("-12.34"   , "1:1-1:7 LITREAL(-12.34)"    , "1:7-1:7 EOF");
   trun("+1.23e10" , "1:1-1:9 LITREAL(1.23E10)"   , "1:9-1:9 EOF");
   trun("-1.2E-10" , "1:1-1:9 LITREAL(-1.2E-10)"  , "1:9-1:9 EOF");

   // char literals
   trun("'A'"     , "1:1-1:4 LITCHAR(A)"    , "1:4-1:4 EOF");
   trun("'b'"     , "1:1-1:4 LITCHAR(b)"    , "1:4-1:4 EOF");
   trun("'*'"     , "1:1-1:4 LITCHAR(*)"    , "1:4-1:4 EOF");
   trun("' '"     , "1:1-1:4 LITCHAR( )"    , "1:4-1:4 EOF");
   trun("'\t'"    , "1:1-1:4 LITCHAR(\t)"   , "1:4-1:4 EOF");
   trun("'\\b'"   , "1:1-1:5 LITCHAR(\b)"   , "1:5-1:5 EOF");
   trun("'\\t'"   , "1:1-1:5 LITCHAR(\t)"   , "1:5-1:5 EOF");
   trun("'\\n'"   , "1:1-1:5 LITCHAR(\n)"   , "1:5-1:5 EOF");
   trun("'\\r'"   , "1:1-1:5 LITCHAR(\r)"   , "1:5-1:5 EOF");
   trun("'\\f'"   , "1:1-1:5 LITCHAR(\f)"   , "1:5-1:5 EOF");
   trun("'\\\''"  , "1:1-1:5 LITCHAR(')"    , "1:5-1:5 EOF");
   trun("'\\^@'"  , "1:1-1:6 LITCHAR(\0)"   , "1:6-1:6 EOF");
   trun("'\\^I'"  , "1:1-1:6 LITCHAR(\11)"  , "1:6-1:6 EOF");
   trun("'\\^['"  , "1:1-1:6 LITCHAR(\33)"  , "1:6-1:6 EOF");
   trun("'\\^\\'" , "1:1-1:6 LITCHAR(\34)"  , "1:6-1:6 EOF");
   trun("'\\^]'"  , "1:1-1:6 LITCHAR(\35)"  , "1:6-1:6 EOF");
   trun("'\\^^'"  , "1:1-1:6 LITCHAR(\36)"  , "1:6-1:6 EOF");
   trun("'\\^_'"  , "1:1-1:6 LITCHAR(\37)"  , "1:6-1:6 EOF");
   trun("'\\^?'"  , "1:1-1:6 LITCHAR(\177)" , "1:6-1:6 EOF");
   erun("'\\^$'"  , "1:1-1:6: lexical error: invalid control character in char literal");
   trun("'\\065'" , "1:1-1:7 LITCHAR(A)"    , "1:7-1:7 EOF");
   erun("'\\x'"   , "1:1-1:5: lexical error: invalid escape sequence in char literal");
   erun("'ABC'"   , "1:1-1:6: lexical error: invalid char literal");
   erun("'\n'"    , "1:1-1:2: lexical error: unclosed char literal");

   // string literals
    trun("\"A\""     , "1:1-1:4 LITSTRING(A)"    , "1:4-1:4 EOF");
    trun("\"b\""     , "1:1-1:4 LITSTRING(b)"    , "1:4-1:4 EOF");
    trun("\"*\""     , "1:1-1:4 LITSTRING(*)"    , "1:4-1:4 EOF");
    trun("\" \""     , "1:1-1:4 LITSTRING( )"    , "1:4-1:4 EOF");
    trun("\"\t\""    , "1:1-1:4 LITSTRING(\t)"   , "1:4-1:4 EOF");
    trun("\"\\b\""   , "1:1-1:5 LITSTRING(\b)"   , "1:5-1:5 EOF");
    trun("\"\\t\""   , "1:1-1:5 LITSTRING(\t)"   , "1:5-1:5 EOF");
    trun("\"\\n\""   , "1:1-1:5 LITSTRING(\n)"   , "1:5-1:5 EOF");
    trun("\"\\r\""   , "1:1-1:5 LITSTRING(\r)"   , "1:5-1:5 EOF");
    trun("\"\\f\""   , "1:1-1:5 LITSTRING(\f)"   , "1:5-1:5 EOF");
    trun("\"\\\"\""  , "1:1-1:5 LITSTRING(\")"   , "1:5-1:5 EOF");
    trun("\"\\^@\""  , "1:1-1:6 LITSTRING(\0)"   , "1:6-1:6 EOF");
    trun("\"\\^I\""  , "1:1-1:6 LITSTRING(\11)"  , "1:6-1:6 EOF");
    trun("\"\\^[\""  , "1:1-1:6 LITSTRING(\33)"  , "1:6-1:6 EOF");
    trun("\"\\^\\\"" , "1:1-1:6 LITSTRING(\34)"  , "1:6-1:6 EOF");
    trun("\"\\^]\""  , "1:1-1:6 LITSTRING(\35)"  , "1:6-1:6 EOF");
    trun("\"\\^^\""  , "1:1-1:6 LITSTRING(\36)"  , "1:6-1:6 EOF");
    trun("\"\\^_\""  , "1:1-1:6 LITSTRING(\37)"  , "1:6-1:6 EOF");
    trun("\"\\^?\""  , "1:1-1:6 LITSTRING(\177)" , "1:6-1:6 EOF");
    erun("\"\\^$\""  , "1:2-1:5: lexical error: invalid control character in string literal");
    trun("\"\\065\"" , "1:1-1:7 LITSTRING(A)"    , "1:7-1:7 EOF");
    erun("\"\\x\""   , "1:2-1:4: lexical error: invalid escape sequence in string literal");
    trun("\"ABC\""   , "1:1-1:6 LITSTRING(ABC)"  , "1:6-1:6 EOF");
    erun("\"\n\""    , "1:2-1:3: lexical error: invalid newline in string literal");

   // keywords
   trun("nil"      , "1:1-1:4 NIL"      , "1:4-1:4 EOF");
   trun("if"       , "1:1-1:3 IF"       , "1:3-1:3 EOF");
   trun("then"     , "1:1-1:5 THEN"     , "1:5-1:5 EOF");
   trun("else"     , "1:1-1:5 ELSE"     , "1:5-1:5 EOF");
   trun("while"    , "1:1-1:6 WHILE"    , "1:6-1:6 EOF");
   trun("do"       , "1:1-1:3 DO"       , "1:3-1:3 EOF");
   trun("break"    , "1:1-1:6 BREAK"    , "1:6-1:6 EOF");
   trun("let"      , "1:1-1:4 LET"      , "1:4-1:4 EOF");
   trun("in"       , "1:1-1:3 IN"       , "1:3-1:3 EOF");
   trun("var"      , "1:1-1:4 VAR"      , "1:4-1:4 EOF");
   trun("function" , "1:1-1:9 FUNCTION" , "1:9-1:9 EOF");
   trun("type"     , "1:1-1:5 TYPE"     , "1:5-1:5 EOF");

   // identifiers
   trun("nome"            , "1:1-1:5 ID(nome)"             , "1:5-1:5 EOF");
   trun("camelCase"       , "1:1-1:10 ID(camelCase)"       , "1:10-1:10 EOF");
   trun("with_underscore" , "1:1-1:16 ID(with_underscore)" , "1:16-1:16 EOF");
   trun("A1b2C33"         , "1:1-1:8 ID(A1b2C33)"          , "1:8-1:8 EOF");
   trun("set@"            , "1:1-1:4 ID(set)"              , "1:4-1:5 AT"  , "1:5-1:5 EOF");
   trun("45var"           , "1:1-1:3 LITINT(45)"           , "1:3-1:6 VAR" , "1:6-1:6 EOF");
   erun("_invalid"        , "1:1-1:2: lexical error: invalid character: [_]");

  }

}
