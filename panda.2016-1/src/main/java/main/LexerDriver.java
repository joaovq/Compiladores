package main;

import java_cup.runtime.ComplexSymbolFactory.ComplexSymbol;
import parse.Lexer;
import parse.Terminals;
import parse.Tokens;

import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;

public class LexerDriver {

  public static void main(String[] args) throws Exception {
    Reader input;
    String fileName;

    if (args.length == 0) {
      fileName = "stdin";
      input = new InputStreamReader(System.in);
    }
    else {
      fileName = args[0];
      input = new FileReader(fileName);
    }

    Lexer lex = new Lexer(fileName, input);
    ComplexSymbol tok;
    do {
      tok = (ComplexSymbol) lex.next_token();
      System.out.println(Tokens.dumpToken(tok));
    } while (tok.sym != Terminals.EOF);
  }

}
