package parse;

import errormsg.Loc;
import java_cup.runtime.ComplexSymbolFactory.ComplexSymbol;
import java_cup.runtime.Symbol;

public class Tokens {

  public static String dumpToken(Symbol tok) {
    if (tok instanceof ComplexSymbol) {
      ComplexSymbol t = (ComplexSymbol) tok;
      Loc loc = new Loc(t.xleft, t.xright);
      if (tok.value == null)
        return String.format("%s %s", loc, t.getName());
      else
        return String.format("%s %s(%s)", loc, t.getName(), t.value);
    }
    else if (tok.value == null)
      return String.format("%s", Terminals.terminalNames[tok.sym]);
    else
      return String.format("%s(%s)", Terminals.terminalNames[tok.sym], tok.value);
  }

}
