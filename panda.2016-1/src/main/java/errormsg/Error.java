package errormsg;

public class Error extends RuntimeException {
  public final Loc loc;

  public Error(Loc loc, String message) {
    super(message);
    this.loc = loc;
  }

  @Override
  public String toString() {
    return String.format("%s: %s", loc, getMessage());
  }

  public static void error(Loc loc, String message) {
    throw new Error(loc, message);
  }

  public static void lexicalError(Loc loc, String message) {
    error(loc, "lexical error: " + message);
  }

  public static void syntaxError(Loc loc, String message) {
    error(loc, "syntax error: " + message);
  }

}
