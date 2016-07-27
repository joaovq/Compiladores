package errormsg;

import java_cup.runtime.ComplexSymbolFactory.Location;

public class Loc {

  public final Location left;
  public final Location right;

  public Loc() {
    this(new Location(-1, -1), new Location(-1, -1));
  }

  public Loc(Location left, Location right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public String toString() {
    if (left.getUnit().equals("unknown"))
      return String.format("%d:%d-%d:%d",
                           left.getLine(), left.getColumn(),
                           right.getLine(), right.getColumn());
    else
      return String.format("%s-%d:%d-%d:%d",
                           left.getUnit(),
                           left.getLine(), left.getColumn(),
                           right.getLine(), right.getColumn());
  }

}
