package symbol;

import java.util.Hashtable;
import java.util.Map;

import javaslang.collection.Tree;
import javaslang.collection.Tree.Node;
import util.ToTree;

public class Symbol implements ToTree<String> {
  private final String name;

  private Symbol(String name) {
    this.name = name;
  }

  @Override
  public String toString() {
    return name;
  }

  @Override
  public Node<String> toTree() {
    return Tree.of(name);
  }

  private static Map<String, Symbol> dict = new Hashtable<>();

  /**
   * Make return the unique symbol associated with a string. Repeated calls to
   * <tt>symbol("abc")</tt> will return the same symbol.
   */

  public static Symbol symbol(String name) {
    String u = name.intern(); // a canonical representation for the string
    Symbol s = dict.get(u);
    if (s == null) {
      s = new Symbol(u);
      dict.put(u, s);
    }
    return s;
  }

}
