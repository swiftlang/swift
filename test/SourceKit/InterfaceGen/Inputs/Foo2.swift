// we keep comments before the first line of code
// Keep this line

import Foundation

public class NewLineAfterImport {
}
/// This is the class base
public class FooOverlayClassBase {
  public func f() {}
  var Range = 10
  private var RangePrivate : Int = 10
}
public class FooOverlayClassDerived : FooOverlayClassBase {
  override public func f() {}
}
// This comment should go away
private class PrivateClassNotVisible {

}

class InternalClassVisible {

}





class IgnoreBigWhiteSpaceGap {

}

class PropWithDocComment {
  /// Awesome property.
  var prop = 0
  /// I see doubles.
  var (p1, p2) = (0, 1)
}

enum Colors {
    case Red
    case Blue
}
