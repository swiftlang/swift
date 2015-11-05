@_exported import Foo

public class FooOverlayClassBase {
  public func f() {}
}
public class FooOverlayClassDerived : FooOverlayClassBase {
  override public func f() {}
}

class InternalClassShouldShow {}
