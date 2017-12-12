import ObjectiveC

public class Base : NSObject {
  public init(foo: Int) { super.init() }
}
public class Sub: Base {
  @objc public init() { super.init(foo: 0) }
}
