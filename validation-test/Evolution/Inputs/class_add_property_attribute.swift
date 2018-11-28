public var count = 0

public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public class MathClass {
  public init() {}
}

public class Attributed {
  public init(x: MathClass, y: MathClass) {
    self.x = x
    self.y = y
  }

#if BEFORE
  public var x: MathClass?
  public var y: MathClass
#else
  public weak var x: MathClass?
  public unowned var y: MathClass
#endif
}
