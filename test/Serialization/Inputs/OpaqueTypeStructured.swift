public protocol P { }
public protocol Q { }

extension Int: P { }
extension Double: Q { }

public protocol R {
  associatedtype A
  func f() -> A
}

public struct X: R {
  public func f() -> (some P, [some Q]) {
    return (1, [3.14159, 2.71828])
  }
}
