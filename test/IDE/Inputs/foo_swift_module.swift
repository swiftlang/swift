operator infix %%% {
  associativity left
  precedence 200
}

public func %%% (lhs: Int, rhs: Int) -> Int {
  return lhs + rhs
}

public func visibleImport() {}
public func hiddenImport() {}

public func overlayedFoo() {}

/// FooSwiftStruct Aaa.
/**
 * Bbb.
 * Ccc.
 */
public struct FooSwiftStruct {
  // Indentation is incorrect on purpose, don't fix this.

    /// fooInstanceFunc Aaa.
    /**
     * Bbb
     */
      /**
       * Ccc.
       */
    public func fooInstanceFunc() {}
public init() {}
}
public struct BarGenericSwiftStruct1<T> {
  public init(t: T) {}
  public func bar1InstanceFunc() {}
}
public protocol BarProtocol {
  func instanceFunc()
}
public struct BarGenericSwiftStruct2<T: BarProtocol, U> {
  public init(t: T, u: U) {}
  public func bar2InstanceFunc() {}
}

