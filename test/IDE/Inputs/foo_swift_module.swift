infix operator %%% : High
precedencegroup High {
  associativity: left
  higherThan: BitwiseShiftPrecedence
}

public func %%% (lhs: Int, rhs: Int) -> Int {
  return lhs + rhs
}

postfix operator =>
public postfix func =>(lhs: Int) -> Int {
  return lhs + 1
}
postfix operator =->
internal postfix func =->(lhs: Int) -> Int {
  return lhs + 1
}

public func visibleImport() {}
public func hiddenImport() {}

public func overlayedFoo() {}

public var globalVar: Int = 0

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

/// rdar://18457785
public enum MyQuickLookObject {
  /// A rectangle.
  ///
  /// Uses explicit coordinates to avoid coupling a particular Cocoa type.
  case Rectangle(Float64,Float64,Float64,Float64)
}
