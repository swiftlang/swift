@inlinable
public func foo(x: Int) -> Int {
  return x + 1
}

/// something
func makeX<T: Collection>(_ seed: T) -> T.SubSequence {
  return seed.dropFirst(1)
}

public struct Foo : BazProtocol {
  /// varx
  public var x = makeX("foobar")

  /// method
  public func method1() -> Int? { return 1 }

  /* Foo Bar */
  @_transparent
  public func method2(x: Int) -> Int {
    return x * 12
  }

  @inlinable
  public func method3<T: Equatable>(x: T, y: T) -> T {
    return x == y ? x : y
  }
}
