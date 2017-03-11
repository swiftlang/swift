// RUN: %target-swift-frontend -emit-silgen %s

// This is a "don't crash with duplicate definition errors" test.
// We care about being able to express each of these "redeclarations" when the
// availability doesn't overlap.

class BeforeAndAfter {
  @available(swift, obsoleted: 4.0)
  public init(foo: ()) {}

  @available(swift 4.0)
  public init?(foo: ()) {}

  @available(swift, obsoleted: 4.0)
  public init() {}

  @available(swift 4.0)
  public init() throws {}

  @available(swift, obsoleted: 4.0)
  public static func foo() {}

  @available(swift 4.0)
  public static func foo() throws {}
}
