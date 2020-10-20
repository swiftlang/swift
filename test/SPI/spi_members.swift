// RUN: %target-typecheck-verify-swift

@_spi(Foo)
public class Bar {}

public struct Foo {
  public init() {}

  @_spi(Foo) public func method(_: Bar) {}
  @_spi(Foo) public var property: Bar { Bar() }
}
