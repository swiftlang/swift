@resultBuilder
public struct Builder {
  @_alwaysEmitIntoClient
  public static func buildExpression<X>(_ x: X) -> X {x}
  @_alwaysEmitIntoClient
  public static func buildBlock<X>(_ x: X) -> X {x}
}

public struct View<X> {
  public init(@Builder _: () -> X) {}

  public func closure(_: () -> Void) {}
}
