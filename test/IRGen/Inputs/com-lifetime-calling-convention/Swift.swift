// Minimal integer literal support for synthesized IID getters in the
// cross-target SIL tests. The lifetime operations do not use the IID value.
public protocol _ExpressibleByBuiltinIntegerLiteral {
  init(_builtinIntegerLiteral value: Builtin.IntLiteral)
}
public protocol ExpressibleByIntegerLiteral {
  associatedtype IntegerLiteralType: _ExpressibleByBuiltinIntegerLiteral
  init(integerLiteral value: IntegerLiteralType)
}
public struct Int: ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {}
  public init(integerLiteral value: Int) {}
}
