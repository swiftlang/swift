// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle %s | %FileCheck %s
// REQUIRES: EnableSILOpaqueValues

public protocol C : AnyObject {}

public protocol _ObjectiveCBridgeable {}

// A lot of standard library support is necessary to support raw enums.
// --------------------------------------------------------------------

infix operator  == : ComparisonPrecedence
infix operator  ~= : ComparisonPrecedence

public struct Bool {
  var _value: Builtin.Int1

  public init() {
    let zero: Int64 = 0
    self._value = Builtin.trunc_Int64_Int1(zero._value)
  }

  internal init(_ v: Builtin.Int1) { self._value = v }

  public init(_ value: Bool) {
    self = value
  }
}

extension Bool {
  public func _getBuiltinLogicValue() -> Builtin.Int1 {
    return _value
  }
}

public protocol Equatable {
  /// Returns a Boolean value indicating whether two values are equal.
  ///
  /// Equality is the inverse of inequality. For any values `a` and `b`,
  /// `a == b` implies that `a != b` is `false`.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  static func == (lhs: Self, rhs: Self) -> Bool
}

public func ~= <T : Equatable>(a: T, b: T) -> Bool {
  return a == b
}

public protocol RawRepresentable {
  associatedtype RawValue

  init?(rawValue: RawValue)

  var rawValue: RawValue { get }
}

public func == <T : RawRepresentable>(lhs: T, rhs: T) -> Bool
  where T.RawValue : Equatable {
  return lhs.rawValue == rhs.rawValue
}

public protocol ExpressibleByStringLiteral {}
public protocol ExpressibleByFloatLiteral {}
public protocol ExpressibleByUnicodeScalarLiteral {}
public protocol ExpressibleByExtendedGraphemeClusterLiteral {}

public struct Int64 : ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral, Equatable {
  public var _value: Builtin.Int64
  public init(_builtinIntegerLiteral x: _MaxBuiltinIntegerType) {
    _value = Builtin.s_to_s_checked_trunc_IntLiteral_Int64(x).0
  }
  public typealias IntegerLiteralType = Int64
  public init(integerLiteral value: Int64) {
    self = value
  }
  public static func ==(_ lhs: Int64, rhs: Int64) -> Bool {
    return Bool(Builtin.cmp_eq_Int64(lhs._value, rhs._value))
  }
}

public struct Int : _ExpressibleByBuiltinIntegerLiteral, ExpressibleByIntegerLiteral, Equatable {
  var _value: Builtin.Int64
  public init() {
    self = 0
  }
  public typealias IntegerLiteralType = Int
  public init(_builtinIntegerLiteral x: _MaxBuiltinIntegerType) {
    _value = Builtin.s_to_s_checked_trunc_IntLiteral_Int64(x).0
  }

  public init(integerLiteral value: Int) {
    self = value
  }

  public static func ==(_ lhs: Int, rhs: Int) -> Bool {
    return Bool(Builtin.cmp_eq_Int64(lhs._value, rhs._value))
  }
}

// -----------------------------------------------------------------------------
