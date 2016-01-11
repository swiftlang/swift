//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Bool Datatype and Supporting Operators
//===----------------------------------------------------------------------===//

/// A value type whose instances are either `true` or `false`.
public struct Bool {
  internal var _value: Builtin.Int1

  /// Default-initialize Boolean value to `false`.
  @_transparent
  public init() {
    let zero: Int8 = 0
    self._value = Builtin.trunc_Int8_Int1(zero._value)
  }

  @_transparent
  internal init(_ v: Builtin.Int1) { self._value = v }
}

extension Bool : _BuiltinBooleanLiteralConvertible, BooleanLiteralConvertible {
  @_transparent
  public init(_builtinBooleanLiteral value: Builtin.Int1) {
    self._value = value
  }

  /// Create an instance initialized to `value`.
  @_transparent
  public init(booleanLiteral value: Bool) {
    self = value
  }
}

extension Bool : BooleanType {
  @_transparent
  @warn_unused_result
  public func _getBuiltinLogicValue() -> Builtin.Int1 {
    return _value
  }

  /// Identical to `self`.
  @_transparent public var boolValue: Bool { return self }

  /// Construct an instance representing the same logical value as
  /// `value`.
  public init<T : BooleanType>(_ value: T) {
    self = value.boolValue
  }
}

extension Bool : CustomStringConvertible {
  /// A textual representation of `self`.
  public var description: String {
    return self ? "true" : "false"
  }
}

// This is a magic entry point known to the compiler.
@_transparent
public // COMPILER_INTRINSIC
func _getBool(v: Builtin.Int1) -> Bool { return Bool(v) }

@_transparent
extension Bool : Equatable, Hashable {
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
  ///
  /// - Note: the hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  public var hashValue: Int {
    return self ? 1 : 0
  }
}

//===----------------------------------------------------------------------===//
// Operators
//===----------------------------------------------------------------------===//

// Unary logical complement.
@_transparent
@warn_unused_result
public prefix func !(a: Bool) -> Bool {
  return Bool(Builtin.xor_Int1(a._value, true._value))
}

@_transparent
@warn_unused_result
public func ==(lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.cmp_eq_Int1(lhs._value, rhs._value))
}

