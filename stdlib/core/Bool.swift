//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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
  var value: Builtin.Int1

  /// Default-initialize Boolean value to `false`.
  @transparent
  public init() { value = Builtin.trunc_Word_Int1(0.value) }

  @transparent
  init(_ v : Builtin.Int1) { value = v }
}

extension Bool : _BuiltinBooleanLiteralConvertible, BooleanLiteralConvertible {
  @transparent
  public init(_builtinBooleanLiteral value: Builtin.Int1) {
    self.value = value
  }

  /// Create an instance initialized to `value`.
  @transparent
  public init(booleanLiteral value: Bool) {
    self = value
  }
}

extension Bool : BooleanType {
  @transparent public func _getBuiltinLogicValue() -> Builtin.Int1 {
    return value
  }

  /// Identical to `self`.
  @transparent public var boolValue: Bool { return self }

  /// Construct an instance representing the same logical value as
  /// `value`
  public init<T: BooleanType>(_ value: T) {
    self = value.boolValue
  }
}

extension Bool : Printable {
  /// A textual representation of `self`.
  public var description: String {
    return self ? "true" : "false"
  }
}

// This is a magic entrypoint known to the compiler.
@transparent
public // COMPILER_INTRINSIC
func _getBool(v: Builtin.Int1) -> Bool { return Bool(v) }

@transparent
extension Bool : Equatable, Hashable {
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`
  ///
  /// **Note:** the hash value is not guaranteed to be stable across
  /// different invocations of the same program.  Do not persist the
  /// hash value across program runs.
  public var hashValue: Int {
    return self ? 1 : 0
  }
}

//===----------------------------------------------------------------------===//
// Operators
//===----------------------------------------------------------------------===//

// Unary logical complement.
@transparent
public prefix func !(a: Bool) -> Bool {
  return Bool(Builtin.xor_Int1(a.value, true.value))
}

@transparent
public func ==(lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.cmp_eq_Int1(lhs.value, rhs.value))
}

//===----------------------------------------------------------------------===//
// Unavailable Operators
//===----------------------------------------------------------------------===//

// Unary bitwise complement.
@availability(*, unavailable, message="use the '!' operator instead")
public prefix func ~(a: Bool) -> Bool {
  _preconditionFailure("unavailable function can not be called")
}

// Bitwise 'and'.
@availability(*, unavailable, message="use the '&&' operator instead")
public func & (lhs: Bool, rhs: Bool) -> Bool {
  _preconditionFailure("unavailable function can not be called")
}

// Bitwise 'xor'.
@availability(*, unavailable, message="use the '!=' operator instead")
public func ^ (lhs: Bool, rhs: Bool) -> Bool {
  _preconditionFailure("unavailable function can not be called")
}

// Bitwise 'or'.
@availability(*, unavailable, message="use the '||' operator instead")
public func | (lhs: Bool, rhs: Bool) -> Bool {
  _preconditionFailure("unavailable function can not be called")
}

// Compound assignment (with bitwise and)
@availability(*, unavailable, message="use the '&&' operator instead")
public func &= (inout lhs: Bool, rhs: Bool) {
  _preconditionFailure("unavailable function can not be called")
}

// Compound assignment (with bitwise or)
@availability(*, unavailable, message="use the '||' operator instead")
public func |= (inout lhs: Bool, rhs: Bool) {
  _preconditionFailure("unavailable function can not be called")
}

// Compound assignment (with bitwise xor)
@availability(*, unavailable, message="use the '!=' operator instead")
public func ^= (inout lhs: Bool, rhs: Bool) {
  _preconditionFailure("unavailable function can not be called")
}

