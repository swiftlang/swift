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

// Bool is the standard way to reason about truth values.
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
  public static func _convertFromBuiltinBooleanLiteral(value: Builtin.Int1) 
                       -> Bool {
    return Bool(value)
  }

  @transparent
  public static func convertFromBooleanLiteral(value: Bool) -> Bool {
    return value
  }
}

extension Bool : BooleanType {
  @transparent public func _getBuiltinLogicValue() -> Builtin.Int1 {
    return value
  }

  @transparent public var boolValue: Bool { return self }

  // Bool can be constructed from BooleanType
  public init(_ v : BooleanType) {
    self = v.boolValue
  }
}

extension Bool : Printable {
  public var description: String {
    return self ? "true" : "false"
  }
}

// This is a magic entrypoint known to the compiler.
@transparent func _getBool(v: Builtin.Int1) -> Bool { return Bool(v) }

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary bitwise complement.
@transparent prefix
public func ~(a: Bool) -> Bool {
  return a ^ true
}

// Unary logical complement.
@transparent prefix
public func !(a: Bool) -> Bool {
  return ~a
}

@transparent
public func ==(lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.cmp_eq_Int1(lhs.value, rhs.value))
}

@transparent
extension Bool : Equatable, Hashable {
  public var hashValue: Int {
    return self ? 1 : 0
  }
}

// Bitwise 'and'.
@transparent public func & (lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.and_Int1(lhs.value, rhs.value))
}

// Bitwise 'xor'.
@transparent public func ^ (lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.xor_Int1(lhs.value, rhs.value))
}

// Bitwise 'or'.
@transparent public func | (lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.or_Int1(lhs.value, rhs.value))
}

// Compound assignment (with bitwise and)
@transparent
public func &= (inout lhs: Bool, rhs: Bool) {
  lhs = lhs & rhs
}

// Compound assignment (with bitwise or)
@transparent
public func |= (inout lhs: Bool, rhs: Bool) {
  lhs = lhs | rhs
}

// Compound assignment (with bitwise xor)
@transparent
public func ^= (inout lhs: Bool, rhs: Bool) {
  lhs = lhs ^ rhs
}

