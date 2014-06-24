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
@public struct Bool {
  var value: Builtin.Int1

  /// Default-initialize Boolean value to `false`.
  @transparent @public 
  init() { value = Builtin.trunc_Word_Int1(0.value) }

  @transparent
  init(_ v : Builtin.Int1) { value = v }

  @public static var false : Bool {
    @transparent
    get {
      return Bool()
    }
  }
  @public static var true : Bool {
    @transparent
    get {
      return Bool(Builtin.trunc_Word_Int1(1.value))
    }
  }
}

@public var true : Bool {
  @transparent
  get {
    return Bool.true
  }
}
@public var false : Bool {
  @transparent
  get {
    return Bool.false
  }
}

extension Bool : LogicValue {
  @transparent func _getBuiltinLogicValue() -> Builtin.Int1 {
    return value
  }

  @transparent @public func getLogicValue() -> Bool { return self }

  // Bool can be constructed from LogicValue
  @public init(_ v : LogicValue) {
    self = v.getLogicValue()
  }
}

extension Bool : Printable {
  @public var description: String {
    return self ? "true" : "false"
  }
}

// This is a magic entrypoint known to the compiler.
@transparent func _getBool(v: Builtin.Int1) -> Bool { return Bool(v) }

//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

// Unary bitwise complement.
@prefix @transparent @public
func ~(a: Bool) -> Bool {
  return a ^ true
}

// Unary logical complement.
@prefix @transparent @public
func !(a: Bool) -> Bool {
  return ~a
}

@transparent @public
func ==(lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.cmp_eq_Int1(lhs.value, rhs.value))
}

@transparent
extension Bool : Equatable, Hashable {
  @public var hashValue: Int {
    return self ? 1 : 0
  }
}

// Bitwise 'and'.
@transparent @public func & (lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.and_Int1(lhs.value, rhs.value))
}

// Bitwise 'xor'.
@transparent @public func ^ (lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.xor_Int1(lhs.value, rhs.value))
}

// Bitwise 'or'.
@transparent @public func | (lhs: Bool, rhs: Bool) -> Bool {
  return Bool(Builtin.or_Int1(lhs.value, rhs.value))
}

// Compound assignment (with bitwise and)
@assignment @transparent @public
func &= (inout lhs: Bool, rhs: Bool) {
  lhs = lhs & rhs
}

// Compound assignment (with bitwise or)
@assignment @transparent @public
func |= (inout lhs: Bool, rhs: Bool) {
  lhs = lhs | rhs
}

// Compound assignment (with bitwise xor)
@assignment @transparent @public
func ^= (inout lhs: Bool, rhs: Bool) {
  lhs = lhs ^ rhs
}

