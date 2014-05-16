//===--- Bit.swift - A 1-bit type that can be used as an Index ------------===//
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
//
//  Used to index SequenceOfOne<T>
//
//===----------------------------------------------------------------------===//
enum Bit : Int, RandomAccessIndex {
  case zero = 0, one = 1

  func succ() -> Bit {
    _precondition(self == .zero, "Can't increment past one")
    return .one
  }

  func pred() -> Bit {
    _precondition(self == .one, "Can't decrement past zero")
    return .zero
  }

  func distanceTo(other: Bit) -> Int {
    return toRaw().distanceTo(other.toRaw())
  }

  func advancedBy(distance: Int) -> Bit {
    return toRaw().advancedBy(distance) > 0 ? one : zero
  }
}

func == (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.toRaw() == rhs.toRaw()
}

func < (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.toRaw() < rhs.toRaw()
}

extension Bit : IntegerArithmetic {
  static func _withOverflow(x: Int, _ b: Bool) -> (Bit, Bool) {
    return (Bit.fromRaw(x)!, b)
  }
  
  static func uncheckedAdd(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.uncheckedAdd(lhs.toRaw(), rhs.toRaw()))
  }

  static func uncheckedSubtract(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.uncheckedSubtract(lhs.toRaw(), rhs.toRaw()))
  }

  static func uncheckedMultiply(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.uncheckedMultiply(lhs.toRaw(), rhs.toRaw()))
  }

  static func uncheckedDivide(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.uncheckedDivide(lhs.toRaw(), rhs.toRaw()))
  }

  static func uncheckedModulus(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.uncheckedModulus(lhs.toRaw(), rhs.toRaw()))
  }

  func toIntMax() -> IntMax {
    return IntMax(toRaw())
  }
}
