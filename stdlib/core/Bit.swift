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
@public enum Bit : Int, RandomAccessIndex {
  case zero = 0, one = 1

  @public func successor() -> Bit {
    _precondition(self == .zero, "Can't increment past one")
    return .one
  }

  @public func predecessor() -> Bit {
    _precondition(self == .one, "Can't decrement past zero")
    return .zero
  }

  @public func distanceTo(other: Bit) -> Int {
    return toRaw().distanceTo(other.toRaw())
  }

  @public func advancedBy(distance: Int) -> Bit {
    return toRaw().advancedBy(distance) > 0 ? one : zero
  }
}

@public func == (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.toRaw() == rhs.toRaw()
}

@public func < (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.toRaw() < rhs.toRaw()
}

extension Bit : IntegerArithmetic {
  static func _withOverflow(x: Int, _ b: Bool) -> (Bit, Bool) {
    return (Bit.fromRaw(x)!, b)
  }
  
  @public static func addWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.addWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public static func subtractWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.subtractWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public static func multiplyWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.multiplyWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public static func divideWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.divideWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public static func modulusWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.modulusWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public func toIntMax() -> IntMax {
    return IntMax(toRaw())
  }
}
