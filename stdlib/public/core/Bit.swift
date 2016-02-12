//===--- Bit.swift - A 1-bit type that can be used as an Index ------------===//
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
//
//  Used to index CollectionOfOne<T>
//
//===----------------------------------------------------------------------===//

/// A `RandomAccessIndexType` that has two possible values.  Used as
/// the `Index` type for `CollectionOfOne<T>`.
public enum Bit : Int, Comparable, RandomAccessIndexType {

  public typealias Distance = Int

  case Zero = 0, One = 1

  /// Returns the next consecutive value after `self`.
  ///
  /// - Requires: `self == .Zero`.
  public func successor() -> Bit {
    _precondition(self == .Zero, "Can't increment past one")
    return .One
  }

  /// Returns the previous consecutive value before `self`.
  ///
  /// - Requires: `self != .Zero`.
  public func predecessor() -> Bit {
    _precondition(self == .One, "Can't decrement past zero")
    return .Zero
  }

  public func distanceTo(other: Bit) -> Int {
    return rawValue.distanceTo(other.rawValue)
  }

  public func advancedBy(n: Distance) -> Bit {
    return rawValue.advancedBy(n) > 0 ? One : Zero
  }
}

@warn_unused_result
public func == (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.rawValue == rhs.rawValue
}

@warn_unused_result
public func < (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.rawValue < rhs.rawValue
}

extension Bit : IntegerArithmeticType {
  static func _withOverflow(value : (intResult: Int, overflow: Bool)
  ) -> (Bit, overflow: Bool) {
    if let bit = Bit(rawValue: value.intResult) {
      return (bit, overflow: value.overflow)
    }

    let bitRaw = value.intResult % 2 + (value.intResult < 0 ? 2 : 0)
    let bit = Bit(rawValue: bitRaw)!
    return (bit, overflow: true)
  }

  /// Add `lhs` and `rhs`, returning a result and a `Bool` that is
  /// `true` iff the operation caused an arithmetic overflow.
  public static func addWithOverflow(
    lhs: Bit, _ rhs: Bit
  ) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.addWithOverflow(lhs.rawValue, rhs.rawValue))
  }

  /// Subtract `lhs` and `rhs`, returning a result and a `Bool` that is
  /// `true` iff the operation caused an arithmetic overflow.
  public static func subtractWithOverflow(
    lhs: Bit, _ rhs: Bit
  ) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.subtractWithOverflow(lhs.rawValue, rhs.rawValue))
  }

  /// Multiply `lhs` and `rhs`, returning a result and a `Bool` that is
  /// `true` iff the operation caused an arithmetic overflow.
  public static func multiplyWithOverflow(
    lhs: Bit, _ rhs: Bit
  ) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.multiplyWithOverflow(lhs.rawValue, rhs.rawValue))
  }

  /// Divide `lhs` and `rhs`, returning a result and a `Bool` that is
  /// `true` iff the operation caused an arithmetic overflow.
  public static func divideWithOverflow(
    lhs: Bit, _ rhs: Bit
  ) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.divideWithOverflow(lhs.rawValue, rhs.rawValue))
  }

  /// Divide `lhs` and `rhs`, returning the remainder and a `Bool` that is
  /// `true` iff the operation caused an arithmetic overflow.
  public static func remainderWithOverflow(
    lhs: Bit, _ rhs: Bit
  ) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.remainderWithOverflow(lhs.rawValue, rhs.rawValue))
  }

  /// Represent this number using Swift's widest native signed integer
  /// type.
  public func toIntMax() -> IntMax {
    return IntMax(rawValue)
  }
}
