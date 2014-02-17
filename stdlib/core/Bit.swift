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
//  Used to index T? and SequenceOfOne<T>
//
//===----------------------------------------------------------------------===//
enum Bit : Int, RandomAccessIndex {
  case zero = 0, one = 1

  func succ() -> Bit {
    assert(self == .zero, "Can't increment past one")
    return .one
  }

  func pred() -> Bit {
    assert(self == .one, "Can't decrement past zero")
    return .zero
  }
}

func == (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.toRaw() == rhs.toRaw()
}

func < (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.toRaw() < rhs.toRaw()
}

extension Bit : NumericOperations {
  static func _withOverflow(x: Int, b: Bool) -> (Bit, Bool) {
    return (Bit.fromRaw(x)!, b)
  }
  
  static func add(lhs: Bit, rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.add(lhs.toRaw(), rhs.toRaw()))
  }

  static func sub(lhs: Bit, rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.add(lhs.toRaw(), rhs.toRaw()))
  }

  static func mul(lhs: Bit, rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.add(lhs.toRaw(), rhs.toRaw()))
  }

  static func div(lhs: Bit, rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.add(lhs.toRaw(), rhs.toRaw()))
  }

  static func rem(lhs: Bit, rhs: Bit) -> (Bit, Bool) {
    return _withOverflow(Int.add(lhs.toRaw(), rhs.toRaw()))
  }

  func toIntMax() -> IntMax {
    return IntMax(toRaw())
  }
}

// This overload is needed to work around the ambiguity of the two
// generic "-" operators for RandomAccessIndex.
func - (lhs: Bit, rhs: Bit) -> Bit {
  return Bit.fromRaw( lhs.toRaw() - rhs.toRaw() )!
}
