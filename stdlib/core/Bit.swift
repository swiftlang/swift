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
@public enum Bit : Int, RandomAccessIndex, Reflectable {
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
  
  func getMirror() -> Mirror {
    return _BitMirror(self)
  }
}

struct _BitMirror: Mirror {
  let _value: Bit
  
  init(_ v: Bit) {
    self._value = v
  }
  
  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return 0 }

  subscript(i: Int) -> (String, Mirror) { 
    _fatalError("Mirror access out of bounds")
  }

  var summary: String { 
    switch _value {
      case .zero: return ".zero"
      case .one:  return ".one"
    }
  }

  var quickLookObject: QuickLookObject? { return .None }

  var disposition: MirrorDisposition { return .Enum }
}

@public func == (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.toRaw() == rhs.toRaw()
}

@public func < (lhs: Bit, rhs: Bit) -> Bool {
  return lhs.toRaw() < rhs.toRaw()
}

extension Bit : IntegerArithmetic {
  static func _withOverflow(v: (Int, overflow: Bool)) -> (Bit, overflow: Bool) {
    return (Bit.fromRaw(v.0)!, v.overflow)
  }
  
  @public static func addWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.addWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public static func subtractWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.subtractWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public static func multiplyWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.multiplyWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public static func divideWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.divideWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public static func modulusWithOverflow(lhs: Bit, _ rhs: Bit) -> (Bit, overflow: Bool) {
    return _withOverflow(Int.modulusWithOverflow(lhs.toRaw(), rhs.toRaw()))
  }

  @public func toIntMax() -> IntMax {
    return IntMax(toRaw())
  }
}
