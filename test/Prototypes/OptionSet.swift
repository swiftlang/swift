//===--- OptionSet.swift - Test for library-only option sets --------------===//
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
// No effort has yet been made to unify this with a Set protocol or
// anything; so far this is just about creating option sets that mimic
// the behavior of what we get from NS_OPTIONS.
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift

protocol SetAlgebraBaseType {
  typealias Element
  func subtract(other: Self) -> Self
  func isSubsetOf(other: Self) -> Bool
  func isDisjointWith(other: Self) -> Bool
  func isSupersetOf(other: Self) -> Bool
  func isSupersetOf<
    S : SequenceType where S.Generator.Element == Element
  >(other: S) -> Bool
  var isEmpty: Bool { get }
  func _deduceElement(Element)
}

protocol SetAlgebraType : Equatable, SetAlgebraBaseType {
  init()
  func contains(member: Element) -> Bool
  func union(other: Self) -> Self
  func intersect(other: Self) -> Self
  func exclusiveOr(other: Self) -> Self
}

extension SetAlgebraType {
  final func isSubsetOf(other: Self) -> Bool {
    return self.intersect(other) == self
  }
  
  final func isSupersetOf(other: Self) -> Bool {
    return other.isSubsetOf(self)
  }

  final func isDisjointWith(other: Self) -> Bool {
    return self.intersect(other).isEmpty
  }

  final func isSupersetOf<
    S : SequenceType where S.Generator.Element == Element
  >(other: S) -> Bool {
    return !other.contains { !self.contains($0) }
  }
  
  final func subtract(other: Self) -> Self {
    return self.intersect(self.exclusiveOr(other))
  }

  final var isEmpty: Bool {
    return self == Self()
  }
}

extension SetAlgebraType where Self : CollectionType {
  final func isStrictSupersetOf(other: Self) -> Bool {
    return self.count() > other.count() && self.isSupersetOf(other)
  }

  final func isStrictSubsetOf(other: Self) -> Bool {
    return other.isStrictSupersetOf(self)
  }
}

protocol MutableSetAlgebraBaseType {
  typealias Element
  init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)

  mutating func subtractInPlace(other: Self)

  mutating func unionInPlace<
    S : SequenceType where S.Generator.Element == Element
  >(sequence: S)
  
  mutating func subtractInPlace<
    S : SequenceType where S.Generator.Element == Element
  >(sequence: S)

  mutating func exclusiveOrInPlace<
    S : SequenceType where S.Generator.Element == Element
  >(sequence: S)
}

protocol MutableSetAlgebraType
  : SetAlgebraType, ArrayLiteralConvertible, MutableSetAlgebraBaseType {
  mutating func insert(member: Element)
  mutating func remove(member: Element) -> Element?
  mutating func unionInPlace(other: Self)
  mutating func intersectInPlace(other: Self)
  mutating func exclusiveOrInPlace(other: Self)
}

extension MutableSetAlgebraType {
  init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S) {
    self.init()
    for e in sequence { insert(e) }
  }
  
  init(arrayLiteral: Element...) {
    self.init(arrayLiteral)
  }
  
  final mutating func subtractInPlace(other: Self) {
    self.intersectInPlace(self.exclusiveOr(other))
  }

  final mutating func unionInPlace<
    S : SequenceType where S.Generator.Element == Element
  >(sequence: S) {
    for x in sequence { insert(x) }
  }
  
  final mutating func subtractInPlace<
    S : SequenceType where S.Generator.Element == Element
  >(sequence: S) {
    for x in sequence { remove(x) }
  }
  
  final mutating func exclusiveOrInPlace<
    S : SequenceType where S.Generator.Element == Element
  >(sequence: S) {
    for x in sequence {
      if let _ = self.remove(x) {
      } else {
        self.insert(x)
      }
    }
  }
}

extension Set : MutableSetAlgebraType {
  func _deduceElement(Element) {}
}

// We can't constrain the associated Element type to be the same as
// Self, but we can do almost as well with a default and a constrained
// extension
protocol OptionSetType : MutableSetAlgebraType, RawRepresentable {
  typealias Element = Self
  init(rawValue: RawValue)
}

func == <
  T : RawRepresentable where T.RawValue : Equatable
>(lhs: T, rhs: T) -> Bool {
  return lhs.rawValue == rhs.rawValue
}

extension OptionSetType {
  final
  func contains(member: Self) -> Bool {
    return self.isSupersetOf(member)
  }
  
  final func _deduceElement(Self) {}
}

extension OptionSetType
where Self.Element == Self, Self.RawValue : BitwiseOperationsType
{
  init() {
    self.init(rawValue: .allZeros)
  }

  final
  mutating func insert(member: Element) {
    self.unionInPlace(member)
  }
  
  final
  mutating func remove(member: Element) -> Element? {
    let r = isSupersetOf(member) ? Optional(member) : nil
    self.subtractInPlace(member)
    return r
  }

  final
  func union(other: Self) -> Self {
    var r: Self = Self(rawValue: self.rawValue)
    r.unionInPlace(other)
    return r
  }
  
  final
  func intersect(other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.intersectInPlace(other)
    return r
  }
  
  final
  func exclusiveOr(other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.exclusiveOrInPlace(other)
    return r
  }
  
  final
  mutating func unionInPlace(other: Self) {
    self = Self(rawValue: self.rawValue | other.rawValue)
  }
  
  final
  mutating func intersectInPlace(other: Self) {
    self = Self(rawValue: self.rawValue & other.rawValue)
  }
  
  final
  mutating func exclusiveOrInPlace(other: Self) {
    self = Self(rawValue: self.rawValue ^ other.rawValue)
  }
  
  init(_ components: Self...) {
    self.init(
      rawValue: lazy(components).map { $0.rawValue }.reduce(.allZeros) {
        $0 | $1
      }
    )
  }
}

//===--- Tests ------------------------------------------------------------===//

struct X : OptionSetType {
  let rawValue: Int
  init(rawValue: Int) { self.rawValue = rawValue }

  static let Box = X(rawValue: 1)
  static let Carton = X(rawValue: 2)
  static let Bag = X(rawValue: 4)
  static let Satchel = X(rawValue: 8)
  static let BoxOrBag: X = [Box, Bag]
  static let BoxOrCartonOrBag = X(Box, Carton, Bag)
  static let SatchelOrBag = Satchel.union(Bag)
}

debugPrint(X.Box)
debugPrint(X.Carton)
debugPrint(X.Bag)
debugPrint(X.Satchel)
debugPrint([X.Satchel, X.Bag, X.Box] as X)
debugPrint(X.BoxOrCartonOrBag)
debugPrint(X.BoxOrBag == ([.Box, .Satchel] as X))

/*
// Hack this part out if you want to play with OptionSet outside the
// test framework.
import StdlibUnittest

var tests = TestSuite("OptionSet")

tests.test("basics") {
  enum Options : Int { case one, two, three, four }

  var x = OptionSet<Options>()
  x = OptionSet(.one, .two)
  expectEqual(x, OptionSet(.one) | OptionSet(.two))

  let y = OptionSet<Options>(.four)

  let z = x | y
  expectEqual(OptionSet(.one, .two, .four), z)
  
  expectEqual(nil, ~z & z)
}

runAllTests()
*/
