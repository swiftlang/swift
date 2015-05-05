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

//===--- workarounds for <rdar://20443893> --------------------------------===//
// Skip to the next divider for the real content.  These protocols and
// operators duplicate the hierarchy for RawOptionSetType, less an
// init? that is causing SILGen fits.
//===----------------------------------------------------------------------===//

/// A type that can be converted to an associated "raw" type, then
/// converted back to produce an instance equivalent to the original.
public protocol MyRawRepresentable {
  /// The "raw" type that can be used to represent all values of `Self`.
  ///
  /// Every distinct value of `self` has a corresponding unique
  /// value of `RawValue`, but `RawValue` may have representations
  /// that do not correspond to an value of `Self`.
  typealias RawValue

  /// Convert from a value of `RawValue`, yielding `nil` iff
  /// `rawValue` does not correspond to a value of `Self`.
  // FIXME: disabled due to <rdar://20443893>
  // init?(rawValue: RawValue)

  /// The corresponding value of the "raw" type.
  ///
  /// `Self(rawValue: self.rawValue)!` is equivalent to `self`.
  var rawValue: RawValue { get }
}

// Workaround for our lack of circular conformance checking. Allow == to be
// defined on _MyRawOptionSetType in order to satisfy the Equatable requirement of
// MyRawOptionSetType without a circularity our type-checker can't yet handle.

/// This protocol is an implementation detail of `MyRawOptionSetType`; do
/// not use it directly.
///
/// Its requirements are inherited by `MyRawOptionSetType` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _MyRawOptionSetType : MyRawRepresentable, Equatable {
  typealias RawValue : BitwiseOperationsType, Equatable
  init(rawValue: RawValue)
}

public func == <T : _MyRawOptionSetType>(a: T, b: T) -> Bool {
  return a.rawValue == b.rawValue
}

public func & <T : _MyRawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue & b.rawValue)
}
public func | <T : _MyRawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue | b.rawValue)
}
public func ^ <T : _MyRawOptionSetType>(a: T, b: T) -> T {
  return T(rawValue: a.rawValue ^ b.rawValue)
}
public prefix func ~ <T : _MyRawOptionSetType>(a: T) -> T {
  return T(rawValue: ~a.rawValue)
}

/// Protocol for `NS_OPTIONS` imported from Objective-C
public protocol MyRawOptionSetType : _MyRawOptionSetType, BitwiseOperationsType,
    NilLiteralConvertible {
  // FIXME: Disabled pending <rdar://problem/14011860> (Default
  // implementations in protocols)
  // The Clang importer synthesizes these for imported NS_OPTIONS.

  /* init?(rawValue: RawValue) { self.init(rawValue) } */
}

//===--- end workarounds for <rdar://20443893> ----------------------------===//
// Real content starts here
//===----------------------------------------------------------------------===//

protocol SetAlgebraBaseType {
  typealias Basis
  func subtract(other: Self) -> Self
  func isSubsetOf(other: Self) -> Bool
  func isDisjointWith(other: Self) -> Bool
  func isSupersetOf(other: Self) -> Bool
  func isSupersetOf<
    S : SequenceType where S.Generator.Element == Basis
  >(other: S) -> Bool
}

protocol SetAlgebraType : Equatable, SetAlgebraBaseType {
  
  init()
  func contains(member: Basis) -> Bool
  func union(other: Self) -> Self
  func intersect(other: Self) -> Self
  func exclusiveOr(other: Self) -> Self

  var isEmpty: Bool { get }
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
    S : SequenceType where S.Generator.Element == Basis
  >(other: S) -> Bool {
    return !other._prext_contains { !self.contains($0) }
  }

  final func subtract(other: Self) -> Self {
    return self.intersect(self.exclusiveOr(other))
  }
}

extension SetAlgebraType where Self : CollectionType {
  final func isStrictSupersetOf(other: Self) -> Bool {
    return count(self) > count(other) && self.isSupersetOf(other)
  }

  final func isStrictSubsetOf(other: Self) -> Bool {
    return other.isStrictSupersetOf(self)
  }
}

protocol MutableSetAlgebraBaseType {
  typealias Basis
  init()
  init<S : SequenceType where S.Generator.Element == Basis>(_ sequence: S)

  mutating func subtractInPlace(other: Self)
  
  mutating func unionInPlace<
    S : SequenceType where S.Generator.Element == Basis
  >(sequence: S)
  
  mutating func subtractInPlace<
    S : SequenceType where S.Generator.Element == Basis
  >(sequence: S)

  mutating func exclusiveOrInPlace<
    S : SequenceType where S.Generator.Element == Basis
  >(sequence: S)
}

protocol MutableSetAlgebraType : SetAlgebraType, MutableSetAlgebraBaseType {
  mutating func insert(member: Basis)
  mutating func remove(member: Basis) -> Basis?
  mutating func unionInPlace(other: Self)
  mutating func intersectInPlace(other: Self)
  mutating func exclusiveOrInPlace(other: Self)
}

extension MutableSetAlgebraType {
  init<S : SequenceType where S.Generator.Element == Basis>(_ sequence: S) {
    self.init()
    for e in sequence { insert(e) }
  }
  
  final mutating func subtractInPlace(other: Self) {
    self.intersectInPlace(self.exclusiveOr(other))
  }
  
  final mutating func unionInPlace<
    S : SequenceType where S.Generator.Element == Basis
  >(sequence: S) {
    for x in sequence { insert(x) }
  }
  
  final mutating func subtractInPlace<
    S : SequenceType where S.Generator.Element == Basis
  >(sequence: S) {
    for x in sequence { remove(x) }
  }
  
  final mutating func exclusiveOrInPlace<
    S : SequenceType where S.Generator.Element == Basis
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
  typealias Basis = Element
}

/*
extension MutableSetAlgebraType {
  init<S : SequenceType where S.Generator.Element == T>(_ sequence: S) {
    self.init()
    for e in sequence { insert(e) }
  }
  
  func isSubsetOf<
    S : SequenceType where S.Generator.Element == T
  >(sequence: S) -> Bool

  func union(other: Self) -> Self
  func intersect(other: Self) -> Self
  func exclusiveOr(other: Self) -> Self

  func isDisjointWith<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Bool
  func isStrictSubsetOf<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Bool
  func isStrictSubsetOf(other: Self) -> Bool
  func isStrictSupersetOf<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Bool
  func union<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Set<T>

  func subtract<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Set<T>
  func intersect<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Set<T>
  func exclusiveOr<S : SequenceType where S.Generator.Element == T>(sequence: S) -> Set<T>
  
  mutating func unionInPlace<S : SequenceType where S.Generator.Element == T>(sequence: S)
  mutating func subtractInPlace<S : SequenceType where S.Generator.Element == T>(sequence: S)
  mutating func intersectInPlace<S : SequenceType where S.Generator.Element == T>(sequence: S)
  mutating func exclusiveOrInPlace<S : SequenceType where S.Generator.Element == T>(sequence: S)
}
*/

/// Elements of this set can be instances of any enumeration with Int
/// as its raw-value type.
public struct OptionSet<
  Element: RawRepresentable where Element.RawValue == Int
> : MyRawOptionSetType
{
  public init() {
    self.rawValue = 0
  }

  // This one shouldn't be necessary because of the one below, right?
  public init(_ element: Element) {
    self.rawValue = 1 << RawValue(element.rawValue)
  }

  public init(_ elements: Element...) {
    self.rawValue = elements.reduce(0 as RawValue) {
      $0 | (1 << RawValue($1.rawValue))
    }
  }

  //===--- protocol requirements ------------------------------------------===//
  public typealias RawValue = UIntMax       // RawRepresentable
  public let rawValue: RawValue             // RawRepresentable

  public init(nilLiteral: ()) {             // NilLiteralConvertible
    self.init()
  }

  public static var allZeros: OptionSet {   // BitwiseOperationsType
    return OptionSet()
  }
  
  public init(rawValue: RawValue) {         // RawOptionSetType
    self.rawValue = rawValue
  }
}

extension OptionSet : CustomDebugStringConvertible {
  public var debugDescription: String {
    return "OptionSet(rawValue: 0b\(String(rawValue, radix: 2)))"
  }
}

//===--- Tests ------------------------------------------------------------===//
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
  println(y)

  let z = x | y
  expectEqual(OptionSet(.one, .two, .four), z)
  
  expectEqual(nil, ~z & z)
}

runAllTests()
