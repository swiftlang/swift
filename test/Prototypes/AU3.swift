//===--- AU3.swift --------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: rm -rf %t && mkdir -p %t && %gyb -DWORD_BITS=%target-ptrsize %s -o %t/out.swift
// RUN: %line-directive %t/out.swift -- %target-build-swift %t/out.swift -o %t/a.out -Onone
// RUN: %line-directive %t/out.swift -- %target-run %t/a.out

import StdlibUnittest

public protocol UnicodeView : BidirectionalCollection {
  func wrap(_: Index) -> AnyUnicodeIndex_
  func unwrap(_: AnyUnicodeIndex_) -> Index
}

//===--- Wrapper ----------------------------------------------------------===//

/// A type that forwards its implementation to an instance of `Base`.
///
/// Typical usage is to create protocol extensions of Wrapper constrained on
/// other conformances of `Self` and `Base`.
public protocol Wrapper {
  associatedtype Base
  var base: Base { get }
}

/// Wrappers that present a `BidirectionalCollection` instance as another
/// `BidirectionalCollection` forward all implementation directly to base
public extension Wrapper
where Base : BidirectionalCollection,
Self : BidirectionalCollection {
  var startIndex: Base.Index { return base.startIndex }
  var endIndex: Base.Index { return base.endIndex }
  func formIndex(after i: inout Base.Index) {
    base.formIndex(after: &i)
  }
  func index(after i: Base.Index) -> Base.Index {
    return base.index(after: i)
  }
  func formIndex(before i: inout Base.Index) {
    base.formIndex(before: &i)
  }
  func index(before i: Base.Index) -> Base.Index {
    return base.index(before: i)
  }
  func formIndex(_ i: inout Base.Index, offsetBy n: Base.IndexDistance) {
    base.formIndex(&i, offsetBy: n)
  }
  func index(_ i: Base.Index, offsetBy n: Base.IndexDistance) -> Base.Index {
    return base.index(i, offsetBy: n)
  }
  func index(
    _ i: Base.Index,
    offsetBy n: Base.IndexDistance,
    limitedBy limit: Base.Index
  ) -> Base.Index? {
    return base.index(i, offsetBy: n, limitedBy: limit)
  }
  func distance(from i: Base.Index, to j: Base.Index) -> Base.IndexDistance {
    return base.distance(from: i, to: j)
  }
  var count: Base.IndexDistance {
    return base.count
  }
  var underestimatedCount: Int {
    return base.underestimatedCount
  }
  public subscript(i: Base.Index) -> Base.Iterator.Element {
    return base[i]
  }
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Base.Iterator.Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer(body)
  }
  public var first: Base.Iterator.Element? { return base.first }
  public var last: Base.Iterator.Element? { return base.last }

  public func map<T>(_ transform: (Base.Iterator.Element) throws -> T) rethrows -> [T] {
    return try base.map(transform)
  }
  public func filter(
    _ includedInResult: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    return try base.filter(includedInResult)
  }
  public func forEach(_ body: (Base.Iterator.Element) throws -> Void) rethrows {
    try base.forEach(body)
  }
  public func _copyToContiguousArray() -> ContiguousArray<Base.Iterator.Element> {
    return base._copyToContiguousArray()
  }
}


//===--- AnyUnicodeIndex --------------------------------------------------===//

/// Type eraser for indices into type-erased unicode views.
///
/// Not the underlying index, but a wrapper that UnicodeViews can create for it.
public protocol AnyUnicodeIndex_ {
  var codeUnitOffset: Int64 { get }
  func isLess(than other: AnyUnicodeIndex_) -> Bool
  func isEqual(to other: AnyUnicodeIndex_) -> Bool
}

/// Default support for comparison
public extension AnyUnicodeIndex_ where Self : Comparable {
  func isLess(than other: AnyUnicodeIndex_) -> Bool {
    if let o = other as? Self {
      return self < o
    }
    return self.codeUnitOffset < other.codeUnitOffset
  }
  func isEqual(to other: AnyUnicodeIndex_) -> Bool {
    if let o = other as? Self {
      return self == o
    }
    return self.codeUnitOffset == other.codeUnitOffset
  }
}

/// Wrapper for instances of AnyUnicodeIndex_ that makes it a suitable Index
/// type (Comparable has self-requirements).
public struct AnyUnicodeIndex : Comparable {
  public var codeUnitOffset: Int64 { return base.codeUnitOffset }
  public var base: AnyUnicodeIndex_
  public init(_ base: AnyUnicodeIndex_) { self.base = base }
  public static func < (lhs: AnyUnicodeIndex, rhs: AnyUnicodeIndex) -> Bool {
    return lhs.base.isLess(than: rhs.base)
  }
  public static func == (lhs: AnyUnicodeIndex, rhs: AnyUnicodeIndex) -> Bool {
    return lhs.base.isEqual(to: rhs.base)
  }
}
//===----------------------------------------------------------------------===//


//===--- AnyUnicodeView ---------------------------------------------------===//
public protocol AnyUnicodeView_ {
  var startIndex: AnyUnicodeIndex_ { get }
  var endIndex: AnyUnicodeIndex_ { get }
  func formIndex(after: inout AnyUnicodeIndex_)
  func index(after: AnyUnicodeIndex_) -> AnyUnicodeIndex_
  func formIndex(before: inout AnyUnicodeIndex_)
  func index(before: AnyUnicodeIndex_) -> AnyUnicodeIndex_
  func formIndex(_ i: inout AnyUnicodeIndex_, offsetBy: Int64)
  func index(_ i: AnyUnicodeIndex_, offsetBy: Int64) -> AnyUnicodeIndex_
  func index(
    _ i: AnyUnicodeIndex_,
    offsetBy n: Int64,
    limitedBy limit: AnyUnicodeIndex_
  ) -> AnyUnicodeIndex_?
  func distance(from i: AnyUnicodeIndex_, to j: AnyUnicodeIndex_) -> Int64
  var count: Int64 { get }
  var underestimatedCount: Int { get }
}

/// Wrappers that present some `AnyUnicodeView_` instance as a
/// `BidirectionalCollection` unwrap indices and forward all implementation to
/// base
public extension Wrapper
where Base == AnyUnicodeView_,
Self : BidirectionalCollection {
  var startIndex: AnyUnicodeIndex { return AnyUnicodeIndex(base.startIndex) }
  var endIndex: AnyUnicodeIndex { return AnyUnicodeIndex(base.endIndex) }
  func formIndex(after i: inout AnyUnicodeIndex) {
    base.formIndex(after: &i.base)
  }
  func index(after i: AnyUnicodeIndex) -> AnyUnicodeIndex {
    return AnyUnicodeIndex(base.index(after: i.base))
  }
  func formIndex(before i: inout AnyUnicodeIndex) {
    base.formIndex(before: &i.base)
  }
  func index(before i: AnyUnicodeIndex) -> AnyUnicodeIndex {
    return AnyUnicodeIndex(base.index(before: i.base))
  }
  func formIndex(_ i: inout AnyUnicodeIndex, offsetBy n: Int64) {
    base.formIndex(&i.base, offsetBy: n)
  }
  func index(_ i: AnyUnicodeIndex, offsetBy n: Int64) -> AnyUnicodeIndex {
    return AnyUnicodeIndex(base.index(i.base, offsetBy: n))
  }
  func index(
    _ i: AnyUnicodeIndex,
    offsetBy n: Int64,
    limitedBy limit: AnyUnicodeIndex
  ) -> AnyUnicodeIndex? {
    return base.index(i.base, offsetBy: n, limitedBy: limit.base)
      .map(AnyUnicodeIndex.init)
  }
  func distance(from i: AnyUnicodeIndex, to j: AnyUnicodeIndex) -> Int64 {
    return base.distance(from: i.base, to: j.base)
  }
  var count: Int64 {
    return base.count
  }
  var underestimatedCount: Int {
    return base.underestimatedCount
  }
}


/// Wrappers that present some `UnicodeView` as an `AnyUnicodeView_` forward all
/// implementations by using `base` to `wrap`/`unwrap` indices and `numericCast`
/// to translate `IndexDistance`s.
public extension Wrapper
where Base : UnicodeView, Self : AnyUnicodeView_ {
  var startIndex: AnyUnicodeIndex_ { return base.wrap(base.startIndex) }
  var endIndex: AnyUnicodeIndex_ { return base.wrap(base.endIndex) }

  func formIndex(after i: inout AnyUnicodeIndex_) {
    var i1 = base.unwrap(i)
    base.formIndex(after: &i1)
    i = base.wrap(i1)
  }
  func index(after i: AnyUnicodeIndex_) -> AnyUnicodeIndex_ {
    return base.wrap(base.index(after: base.unwrap(i)))
  }
  func formIndex(before i: inout AnyUnicodeIndex_) {
    var i1 = base.unwrap(i)
    base.formIndex(before: &i1)
    i = base.wrap(i1)
  }
  func index(before i: AnyUnicodeIndex_) -> AnyUnicodeIndex_ {
    return base.wrap(base.index(before: base.unwrap(i)))
  }
  func formIndex(_ i: inout AnyUnicodeIndex_, offsetBy n: Int64) {
    var i1 = base.unwrap(i)
    base.formIndex(&i1, offsetBy: numericCast(n))
    i = base.wrap(i1)
  }
  func index(_ i: AnyUnicodeIndex_, offsetBy n: Int64) -> AnyUnicodeIndex_ {
    return base.wrap(base.index(base.unwrap(i), offsetBy: numericCast(n)))
  }
  func index(
    _ i: AnyUnicodeIndex_,
    offsetBy n: Int64,
    limitedBy limit: AnyUnicodeIndex_
  ) -> AnyUnicodeIndex_? {
    return base.index(
      base.unwrap(i), offsetBy: numericCast(n), limitedBy: base.unwrap(limit)
    ).map(base.wrap)
  }
  func distance(from i: AnyUnicodeIndex_, to j: AnyUnicodeIndex_) -> Int64 {
    return numericCast(base.distance(from: base.unwrap(i), to: base.unwrap(j)))
  }
  var count: Int64 {
    return numericCast(base.count)
  }
  var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public subscript(i: AnyUnicodeIndex_) -> Base.Iterator.Element {
    return base[base.unwrap(i)]
  }
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Base.Iterator.Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer(body)
  }
  public var first: Base.Iterator.Element? { return base.first }
  public var last: Base.Iterator.Element? { return base.last }

  public func map<T>(_ transform: (Base.Iterator.Element) throws -> T) rethrows -> [T] {
    return try base.map(transform)
  }
  public func filter(
    _ includedInResult: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    return try base.filter(includedInResult)
  }
  public func forEach(_ body: (Base.Iterator.Element) throws -> Void) rethrows {
    try base.forEach(body)
  }
  public func _copyToContiguousArray() -> ContiguousArray<Base.Iterator.Element> {
    return base._copyToContiguousArray()
  }
}

// Generate wrappers for specific element types.
% for Element in 'UInt16', 'UInt32', 'UnicodeScalar', 'Character':

//===--- Any${Element}Iterator --------------------------------------------===//
/// Type-eraser for Iterators over ${Element}s
public protocol Any${Element}Iterator_ {
  mutating func next() -> ${Element}?
}

public struct Any${Element}Iterator : IteratorProtocol {
  public var base: Any${Element}Iterator_
  public init(_ base: Any${Element}Iterator_) { self.base = base }
  public mutating func next() -> ${Element}? { return base.next() }

  public struct Adapter<Base : IteratorProtocol> : Any${Element}Iterator_
  where Base.Element == ${Element}
  {
    public mutating func next() -> ${Element}? { return base.next() }
    public var base: Base
  }
}

/// Wrappers that present a base `Sequence` as an `AnyUnicodeView_` adapt and
/// forward the base's `iterator`.
public extension Wrapper
where Self : AnyUnicodeView_,
Base : Sequence, Base.Iterator.Element == ${Element} {
  func makeIterator() -> Any${Element}Iterator_ {
    return Any${Element}Iterator.Adapter<Base.Iterator>(
      base: base.makeIterator())
  }
}
//===----------------------------------------------------------------------===//

//===--- Any${Element}UnicodeView -----------------------------------------===//

/// Type eraser for `UnicodeView`s with elements of type ${Element}.
public protocol Any${Element}UnicodeView_ : AnyUnicodeView_ {
  func makeIterator() -> Any${Element}Iterator_
  subscript(_: AnyUnicodeIndex_) -> ${Element} { get }
  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<${Element}>) throws -> R
  ) rethrows -> R?
  var first: ${Element}? { get }
  var last: ${Element}? { get }
  func map<T>(_ transform: (${Element}) throws -> T) rethrows -> [T]
  func filter(_ inResult: (${Element}) throws -> Bool) rethrows -> [${Element}]
  func forEach(_ body: (${Element}) throws -> Void) rethrows
  func _copyToContiguousArray() -> ContiguousArray<${Element}>
}

public struct Any${Element}UnicodeView : BidirectionalCollection, Wrapper {
  public typealias Iterator = Any${Element}Iterator
  public func makeIterator() -> Any${Element}Iterator {
    return Any${Element}Iterator(base2.makeIterator())
  }
  public subscript(i: AnyUnicodeIndex) -> ${Element} {
    return base2[i.base]
  }
  
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<${Element}>) throws -> R
  ) rethrows -> R? {
    return try base2.withExistingUnsafeBuffer(body)
  }
  
  public var first: ${Element}? { return base2.first }
  public var last: ${Element}? { return base2.last }
  public func map<T>(_ transform: (${Element}) throws -> T) rethrows -> [T] {
    return try base2.map(transform)
  }
  public func filter(
    _ includedInResult: (${Element}) throws -> Bool
  ) rethrows -> [${Element}] {
    return try base2.filter(includedInResult)
  }
  public func forEach(_ body: (${Element}) throws -> Void) rethrows {
    try base2.forEach(body)
  }
  public func _copyToContiguousArray() -> ContiguousArray<${Element}> {
    return base2._copyToContiguousArray()
  }
  // `base` must have exactly this signature to match the needed constrained
  // `Wrapper` extension, so we compute it and store the more refined
  // existential in `base2`.
  public var base: AnyUnicodeView_ { return base2 }
  public var base2: Any${Element}UnicodeView_
}

/// Adapt any `UnicodeView` with elements of type `Element`
extension Any${Element}UnicodeView {
  /// An adaptor used to wrap `Base`
  public struct AdaptBidirectional<Base: UnicodeView>
  : Any${Element}UnicodeView_, Wrapper
  where Base.Iterator.Element == ${Element} {
    public var base: Base
  }

  /// Creates an instance that wraps `base`.
  public init<Base: UnicodeView>(_ base: Base)
  where Base.Iterator.Element == ${Element} {
    self.base2 = AdaptBidirectional<Base>(base: base)
  }
}

//===----------------------------------------------------------------------===//

% end

public struct
MappedViewOfUnicodeCodeUnits<Base : RandomAccessCollection>
: Wrapper, UnicodeView {
  public var base: Base

  // FIXME: the compiler needs deduction help here, but shouldn't
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  
  struct AnyIndex : AnyUnicodeIndex_, Wrapper, Comparable {
    var base: Base.Index
    var codeUnitOffset_: Base.IndexDistance
    var codeUnitOffset: Int64 {
      return numericCast(codeUnitOffset_)
    }
  }
  
  public func wrap(_ i: Index) -> AnyUnicodeIndex_ {
    return AnyIndex(base: i, codeUnitOffset_: base.offset(of: i))
  }
  public func unwrap(_ i: AnyUnicodeIndex_) -> Index {
    return (i as? AnyIndex).map { $0.base }
    ?? base.index(atOffset: i.codeUnitOffset)
  }
}

public extension Wrapper
where Base : Comparable, Self : AnyUnicodeIndex_ {
  static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.base < rhs.base
  }
  static func == (lhs: Self, rhs: Self) -> Bool {
    return lhs.base == rhs.base
  }
}

extension _UnicodeViews.Scalars : UnicodeView {
  struct AnyIndex : AnyUnicodeIndex_, Wrapper, Comparable {
    var base: Index
    var codeUnitOffset: Int64 {
      return _UnicodeViews.Scalars.codeUnitOffset(of: base)
    }
  }
  
  public func wrap(_ i: Index) -> AnyUnicodeIndex_ {
    return AnyIndex(base: i)
  }
  public func unwrap(_ i: AnyUnicodeIndex_) -> Index {
    return (i as? AnyIndex).map { $0.base }
    ?? index(atCodeUnitOffset: i.codeUnitOffset)
  }
}

let sample = "abcdefghijklmnopqrstuvwxyz\n"
  + "🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\n"
  + "Σὲ 👥🥓γνωρίζω ἀπὸ τὴν κόψη χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!\n"
  + "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,\n"
  + "გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო\n"
  + "Зарегистрируйтесь сейчас на Десятую Международную Конференцию по\n"
  + "  ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่\n"
  + "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"


var suite = TestSuite("AU3")

suite.test("Bidirectional/UnicodeScalar") {
  let a = AnyUnicodeScalarUnicodeView(
    _UnicodeViews(Array(sample.utf16), ValidUTF16.self).scalars)
  expectEqualSequence(sample.unicodeScalars, a)
  var lastCodeUnitOffset = Int64.min
  // Make sure it works as a collection, too.
  for (s, i) in zip(sample.unicodeScalars, a.indices) {
    expectEqual(s, a[i])
    let o = i.codeUnitOffset
    expectLT(lastCodeUnitOffset, o)
    lastCodeUnitOffset = o
  }
}

suite.test("RandomAccess/UnicodeScalar") {
  let a = AnyUnicodeScalarUnicodeView(
    MappedViewOfUnicodeCodeUnits(base: Array(sample.unicodeScalars)))
  expectEqualSequence(sample.unicodeScalars, a)
  var lastCodeUnitOffset = Int64.min
  // Make sure it works as a collection, too.
  for (s, i) in zip(sample.unicodeScalars, a.indices) {
    expectEqual(s, a[i])
    let o = i.codeUnitOffset
    expectLT(lastCodeUnitOffset, o)
    lastCodeUnitOffset = o
  }
}

runAllTests()
