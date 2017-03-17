//===--- AnyUnicode.swift -------------------------------------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

extension _UTF16StringStorage {
  var characters: _UnicodeViews<_UTF16StringStorage, UTF16>.CharacterView {
    return _UnicodeViews(self, UTF16.self).characters
  }
}

extension _Latin1StringStorage {
  var characters: LazyMapBidirectionalCollection<_Latin1StringStorage, Character> {
    return self.lazy.map { Character(UnicodeScalar(UInt32($0))!) }
  }
}

import StdlibUnittest

protocol AnyUnicodeIndex_ {
  var codeUnitOffset: Int64 { get }
}

func == <T: AnyUnicodeIndex_>(l: T, r: T) -> Bool {
  return l.codeUnitOffset == r.codeUnitOffset
}

func < <T: AnyUnicodeIndex_>(l: T, r: T) -> Bool {
  return l.codeUnitOffset < r.codeUnitOffset
}

// FIXME: Not sure if we want to expose the existential versions of these
func ==(l: AnyUnicodeIndex_, r: AnyUnicodeIndex_) -> Bool {
  return l.codeUnitOffset == r.codeUnitOffset
}

func < (l: AnyUnicodeIndex_, r: AnyUnicodeIndex_) -> Bool {
  return l.codeUnitOffset < r.codeUnitOffset
}

struct UnicodeIndex : Comparable, AnyUnicodeIndex_ {
  var codeUnitOffset: Int64 { return base.codeUnitOffset }
  
  var base: AnyUnicodeIndex_
  init<Base: AnyUnicodeIndex_>(_ base: Base) {
    self.base = base
  }

  struct Adapter<BaseView: UnicodeView> : Comparable, AnyUnicodeIndex_
  {
    init(_ base: BaseView.Index) { self.base = base }
    var codeUnitOffset : Int64 { return BaseView.codeUnitOffset(of: base) }
    let base: BaseView.Index
  }
}

struct SimpleUnicodeIndex : Comparable, AnyUnicodeIndex_ {
  var codeUnitOffset: Int64
  static func == (l: SimpleUnicodeIndex, r: SimpleUnicodeIndex) -> Bool {
    return l.codeUnitOffset == r.codeUnitOffset
  }
  static func < (l: SimpleUnicodeIndex, r: SimpleUnicodeIndex) -> Bool {
    return l.codeUnitOffset < r.codeUnitOffset
  }
}

protocol AnyCodeUnits_ {
  typealias IndexDistance = Int64
  typealias Index = Int64
  typealias Element = UInt32
  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after: Index) -> Index
  func index(before: Index) -> Index
  func index(_ i: Index, offsetBy: Int64) -> Index
  subscript(i: Index) -> Element { get }
  //  subscript(r: Range<Index>) -> AnyCodeUnits { get }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

struct AnyCodeUnits : RandomAccessCollection, AnyCodeUnits_ {
  typealias IndexDistance = Int64
  typealias Index = Int64
  typealias Element = UInt32
  
  let base: AnyCodeUnits_

  // FIXME: associated type deduction seems to need a hint here.
  typealias Indices = DefaultRandomAccessIndices<AnyCodeUnits>
  var startIndex: Index { return base.startIndex }
  var endIndex: Index { return base.endIndex }
  func index(after i: Index) -> Index { return base.index(after: i) }
  func index(before i: Index) -> Index { return base.index(before: i) }
  func index(_ i: Index, offsetBy: Int64) -> Index { return base.index(i, offsetBy: i) }
  subscript(i: Index) -> Element { return base[i] }

  init<C: RandomAccessCollection>(_ c: C)
  where C.Iterator.Element : UnsignedInteger {
    base = ZeroExtender(base: c)
  }
  
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? { return try base.withExistingUnsafeBuffer(body) }

  /// Adapts any random access collection of unsigned integer to AnyCodeUnits_
  struct ZeroExtender<
    Base: RandomAccessCollection
  > where Base.Iterator.Element : UnsignedInteger {
    let base: Base
  }  
}

extension AnyCodeUnits.ZeroExtender : RandomAccessCollection, AnyCodeUnits_ {
  typealias IndexDistance = Int64
  typealias Index = Int64
  typealias Element = UInt32
  // FIXME: associated type deduction seems to need a hint here.
  typealias Indices = DefaultRandomAccessIndices<
    AnyCodeUnits.ZeroExtender<Base>>
  
  var startIndex: Index { return 0 }
  var endIndex: Index { return numericCast(base.count) }
  
  func index(after i: Index) -> Index {
    return i + 1
  }
  
  func index(before i: Index) -> Index {
    return i - 1
  }
  
  func index(_ i: Index, offsetBy n: Int64) -> Index {
    return i + n
  }
  
  subscript(i: Index) -> Element {
    return numericCast(base[base.index(atOffset: i)])
  }

  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer {
      try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
    }.flatMap { $0 }
  }
}

protocol AnyUnicodeView_ {
  var startIndex: UnicodeIndex { get }
  var endIndex: UnicodeIndex { get }
  func index(after: UnicodeIndex) -> UnicodeIndex
  func index(before: UnicodeIndex) -> UnicodeIndex
  func index(_ i: UnicodeIndex, offsetBy: Int64) -> UnicodeIndex
  func distance(from i: UnicodeIndex, to j: UnicodeIndex) -> Int64
}

protocol UnicodeViewWithCommonIndex : BidirectionalCollection, AnyUnicodeView_ {
}

extension UnicodeViewWithCommonIndex {
  static func codeUnitOffset(of i: UnicodeIndex) -> Int64 {
    return i.codeUnitOffset
  }
}

protocol AnyUnicodeViewAdapter : AnyUnicodeView_ {
  var anyBase: AnyUnicodeView_ { get }
}

extension AnyUnicodeViewAdapter {
  var startIndex: UnicodeIndex { return anyBase.startIndex }
  var endIndex: UnicodeIndex { return anyBase.endIndex }
  func index(after i: UnicodeIndex) -> UnicodeIndex { return anyBase.index(after: i) }
  func index(before i: UnicodeIndex) -> UnicodeIndex { return anyBase.index(before: i) }
  func index(_ i: UnicodeIndex, offsetBy n: Int64) -> UnicodeIndex {
    return anyBase.index(i, offsetBy: n)
  }
  func distance(from i: UnicodeIndex, to j: UnicodeIndex) -> Int64 {
    return anyBase.distance(from: i, to: j)
  }
}

protocol UnicodeViewAdapter : BidirectionalCollection {
  associatedtype Base : UnicodeView
  var base: Base { get }
}

extension UnicodeViewAdapter {
  var startIndex: UnicodeIndex {
    return _index(base.startIndex)
  }
  
  var endIndex: UnicodeIndex {
    return _index(base.endIndex)
  }

  internal func _index(_ i: Base.Index) -> UnicodeIndex {
    return UnicodeIndex(UnicodeIndex.Adapter<Base>(i))
  }
  
  internal func _unwrap(_ i: UnicodeIndex) -> Base.Index {
    if let j = i.base as? UnicodeIndex.Adapter<Base> {
      return j.base
    }
    return base.index(atCodeUnitOffset: i.codeUnitOffset)
  }
  
  subscript(i: UnicodeIndex) -> Base.Iterator.Element {
    return base[_unwrap(i)]
  }

  func index(after i: UnicodeIndex) -> UnicodeIndex {
    return _index(base.index(after: _unwrap(i)))
  }
  
  func index(before i: UnicodeIndex) -> UnicodeIndex {
    return _index(base.index(before: _unwrap(i)))
  }
  
  func index(_ i: UnicodeIndex, offsetBy n: Int64) -> UnicodeIndex {
    return _index(base.index(_unwrap(i), offsetBy: numericCast(n)))
  }
  
  func distance(from i: UnicodeIndex, to j: UnicodeIndex) -> Int64 {
    return numericCast(base.distance(from: _unwrap(i), to: _unwrap(j)))
  }

  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Base.Iterator.Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer(body)
  }
}

protocol CodeUnitAdapter : RandomAccessCollection {
  associatedtype Base : RandomAccessCollection
  var base: Base { get }
}

extension CodeUnitAdapter {
  var startIndex: UnicodeIndex {
    return UnicodeIndex(SimpleUnicodeIndex(codeUnitOffset: 0))
  }
  
  var endIndex: UnicodeIndex {
    return UnicodeIndex(SimpleUnicodeIndex(codeUnitOffset: numericCast(base.count)))
  }
  
  func index(after i: UnicodeIndex) -> UnicodeIndex {
    return UnicodeIndex(SimpleUnicodeIndex(codeUnitOffset: i.codeUnitOffset + 1))
  }
  
  func index(before i: UnicodeIndex) -> UnicodeIndex {
    return UnicodeIndex(SimpleUnicodeIndex(codeUnitOffset: i.codeUnitOffset - 1))
  }
  
  func index(_ i: UnicodeIndex, offsetBy n: Int64) -> UnicodeIndex {
    return UnicodeIndex(SimpleUnicodeIndex(codeUnitOffset: i.codeUnitOffset + n))
  }
  
  func distance(from i: UnicodeIndex, to j: UnicodeIndex) -> Int64 {
    return j.codeUnitOffset - i.codeUnitOffset
  }
}

protocol AnyUTF16_ : AnyUnicodeView_ {
  typealias Element = UInt16
  subscript(i: UnicodeIndex) -> Element { get }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

struct AnyUTF16 : UnicodeViewWithCommonIndex, AnyUnicodeViewAdapter {
  let base: AnyUTF16_
  internal var anyBase: AnyUnicodeView_ { return base }
  
  typealias Element = UInt16
  typealias Index = UnicodeIndex

  subscript(i: Index) -> Element { return base[i] }
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer(body)
  }
}

/// Construct from (possibly zero-extended) code units
extension AnyUTF16 {
  init<Base: RandomAccessCollection>(_ base: Base)
  where Base.Iterator.Element : UnsignedInteger {
    self.base = ZeroExtender(base: base)
  }

  /// Adapts any random access collection of unsigned integer to AnyUTF16_
  struct ZeroExtender<Base: RandomAccessCollection> : AnyUTF16_, CodeUnitAdapter
  where Base.Iterator.Element : UnsignedInteger {
    let base: Base

    subscript(i: Index) -> Element {
      return numericCast(base[base.index(atOffset: i.codeUnitOffset)])
    }

    public func withExistingUnsafeBuffer<R>(
      _ body: (UnsafeBufferPointer<Element>) throws -> R
    ) rethrows -> R? {
      return try base.withExistingUnsafeBuffer {
        try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
      }.flatMap { $0 }
    }
  }
}

extension AnyUTF16 {
  init<Base: UnicodeView>(_ base: Base)
  where Base.Iterator.Element == UInt16 {
    self.base = Eraser(base)
  }

  /// Adapts any random access collection of unsigned integer to AnyUTF16_
  struct Eraser<Base: UnicodeView> : AnyUTF16_, UnicodeViewAdapter
  where Base.Iterator.Element == UInt16 {
    let base: Base

    init(_ base: Base) { self.base = base }
    
    subscript(i: Index) -> Element {
      return base[_unwrap(i)]
    }

    public func withExistingUnsafeBuffer<R>(
      _ body: (UnsafeBufferPointer<Element>) throws -> R
    ) rethrows -> R? {
      return try base.withExistingUnsafeBuffer {
        try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
      }.flatMap { $0 }
    }
  }
}

extension AnyUTF16 {
  init<CodeUnits: RandomAccessCollection, Encoding: UnicodeEncoding>(
    transcoding codeUnits: CodeUnits, from e: Encoding.Type
  )
  where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
    self.base = Eraser(
      _UnicodeViews(codeUnits[...], Encoding.self).transcoded(to: UTF16.self))
  }
}

#if false
protocol AnyUnicodeScalars_ : AnyUnicodeView_ {
  typealias Element = UnicodeScalar
  subscript(i: UnicodeIndex) -> Element { get }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

struct AnyUnicodeScalars : UnicodeViewCollection, AnyUnicodeViewAdapter {
  let base: AnyUnicodeScalars_
  internal var anyBase: AnyUnicodeView_ { return base }
  
  typealias Element = UnicodeScalar
  typealias Index = UnicodeIndex
  typealias IndexDistance = Int64
  
  subscript(i: Index) -> Element { return base[i] }
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer(body)
  }

  init<C: UnicodeViewCollection & AnyUnicodeScalars_>(_ c: C)
  where C.Iterator.Element : UnsignedInteger {
    base = Adapter(base: c)
  }

  struct Adapter<
    Base: UnicodeViewCollection & AnyUnicodeScalars_
  > where Base.Iterator.Element : UnsignedInteger {
    let base: Base
  }
}

/// Adapts any bidirectional collection of unsigned integer to AnyUnicodeScalars_
extension AnyUnicodeScalars.Adapter : UnicodeViewAdapter, AnyUnicodeScalars_ {
  typealias Index = UnicodeIndex

  var endIndex: Index { return base.endIndex }
  var startIndex: Index { return base.startIndex }
  
  subscript(i: Index) -> UnicodeScalar {
    return base[i]
  }

  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer {
      try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
    }.flatMap { $0 }
  }
}

protocol AnyUnicodeScalars_ : AnyUnicodeView_ {
  typealias Element = UnicodeScalar
  typealias IndexDistance = Int64
  typealias Index = SimpleUnicodeIndex
  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after: Index) -> Index
  func index(before: Index) -> Index
  subscript(i: Index) -> Element { get }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

struct AnyUnicodeScalars : UnicodeViewCollection, AnyUnicodeScalars_ {
  let base: AnyUnicodeScalars_
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = AnyUnicodeScalars_.Element
  var startIndex: Index { return base.startIndex }
  var endIndex: Index { return base.endIndex }
  func index(after i: Index) -> Index { return base.index(after: i) }
  func index(before i: Index) -> Index { return base.index(before: i) }
  subscript(i: Index) -> Element { return base[i] }
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer(body)
  }

  init<C: BidirectionalCollection>(_ c: C)
  where C.Iterator.Element == UnicodeScalar {
    base = Adapter(base: c)
  }

  struct Adapter<
    Base: BidirectionalCollection
  > where Base.Iterator.Element == UnicodeScalar {
    let base: Base
  }
}

/// Adapts any bidirectional collection of unicode scalar values to
/// AnyUnicodeScalars_
extension AnyUnicodeScalars.Adapter
  : BidirectionalCollection, AnyUnicodeScalars_ 
{
  typealias IndexDistance = Int64
  typealias Index = SimpleUnicodeIndex
  typealias Element = AnyUnicodeScalars_.Element
  
  var startIndex: Index { return Index(offset: 0) }
  var endIndex: Index { return Index(offset: numericCast(base.count)^) }
  
  func index(after i: Index) -> Index {
    return Index(offset: numericCast(
        base.offset(of: base.index(after: base.index(atOffset: i.offset)))))
  }
  
  func index(before i: Index) -> Index {
    return Index(offset: numericCast(
        base.offset(of: base.index(before: base.index(atOffset: i.offset)))))
  }
  
  func index(_ i: Index, offsetBy n: Int64) -> Index {
    return Index(offset: numericCast(
      base.offset(
        of: base.index(base.index(atOffset: i.offset),
            offsetBy: n^))))
  }
  
  subscript(i: Index) -> Element {
    return base[base.index(atOffset: i.offset)]
  }

  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer {
      try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
    }.flatMap { $0 }
  }
}

protocol AnyUnicodeBidirectionalUInt32_ {
  typealias IndexDistance = Int64
  typealias Index = SimpleUnicodeIndex
  typealias Element = UInt32
  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after: Index) -> Index
  func index(before: Index) -> Index
  subscript(i: Index) -> Element { get }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

// Adapts any bidirectional collection of UInt32 to AnyUnicodeBidirectionalUInt32_
struct AnyUnicodeBidirectionalUInt32 : BidirectionalCollection, AnyUnicodeBidirectionalUInt32_ {
  let base: AnyUnicodeBidirectionalUInt32_
  typealias IndexDistance = Int64
  typealias Index = SimpleUnicodeIndex
  typealias Element = UInt32
  var startIndex: Index { return base.startIndex }
  var endIndex: Index { return base.endIndex }
  func index(after i: Index) -> Index { return base.index(after: i) }
  func index(before i: Index) -> Index { return base.index(before: i) }
  subscript(i: Index) -> Element { return base[i] }
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer {
      try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
    }.flatMap { $0 }
  }

  struct ZeroExtender<
    Base: BidirectionalCollection
  > where Base.Iterator.Element : UnsignedInteger {
    let base: Base
  }

  init<C: BidirectionalCollection>(_ c: C)
  where C.Iterator.Element : UnsignedInteger {
    base = ZeroExtender(base: c)
  }
}

/// Adapts any bidirectional collection of unsigned integer to AnyUnicodeBidirectionalUInt32_
extension AnyUnicodeBidirectionalUInt32.ZeroExtender
  : BidirectionalCollection, AnyUnicodeBidirectionalUInt32_ {
  typealias IndexDistance = Int64
  typealias Index = SimpleUnicodeIndex
  typealias Element = UInt32
  

  var startIndex: Index { return Index(offset: 0) }
  var endIndex: Index { return Index(offset: base.count^) }
  
  func index(after i: Index) -> Index {
    return Index(offset: numericCast(
        base.offset(of: base.index(after: base.index(atOffset: i.offset)))))
  }
  
  func index(before i: Index) -> Index {
    return Index(offset: numericCast(
        base.offset(of: base.index(before: base.index(atOffset: i.offset)))))
  }
  
  func index(_ i: Index, offsetBy n: Int64) -> Index {
    return Index(offset: numericCast(
      base.offset(
        of: base.index(base.index(atOffset: i.offset),
            offsetBy: n^))))
  }
  
  subscript(i: Index) -> Element {
    return numericCast(base[base.index(atOffset: i.offset)])
  }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer {
      try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
    }.flatMap { $0 }
  }
}

protocol AnyCharacters_ {
  typealias IndexDistance = Int64
  typealias Index = SimpleUnicodeIndex
  typealias Element = Character
  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after: Index) -> Index
  func index(before: Index) -> Index
  subscript(i: Index) -> Element { get }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

struct AnyCharacters : BidirectionalCollection, AnyCharacters_ {
  let base: AnyCharacters_
  typealias IndexDistance = Int64
  typealias Index = SimpleUnicodeIndex
  typealias Element = Character
  var startIndex: Index { return base.startIndex }
  var endIndex: Index { return base.endIndex }
  func index(after i: Index) -> Index { return base.index(after: i) }
  func index(before i: Index) -> Index { return base.index(before: i) }
  subscript(i: Index) -> Element { return base[i] }
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer {
      try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
    }.flatMap { $0 }
  }

  struct Adapter<Base: BidirectionalCollection>
  where Base.Iterator.Element == Character,
  // FIXME: we may not want to keep this constraint
  Base.Index : SignedInteger { 
    let base: Base
  }

  init<C: BidirectionalCollection>(_ c: C)
  where C.Iterator.Element == Character,
  // FIXME: we may not want to keep this constraint
  C.Index : SignedInteger { 
    base = Adapter(base: c)
  }
}

extension AnyCharacters.Adapter : AnyCharacters_, BidirectionalCollection {
  typealias IndexDistance = Int64
  typealias Index = SimpleUnicodeIndex
  typealias Element = Character

  var startIndex: Index { return Index(offset: base.startIndex^) }
  var endIndex: Index { return Index(offset: base.endIndex^) }
  
  func index(after i: Index) -> Index {
    return Index(offset: numericCast(base.index(after: i.offset^)))
  }
  
  func index(before i: Index) -> Index {
    return Index(offset: numericCast(base.index(before: i.offset^)))
  }
  
  func index(_ i: Index, offsetBy n: Int64) -> Index {
    return Index(
      offset: numericCast(
        base.index(i.offset^, offsetBy: n^)))
  }
  
  subscript(i: Index) -> Element {
    return base[i.offset^]
  }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer {
      try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
    }.flatMap { $0 }
  }
}
#endif

protocol AnyUnicode : Swift._AnyUnicode {
  var codeUnits: AnyCodeUnits { get }
  var rawUTF16: AnyUTF16 { get }
#if false
  var utf32: AnyUnicodeBidirectionalUInt32 { get }
  var fccNormalizedUTF16: AnyUTF16 { get }
  // FIXME: Can this be Random Access?  If all encodings use a single code unit
  // per ASCII character and can statelessly identify a code unit that
  // represents ASCII, then yes.  Look into, e.g. shift-JIS.
  var extendedASCII: AnyUnicodeBidirectionalUInt32 { get }
  var characters: AnyCharacters { get }
#endif
}

extension AnyUnicode {
  func isLatin1() -> Bool {
    return isKnownLatin1 || !rawUTF16.contains { $0 > 0xFF }
  }
  
  func isASCII() -> Bool {
    return isKnownASCII || !rawUTF16.contains { $0 > 0x7f }
  }
}

extension AnyUnicode {
  func isValidEncoding() -> Bool {
    return encoding.decodeForward(
      codeUnits, repairingIllFormedSequences: false
    ) { _ in }.errorCount == 0
  }
}

// Work around name collision ambiguity
extension _FixedFormatUnicode {
  internal var _codeUnits: CodeUnits { return codeUnits }
#if false
  internal var _characters: CharacterView { return characters }
  internal var _fccNormalizedUTF16: FCCNormalizedUTF16View {
    return fccNormalizedUTF16
  }
#endif
}

extension AnyUnicode
where Self : _FixedFormatUnicode,
Self.CodeUnits : RandomAccessCollection,
Self.CodeUnits.Iterator.Element : UnsignedInteger {
  var codeUnits: AnyCodeUnits {
    return AnyCodeUnits(self._codeUnits)
  }
}


extension AnyUnicode
where Self : _FixedFormatUnicode,
Self.CodeUnits : RandomAccessCollection,
Self.CodeUnits.Iterator.Element : UnsignedInteger,
Self.Encoding == Latin1 {
  var rawUTF16: AnyUTF16 { return AnyUTF16(self.codeUnits) }
}

extension AnyUnicode
where Self : _FixedFormatUnicode,
Self.CodeUnits : RandomAccessCollection,
Self.CodeUnits.Iterator.Element : UnsignedInteger,
Self.Encoding.EncodedScalar == UTF16.EncodedScalar {
  var rawUTF16: AnyUTF16 { return AnyUTF16(self.codeUnits) }
}

extension AnyUnicode
where Self : _FixedFormatUnicode,
Self.CodeUnits : RandomAccessCollection,
Self.CodeUnits.Iterator.Element : UnsignedInteger,
Self.RawUTF16View : BidirectionalCollection,
Self.RawUTF16View.Iterator.Element == UTF16.CodeUnit,
Self.RawUTF16View : UnicodeView,
/*Self.FCCNormalizedUTF16View.Iterator.Element : UnsignedInteger,*/
Self.CodeUnits.Index == Self.CodeUnits.SubSequence.Index, 
Self.CodeUnits.SubSequence : RandomAccessCollection, 
Self.CodeUnits.SubSequence == Self.CodeUnits.SubSequence.SubSequence, 
Self.CodeUnits.Iterator.Element == Self.CodeUnits.SubSequence.Iterator.Element, 
Self.CodeUnits.SubSequence.Iterator.Element == Self.Encoding.EncodedScalar.Iterator.Element
/*
,
Self.CharacterView.Iterator.Element == Character,
Self.CharacterView.Index : SignedInteger
*/
{
  var codeUnits: AnyCodeUnits {
    return AnyCodeUnits(self._codeUnits)
  }

  var rawUTF16: AnyUTF16 {
    return AnyUTF16(transcoding: self._codeUnits, from: Encoding.self)
  }
  
#if false
  // FIXME: this could be more efficient for encodings such as Latin1
  var utf32: AnyUnicodeBidirectionalUInt32 {
      return AnyUnicodeBidirectionalUInt32(
      _UnicodeViews(
        _codeUnits, Encoding.self
      ).transcoded(to: UTF32.self)
    )
  }
  var fccNormalizedUTF16: AnyUTF16 {
    return AnyUTF16(fccNormalizedUTF16 as FCCNormalizedUTF16View)
  }
  // FIXME: Could be more efficient generally
  var extendedASCII: AnyUnicodeBidirectionalUInt32 {
    return utf32
  }
  var characters: AnyCharacters {
    return AnyCharacters(_characters)
  }
#endif
}

class AnyUnicodeBox : AnyUnicode, FactoryInitializable {
  var encoding: AnyUnicodeEncoding.Type { fatalError("override me!") }
  var codeUnits: AnyCodeUnits { fatalError("override me!") }
  var rawUTF16: AnyUTF16 { fatalError("override me!") }
#if false
  var utf32: AnyUnicodeBidirectionalUInt32 { fatalError("override me!") }
  var fccNormalizedUTF16: AnyUTF16 { fatalError("override me!") }
#endif
  
#if false
  // FIXME: Can this be Random Access?  If all encodings use a single code unit
  // per ASCII character and can statelessly identify a code unit that
  // represents ASCII, then yes.  Look into, e.g. shift-JIS.
  var extendedASCII: AnyUnicodeBidirectionalUInt32 { fatalError("override me!") }
  var characters: AnyCharacters { fatalError("override me!") }
#endif
  var isKnownLatin1: Bool { fatalError("override me!") }
  var isKnownASCII: Bool { fatalError("override me!") }
  var isKnownValidEncoding: Bool { fatalError("override me!") }
  var isKnownFCCNormalized: Bool { fatalError("override me!") }
  var isKnownFCDForm: Bool { fatalError("override me!") }
  var isKnownNFDNormalized: Bool { fatalError("override me!") }
  var isKnownNFCNormalized: Bool { fatalError("override me!") }

  func isLatin1() -> Bool  { fatalError("override me!") }
  func isASCII() -> Bool { fatalError("override me!") }
  func isValidEncoding() -> Bool { fatalError("override me!") }

  class Instance<T: AnyUnicode> : AnyUnicodeBox {
    var base: T
    override var encoding: AnyUnicodeEncoding.Type { return base.encoding }
    override var codeUnits: AnyCodeUnits { return base.codeUnits }
    override var rawUTF16: AnyUTF16 { return base.rawUTF16 }
#if false
    override var utf32: AnyUnicodeBidirectionalUInt32 { return base.utf32 }
    override var fccNormalizedUTF16: AnyUTF16 { return base.fccNormalizedUTF16 }
    override var extendedASCII: AnyUnicodeBidirectionalUInt32 {
      return base.extendedASCII
    }
    override var characters: AnyCharacters { return base.characters }
#endif
    override var isKnownLatin1: Bool { return base.isKnownLatin1 }
    override var isKnownASCII: Bool { return base.isKnownASCII }
    override var isKnownValidEncoding: Bool { return base.isKnownValidEncoding }
    override var isKnownFCCNormalized: Bool { return base.isKnownFCCNormalized }
    override var isKnownFCDForm: Bool { return base.isKnownFCDForm }
    override var isKnownNFDNormalized: Bool { return base.isKnownNFDNormalized }
    override var isKnownNFCNormalized: Bool { return base.isKnownNFCNormalized }
    override func isLatin1() -> Bool  { return base.isLatin1() }
    override func isASCII() -> Bool  { return base.isASCII() }
    override func isValidEncoding() -> Bool  { return base.isValidEncoding() }
    init(_ x: T) {
      base = x
      super.init(fromSubclass: ())
    }
  }

  convenience init<T: AnyUnicode>(wrapping x: T) {
    self.init(Instance(x))
  }
  
  init(fromSubclass: ()) {}
}

  
enum AnyStringContents {
case utf16(_UTF16StringStorage)
case latin1(_Latin1StringStorage)
case any(AnyUnicodeBox)
}

extension _UTF16StringStorage : AnyUnicode {
  
}

extension _Latin1StringStorage : AnyUnicode {
  
}

extension AnyStringContents : AnyUnicode {
  var encoding: AnyUnicodeEncoding.Type {
    switch self {
    case .utf16(let storage):
      return storage.isKnownValidEncoding ? ValidUTF16.self : UTF16.self
    case .latin1(_):
      return Latin1.self
    case .any(let base):
      return base.encoding
    }
  }
  var rawUTF16: AnyUTF16 {
    switch self {
    case .utf16(let storage):
      return AnyUTF16(storage)
    case .latin1(let storage):
      return AnyUTF16(storage)
    case .any(let base):
      return base.rawUTF16
    }
  }

#if false
  var utf32: AnyUnicodeBidirectionalUInt32 {
    switch self {
    case .utf16(let storage):
      return AnyUnicodeBidirectionalUInt32(
        _UnicodeViews(storage, UTF16.self).transcoded(to: UTF32.self)
      )
    case .latin1(let storage):
      return AnyUnicodeBidirectionalUInt32(storage)
    case .any(let base):
      return base.utf32
    }
  }

  var extendedASCII: AnyUnicodeBidirectionalUInt32 {
    switch self {
    case .utf16(let storage):
      return AnyUnicodeBidirectionalUInt32(storage)
    case .latin1(let storage):
      return AnyUnicodeBidirectionalUInt32(storage)
    case .any(let base):
      return base.extendedASCII
    }
  }

  var characters: AnyCharacters {
    switch self {
    case .utf16(let storage):
      return AnyCharacters(storage.characters)
    case .latin1(let storage):
      return AnyCharacters(storage.characters)
    case .any(let base):
      return base.characters
    }
  }

  var fccNormalizedUTF16: AnyUTF16 {
    switch self {
    case .utf16(let storage):
      return storage.fccNormalizedUTF16
    case .latin1(let storage):
      return storage.fccNormalizedUTF16
    case .any(let base):
      return base.fccNormalizedUTF16
    }
  }
#endif

  var codeUnits: AnyCodeUnits {
    switch self {
    case .utf16(let storage):
      return AnyCodeUnits(storage)
    case .latin1(let storage):
      return AnyCodeUnits(storage)
    case .any(let base):
      return base.codeUnits
    }
  }

  var isKnownASCII: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownASCII
    case .latin1(let storage):
      return storage.isKnownASCII
    case .any(let base):
      return base.isKnownASCII
    }
  }

  var isKnownLatin1: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownLatin1
    case .latin1(let storage):
      return storage.isKnownLatin1
    case .any(let base):
      return base.isKnownLatin1
    }
  }

  var isKnownValidEncoding: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownValidEncoding
    case .latin1(let storage):
      return storage.isKnownValidEncoding
    case .any(let base):
      return base.isKnownValidEncoding
    }
  }

  var isKnownFCCNormalized: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownFCCNormalized
    case .latin1(let storage):
      return storage.isKnownFCCNormalized
    case .any(let base):
      return base.isKnownFCCNormalized
    }
  }

  var isKnownFCDForm: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownFCDForm
    case .latin1(let storage):
      return storage.isKnownFCDForm
    case .any(let base):
      return base.isKnownFCDForm
    }
  }

  var isKnownNFDNormalized: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownNFDNormalized
    case .latin1(let storage):
      return storage.isKnownNFDNormalized
    case .any(let base):
      return base.isKnownNFDNormalized
    }
  }

  var isKnownNFCNormalized: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownNFCNormalized
    case .latin1(let storage):
      return storage.isKnownNFCNormalized
    case .any(let base):
      return base.isKnownNFCNormalized
    }
  }

  init<T: AnyUnicode>(_ x: T) {
    if let s = x as? _Latin1StringStorage {
      self = .latin1(s)
    }
    else if let s = x as? _UTF16StringStorage {
      self = .utf16(s)
    }
    else {
      self = .any(AnyUnicodeBox(wrapping: x))
    }
  }
}

print(MemoryLayout<AnyStringContents>.size)

var suite = TestSuite("AnyUnicode")
suite.test("basics") {
  let x = AnyUTF16.ZeroExtender(base: Array(3...7) as [UInt16])
  let y = AnyUTF16.ZeroExtender(base: Array(3...7) as [UInt8])
  expectTrue(x.elementsEqual(y))
}

suite.test("AnyStringContents") {
  let sample = "abcdefghijklmnopqrstuvwxyz\n"
  + "🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\n"
  + "Σὲ 👥🥓γνωρίζω ἀπὸ τὴν κόψη χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!\n"
  + "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,\n"
  + "გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო\n"
  + "Зарегистрируйтесь сейчас на Десятую Международную Конференцию по\n"
  + "  ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่\n"
  + "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"

  var s = AnyStringContents(_UTF16StringStorage(sample.utf16))
#if false
  for c in s.characters {
    print("\(c)|", terminator: "")
  }
#endif
}
runAllTests()
