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

import StdlibUnittest

struct UnicodeIndex : Comparable {
  var offset: Int64
  static func == (l: UnicodeIndex, r: UnicodeIndex) -> Bool {
    return l.offset == r.offset
  }
  static func < (l: UnicodeIndex, r: UnicodeIndex) -> Bool {
    return l.offset < r.offset
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
    return numericCast(
      base.offset(of: base.index(after: base.index(atOffset: i))))
  }
  
  func index(before i: Index) -> Index {
    return numericCast(
      base.offset(of: base.index(before: base.index(atOffset: i))))
  }
  
  func index(_ i: Index, offsetBy n: Int64) -> Index {
    return numericCast(
      base.offset(
        of: base.index(base.index(atOffset: i),
          offsetBy: numericCast(n))))
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

protocol AnyUTF16_ {
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt16
  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after: Index) -> Index
  func index(before: Index) -> Index
  subscript(i: Index) -> Element { get }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

struct AnyUTF16 : BidirectionalCollection, AnyUTF16_ {
  let base: AnyUTF16_
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt16
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
  where C.Iterator.Element : UnsignedInteger {
    base = ZeroExtender(base: c)
  }

  struct ZeroExtender<
    Base: BidirectionalCollection
  > where Base.Iterator.Element : UnsignedInteger {
    let base: Base
  }
}

/// Adapts any bidirectional collection of unsigned integer to AnyUTF16_
extension AnyUTF16.ZeroExtender : BidirectionalCollection, AnyUTF16_ {
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt16
  
  var startIndex: Index { return Index(offset: 0) }
  var endIndex: Index { return Index(offset: numericCast(base.count)) }
  
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
            offsetBy: numericCast(n)))))
  }
  
  subscript(i: Index) -> Element {
    return numericCast(base[base.index(atOffset: i.offset)])
  }

  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer {
      try ($0 as Any as? UnsafeBufferPointer<Element>).map(body)
    }.flatMap { $0 }
  }
}

protocol AnyRandomAccessUTF16_ : AnyUTF16_ {
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt16
  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after: Index) -> Index
  func index(before: Index) -> Index
  func index(_: Index, offsetBy: IndexDistance) -> Index
  func distance(from: Index, to: Index) -> IndexDistance
  
  subscript(i: Index) -> Element { get }

  func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

// A wrapper that holds any instance of AnyRandomAccessUTF16_ and makes it conform to
// RandomAccessCollection
struct AnyRandomAccessUTF16 : RandomAccessCollection, AnyRandomAccessUTF16_ {
  let base: AnyRandomAccessUTF16_
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt16
  var startIndex: Index { return base.startIndex }
  var endIndex: Index { return base.endIndex }
  func index(after i: Index) -> Index { return base.index(after: i) }
  func index(before i: Index) -> Index { return base.index(before: i) }
  func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    return base.index(i, offsetBy: n)
  }
  func distance(from i: Index, to j: Index) -> IndexDistance {
    return base.distance(from: i, to: j)
  }
  subscript(i: Index) -> Element { return base[i] }
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try base.withExistingUnsafeBuffer(body)
  }

  init<C: RandomAccessCollection>(_ c: C)
  where C.Iterator.Element : UnsignedInteger {
    base = ZeroExtender(base: c)
  }

  struct ZeroExtender<
    Base: RandomAccessCollection
  > where Base.Iterator.Element : UnsignedInteger {
    let base: Base
  }
}

/// Adapts any random access collection of unsigned integer to
/// AnyRandomAccessUTF16_, so it can be wrapped in AnyRandomAccessUTF16
extension AnyRandomAccessUTF16.ZeroExtender
: RandomAccessCollection, AnyRandomAccessUTF16_ {
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt16
  

  var startIndex: Index { return Index(offset: 0) }
  var endIndex: Index { return Index(offset: numericCast(base.count)) }
  
  func index(after i: Index) -> Index {
    return Index(
      offset: numericCast(
        base.offset(of: base.index(after: base.index(atOffset: i.offset)))))
  }
  
  func index(before i: Index) -> Index {
    return Index(
      offset: numericCast(
        base.offset(
          of: base.index(before: base.index(atOffset: i.offset)))))
  }
  
  func index(_ i: Index, offsetBy n: Int64) -> Index {
    return Index(
      offset: numericCast(
        base.offset(
          of: base.index(
            base.index(atOffset: i.offset),
            offsetBy: numericCast(n)))))
  }
  
  func distance(from i: Index, to j: Index) -> IndexDistance {
    return numericCast(
      base.distance(
        from: base.index(atOffset: i.offset),
        to: base.index(atOffset: j.offset)))
  }
  
  subscript(i: Index) -> Element {
    return numericCast(base[base.index(atOffset: i.offset)])
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
  typealias Index = UnicodeIndex
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
  typealias Index = UnicodeIndex
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
}

/// Adapts any bidirectional collection of unsigned integer to AnyUnicodeBidirectionalUInt32_
extension AnyUnicodeBidirectionalUInt32.ZeroExtender
  : BidirectionalCollection, AnyUnicodeBidirectionalUInt32_ {
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt32
  

  var startIndex: Index { return Index(offset: 0) }
  var endIndex: Index { return Index(offset: numericCast(base.count)) }
  
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
            offsetBy: numericCast(n)))))
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
  typealias Index = UnicodeIndex
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
  typealias Index = UnicodeIndex
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
}

protocol AnyUnicode_ : Swift._AnyUnicode {
  var codeUnits: AnyCodeUnits { get }
  var utf16: AnyUTF16 { get }
  var utf32: AnyUnicodeBidirectionalUInt32 { get }
  // FIXME: Can this be Random Access?  If all encodings use a single code unit
  // per ASCII character and can statelessly identify a code unit that
  // represents ASCII, then yes.  Look into, e.g. shift-JIS.
  var extendedASCII : AnyUnicodeBidirectionalUInt32 { get }
  var characters : AnyCharacters { get }
}

struct AnyRandomAccessUnsignedIntegers<
  Base: RandomAccessCollection, Element_ : UnsignedInteger
> : RandomAccessCollection
where Base.Iterator.Element : UnsignedInteger {
  typealias IndexDistance = Int64
  typealias Index = Int64
  typealias Element = Element_
  // FIXME: associated type deduction seems to need a hint here.
  typealias Indices = DefaultRandomAccessIndices<AnyRandomAccessUnsignedIntegers>
  
  let base: Base

  var startIndex: Index { return 0 }
  var endIndex: Index { return numericCast(base.count) }
  
  func index(after i: Index) -> Index {
    return numericCast(
      base.offset(of: base.index(after: base.index(atOffset: i))))
  }
  
  func index(before i: Index) -> Index {
    return numericCast(
      base.offset(of: base.index(before: base.index(atOffset: i))))
  }
  
  func index(_ i: Index, offsetBy n: Int64) -> Index {
    return numericCast(
      base.offset(
        of: base.index(base.index(atOffset: i),
          offsetBy: numericCast(n))))
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

enum UTF16CompatibleStringContents {
case utf16(_UTF16StringStorage)
case latin1(_Latin1StringStorage)
}

extension UTF16CompatibleStringContents : _FixedFormatUnicode {
  typealias Encoding = UTF16
  typealias RawUTF16View = AnyUTF16
  typealias CodeUnits = AnyRandomAccessUTF16
  typealias FCCNormalizedUTF16View = AnyUTF16
  
  var rawUTF16: AnyUTF16 {
    switch self {
    case .utf16(let storage):
      return AnyUTF16(storage)
    case .latin1(let storage):
      return AnyUTF16(storage)
    }
  }

  var fccNormalizedUTF16: FCCNormalizedUTF16View {
    switch self {
    case .utf16(let storage):
      return AnyUTF16(storage.fccNormalizedUTF16)
    case .latin1(let storage):
      return AnyUTF16(storage.fccNormalizedUTF16)
    }
  }

  var codeUnits: CodeUnits {
    switch self {
    case .utf16(let storage):
      return CodeUnits(storage)
    case .latin1(let storage):
      return CodeUnits(storage)
    }
  }

  var isKnownLatin1: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownLatin1
    case .latin1(let storage):
      return storage.isKnownLatin1
    }
  }

  var isKnownValidEncoding: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownValidEncoding
    case .latin1(let storage):
      return storage.isKnownValidEncoding
    }
  }

  var isKnownFCCNormalized: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownFCCNormalized
    case .latin1(let storage):
      return storage.isKnownFCCNormalized
    }
  }

  var isKnownFCDForm: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownFCDForm
    case .latin1(let storage):
      return storage.isKnownFCDForm
    }
  }

  var isKnownNFDNormalized: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownNFDNormalized
    case .latin1(let storage):
      return storage.isKnownNFDNormalized
    }
  }

  var isKnownNFCNormalized: Bool {
    switch self {
    case .utf16(let storage):
      return storage.isKnownNFCNormalized
    case .latin1(let storage):
      return storage.isKnownNFCNormalized
    }
  }
}

var suite = TestSuite("AnyUnicode")
suite.test("basics") {
  let x = AnyUTF16.ZeroExtender(base: Array(3...7) as [UInt16])
  let y = AnyUTF16.ZeroExtender(base: Array(3...7) as [UInt8])
  expectTrue(x.elementsEqual(y))
}

runAllTests()
