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
// RUN: rm -rf %t && mkdir -p %t && %gyb -DWORD_BITS=%target-ptrsize %s -o %t/out.swift
// RUN: %line-directive %t/out.swift -- %target-build-swift %t/out.swift -o %t/a.out -Onone
// RUN: %line-directive %t/out.swift -- %target-run %t/a.out

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

  func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R
  
  var contiguousStorage : ContiguousStorage<Element>? { get }
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
  
  public func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R { return try base.withUnsafeElementStorage(body) }
  
  public var contiguousStorage : ContiguousStorage<Element>? {
    return base.contiguousStorage
  }
}

/// Adapts any random access collection of unsigned integer to AnyCodeUnits_
struct AnyCodeUnitsZeroExtender<
  Base: RandomAccessCollection
> : RandomAccessCollection, AnyCodeUnits_
where Base.Iterator.Element : UnsignedInteger {
  typealias IndexDistance = Int64
  typealias Index = Int64
  typealias Element = UInt32
  // FIXME: associated type deduction seems to need a hint here.
  typealias Indices = DefaultRandomAccessIndices<AnyCodeUnitsZeroExtender>
  
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

  public func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R {
    return try base.withUnsafeElementStorage { b in
      if let b1 = b {
        if let b2 = b1 as Any as? UnsafeBufferPointer<Element> {
          return try body(b2)
        }
      }
      return try body(nil)
    }
  }
  
  public var contiguousStorage : ContiguousStorage<Element>? {
    if let b = base.contiguousStorage {
      return b as Any as? ContiguousStorage<Element>
    }
    return nil
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

  func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R
  
  var contiguousStorage : ContiguousStorage<Element>? { get }
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
  public func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R { return try base.withUnsafeElementStorage(body) }
  
  public var contiguousStorage : ContiguousStorage<Element>? {
    return base.contiguousStorage
  }
}

/// Adapts any bidirectional collection of unsigned integer to AnyUTF16_
struct AnyUTF16ZeroExtender<
  Base: BidirectionalCollection
> : BidirectionalCollection, AnyUTF16_
where Base.Iterator.Element : UnsignedInteger {
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt16
  
  let base: Base

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

  public func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R {
    return try base.withUnsafeElementStorage { b in
      if let b1 = b {
        if let b2 = b1 as Any as? UnsafeBufferPointer<Element> {
          return try body(b2)
        }
      }
      return try body(nil)
    }
  }
  
  public var contiguousStorage : ContiguousStorage<Element>? {
    if let b = base.contiguousStorage {
      return b as Any as? ContiguousStorage<Element>
    }
    return nil
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

  func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R
  
  var contiguousStorage : ContiguousStorage<Element>? { get }
}

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
  public func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R { return try base.withUnsafeElementStorage(body) }
  
  public var contiguousStorage : ContiguousStorage<Element>? {
    return base.contiguousStorage
  }
}

/// Adapts any bidirectional collection of unsigned integer to AnyUnicodeBidirectionalUInt32_
struct AnyUnicodeBidirectionalUInt32ZeroExtender<
  Base: BidirectionalCollection
> : BidirectionalCollection, AnyUnicodeBidirectionalUInt32_
where Base.Iterator.Element : UnsignedInteger {
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt32
  
  let base: Base

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

  public func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R {
    return try base.withUnsafeElementStorage { b in
      if let b1 = b {
        if let b2 = b1 as Any as? UnsafeBufferPointer<Element> {
          return try body(b2)
        }
      }
      return try body(nil)
    }
  }
  
  public var contiguousStorage : ContiguousStorage<Element>? {
    if let b = base.contiguousStorage {
      return b as Any as? ContiguousStorage<Element>
    }
    return nil
  }
}

protocol AnyCharacters_ {
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt32
  var startIndex: Index { get }
  var endIndex: Index { get }
  func index(after: Index) -> Index
  func index(before: Index) -> Index
  subscript(i: Index) -> Element { get }

  func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R
  
  var contiguousStorage : ContiguousStorage<Element>? { get }
}

struct AnyCharacters : BidirectionalCollection, AnyCharacters_ {
  let base: AnyCharacters_
  typealias IndexDistance = Int64
  typealias Index = UnicodeIndex
  typealias Element = UInt32
  var startIndex: Index { return base.startIndex }
  var endIndex: Index { return base.endIndex }
  func index(after i: Index) -> Index { return base.index(after: i) }
  func index(before i: Index) -> Index { return base.index(before: i) }
  subscript(i: Index) -> Element { return base[i] }
  public func withUnsafeElementStorage<R>(
    _ body: (UnsafeBufferPointer<Element>?) throws -> R
  ) rethrows -> R { return try base.withUnsafeElementStorage(body) }
  
  public var contiguousStorage : ContiguousStorage<Element>? {
    return base.contiguousStorage
  }
}

protocol AnyUnicode {
  var encoding: AnyUnicodeEncoding { get }
  var codeUnits: AnyCodeUnits { get }
  var utf16: AnyUTF16 { get }
  var utf32: AnyUnicodeBidirectionalUInt32 { get }
  // FIXME: Can this be Random Access?  If all encodings use a single code unit
  // per ASCII character and can statelessly identify a code unit that
  // represents ASCII, then yes.  Look into, e.g. shift-JIS.
  var extendedASCII : AnyUnicodeBidirectionalUInt32 { get }
  var characters : AnyCharacters { get }
  
  func isASCII(scan: Bool/* = true */) -> Bool 
  func isLatin1(scan: Bool/* = true */) -> Bool 
  func isNormalizedNFC(scan: Bool/* = true*/) -> Bool
  func isNormalizedNFD(scan: Bool/* = true*/) -> Bool
  func isInFastCOrDForm(scan: Bool/* = true*/) -> Bool
}

var suite = TestSuite("AnyUnicode")
suite.test("basics") {
  let g : [UInt16] = Array(3...7)
  let h : [UInt8] = Array(3...7)
  var x = AnyUTF16ZeroExtender(base: g)
  var y = AnyUTF16ZeroExtender(base: h)
  expectTrue(x.elementsEqual(y))
}

runAllTests()
