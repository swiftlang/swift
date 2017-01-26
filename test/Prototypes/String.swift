// RUN: rm -rf %t && mkdir -p %t && %gyb -DWORD_BITS=%target-ptrsize %s -o %t/out.swift
// RUN: %line-directive %t/out.swift -- %target-build-swift -parse-stdlib -Xfrontend -disable-access-control %t/out.swift -o %t/a.out -Onone
// RUN: %line-directive %t/out.swift -- %target-run %t/a.out
// REQUIRES: executable_test

import Swift

/// A collection that has an underlying collection of code units and an
/// encoding.  Strings will conform to this protocol so that pattern matching
/// and other facilities can probe for information that will allow them to do
/// their work more efficiently.  For example, a pattern that's represented as
/// UTF8 might probe the string being searched to see if it has a compatible
/// representation, in which case we might be able to bypass transcoding.
///
/// To operate on ordinary collections, a wrapper with Encoding == Void and
/// CodeUnits == EmptyCollection<Void> can be used.
protocol EncodedCollection : Collection {
  associatedtype Encoding
  associatedtype CodeUnits : Collection
  var codeUnits : CodeUnits { get }
}

protocol EncodedScalarProtocol : RandomAccessCollection {
  var utf8: UTF8.EncodedScalar { get }
  var utf16: UTF16.EncodedScalar { get }
  var utf32: UTF32.EncodedScalar { get }
}

protocol Unicode : EncodedCollection, BidirectionalCollection {
  associatedtype CodeUnits : BidirectionalCollection
  associatedtype Encoding : UnicodeCodec
  associatedtype Scalars : BidirectionalCollection
  // where Iterator.Element == UnicodeScalar
  associatedtype UTF8Units : EncodedCollection
}

/*
/// Strings are `BidirectionalCollection`s of `Character` whose `Index` type is
/// `StringIndex`
protocol StringProtocol
  : EncodedCollection, BidirectionalCollection {
  associatedtype CodeUnits : BidirectionalCollection
  associatedtype Encoding : UnicodeCodec

  // This wouldn't be here; it's really a trick to (weakly) simulate
  // the ability to constrain our Index type to be String.Index and
  // our element to be Character.
  associatedtype Index = StringIndex
  subscript(_: StringIndex) -> Character { get }
}
*/

/// Storage for string representations.
///
/// Requires: CodeUnit is trivial.  In practice it will be UInt16 or UInt8.
///
/// Capacity is parameterized because we don't really want to store anything
/// when the storage is immutable (e.g., storage for Character)
final class StringStorage<CodeUnit, Capacity>
  : ManagedBuffer<(count: Int, capacity: Capacity), CodeUnit> {

  // FIXME: we should find a way to share code with ContiguousArrayBuffer.  We
  // can't just use it directly because the storage class for Arrays has to be-a
  // NSArray, while we (eventually) want this class to be-a NSString.  That also
  // means we'll have to stop inheriting ManagedBuffer and used
  // ManagedBufferPointer (or _ManagedBufferPointer if it's internalized)
  // instead.

  /// Makes one.
  ///
  /// Don't forget to assign something to storedCapacity if you're going to use
  /// it!
  class func create(
    minimumCapacity: Int,
    count: Int = 0,
    storedCapacity:
      (ManagedBuffer<(count: Int, capacity: Capacity), CodeUnit>)->Capacity
  ) -> StringStorage {
    let r = super.create(minimumCapacity: minimumCapacity) {
      (count: count, capacity: storedCapacity($0))
    }
    return unsafeDowncast(r, to: StringStorage<CodeUnit, Capacity>.self)
  }

  var count : Int {
    get {
      return withUnsafeMutablePointerToHeader { $0.pointee.count }
    }
    set {
      withUnsafeMutablePointerToHeader { $0.pointee.count = newValue }
    }
  }

  var storedCapacity : Capacity {
    get {
      return withUnsafeMutablePointerToHeader { $0.pointee.capacity }
    }
    set {
      withUnsafeMutablePointerToHeader { $0.pointee.capacity = newValue }
    }
  }
}

extension StringStorage : MutableCollection {
  typealias Index = UIntWordBitsMinus4

  var startIndex : Index { return Index(0) }
  var endIndex : Index { return Index(count) }

  subscript(i: Index) -> CodeUnit {
    get {
      return withUnsafeMutablePointerToElements {
        $0[numericCast(i)]
      }
    }
    set {
      withUnsafeMutablePointerToElements {
        $0[numericCast(i)] = newValue
      }
    }
  }
  public func index(after i: Index) -> Index { return i + (1 as Index) }
}

extension StringStorage : RandomAccessCollection {
  public func index(before i: Index) -> Index { return i - (1 as Index) }
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return numericCast(numericCast(i) + n)
  }

  public subscript(bounds: Range<Index>)
  -> MutableRandomAccessSlice<StringStorage> {
    get {
      return MutableRandomAccessSlice(base: self, bounds: bounds)
    }
    set {
      // Reference semantics means we might already be done.
      if newValue.base === self { return }

      let targetIndices = bounds.lowerBound..<bounds.upperBound
      _sanityCheck(targetIndices.count == newValue.count)
      for (i, e) in zip(targetIndices, newValue) {
        self[i] = e
      }
    }
  }
}
//===--- end StringStorage ------------------------------------------------===//

struct Character {
  enum Content {
    typealias Storage = StringStorage<UTF16.CodeUnit, ()>
  case inline(UInt63) // Up to 4 UTF16 code units
  case allocated(Storage)
  }
}

extension Character.Content : RandomAccessCollection {
  typealias Index = UIntWordBitsMinus4

  // FIXME: should add something that reads from a string until the next
  // grapheme cluster boundary.  That would be the usual way to construct these
  // and would avoid pre-measuring to find the boundary.

  // FIXME: consider making a custom iterator for this.  If we have an inline
  // representation it could be lots faster than IndexingIterator

  // FIXME: get someone who knows the unicode planes really well to tell us
  // whether the inline encoding makes good use of bits, especially since we are
  // dropping the top bit of the last element (e.g. maybe we should do that with
  // the top bit of the first element).
  init<C: Collection>(_ source: C)
  where C.Iterator.Element == UTF16.CodeUnit {
    var i = source.makeIterator()
    let u0 = UInt64(i.next()!) // A character is never empty
    var u1: UInt64 = 0xFFFF_0000
    var u2: UInt64 = 0xFFFF_0000_0000
    var u3: UInt64 = 0x7FFF_0000_0000_0000
    if let u1_ = i.next() {
      u1 = UInt64(u1_) << 16
      if let u2_ = i.next() {
        u2 = UInt64(u2_) << 32
        if let u3_ = i.next() {
          u3 = i.next() != nil ? UInt64(u3_) << 48 : ~0
        }
      }
    }
    if Int64(bitPattern: u3) < 0 {
      self = .inline(UInt63(u3 | u2 | u1 | u0))
    }
    else {
      let n: Int = numericCast(source.count)
      let storage = Storage.create(minimumCapacity: n, count: n) { _ in () }
      for (i, x) in zip(storage.indices, source) {
        storage[i] = x
      }
      self = .allocated(storage)
    }
  }

  var startIndex: Index { return Index(0) }

  var endIndex: Index {
    switch self {
    case .allocated(let storage): return storage.endIndex
    case .inline(let bits):
      if bits >= 0x7fff_ffff_ffff_0000 { return Index(1) }
      if bits >= 0x7fff_ffff_0000_0000 { return Index(2) }
      if bits >= 0x7fff_0000_0000_0000 { return Index(3) }
      return Index(4)
    }
  }

  subscript(i: Index) -> UTF16.CodeUnit {
    switch self {
    case .allocated(let storage): return storage[i]
    case .inline(let bits):
      let _16i = (numericCast(i) as UInt) << 4
      return UTF16.CodeUnit(
        truncatingBitPattern: numericCast(bits) >> _16i)
    }
  }

  func index(after i: Index) -> Index {
    return i + (1 as Index)
  }
  func index(before i: Index) -> Index {
    return i - (1 as Index)
  }
  func index(_ i: Index, offsetBy n: Int) -> Index {
    return numericCast(numericCast(i) + n)
  }
}

/*
extension Character : StringProtocol {

}

final class StringBuffer : ManagedBuffer<(count: ), UInt16> {
  static func create(minimumCapacity: Int) -> StringBuffer {
    return super.create(
      minimumCapacity: minimumCapacity, makingHeaderWith: { _ in () })
    as! StringBuffer
  }

  enum Encoding {
  case ascii
  case latin1
  case utf16
  }
  var encoding: Encoding = .latin1
  var count: Int = 42
  var storageCapacity: Int = 0
}

class AnyStringBase  {
  typealias Encoding = UTF32
}

class AnyString<T: StringProtocol> : AnyStringBase {
  init(_ x: T) {
    stored = x
  }
  var stored: T
}

// Approximates Builtin.Int62, which fits without growing String past
// 64 bits.
enum SixBits : UInt8 {
case _00, _01, _02, _03, _04, _05, _06, _07
case _10, _11, _12, _13, _14, _15, _16, _17
case _20, _21, _22, _23, _24, _25, _26, _27
case _30, _31, _32, _33, _34, _35, _36, _37
case _40, _41, _42, _43, _44, _45, _46, _47
case _50, _51, _52, _53, _54, _55, _56, _57
case _60, _61, _62, _63, _64, _65, _66, _67
case _70, _71, _72, _73, _74, _75, _76, _77
}
typealias Int62 = (UInt32, UInt16, UInt8, SixBits)

struct SmallString {
  var bits: Int62
}

// 64 bits
struct String : StringProtocol {
  enum Content {
  case small(SmallString)
  case large(StringBuffer)
  case any(AnyStringBase)
  }
  var content: Content

  typealias Index = StringIndex

  var startIndex: Index { return Index(offset: 0) }
  var endIndex: Index {
    fatalError()
  }
  subscript(_: Index) -> SubString { fatalError() }
  public func index(after i: Index) -> Index { fatalError() }
  public func index(_ i: Index, offsetBy n: Int) -> Index { fatalError() }
//  public func index(_ i: Self.Index, offsetBy n: Self.IndexDistance, limitedBy limit: Self.Index) -> Self.Index?
  typealias Encoding = Void  // No statically-known encoding
  var codeUnits : EmptyCollection<()>? { return nil }
}

// 16 bytes 32-bit platforms and 24 bytes on 64-bit platforms
struct SubString {
  enum Content {
  case smallish(Int62, UInt, UInt) // Up to 190 bits including offset
  case large(String, String.Index, String.Index)
  }
  var content: Content
}

let _=print(StringBuffer.create(minimumCapacity: 20))
let _=print(MemoryLayout<String>.size, MemoryLayout<String>.stride)
let _=print(MemoryLayout<SubString>.size, MemoryLayout<SubString>.stride)
*/
