// RUN: rm -rf %t && mkdir -p %t && %gyb -DWORD_BITS=%target-ptrsize %s -o %t/out.swift
// RUN: %line-directive %t/out.swift -- %target-build-swift %t/out.swift -o %t/a.out -Onone -g
// RUN: %line-directive %t/out.swift -- %target-run %t/a.out
// REQUIRES: executable_test


/// Copy in StringNormalization.swift so we can prototype:

//===--- StringNormalization.swift ----------------------------------------===//
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

import SwiftShims
import Darwin


var _printDebugging = false
func _debug(_ s: Swift.String) {
  guard _printDebugging else { return }
  print(s)
}
func _withDebugging(_ f: ()->()) {
  let prior = _printDebugging
  _printDebugging = true
  f()
  _printDebugging = prior
}


//
// A "normalization segment" is a sequence that starts with a unicode scalar
// value which has a normalization boundary before it, and includes all
// subsequent unicode scalar values who do not.
//
// A "native index" is an index into the original String's code units. It can be
// interchanged with many of the other String indices. This is what is
// represented by UnicodeView's "EncodedOffset" Int64.
//

// Ask ICU if the given unicode scalar value has a normalization boundary before
// it, that is it begins a new normalization segment.
internal func _hasBoundary(before value: UInt32) -> Bool {
  return __swift_stdlib_unorm2_hasBoundaryBefore(_fccNormalizer, value) != 0
}

// TODO: explore using hasBoundary(after:), and whether that will identify
// finer-grained segments.

public struct FCCNormalizedLazySegments2<
  CodeUnits : RandomAccessCollection,
  FromEncoding : UnicodeEncoding
>
where
  CodeUnits.Index == CodeUnits.SubSequence.Index,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence == CodeUnits.SubSequence.SubSequence,
  CodeUnits.Iterator.Element == CodeUnits.SubSequence.Iterator.Element,
  CodeUnits.Iterator.Element == FromEncoding.EncodedScalar.Iterator.Element
{
  let codeUnits: CodeUnits

  public init(
    _ codeUnits: CodeUnits,
    _: FromEncoding.Type = FromEncoding.self
  ) {
    self.codeUnits = codeUnits
  }

  public init(_ unicodeView: _UnicodeViews<CodeUnits, FromEncoding>) {
    self.init(unicodeView.codeUnits)
  }

  // Find the segment that terminates at `endingAt`. Optimized for backwards
  // traversal.
  internal func priorSegment(
    endingAt end: CodeUnits.Index
  ) -> Index {
    precondition(end != codeUnits.startIndex)

    // Decode scalars backwards, including them until we after we find one that
    // has a boundary before it (and include that one).
    var start = end
    while start != codeUnits.startIndex {
      // Include this scalar
      let (scalarValue: value, startIndex: scalarStart, e)
        = decodeOne(priorTo: start)
      _sanityCheck(e == start, "Internal inconsistency in decodeOne")

      // Include this scalar
      start = scalarStart

      if _hasBoundary(before: value) {
        // We're done
        break
      }
    }

    return Index(
      nativeOffset: codeUnits.distance(
        from: codeUnits.startIndex, to: start
      ),
      nativeCount: codeUnits.distance(from: start, to: end),
      segment: formSegment(from: start, until: end)
    )
  }

  // Find the segment that starts with `startingAt`. Optimized for forwards
  // traversal.
  internal func nextSegment(
    startingAt start: CodeUnits.Index
  ) -> Index {
    if start == codeUnits.endIndex {
      return endIndex
    }

    // Parse the first scalar, it will always be in the segment
    var (scalarValue: value, startIndex: s, endIndex: end)
      = decodeOne(from: start)
    _sanityCheck(start == codeUnits.startIndex || 
                 _hasBoundary(before: value), "Not on segment boundary")
    _sanityCheck(s == start, "Internal inconsistency in decodeOne")

    // Include any subsequent scalars that don't have boundaries before them
    while end != codeUnits.endIndex {
      let (scalarValue: value, startIndex: s, endIndex: scalarEnd)
        = decodeOne(from: end)
      _sanityCheck(s == end, "Internal inconsistency in decodeOne")

      if _hasBoundary(before: value) {
        // Exclude this scalar
        break
      }

      // Include this scalar
      end = scalarEnd
    }

    return Index(
      nativeOffset: codeUnits.distance(
        from: codeUnits.startIndex, to: start
      ),
      nativeCount: codeUnits.distance(from: start, to: end),
      segment: formSegment(from: start, until: end)
    )
  }

  // Normalize a segment. Indices must be on scalar boundaries.
  internal func formSegment(
    from start: CodeUnits.Index,
    until end: CodeUnits.Index
  ) -> FCCNormalizedSegment {
    precondition(start != end, "TODO: should we just have empty segment?")

    _debug("Forming segment around \(start)..<\(end)")

    let utf16CodeUnits = unicodeView(
      from: start, until: end
    ).scalarsTranscoded(
      to: UTF16.self
    )

    _debug("  Code units are \(Array(utf16CodeUnits))")


    // TODO: Find way to re-use the storage, maybe iterator pattern?
    var buffer = UTF16CodeUnitBuffer(utf16CodeUnits.lazy.joined())

    // TODO: fast pre-normalized checks (worth doing before calling over to

    _sanityCheck(buffer.count > 0, "How did this happen? Failed precondition?")

    // Ask ICU to normalize
    //
    // FIXME: withMutableArray kind of defeats the purpose of the small
    // buffer :-(
    buffer.withMutableArray { (array: inout [UInt16]) -> () in
      array.withUnsafeBufferPointer {
        // TODO: Just reserving one or two extra up front. If we're segment-
        // based, should be finite number of possible decomps.
        let originalCount = buffer.count
        while true {

        	_debug("  Trying to normalize: \(Array($0))")

          var error = __swift_stdlib_U_ZERO_ERROR
          let usedCount = $0.baseAddress == nil ? 0
          : __swift_stdlib_unorm2_normalize(
            _fccNormalizer, $0.baseAddress!, numericCast($0.count),
            &array, numericCast(array.count), &error)
          if __swift_stdlib_U_SUCCESS(error) {
            array.removeLast(array.count - numericCast(usedCount))
            return
          }
          _sanityCheck(
            error == __swift_stdlib_U_BUFFER_OVERFLOW_ERROR,
            "Unknown normalization error")

          // Maximum number of NFC to FCC decompositions for a single unicode
          // scalar value
          //
          // TODO: what is this really? Should be much less
          let maxDecompSize = 8

          // Very loose canary to check that we haven't grown exceedingly large
          // (indicative of logic error). Loose by assuming that every original
          // character could be decomposed the maximum number of times. Without
          // this, an error would loop until we run out of memory or the array
          // is larger than 2^32 on 64bit platforms.
          _sanityCheck(buffer.count < originalCount*maxDecompSize)

          // extend array storage by 25%
          array.append(
            contentsOf: repeatElement(0, count: (array.count + 3) >> 2))
        }
      }
    }

    return FCCNormalizedSegment(buffer)
  }


  // Decode one or more code units, returning the unicode scalar value and the
  // indices spanning the code units parsed. `from` should be on scalar boundary
  internal func decodeOne(from start: CodeUnits.Index)
    -> (scalarValue: UInt32,
        startIndex: CodeUnits.Index,
        endIndex: CodeUnits.Index) {
    precondition(start != codeUnits.endIndex, "Given empty slice")

    let encodedScalar = unicodeView(from: start).encodedScalars.first!
    return (scalarValue: encodedScalar.utf32[0],
            startIndex: start,
            endIndex: codeUnits.index(start, offsetBy: numericCast(encodedScalar.count)))
  }

  // As decodeOne(from:), but in reverse. `priorTo` is the index after the last
  // code unit in the scalar, i.e. it is exclusive.
  internal func decodeOne(priorTo end: CodeUnits.Index)
    -> (scalarValue: UInt32,
        startIndex: CodeUnits.Index,
        endIndex: CodeUnits.Index) {
    precondition(end != codeUnits.startIndex, "Given empty slice")

    let encodedScalar = unicodeView(until: end).encodedScalars.last!
    return (scalarValue: encodedScalar.utf32[0],
            startIndex: codeUnits.index(end, offsetBy: -numericCast(encodedScalar.count)),
            endIndex: end)
  }

  // Get the rest of the Unicode view
  internal func unicodeView(
    from start: CodeUnits.Index? = nil,
    until end: CodeUnits.Index? = nil
  ) -> _UnicodeViews<CodeUnits.SubSequence, FromEncoding> {
    let end = end ?? codeUnits.endIndex
    let start = start ?? codeUnits.startIndex
    return _UnicodeViews(codeUnits[start..<end], FromEncoding.self)
  }
}

extension FCCNormalizedLazySegments2 : BidirectionalCollection {
  // TODO?: This is really more like an iterator...
  public struct Index : Comparable {
    // The corresponding native begin/end indices for this segment
    let nativeOffset: CodeUnits.IndexDistance
    let nativeCount: CodeUnits.IndexDistance
    let segment: FCCNormalizedSegment

    public static func <(lhs: Index, rhs: Index) -> Bool {
      if lhs.nativeOffset < rhs.nativeOffset {
        // Our ends should be ordered similarly, unless lhs is the last index
        // before endIndex and rhs is the endIndex.
        _sanityCheck(
          lhs.nativeOffset + lhs.nativeCount 
            < rhs.nativeOffset + rhs.nativeCount ||
          rhs.nativeCount == 0,
          "overlapping segments?")

        return true
      }

      return false
    }

    public static func ==(lhs: Index, rhs: Index) -> Bool {

      if lhs.nativeOffset == rhs.nativeOffset {
        _sanityCheck(
          lhs.nativeCount == rhs.nativeCount,
          "overlapping segments?")

        return true
      }

      return false
    }
  }

  // TODO: formIndex(after:) that re-uses storage

  public var startIndex: Index {
    return nextSegment(startingAt: codeUnits.startIndex)
  }
  public var endIndex: Index {
    return Index(
      nativeOffset: codeUnits.count,
      nativeCount: 0,
      segment: FCCNormalizedSegment()
    )
  }

  public func index(after idx: Index) -> Index {
    return nextSegment(
      startingAt: codeUnits.index(atOffset: idx.nativeOffset + idx.nativeCount)
    )
  }
  public func index(before idx: Index) -> Index {
    return priorSegment(
      endingAt: codeUnits.index(atOffset: idx.nativeOffset)
    )
  }
  public subscript(position: Index) -> FCCNormalizedSegment {
    return position.segment
  }

  public typealias SubSequence = BidirectionalSlice<FCCNormalizedLazySegments2>
}

extension _UnicodeViews {
  public struct FCCNormalizedUTF16View2: BidirectionalCollectionWrapper {
    public typealias Base = FlattenBidirectionalCollection<
      FCCNormalizedLazySegments2<CodeUnits, Encoding>
    >
    public var base: Base
    public typealias Index = Base.Index
    public typealias IndexDistance = Base.IndexDistance

    public init(_ unicodeView: _UnicodeViews<CodeUnits, Encoding>) {
      self.base = Base(FCCNormalizedLazySegments2(unicodeView))
    }
  }


  public var fccNormalizedUTF16_2: FCCNormalizedUTF16View2 {
    return FCCNormalizedUTF16View2(self)
  }
}

  extension _UnicodeViews.FCCNormalizedUTF16View2 : BidirectionalCollection {
  	
  }

// extension _UnicodeViews.FCCNormalizedUTF16View2 : UnicodeView {
//   // Returns the index for the segment containing `atEncodedOffset`
//   //
//   // TODO: What are the desired semantics if given an offset that does not begin
//   // a segment? We should be very explicit about our contract here.
//   public func index(atEncodedOffset i: Int64) -> Index {
//     let segmentIdx = base._base.nextSegment(
//       startingAt: base._base.codeUnits.index(
//         base._base.codeUnits.startIndex, offsetBy: numericCast(i)
//      )
//     )
//     return Index(segmentIdx, segmentIdx.segment.startIndex)
//   }

//   // Returns the offset of the beggining of `of`'s segment
//   public static func encodedOffset(of i: Index) -> Int64 {
//     return numericCast(i._outer.nativeOffset)
//   }
// }


//////////////

/// Hack in the prototype

public extension UnicodeStorage
where Encoding.EncodedScalar == UTF16.EncodedScalar,
  CodeUnits.Iterator.Element == UTF16.CodeUnit,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
  
  // FIXME: we should have a way to represent the validity of the encoding of
  // this result—and maybe other nice properties—in the type system.  So maybe
  // this thing should conform to UnicodeStorage
  var fccNormalizedUTF16_2
  : _UnicodeViews<CodeUnits,Encoding>.FCCNormalizedUTF16View2 {
    return _UnicodeViews(codeUnits, Encoding.self).fccNormalizedUTF16_2
  }
}


import StdlibUnittest

var suite = TestSuite("AnyUnicode")

func printCU(_ x: UInt16) {
		print(String(x, radix: 16))	
}

suite.test("Angstrom") {
	let strNonNormal: [UInt16] = [0x212b] // "Å" 
	let strNFD: [UInt16] = [0x0041, 0x030a] // "Å"
	let strNFC: [UInt16] = [0x00c5] // "Å" 

	print("NonNormal")
	for cu in strNonNormal {
		printCU(cu)
	}

	print("NFD")
	for cu in strNFD {
		printCU(cu)
	}

	print("NFC")
	for cu in strNFC {
		printCU(cu)
	}

	let strNonNormal_unicode = _UnicodeViews(strNonNormal, UTF16.self)
	let strNFD_unicode = _UnicodeViews(strNFD, UTF16.self)
	let strNFC_unicode = _UnicodeViews(strNFC, UTF16.self)

	_withDebugging {
		print("non-normal => FCC")
		for cu in strNonNormal_unicode.fccNormalizedUTF16_2 {
			printCU(cu)
		}
	}

	print("NFD => FCC")
	for cu in strNFD_unicode.fccNormalizedUTF16_2 {
		printCU(cu)
	}
	print("NFC => FCC")
	for cu in strNFC_unicode.fccNormalizedUTF16_2 {
		printCU(cu)
	}

	// print(strNonNormal_unicode.fccNormalizedUTF16_2 == strNFD_unicode.fccNormalizedUTF16_2)
	// print(strNonNormal_unicode.fccNormalizedUTF16_2 == strNFC_unicode.fccNormalizedUTF16_2)
	// print(strNFD_unicode.fccNormalizedUTF16_2 == strNFC_unicode.fccNormalizedUTF16_2)

	expectTrue(true)

}

suite.test("UInt61.packing") {
//  let five: UInt61 = 5
  // Can't represent 32 as a 5-bit value
  expectNil(UInt61(packing: CollectionOfOne(32 as UInt), bitsPerElement: 5))
  
  // The last value in the collection can't have all bits set because we can't
  // distinguish that from "no more elements are left"
  expectNil(UInt61(packing: CollectionOfOne(31 as UInt), bitsPerElement: 5))

  expectEqual(
    31,
    UInt61(packing: CollectionOfOne(30 as UInt), bitsPerElement: 5))

  expectEqual(
    1,
    UInt61(packing: CollectionOfOne(0 as UInt), bitsPerElement: 5))

  expectEqual(
    0b11111_00000,
    UInt61(packing: [31, 30] as [UInt], bitsPerElement: 5))

  expectNil(UInt61(packing: [31, 30, 31] as [UInt], bitsPerElement: 5))

  expectEqual(
    0b11111_00000_11111_00000,
    UInt61(packing: [31, 30, 31, 30] as [UInt], bitsPerElement: 5))

  let primesAndMore: [UInt] = [31, 29, 23, 19, 17, 13, 11, 7, 5, 3, 2, 1, 0]
  expectEqual(
    0b1_00010_00011_00100_00110_01000_01100_01110_10010_10100_11000_11110_00000,
    UInt61(packing: primesAndMore, bitsPerElement: 5))
  
  if let c = expectNotNil(
    PackedUnsignedIntegers<UInt61, UInt>(primesAndMore, bitsPerElement: 5)
  ) {
    expectEqualSequence(primesAndMore, Array(c))
  }
  
  // There's only 1 bit left in the high-order partial segment.
  expectNil(
    UInt61(packing:
      [31, 29, 23, 19, 17, 13, 11, 7, 5, 3, 2, 1, 1] as [UInt],
      bitsPerElement: 5))
}

extension String.Content {
  var bitsPerCodeUnit: Int {
    switch _rep {
    case .utf16: return 16
    case .latin1: return 8
    case .cocoa(let n): return n.isKnownASCII ? 8 : 16
    case .any: return 16
    case .inline7or16(let x): return x.bitsPerElement
    case .inline5or6(let x):  return x.bitsPerElement
    }
  }
}

suite.test("Content.Packing") {
  // We can store up to 4 UTF16 code units in the inline7or16 case
  var c = String.Content(utf16: "a".utf16)
  expectEqualSequence("a".utf16, Array(c.utf16))
  expectNotNil(c._inline7or16)
  expectEqual(16, c.bitsPerCodeUnit)
  
  c = String.Content(utf16: "ab".utf16)
  expectEqualSequence("ab".utf16, Array(c.utf16))
  expectNotNil(c._inline7or16)
  expectEqual(16, c.bitsPerCodeUnit)
  
  c = String.Content(utf16: "abc".utf16)
  expectEqualSequence("abc".utf16, Array(c.utf16))
  expectNotNil(c._inline7or16)
  expectEqual(16, c.bitsPerCodeUnit)
  
  c = String.Content(utf16: "abcd".utf16)
  expectEqualSequence("abcd".utf16, Array(c.utf16))
  expectNotNil(c._inline7or16)
  expectEqual(16, c.bitsPerCodeUnit)
  
  c = String.Content(utf16: "ab\u{ffe9}d".utf16)
  expectEqualSequence("ab\u{ffe9}d".utf16, Array(c.utf16))
  expectNotNil(c._inline7or16)
  expectEqual(16, c.bitsPerCodeUnit)

  // If the last code unit is sufficiently high-valued, we overflow the
  // high-order partial segment and fall back to dynamic storage
  c = String.Content(utf16: "abc\u{ffe9}".utf16)
  expectEqualSequence("abc\u{ffe9}".utf16, Array(c.utf16))
  expectNotNil(c._utf16)

  // If it's all ASCII, we may fit in the inline 7 case.
  c = String.Content(utf16: "abcde".utf16)
  expectEqualSequence("abcde".utf16, Array(c.utf16))
  expectNotNil(c._inline7or16)
  expectEqual(7, c.bitsPerCodeUnit)

  c = String.Content(utf16: "abcdefgh".utf16)
  expectEqualSequence("abcdefgh".utf16, Array(c.utf16))
  expectNotNil(c._inline7or16)
  expectEqual(7, c.bitsPerCodeUnit)

  // A sufficiently low-valued code unit will fit in the high-order partial
  // segment
  c = String.Content(utf16: "abcdefgh.".utf16)
  expectEqualSequence("abcdefgh.".utf16, Array(c.utf16))
  expectNotNil(c._inline7or16)
  expectEqual(7, c.bitsPerCodeUnit)

  // Higher than that and we fall back to a 6-bit representation
  c = String.Content(utf16: "abcdefghi".utf16)
  expectEqualSequence("abcdefghi".utf16, Array(c.utf16))
  expectNotNil(c._inline5or6)
  expectEqual(6, c.bitsPerCodeUnit)

  c = String.Content(utf16: "abcdefghij".utf16)
  expectEqualSequence("abcdefghij".utf16, Array(c.utf16))
  expectNotNil(c._inline5or6)
  expectEqual(6, c.bitsPerCodeUnit)

  // If it gets too long for 6 bits it falls over into 5 bits per character
  // Note "b" falls outside the common characters representable in 5 bits
  c = String.Content(utf16: "aecdefghij0".utf16)
  expectEqualSequence("aecdefghij0".utf16, Array(c.utf16))
  expectNotNil(c._inline5or6)
  expectEqual(5, c.bitsPerCodeUnit)

  c = String.Content(utf16: "aecdefghij01".utf16)
  expectEqualSequence("aecdefghij01".utf16, Array(c.utf16))
  expectNotNil(c._inline5or6)
  expectEqual(5, c.bitsPerCodeUnit)

  // After 12 characters, we fall back to deep latin-1
  c = String.Content(utf16: "aecdefghij010".utf16)
  expectEqualSequence("aecdefghij010".utf16, Array(c.utf16))
  expectNotNil(c._latin1)
  expectEqual(8, c.bitsPerCodeUnit)
}

let sample = "abcdefghijklmnopqrstuvwxyz\n"
  + "🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\n"
  + "Σὲ 👥🥓γνωρίζω ἀπὸ τὴν κόψη χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!\n"
  + "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,\n"
  + "გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო\n"
  + "Зарегистрируйтесь сейчас на Десятую Международную Конференцию по\n"
  + "  ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่\n"
  + "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"

suite.test("MemoryLayout") {
  expectEqual(8, MemoryLayout<String>.size)
  expectEqual(8, MemoryLayout<String>.stride)
}

suite.test("InplaceViewMutation") {
  var s = String(sample)
  var s1 = s
  expectEqualSequence(s.content.unicodeScalars, s1.content.unicodeScalars)
  s.content.unicodeScalars.removeLast(1)
  expectEqualSequence(
    s.content.unicodeScalars, s1.content.unicodeScalars.dropLast()
  )
}

suite.test("ViewValueSemantics") {
  var sharesBuffer1 = String(sample).content.utf16
  let sharesBuffer2 = sharesBuffer1
  var snapshot = Array(sharesBuffer2)

  // Unwrap the view
  let wrapped = sharesBuffer1.base2 as! AnyUInt16UnicodeView.Adapter<
    RandomAccessUnicodeView<_UTF16StringStorage>>
  
  // Tunnel all the way to the underlying storage, which has reference
  // semantics.  FIXME: there should be no way for the user to do this!
  var storage: _UTF16StringStorage = wrapped.base.base

  // Make a mutation there, and expect both views to reflect that.
  storage.removeLast(1) 
  expectEqualSequence(snapshot.dropLast(), storage)
  expectEqualSequence(snapshot.dropLast(), sharesBuffer2) 
  expectEqualSequence(snapshot.dropLast(), sharesBuffer1)

  snapshot.removeLast()
  
  // However, mutating sharesBuffer1 makes a copy rather than modifying storage
  let oldCount = sharesBuffer1.count
  expectEqual(oldCount, numericCast(storage.count))
  sharesBuffer1.removeLast()
  expectEqual(oldCount, numericCast(storage.count))
  expectEqual(oldCount - numericCast(1), sharesBuffer1.count)
  expectEqualSequence(snapshot, storage) 
  expectEqualSequence(snapshot, sharesBuffer2) 
  expectEqualSequence(snapshot.dropLast(), Array(sharesBuffer1))
  // FIXME: something in the collection forwarding stack is not forwarding
  // _tryToReplaceSubrange, or this test would fail.  That means we're probably
  // missing an optimization when it actually *is* OK to mutate the storage in
  // place.
}

suite.test("UTF16/smoke") {
  var s = String(sample)
  expectFalse(s.content.isKnownLatin1)
  expectFalse(s.content.isLatin1())
  expectFalse(s.content.isKnownASCII)
  expectFalse(s.content.isASCII())
  
  s.replaceSubrange(
    s.index(atOffset: 3)..<s.index(atOffset: 10), with: "BRAK")
  expectEqual(
    "abcBRAKklmnopqrstuvw", Swift.String(s[..<s.index(atOffset: 20)]))
  expectFalse(s.content.isKnownLatin1)
  expectFalse(s.content.isLatin1())
  expectFalse(s.content.isKnownASCII)
  expectFalse(s.content.isASCII())

  let s1 = s
  // ensure that we modify a non-unique buffer this time
  defer { _fixLifetime(s1) } 
  
  s.replaceSubrange(
    s.index(atOffset: 5)..<s.index(atOffset: 6),
    with: "---bl\u{f8}\u{f8}g---"
  )
  expectEqual(
    "abcBR---bl\u{f8}\u{f8}g---Kklmnopqr",
    Swift.String(s[..<s.index(atOffset: 25)]))
  expectFalse(s.content.isKnownLatin1)
  expectFalse(s.content.isLatin1())
  expectFalse(s.content.isKnownASCII)
  expectFalse(s.content.isASCII())
}

suite.test("Latin1/smoke") {
  var s = String(
    sample.lazy.flatMap({ $0.unicodeScalars }).filter { $0.value < 0x100 }.map {
      Character($0)
    })
  expectNotNil(s.content._latin1)
  expectTrue(s.content.isKnownLatin1)
  expectTrue(s.content.isLatin1())
  expectEqual(!s.content.utf16.contains { $0 > 0x7f }, s.content.isKnownASCII)
  expectEqual(!s.content.utf16.contains { $0 > 0x7f }, s.content.isASCII())
  
  s.replaceSubrange(s.index(atOffset: 3)..<s.index(atOffset: 10), with: "BRAK")
  expectEqual(
    "abcBRAKklmnopqrstuvw", Swift.String(s[..<s.index(atOffset: 20)]))
  expectTrue(s.content.isKnownLatin1)
  expectTrue(s.content.isLatin1())
  expectTrue(s.content.isKnownASCII)
  expectTrue(s.content.isASCII())
  
  let s1 = s
  // ensure that we modify a non-unique buffer this time
  defer { _fixLifetime(s1) } 
  
  s.replaceSubrange(
    s.index(atOffset: 5)..<s.index(atOffset: 6),
    with: "---bl\u{f8}\u{f8}g---"
  )
  expectNotNil(s.content._latin1)
  expectEqual(
    "abcBR---bl\u{f8}\u{f8}g---Kklmnopqr",
    Swift.String(s[..<s.index(atOffset: 25)]))
  expectTrue(s.content.isKnownLatin1)
  expectTrue(s.content.isLatin1())
  expectFalse(s.content.isKnownASCII)
  expectFalse(s.content.isASCII())
  
  s.replaceSubrange(
    s.index(atOffset: 11)..<s.index(atOffset: 11),
    with: "🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸")
  expectNil(s.content._latin1)
  expectFalse(s.content.isKnownLatin1)
  expectFalse(s.content.isLatin1())
  expectFalse(s.content.isKnownASCII)
  expectFalse(s.content.isASCII())
  expectEqual(
    "abcBR---blø🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸øg---Kklm",
    Swift.String(s[..<s.index(atOffset: 25)]))
}

suite.test("RangeReplaceable/UTF16/FastPath") {
  // Exercise the case where the source text is-a Unicode.
  var s = String(sample)
  s.replaceSubrange(
    s.index(atOffset: 3)..<s.index(atOffset: 10), with: String("BRAK"))
  expectEqual(
    "abcBRAKklmnopqrstuvw", Swift.String(s[..<s.index(atOffset: 20)]))

  let s1 = s
  // ensure that we modify a non-unique buffer this time
  defer { _fixLifetime(s1) } 
  
  s.replaceSubrange(
    s.index(atOffset: 5)..<s.index(atOffset: 6),
    with: String("---bl\u{f8}\u{f8}g---")
  )
  expectEqual(
    "abcBR---bl\u{f8}\u{f8}g---Kklmnopqr",
    Swift.String(s[..<s.index(atOffset: 25)]))
}

suite.test("RangeReplaceable/Latin1/FastPath") {
  // Exercise the case where the source text is-a Unicode.
  var s = String(
    sample.unicodeScalars.lazy.filter { $0.value < 0x100 }.map {
      Character($0)
    })
  s.replaceSubrange(
    s.index(atOffset: 3)..<s.index(atOffset: 10), with: String("BRAK"))
  expectEqual(
    "abcBRAKklmnopqrstuvw", Swift.String(s[..<s.index(atOffset: 20)]))

  let s1 = s
  // ensure that we modify a non-unique buffer this time
  defer { _fixLifetime(s1) } 
  
  s.replaceSubrange(
    s.index(atOffset: 5)..<s.index(atOffset: 6),
    with: String("---bl\u{f8}\u{f8}g---")
  )
  expectEqual(
    "abcBR---bl\u{f8}\u{f8}g---Kklmnopqr",
    Swift.String(s[..<s.index(atOffset: 25)]))
}

suite.test("Bidirectional/UnicodeScalar") {
  let a = AnyUnicodeScalarUnicodeView(
    _UnicodeViews(Array(sample.utf16), ValidUTF16.self).scalars)
  expectEqualSequence(sample.unicodeScalars, a)
  var lastEncodedOffset = Int64.min
  // Make sure it works as a collection, too.
  for (s, i) in zip(sample.unicodeScalars, a.indices) {
    expectEqual(s, a[i])
    let o = i.encodedOffset
    expectLT(lastEncodedOffset, o)
    lastEncodedOffset = o
  }
}

suite.test("RandomAccess/UnicodeScalar") {
  let a = AnyUnicodeScalarUnicodeView(
    RandomAccessUnicodeView(Array(sample.unicodeScalars)))
  expectEqualSequence(sample.unicodeScalars, a)
  var lastEncodedOffset = Int64.min
  // Make sure it works as a collection, too.
  for (s, i) in zip(sample.unicodeScalars, a.indices) {
    expectEqual(s, a[i])
    let o = i.encodedOffset
    expectLT(lastEncodedOffset, o)
    lastEncodedOffset = o
  }
}

suite.test("CharacterView") {
  // FIXME: precondition checks in Character prevent us from trying this last
  // one.
  let s = "🇸🇸🇬🇱abc🇱🇸🇩🇯🇺🇸\nΣὲ 👥🥓γ͙᷏̃̂᷀νω" // + "👩‍❤️‍👩"
  let a: [Character] = [
    "🇸🇸", "🇬🇱", "a", "b", "c", "🇱🇸", "🇩🇯", "🇺🇸", "\n",
    "Σ", "ὲ", " ", "👥", "🥓", "γ͙᷏̃̂᷀", "ν", "ω"
  ] // + "👩‍❤️‍👩"

  // FIXME: the generic arguments should be deducible, but aren't; <rdar://30323161>
  let v8 = _UnicodeViews(Array(s.utf8), UTF8.self).characters
  expectEqual(a, Array(v8))

  // FIXME: We need to wrap s.utf16 in Array because of <rdar://30386193> Unaccountable link errors
  // FIXME: the generic arguments should be deducible; <rdar://30323161>
  let v16 = _UnicodeViews(Array(s.utf16), UTF16.self).characters
  expectEqual(a, Array(v16))

  expectEqual(v8.reversed(), a.reversed())
  expectEqual(v16.reversed(), a.reversed())
}

suite.test("basic") {
  let s = sample
  let s32 = s.unicodeScalars.lazy.map { $0.value }
  let s16 = Array(s.utf16)
  let s8 = Array(s.utf8)
  let s16to32 = _UnicodeViews(s16, UTF16.self).transcoded(to: UTF32.self)
  let s16to8 = _UnicodeViews(s16, UTF16.self).transcoded(to: UTF8.self)
  let s8to16 = _UnicodeViews(s8, UTF8.self).transcoded(to: UTF16.self)
  let s8Vto16 = _UnicodeViews(s8, ValidUTF8.self).transcoded(to: UTF16.self)
  expectTrue(s32.elementsEqual(s16to32))
  expectTrue(s8.elementsEqual(s16to8))
  expectTrue(s16.elementsEqual(s8to16))
  expectTrue(s16.elementsEqual(s8Vto16))

  expectTrue(s32.reversed().elementsEqual(s16to32.reversed()))
  expectTrue(s8.reversed().elementsEqual(s16to8.reversed()))
  expectTrue(s16.reversed().elementsEqual(s8to16.reversed()))
  expectTrue(s16.reversed().elementsEqual(s8Vto16.reversed()))
}

suite.test("literals") {
  let ascii: String = "abcdef"
  expectEqual(6, ascii.count)
  expectEqualSequence("abcdef" as Swift.String, ascii)
  let unicode: String = "abcdef🦊"
  expectEqual(7, unicode.count)
  expectEqualSequence("abcdef🦊" as Swift.String, unicode)
}

suite.test("printing") {
  let s: String = "a \"string\" with \"quotes\""
  expectEqual(
    "a \"string\" with \"quotes\"", s.description)
  expectEqual(
    "a \"string\" with \"quotes\"".debugDescription, s.debugDescription)
}

suite.test("character") {
  let s1: String = "Héllo, 🌍!"
  var s2: Swift.String = ""
  for c in s1 {
    s2.append(c)
  }
  // expectTrue(s1.elementsEqual(s2.characters))
  expectEqual(9, s1.count)

  // emoji with skin tone should be 1 character (are 2 in Swift 3)
  let skinTone: String = "✌🏾"
  expectEqual(1, skinTone.count)

  let couple: String = "abc👩‍❤️‍👩def"
  expectEqual(7, couple.count)
}

suite.test("string-compare-hash") {
  let s1: String = "abcdez"
  let s2: String = "abcdfz"
  let s3: String = "abcde\u{304}z"
  let s4: String = "abcd\u{113}z"

  expectEqual(s1, s1)
  expectEqual(s2, s2)
  expectEqual(s3, s3)
  expectEqual(s4, s4)

  expectLT(s1, s2)
  expectNotEqual(s1.hashValue, s2.hashValue)
  expectLT(s2, s3)
  expectNotEqual(s2.hashValue, s3.hashValue)
  expectEqual(s3, s4)
  expectEqual(s3.hashValue, s4.hashValue)
}

suite.test("replaceSubrange") {
  let initial: String = "hello world!"

  let cases: [(Int64, String, String, String)] = 
    [(0, "hello", "goodbye", "goodbye world!"),        // Edit start
     (8, "world!", "moon?", "goodbye moon?"),          // Edit end
     (4, "bye", " night", "good night moon?"),         // Edit middle
     (4, "", " 🦊🦊🦊", "good 🦊🦊🦊 night moon?")]  // wide Characters

  var testSubject = initial;
  for (start, needle, replacement, result) in cases {
    let startIndex = testSubject.index(atOffset: start)
    
    testSubject.replaceSubrange(
      startIndex..<testSubject.index(startIndex, offsetBy: needle.count),
      with: replacement)
    
    expectEqual(testSubject, result)
  }

  // Make sure the initial value wasn't mutated
  expectEqual(initial, "hello world!")

  // Check implicit RangeReplaceable stuff works
  var hello: String = "Hello!"
  hello.removeLast()
  let newElements: String = ", 🌎!"
  hello += newElements
  expectEqual(hello, "Hello, 🌎!")
}

suite.test("cstring") {
  let s1: String = "abracadabra"
  expectEqual(s1.withCString(strlen), 11)
  let s2: String = "3.14159"
  expectEqual(3.14159,s2.withCString(atof))
  
  let s3: Swift.String = "some string"
  s3.withCString {
    let s = String(cString: $0)
    expectEqual("some string", s)
  }

  let utf16 = Array(s3.utf16) + [0]
  let s4 = utf16.withUnsafeBufferPointer {
    String(cString: $0.baseAddress!, encoding: UTF16.self)
  }
  expectEqual(s4, "some string")
}

suite.test("fcc-normalized-view") {
  let a: UInt16 = 0x0061
  let aTic: UInt16 = 0x00e0
  let aBackTic: UInt16 = 0x00e1
  typealias UTF16String = _UnicodeViews<[UInt16], UTF16>
  // typealias NormalizedView = FCCNormalizedUTF16View_2<[UInt16], UTF16>

  // Helper functions, eagerly forms arrays of the forwards and reverse
  // FCC normalized UTF16 code units
  func fccNormView(_ codeUnits: [UInt16])
    -> (forward: [UInt16], reversed: [UInt16]) {
    let view = UTF16String(codeUnits).fccNormalizedUTF16
    return (forward: Array(view),
            reversed: Array(view.reversed()))
  }

  // Test canonical equivalence for:
  //   1) a + ̀ + ́ == à + ́
  //   2) a + ́ + ̀ == á + ̀
  // BUT, the two are distinct, #1 != #2
  do {
    let str1form1 = [a, 0x0300, 0x0301]
    let str1form2 = [aTic, 0x0301]
    let str2form1 = [a, 0x0301, 0x0300]
    let str2form2 = [aBackTic, 0x0300]

    let (norm1_1, norm1_1rev) = fccNormView(str1form1)
    let (norm1_2, norm1_2rev) = fccNormView(str1form2)
    let (norm2_1, norm2_1rev) = fccNormView(str2form1)
    let (norm2_2, norm2_2rev) = fccNormView(str2form2)

    expectEqualSequence(norm1_1, norm1_2)
    expectEqualSequence(norm2_1, norm2_2)
    for (cu1, cu2) in zip(norm1_1, norm2_1) {
      expectNotEqual(cu1, cu2)
    }
    expectEqualSequence(norm1_1rev, norm1_2rev)
    expectEqualSequence(norm2_1rev, norm2_2rev)
    for (cu1, cu2) in zip(norm1_1rev, norm2_1rev) {
      expectNotEqual(cu1, cu2)
    }
  }

  // Test canonical equivalence, and non-combining-ness of FCC for:
  //   1) a + ̖ + ̀ == à + ̖ == a + ̀ + ̖
  //   All will normalize under FCC as a + ̖ + ̀
  do {
    let form1 = [a, 0x0316, 0x0300]
    let form2 = [a, 0x0300, 0x0316]
    let form3 = [aTic, 0x0316]

    let (norm1, norm1rev) = fccNormView(form1)
    let (norm2, norm2rev) = fccNormView(form2)
    let (norm3, norm3rev) = fccNormView(form3)

    expectEqualSequence(norm1, norm2)
    expectEqualSequence(norm2, norm3)
    expectEqualSequence(norm1rev, norm2rev)
    expectEqualSequence(norm2rev, norm3rev)

    // Form 1 is already in FCC
    expectEqualSequence(norm3, form1)
    expectEqualSequence(norm3rev, form1.reversed())
  }

  // Test non-start first scalars
  do {
    let form1 = [0x0300, a, 0x0300]
    let form2 = [0x0300, aTic] // In FCC normal form
    let (norm1, norm1rev) = fccNormView(form1)
    let (norm2, norm2rev) = fccNormView(form2)

    // Sanity check existing impl
    expectEqualSequence(norm1, norm2)
    expectEqualSequence(norm1rev, norm2rev)
    expectEqualSequence(norm1, form2)
  }
}

suite.test("_StringCore") {
  let oldStr : Swift.String = "abcdefg"
  let newStr : String = "abcdefg"
  expectEqualSequence(oldStr._core, newStr._core)

  // abcdefg ==> abcdef
  var str = String()
  str._core = newStr._core.dropLast()
  expectEqualSequence(
    ("abcdef" as StaticString).withUTF8Buffer { $0.map { UTF16.CodeUnit($0) } },
    str._core)

  // abcdef ==> abc̀df
  var idx = str._core.startIndex
  idx = str._core.index(idx, offsetBy: 3)
  str._core.insert(contentsOf: [0x0300], at: idx)
  idx = str._core.index(idx, offsetBy: 2)
  str._core.remove(at: idx)
  expectEqualSequence(
    ("abc\u{300}df" as StaticString).withUTF8Buffer {
      Array(_UnicodeViews($0, UTF8.self).transcoded(to: UTF16.self))
    }, Array(str._core))
}

import Foundation
suite.test("bridging") {
  do {
    let s: String = "abc\n🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\nΣὲ 👥🥓γνωρίζω\nგთხოვთ\nงบู๊กู้ขึ้นม่\nᚹᛖᛥᚫ"
    let n = s as NSString
    // Generalized NSString representation
    expectNotNil(TrueReference(n))
    let s2 = n as String
    expectEqual(s, s2)
  }

  do {
    let s: String = "ace of case"
    expectNotNil(s.content._inline5or6) // 5-bit per character inline buffer
    let n = s as NSString
    // Need to fix bridging by introducing a shim for NSString construction.
//    expectNil(TrueReference(n))         // 5-bit per character tagged NSString
    let s2 = n as String
    expectEqual(s, s2)
  }
  
  do {
    let s: String = "UL2O85UL2" 
    expectNotNil(s.content._inline5or6) // 6-bit per character inline buffer
    let n = s as NSString
    // Need to fix bridging by introducing a shim for NSString construction.
//    expectNil(TrueReference(n))         // 6-bit per character tagged NSString
    let s2 = n as String
    expectEqual(s, s2)
  }

  do {
    let s: String = "ZZZZ"              
    expectNotNil(s.content._inline7or16) // 7 bit ASCII inline buffer
    let n = s as NSString
    // Need to fix bridging by introducing a shim for NSString construction.
//    expectNil(TrueReference(n))          // ASCII tagged NSString representation
    let s2 = n as String
    expectEqual(s, s2)
  }

  do {
    let s: String = "¢¤¥"          // UTF16 inline buffer
    expectNotNil(s.content._inline7or16)
    let n = s as NSString
    expectNotNil(TrueReference(n)) // No non-ASCII tagged NSStrings, it seems
    let s2 = n as String
    expectEqual(s, s2)
  }
}

suite.test("AnyUInt16UnicodeView.DoubleWrapping") {
  let s: String = "Bouffant Hairdo"
  let u1 = s.content.utf16
  let u2 = AnyUInt16UnicodeView(u1)
  expectEqual(type(of: u1.base), type(of: u2.base))
}

suite.test("Substring") {
  let s: String = "abc\n🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\nΣὲ 👥🥓γνωρίζω\nგთხოვთ\nงบู๊กู้ขึ้นม่\nᚹᛖᛥᚫ "
  let u8 = s.content.utf8
  let d = u8.base2.distance(
    from: u8._unwrap(u8.startIndex),
    to: u8._unwrap(s.index(atOffset: 1)))
  expectNotEqual(0, d)
  for i in s.indices {
    for j in s[i...].indices {
      expectEqualSequence(s.content.utf16[i..<j], s[i..<j].content.utf16)
      expectEqualSequence(s.content.utf8[i..<j], s[i..<j].content.utf8)
      expectEqualSequence(
        s.content.unicodeScalars[i..<j], s[i..<j].content.unicodeScalars)
    }
  }
}

runAllTests()
