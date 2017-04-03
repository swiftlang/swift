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
import Swift


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
          let usedCount = __swift_stdlib_unorm2_normalize(
            _fccNormalizer, $0.baseAddress, numericCast($0.count),
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

runAllTests()