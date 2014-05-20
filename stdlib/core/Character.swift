//===----------------------------------------------------------------------===//
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

struct IntEncoder : Sink {
  var asInt: UInt64 = 0
  var shift: UInt64 = 0
  mutating func put(x: UTF8.CodeUnit) {
    asInt |= UInt64(x) << shift
    shift += 8
  }
}

/// `Character` represents some Unicode grapheme cluster as
/// defined by a canonical, localized, or otherwise tailored
/// segmentation algorithm.
enum Character :
  _BuiltinExtendedGraphemeClusterLiteralConvertible,
  ExtendedGraphemeClusterLiteralConvertible, Equatable {

  // Fundamentally, it is just a String, but it is optimized for the
  // common case where the UTF-8 representation fits in 63 bits.  The
  // remaining bit is used to discriminate between small and large
  // representations.  In the small representation, the unused bytes 
  // representations.  In the small representation, the unused bytes
  // are filled with 0xFF.
  //
  // If the grapheme cluster can be represented in SmallRepresentation, it
  // should be represented as such.
  case LargeRepresentation(OnHeap<String>)
  case SmallRepresentation(Builtin.Int63)

  init(_ scalar: UnicodeScalar) {
    var IE  = IntEncoder()
    UTF8.encode(scalar, output: &IE)
    IE.asInt |= (~0) << IE.shift
    self = SmallRepresentation(Builtin.trunc_Int64_Int63(IE.asInt.value))
  }

  static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
      start: Builtin.RawPointer,
      byteSize: Builtin.Word,
      isASCII: Builtin.Int1) -> Character {
    return Character(
        String._convertFromBuiltinExtendedGraphemeClusterLiteral(
            start, byteSize: byteSize, isASCII: isASCII))
  }

  static func convertFromExtendedGraphemeClusterLiteral(
      value: Character) -> Character {
    return value
  }

  init(_ s: String) {
    // The small representation can accept up to 8 code units as long
    // as the last one is a continuation.  Since the high bit of the
    // last byte is used for the enum's discriminator, we have to
    // reconstruct it.  As a result, we can't store 0x7f in the final
    // byte, because we wouldn't be able to distinguish it from an
    // unused 0xFF byte.  Rather than trying to squeeze in other
    // one-byte code points there, we simplify decoding by banning
    // starting a code point in the last byte, and assuming that its
    // high bit is 1.
    _precondition(
      s.core.count != 0, "Can't form a Character from an empty String")

    var (count, initialUTF8) = s.core._encodeSomeUTF8(0)
    let bits = sizeofValue(initialUTF8) * 8 - 1
    if _fastPath(
      count == s.core.count && (initialUTF8 & (1 << numericCast(bits))) != 0) {
      self = SmallRepresentation(Builtin.trunc_Int64_Int63(initialUTF8.value))
    }
    else {
      self = LargeRepresentation(OnHeap(s))
    }
  }

  /// Return the index of the lowest byte that is 0xFF, or 8 if
  /// there is none
  static func _smallSize(value: UInt64) -> Int {
    var mask: UInt64 = 0xFF
    for (var i = 0; i < 8; ++i) {
      if (value & mask) == mask {
        return i
      }
      mask <<= 8
    }
    return 8
  }

  static func _smallValue(value: Builtin.Int63) -> UInt64 {
    return UInt64(Builtin.zext_Int63_Int64(value)) | (1<<63)
  }
}

extension String {
  init(_ c: Character) {
    switch c {
    case .SmallRepresentation(var _63bits):
      var value = Character._smallValue(_63bits)
      var size = Character._smallSize(value)
      self = String(
        UTF8.self,
        input: UnsafeArray(
          start: UnsafePointer<UTF8.CodeUnit>(Builtin.addressof(&value)), 
          length: size))
    case .LargeRepresentation(var value):
      self = value
    }
  }
}

func ==(lhs: Character, rhs: Character) -> Bool {
  switch (lhs, rhs) {
  case (.LargeRepresentation(let lhsValue), .LargeRepresentation(let rhsValue)):
    return (lhsValue as String) == rhsValue

  case (.SmallRepresentation(let lhsValue), .SmallRepresentation(let rhsValue)):
    return Character._smallValue(lhsValue) == Character._smallValue(rhsValue)

  default:
    return false
  }
}

