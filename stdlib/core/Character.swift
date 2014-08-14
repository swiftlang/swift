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

internal struct IntEncoder : SinkType {
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
public enum Character :
  _BuiltinExtendedGraphemeClusterLiteralConvertible,
  ExtendedGraphemeClusterLiteralConvertible, Equatable, Hashable, Comparable {

  // Fundamentally, it is just a String, but it is optimized for the
  // common case where the UTF-8 representation fits in 63 bits.  The
  // remaining bit is used to discriminate between small and large
  // representations.  In the small representation, the unused bytes
  // are filled with 0xFF.
  //
  // If the grapheme cluster can be represented in SmallRepresentation, it
  // should be represented as such.
  case LargeRepresentation(OnHeap<String>)
  case SmallRepresentation(Builtin.Int63)

  public init(_ scalar: UnicodeScalar) {
    var IE  = IntEncoder()
    UTF8.encode(scalar, output: &IE)
    IE.asInt |= (~0) << IE.shift
    self = SmallRepresentation(Builtin.trunc_Int64_Int63(IE.asInt.value))
  }

  @effects(readonly)
  public static func _convertFromBuiltinUnicodeScalarLiteral(
    value: Builtin.Int32) -> Character {
    return Character(
      String._fromWellFormedCodeUnitSequence(
        UTF32.self, input: CollectionOfOne(UInt32(value))))
  }

  public static func convertFromUnicodeScalarLiteral(
    value: Character) -> Character {
    return value
  }

  @effects(readonly)
  public static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
      start: Builtin.RawPointer,
      byteSize: Builtin.Word,
      isASCII: Builtin.Int1) -> Character {
    return Character(
        String._convertFromBuiltinExtendedGraphemeClusterLiteral(
            start, byteSize: byteSize, isASCII: isASCII))
  }

  public static func convertFromExtendedGraphemeClusterLiteral(
      value: Character) -> Character {
    return value
  }

  public init(_ s: String) {
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
      s._core.count != 0, "Can't form a Character from an empty String")
    _precondition(
      s.startIndex.successor() == s.endIndex,
      "Can't form a Character from a String containing more than one extended grapheme cluster")

    var (count, initialUTF8) = s._core._encodeSomeUTF8(0)
    // Notice that the result of sizeof() is a small non-zero number and can't
    // overflow when multiplied by 8.
    let bits = sizeofValue(initialUTF8) &* 8 &- 1
    if _fastPath(
      count == s._core.count && (initialUTF8 & (1 << numericCast(bits))) != 0) {
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

  struct SmallUTF16 : CollectionType {
    init(var _ u8: UInt64) {
      let input = UnsafeBufferPointer(
        start: UnsafePointer<UTF8.CodeUnit>(Builtin.addressof(&u8)), 
        count: Character._smallSize(u8)
      )
      let count = UTF16.measure(
          UTF8.self, input: input.generate(), repairIllFormedSequences: true)!.0
      _sanityCheck(count <= 4, "Character with more than 4 UTF16 code units")
      self.count = UInt16(count)
      data = 0
      let dest = UnsafeMutablePointer<UTF16.CodeUnit>(Builtin.addressof(&data))
      transcode(
        UTF8.self, UTF16.self, input.generate(), dest, stopOnError: false)
      _fixLifetime(u8)
    }
    var startIndex : Int {
      return 0
    }
    var endIndex : Int {
      return Int(count)
    }
    subscript(i: Int) -> UTF16.CodeUnit {
      var d = data
      return UnsafePointer<UTF16.CodeUnit>(Builtin.addressof(&d))[i]
    }
    func generate() -> IndexingGenerator<SmallUTF16> {
      return IndexingGenerator(self)
    }
    
    var count: UInt16
    var data: UInt64
  }
  
  public var hashValue: Int {
    // FIXME(performance): constructing a temporary string is extremely
    // wasteful and inefficient.
    return String(self).hashValue
  }

  typealias UTF16View = String.UTF16View
  var utf16: UTF16View {
    return String(self).utf16
  }
}

extension String {
  public init(_ c: Character) {
    switch c {
    case .SmallRepresentation(var _63bits):
      var value = Character._smallValue(_63bits)
      var size = Character._smallSize(value)
      self = String._fromWellFormedCodeUnitSequence(
        UTF8.self,
        input: UnsafeBufferPointer(
          start: UnsafeMutablePointer<UTF8.CodeUnit>(
            Builtin.addressof(&value)), 
          count: size))
    case .LargeRepresentation(var value):
      self = value._value
    }
  }
}

public func ==(lhs: Character, rhs: Character) -> Bool {
  // FIXME(performance): constructing two temporary strings is extremely
  // wasteful and inefficient.
  return String(lhs) == String(rhs)
}

public func <(lhs: Character, rhs: Character) -> Bool {
  // FIXME(performance): constructing two temporary strings is extremely
  // wasteful and inefficient.
  return String(lhs) < String(rhs)
}
