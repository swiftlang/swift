//===--- UTF8.swift -------------------------------------------------------===//
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
extension _Unicode.UTF8 : UnicodeEncoding {
  public typealias EncodedScalar = _UIntBuffer<UInt32, UInt8>

  public static var encodedReplacementCharacter : EncodedScalar {
    return EncodedScalar(_storage: 0xbdbfef, _bitCount: 24)
  }

  public static func _isScalar(_ x: CodeUnit) -> Bool {
    return x & 0x80 == 0
  }

  public static func decode(_ source: EncodedScalar) -> UnicodeScalar {
    let bits = source._storage
    switch source._bitCount {
    case 8:
      return UnicodeScalar(_unchecked: bits)
    case 16:
      var value = (bits & 0b0_______________________11_1111__0000_0000) &>> 8
      value    |= (bits & 0b0________________________________0001_1111) &<< 6
      return UnicodeScalar(_unchecked: value)
    case 24:
      var value = (bits & 0b0____________11_1111__0000_0000__0000_0000) &>> 16
      value    |= (bits & 0b0_______________________11_1111__0000_0000) &>> 2
      value    |= (bits & 0b0________________________________0000_1111) &<< 12
      return UnicodeScalar(_unchecked: value)
    default:
      _sanityCheck(source.count == 4)
      var value = (bits & 0b0_11_1111__0000_0000__0000_0000__0000_0000) &>> 24
      value    |= (bits & 0b0____________11_1111__0000_0000__0000_0000) &>> 10
      value    |= (bits & 0b0_______________________11_1111__0000_0000) &<< 4
      value    |= (bits & 0b0________________________________0000_0111) &<< 18
      return UnicodeScalar(_unchecked: value)
    }
  }
  
  public static func encode(_ source: UnicodeScalar) -> EncodedScalar {
    let x = source.value
    if _fastPath(x < (1 << 7)) {
      return EncodedScalar(_storage: x, _bitCount: 8)
    }
    else if _fastPath(x < (1 << 11)) {
      var r = x &>> 6
      r |= (x & 0b11_1111) &<< 8
      r |= 0b1000_0000__1100_0000
      return EncodedScalar(_storage: r, _bitCount: 2*8)
    }
    else if _fastPath(x < (1 << 16)) {
      var r = x &>> 12
      r |= (x & 0b1111__1100_0000) &<< 2
      r |= (x & 0b11_1111) &<< 16
      r |= 0b1000_0000__1000_0000__1110_0000
      return EncodedScalar(_storage:  r, _bitCount: 3*8)
    }
    else {
      var r = x &>> 18
      r |= (x & 0b11__1111_0000__0000_0000) &>> 4
      r |= (x & 0b1111__1100_0000) &<< 10
      r |= (x & 0b11_1111) << 24
      r |= 0b1000_0000__1000_0000__1000_0000__1111_0000
      return EncodedScalar(_storage: r, _bitCount: 4*8)
    }
  }
  
  public struct ForwardParser {
    public typealias _Buffer = _UIntBuffer<UInt32, UInt8>
    public init() { _buffer = _Buffer() }
    public var _buffer: _Buffer
  }
  
  public struct ReverseParser {
    public typealias _Buffer = _UIntBuffer<UInt32, UInt8>
    public init() { _buffer = _Buffer() }
    public var _buffer: _Buffer
  }
}

extension UTF8.ReverseParser : UnicodeParser, _UTFParser {
  public typealias Encoding = _Unicode.UTF8

  public func _parseMultipleCodeUnits() -> (isValid: Bool, bitCount: UInt8) {
    _sanityCheck(_buffer._storage & 0x80 != 0) // this case handled elsewhere
    if _buffer._storage                & 0b0__1110_0000__1100_0000
                                      == 0b0__1100_0000__1000_0000 {
      // 2-byte sequence.  Top 4 bits of decoded result must be nonzero
      let top4Bits =  _buffer._storage & 0b0__0001_1110__0000_0000
      if _fastPath(top4Bits != 0) { return (true, 2*8) }
    }
    else if _buffer._storage     & 0b0__1111_0000__1100_0000__1100_0000
                                == 0b0__1110_0000__1000_0000__1000_0000 {
      // 3-byte sequence. The top 5 bits of the decoded result must be nonzero
      // and not a surrogate
      let top5Bits = _buffer._storage & 0b0__1111__0010_0000__0000_0000
      if _fastPath(
        top5Bits != 0 &&    top5Bits != 0b0__1101__0010_0000__0000_0000) {
        return (true, 3*8)
      }
    }
    else if _buffer._storage & 0b0__1111_1000__1100_0000__1100_0000__1100_0000
                            == 0b0__1111_0000__1000_0000__1000_0000__1000_0000 {
      // Make sure the top 5 bits of the decoded result would be in range
      let top5bits = _buffer._storage
                                  & 0b0__0111__0011_0000__0000_0000__0000_0000
      if _fastPath(
        top5bits != 0
        && top5bits <=              0b0__0100__0000_0000__0000_0000__0000_0000
      ) { return (true, 4*8) }
    }
    return (false, _invalidLength() &* 8)
  }

  /// Returns the length of the invalid sequence that ends with the LSB of
  /// buffer.
  @inline(never)
  func _invalidLength() -> UInt8 {
    if _buffer._storage                 & 0b0__1111_0000__1100_0000
                                       == 0b0__1110_0000__1000_0000 {
      // 2-byte prefix of 3-byte sequence. The top 5 bits of the decoded result
      // must be nonzero and not a surrogate
      let top5Bits = _buffer._storage        & 0b0__1111__0010_0000
      if top5Bits != 0 &&          top5Bits != 0b0__1101__0010_0000 { return 2 }
    }
    else if _buffer._storage               & 0b1111_1000__1100_0000
                                          == 0b1111_0000__1000_0000
    {
      // 2-byte prefix of 4-byte sequence
      // Make sure the top 5 bits of the decoded result would be in range
      let top5bits =        _buffer._storage & 0b0__0111__0011_0000
      if top5bits != 0 &&          top5bits <= 0b0__0100__0000_0000 { return 2 }
    }
    else if _buffer._storage & 0b0__1111_1000__1100_0000__1100_0000
                            == 0b0__1111_0000__1000_0000__1000_0000 {
      // 3-byte prefix of 4-byte sequence
      // Make sure the top 5 bits of the decoded result would be in range
      let top5bits = _buffer._storage & 0b0__0111__0011_0000__0000_0000
      if top5bits != 0 &&   top5bits <= 0b0__0100__0000_0000__0000_0000 {
        return 3
      }
    }
    return 1
  }
  
  public func _bufferedScalar(bitCount: UInt8) -> Encoding.EncodedScalar {
    return Encoding.EncodedScalar(
      _storage: _buffer._storage.byteSwapped &>> (32 - bitCount),
      _bitCount: bitCount
    )
  }
}

extension _Unicode.UTF8.ForwardParser : UnicodeParser, _UTFParser {
  public typealias Encoding = _Unicode.UTF8
  
  public func _parseMultipleCodeUnits() -> (isValid: Bool, bitCount: UInt8) {
    _sanityCheck(_buffer._storage & 0x80 != 0) // this case handled elsewhere
    
    if _buffer._storage & 0b0__1100_0000__1110_0000
                       == 0b0__1000_0000__1100_0000 {
      // 2-byte sequence. At least one of the top 4 bits of the decoded result
      // must be nonzero.
      if _fastPath(_buffer._storage & 0b0_0001_1110 != 0) { return (true, 2*8) }
    }
    else if _buffer._storage         & 0b0__1100_0000__1100_0000__1111_0000
                                    == 0b0__1000_0000__1000_0000__1110_0000 {
      // 3-byte sequence. The top 5 bits of the decoded result must be nonzero
      // and not a surrogate
      let top5Bits =          _buffer._storage & 0b0___0010_0000__0000_1111
      if _fastPath(top5Bits != 0 && top5Bits != 0b0___0010_0000__0000_1101) {
        return (true, 3*8)
      }
    }
    else if _buffer._storage & 0b0__1100_0000__1100_0000__1100_0000__1111_1000
                            == 0b0__1000_0000__1000_0000__1000_0000__1111_0000 {
      // 4-byte sequence.  The top 5 bits of the decoded result must be nonzero
      // and no greater than 0b0__0100_0000
      let top5bits = UInt16(_buffer._storage       & 0b0__0011_0000__0000_0111)
      if _fastPath(
        top5bits != 0
        && top5bits.byteSwapped                   <= 0b0__0000_0100__0000_0000
      ) { return (true, 4*8) }
    }
    return (false, _invalidLength() &* 8)
  }

  /// Returns the length of the invalid sequence that starts with the LSB of
  /// buffer.
  @inline(never)
  func _invalidLength() -> UInt8 {
    if _buffer._storage               & 0b0__1100_0000__1111_0000
                                     == 0b0__1000_0000__1110_0000 {
      // 2-byte prefix of 3-byte sequence. The top 5 bits of the decoded result
      // must be nonzero and not a surrogate
      let top5Bits = _buffer._storage & 0b0__0010_0000__0000_1111
      if top5Bits != 0 && top5Bits   != 0b0__0010_0000__0000_1101 { return 2 }
    }
    else if _buffer._storage                & 0b0__1100_0000__1111_1000
                                           == 0b0__1000_0000__1111_0000
    {
      // Prefix of 4-byte sequence. The top 5 bits of the decoded result
      // must be nonzero and no greater than 0b0__0100_0000
      let top5bits = UInt16(_buffer._storage & 0b0__0011_0000__0000_0111)
      if top5bits != 0 && top5bits.byteSwapped <= 0b0__0000_0100__0000_0000 {
        return _buffer._storage   & 0b0__1100_0000__0000_0000__0000_0000
                                 == 0b0__1000_0000__0000_0000__0000_0000 ? 3 : 2
      }
    }
    return 1
  }
  
  public func _bufferedScalar(bitCount: UInt8) -> Encoding.EncodedScalar {
    var r = _buffer
    r._bitCount = bitCount
    return r
  }
}

