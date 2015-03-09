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
//
// MessagePack encoder and decoder.
//
// MessagePack specification is located at http://msgpack.org/
//
//===----------------------------------------------------------------------===//

/// An encoder for MessagePack.
///
/// This encoder provides a StAX-like interface.
public struct MsgPackEncoder {
  // FIXME: This should be a Sink.
  // Currently it is not for performance reasons (StdlibUnittest
  // code can not be specialized).
  var bytes: [UInt8] = []

  internal var _expectedElementCount: [Int] = [ 0 ]
  internal var _actualElementCount: [Int] = [ 0 ]

  internal mutating func _appendBigEndian(value: Swift.UInt64) {
    var x = value.byteSwapped
    for i in 0..<8 {
      bytes.append(UInt8(truncatingBitPattern: x))
      x >>= 8
    }
  }

  internal mutating func _appendBigEndian(value: Swift.UInt32) {
    var x = value.byteSwapped
    for i in 0..<4 {
      bytes.append(UInt8(truncatingBitPattern: x))
      x >>= 8
    }
  }

  internal mutating func _appendBigEndian(value: Swift.UInt16) {
    var x = value.byteSwapped
    for i in 0..<2 {
      bytes.append(UInt8(truncatingBitPattern: x))
      x >>= 8
    }
  }

  internal mutating func _appendBigEndian(value: Swift.Int64) {
    _appendBigEndian(Swift.UInt64(bitPattern: value))
  }

  internal mutating func _addedElement() {
    _actualElementCount[_actualElementCount.count - 1]++
  }

  public mutating func append(i: Int64) {
    bytes.reserveCapacity(bytes.count + 9)
    bytes.append(0xd3)
    _appendBigEndian(i)

    _addedElement()
  }

  public mutating func append(i: UInt64) {
    bytes.reserveCapacity(bytes.count + 9)
    bytes.append(0xcf)
    _appendBigEndian(i)

    _addedElement()
  }

  public mutating func appendNil() {
    bytes.append(0xc0)

    _addedElement()
  }

  public mutating func append(b: Bool) {
    bytes.append(b ? 0xc3 : 0xc2)

    _addedElement()
  }

  public mutating func append(f: Float32) {
    bytes.reserveCapacity(bytes.count + 5)
    bytes.append(0xca)
    _appendBigEndian(f._toBitPattern())

    _addedElement()
  }

  public mutating func append(f: Float64) {
    bytes.reserveCapacity(bytes.count + 9)
    bytes.append(0xcb)
    _appendBigEndian(f._toBitPattern())

    _addedElement()
  }

  public mutating func append(s: String) {
    let utf8Bytes = Array(s.utf8)
    switch Int64(utf8Bytes.count) {
    case 0...31:
      // fixstr
      bytes.append(0b1010_0000 | UInt8(utf8Bytes.count))
    case 32...0xff:
      // str8
      bytes.append(0xd9)
      bytes.append(UInt8(utf8Bytes.count))
    case 0x100...0xffff:
      // str16
      bytes.append(0xda)
      _appendBigEndian(UInt16(utf8Bytes.count))
    case 0x1_0000...0xffff_ffff:
      // str32
      bytes.append(0xdb)
      _appendBigEndian(UInt32(utf8Bytes.count))
    default:
      // FIXME: better error handling.  Trapping is at least secure.
      fatalError("string is too long")
    }
    bytes += utf8Bytes

    _addedElement()
  }

  public mutating func append(dataBytes: [UInt8]) {
    switch Int64(dataBytes.count) {
    case 0...0xff:
      // bin8
      bytes.append(0xc4)
      bytes.append(UInt8(dataBytes.count))
    case 0x100...0xffff:
      // bin16
      bytes.append(0xc5)
      _appendBigEndian(UInt16(dataBytes.count))
    case 0x1_0000...0xffff_ffff:
      // bin32
      bytes.append(0xc6)
      _appendBigEndian(UInt32(dataBytes.count))
    default:
      // FIXME: better error handling.  Trapping is at least secure.
      fatalError("binary data is too long")
    }
    bytes += dataBytes

    _addedElement()
  }

  public mutating func beginArray(count: Int) {
    switch Int64(count) {
    case 0...0xf:
      // fixarray
      bytes.append(0b1001_0000 | UInt8(count))
    case 0x10...0xffff:
      // array16
      bytes.append(0xdc)
      _appendBigEndian(UInt16(count))
    case 0x1_0000...0xffff_ffff:
      // array32
      bytes.append(0xdd)
      _appendBigEndian(UInt32(count))
    default:
      // FIXME: better error handling.  Trapping is at least secure.
      fatalError("array is too long")
    }

    _expectedElementCount.append(count)
    _actualElementCount.append(0)
  }

  public mutating func endArray() {
    let expectedCount = _expectedElementCount.removeLast()
    let actualCount = _actualElementCount.removeLast()
    if expectedCount != actualCount {
      fatalError("Actual number of elements in the array does not match the expected number")
    }

    _addedElement()
  }

  public mutating func beginMap(mappingCount: Int) {
    switch Int64(mappingCount) {
    case 0...0xf:
      bytes.append(0b1000_0000 | UInt8(mappingCount))
    case 0x10...0xffff:
      bytes.append(0xde)
      _appendBigEndian(UInt16(mappingCount))
    case 0x1_0000...0xffff_ffff:
      bytes.append(0xdf)
      _appendBigEndian(UInt32(mappingCount))
    default:
      // FIXME: better error handling.  Trapping is at least secure.
      fatalError("map is too long")
    }

    _expectedElementCount.append(mappingCount * 2)
    _actualElementCount.append(0)
  }

  public mutating func endMap() {
    let expectedCount = _expectedElementCount.removeLast()
    let actualCount = _actualElementCount.removeLast()
    if expectedCount != actualCount {
      fatalError("Actual number of elements in the map does not match the expected number")
    }

    _addedElement()
  }

  public mutating func appendExtended(#type: Int8, data: [UInt8]) {
    switch Int64(data.count) {
    case 1:
      // fixext1
      bytes.append(0xd4)
    case 2:
      // fixext2
      bytes.append(0xd5)
    case 4:
      // fixext4
      bytes.append(0xd6)
    case 8:
      // fixext8
      bytes.append(0xd7)
    case 16:
      // fixext16
      bytes.append(0xd8)
    case 0...0xff:
      // ext8
      bytes.append(0xc7)
      bytes.append(UInt8(data.count))
    case 0x100...0xffff:
      // ext16
      bytes.append(0xc8)
      _appendBigEndian(UInt16(data.count))
    case 0x1_0000...0xffff_ffff:
      // ext32
      bytes.append(0xc9)
      _appendBigEndian(UInt32(data.count))
    default:
      fatalError("extended data is too long")
    }
    bytes.append(UInt8(bitPattern: type))
    bytes += data
  }
}

internal func _safeUInt32ToInt(x: UInt32) -> Int? {
#if arch(i386) || arch(arm)
  if x > UInt32(Int.max) {
    return nil
  } else {
    return Int(x)
  }
#elseif arch(x86_64) || arch(arm64)
  return Int(x)
#else
  fatalError("unimplemented")
#endif
}

/// A decoder for MessagePack.
///
/// This decoder provides a StAX-like interface.
public struct MsgPackDecoder {
  // FIXME: This should be a Generator.
  // Currently it is not for performance reasons (StdlibUnittest
  // code can not be specialized).
  //
  // Or maybe not, since the caller might want to know how many
  // bytes were consumed.
  internal let _bytes: [UInt8]

  internal var _consumedCount: Int = 0

  public var consumedCount: Int {
    return _consumedCount
  }

  public init(_ bytes: [UInt8]) {
    self._bytes = bytes
  }

  internal func _haveNBytes(count: Int) -> Bool {
    return _bytes.count >= _consumedCount + count
  }

  internal func _lookByte() -> UInt8? {
    if _haveNBytes(1) {
      return _bytes[_consumedCount]
    }
    return nil
  }

  internal mutating func _consumeByte() -> UInt8? {
    if let result = _lookByte() {
      _consumedCount++
      return result
    }
    return nil
  }

  internal mutating func _consumeByteIf(byte: UInt8) -> Bool {
    if _lookByte() == byte {
      _consumedCount++
      return true
    }
    return false
  }

  internal mutating func _readBigEndianUInt16() -> UInt16? {
    if _haveNBytes(2) {
      var result: UInt16 = 0
      for i in 0..<2 {
        result <<= 8
        result |= UInt16(_consumeByte()!)
      }
      return result
    }
    return nil
  }

  internal mutating func _readBigEndianUInt32() -> UInt32? {
    if _haveNBytes(4) {
      var result: UInt32 = 0
      for i in 0..<4 {
        result <<= 8
        result |= UInt32(_consumeByte()!)
      }
      return result
    }
    return nil
  }

  internal mutating func _readBigEndianInt64() -> Int64? {
    if let result = _readBigEndianUInt64() {
      return Int64(bitPattern: result)
    }
    return nil
  }

  internal mutating func _readBigEndianUInt64() -> UInt64? {
    if _haveNBytes(8) {
      var result: UInt64 = 0
      for i in 0..<8 {
        result <<= 8
        result |= UInt64(_consumeByte()!)
      }
      return result
    }
    return nil
  }

  internal mutating func _rewindIfReturnsNil<T>(
    @noescape code: () -> T?
  ) -> T? {
    let originalPosition = _consumedCount
    if let result = code() {
      return result
    }
    _consumedCount = originalPosition
    return nil
  }

  public mutating func readInt64() -> Int64? {
    return _rewindIfReturnsNil {
      if _consumeByteIf(0xd3) {
        return _readBigEndianInt64()
      }
      return nil
    }
  }

  public mutating func readUInt64() -> UInt64? {
    return _rewindIfReturnsNil {
      if _consumeByteIf(0xcf) {
        return _readBigEndianUInt64()
      }
      return nil
    }
  }

  public mutating func readNil() -> Bool {
    return _consumeByteIf(0xc0)
  }

  public mutating func readBool() -> Bool? {
    if _consumeByteIf(0xc3) {
      return true
    }
    if _consumeByteIf(0xc2) {
      return false
    }
    return nil
  }

  public mutating func readFloat32() -> Float32? {
    return _rewindIfReturnsNil {
      if _consumeByteIf(0xca) {
        if let bitPattern = _readBigEndianUInt32() {
          return Float32._fromBitPattern(bitPattern)
        }
      }
      return nil
    }
  }

  public mutating func readFloat64() -> Float64? {
    return _rewindIfReturnsNil {
      if _consumeByteIf(0xcb) {
        if let bitPattern = _readBigEndianUInt64() {
          return Float64._fromBitPattern(bitPattern)
        }
      }
      return nil
    }
  }

  public mutating func readString() -> String? {
    return _rewindIfReturnsNil {
      var maybeLength: Int? = nil
      if let byte = _lookByte() where byte & 0b1110_0000 == 0b1010_0000 {
        // fixstr
        _consumeByte()
        maybeLength = Int(byte & 0b0001_1111)
      } else if _consumeByteIf(0xd9) {
        // str8
        if let length = _consumeByte() {
          if length <= 0x1f {
            // Reject overlong encodings.
            return nil
          }
          maybeLength = Int(length)
        }
      } else if _consumeByteIf(0xda) {
        // str16
        if let length = _readBigEndianUInt16() {
          if length <= 0xff {
            // Reject overlong encodings.
            return nil
          }
          maybeLength = Int(length)
        }
      } else if _consumeByteIf(0xdb) {
        // str32
        if let length = _readBigEndianUInt32() {
          if length <= 0xffff {
            // Reject overlong encodings.
            return nil
          }
          maybeLength = Int(length)
        }
      }
      if let length = maybeLength {
        if _haveNBytes(length) {
          let utf8 = _bytes[_consumedCount..<_consumedCount + length]
          _consumedCount += length
          return String._fromCodeUnitSequenceWithRepair(UTF8.self, input: utf8).0
        }
      }
      return nil
    }
  }

  public mutating func readBinary() -> [UInt8]? {
    return _rewindIfReturnsNil {
      var maybeLength: Int? = nil
      if _consumeByteIf(0xc4) {
        // bin8
        if let length = _consumeByte() {
          maybeLength = Int(length)
        }
      } else if _consumeByteIf(0xc5) {
        // bin16
        if let length = _readBigEndianUInt16() {
          if length <= 0xff {
            // Reject overlong encodings.
            return nil
          }
          maybeLength = Int(length)
        }
      } else if _consumeByteIf(0xc6) {
        // bin32
        if let length = _readBigEndianUInt32() {
          if length <= 0xffff {
            // Reject overlong encodings.
            return nil
          }
          maybeLength = Int(length)
        }
      }
      if let length = maybeLength {
        if _haveNBytes(length) {
          let result = Array(_bytes[_consumedCount..<_consumedCount + length])
          _consumedCount += length
          return result
        }
      }
      return nil
    }
  }

  public mutating func readBeginArray() -> Int? {
    return _rewindIfReturnsNil {
      if let byte = _lookByte() where byte & 0b1111_0000 == 0b1001_0000 {
        // fixarray
        _consumeByte()
        return Int(byte & 0b0000_1111)
      } else if _consumeByteIf(0xdc) {
        // array16
        if let length = _readBigEndianUInt16() {
          if length <= 0xf {
            // Reject overlong encodings.
            return nil
          }
          return Int(length)
        }
      } else if _consumeByteIf(0xdd) {
        // array32
        if let length = _readBigEndianUInt32() {
          if length <= 0xffff {
            // Reject overlong encodings.
            return nil
          }
          return _safeUInt32ToInt(length)
        }
      }
      return nil
    }
  }

  public mutating func readBeginMap() -> Int? {
    return _rewindIfReturnsNil {
      if let byte = _lookByte() where byte & 0b1111_0000 == 0b1000_0000 {
        // fixarray
        _consumeByte()
        return Int(byte & 0b0000_1111)
      } else if _consumeByteIf(0xde) {
        // array16
        if let length = _readBigEndianUInt16() {
          if length <= 0xf {
            // Reject overlong encodings.
            return nil
          }
          return Int(length)
        }
      } else if _consumeByteIf(0xdf) {
        // array32
        if let length = _readBigEndianUInt32() {
          if length <= 0xffff {
            // Reject overlong encodings.
            return nil
          }
          return _safeUInt32ToInt(length)
        }
      }
      return nil
    }
  }

  public mutating func readExtended() -> (type: Int8, data: [UInt8])? {
    return _rewindIfReturnsNil {
      var maybeLength: Int? = nil
      if _consumeByteIf(0xd4) {
        // fixext1
        maybeLength = 1
      } else if _consumeByteIf(0xd5) {
        // fixext2
        maybeLength = 2
      } else if _consumeByteIf(0xd6) {
        // fixext4
        maybeLength = 4
      } else if _consumeByteIf(0xd7) {
        // fixext8
        maybeLength = 8
      } else if _consumeByteIf(0xd8) {
        // fixext16
        maybeLength = 16
      } else if _consumeByteIf(0xc7) {
        // ext8
        if let length = _consumeByte() {
          if length == 1 || length == 2 || length == 4 || length == 8 ||
            length == 16 {
            // Reject overlong encodings.
            return nil
          }
          maybeLength = Int(length)
        }
      } else if _consumeByteIf(0xc8) {
        // ext16
        if let length = _readBigEndianUInt16() {
          if length <= 0xff {
            // Reject overlong encodings.
            return nil
          }
          maybeLength = Int(length)
        }
      } else if _consumeByteIf(0xc9) {
        // ext32
        if let length = _readBigEndianUInt32() {
          if length <= 0xffff {
            // Reject overlong encodings.
            return nil
          }
          maybeLength = Int(length)
        }
      }
      if let length = maybeLength, let type = _consumeByte() {
        if _haveNBytes(length) {
          let result = Array(_bytes[_consumedCount..<_consumedCount + length])
          _consumedCount += length
          return (Int8(bitPattern: type), result)
        }
      }
      return nil
    }
  }
}

public struct MsgPackVariantArray : CollectionType {
  internal var _data: [MsgPackVariant]

  public init(_ data: [MsgPackVariant]) {
    self._data = data
  }

  public func generate() -> IndexingGenerator<MsgPackVariantArray> {
    return IndexingGenerator(self)
  }

  public var startIndex: Int {
    return _data.startIndex
  }

  public var endIndex: Int {
    return _data.endIndex
  }

  public subscript(i: Int) -> MsgPackVariant {
    return _data[i]
  }

  public var count: Int {
    return _data.count
  }
}

public struct MsgPackVariantMap : CollectionType {
  internal var _data: [(MsgPackVariant, MsgPackVariant)]

  public init() {
    self._data = []
  }

  public init(_ data: [(MsgPackVariant, MsgPackVariant)]) {
    self._data = data
  }

  public init(_ data: [String: MsgPackVariant]) {
    self._data = map(data, { (key, value) in
      (MsgPackVariant.String(key), value) })
  }

  public func generate() -> IndexingGenerator<MsgPackVariantMap> {
    return IndexingGenerator(self)
  }

  public var startIndex: Int {
    return _data.startIndex
  }

  public var endIndex: Int {
    return _data.endIndex
  }

  public subscript(i: Int) -> (MsgPackVariant, MsgPackVariant) {
    return _data[i]
  }

  public var count: Int {
    return _data.count
  }

  internal mutating func _append(#key: MsgPackVariant, value: MsgPackVariant) {
    let entry = (key, value)
    _data.append(entry)
  }
}

/// A DOM-like representation of a MessagePack object.
public enum MsgPackVariant {
  case Int64(Swift.Int64)
  case UInt64(Swift.UInt64)
  case Nil
  case Bool(Swift.Bool)
  case Float32(Swift.Float32)
  case Float64(Swift.Float64)
  case String(Swift.String)
  case Binary([UInt8])
  case Array(MsgPackVariantArray)
  case Map(MsgPackVariantMap)
  case Extended(type: Int8, data: [UInt8])

  internal func _serializeToImpl(inout encoder: MsgPackEncoder) {
    switch self {
    case Int64(let i):
      encoder.append(i)

    case UInt64(let i):
      encoder.append(i)

    case Nil:
      encoder.appendNil()

    case Bool(let b):
      encoder.append(b)

    case Float32(let f):
      encoder.append(f)

    case Float64(let f):
      encoder.append(f)

    case String(let s):
      encoder.append(s)

    case Binary(let dataBytes):
      encoder.append(dataBytes)

    case Array(let a):
      encoder.beginArray(a.count)

      // Reserve space assuming homogenous arrays.
      if a.count != 0 {
        switch a[0] {
        case .Float32:
          encoder.bytes.reserveCapacity(
            encoder.bytes.count + a.count * 5)

        case .Int64, .UInt64, .Float64:
          encoder.bytes.reserveCapacity(
            encoder.bytes.count + a.count * 9)

        default:
          ()
        }
      }
      for element in a {
        element._serializeToImpl(&encoder)
      }
      encoder.endArray()

    case Map(let m):
      encoder.beginMap(m.count)
      for (key, value) in m {
        key._serializeToImpl(&encoder)
        value._serializeToImpl(&encoder)
      }
      encoder.endMap()

    case Extended(let type, let data):
      encoder.appendExtended(type: type, data: data)
    }
  }

  public func serializeTo(inout bytes: [UInt8]) {
    bytes += serialize()
  }

  public func serialize() -> [UInt8] {
    var encoder = MsgPackEncoder()
    _serializeToImpl(&encoder)
    return encoder.bytes
  }

  internal static func _deserializeFrom(
    inout decoder: MsgPackDecoder) -> MsgPackVariant? {
    if let i = decoder.readInt64() {
      return MsgPackVariant.Int64(i)
    }
    if let i = decoder.readUInt64() {
      return MsgPackVariant.UInt64(i)
    }
    if decoder.readNil() {
      return MsgPackVariant.Nil
    }
    if let b = decoder.readBool() {
      return MsgPackVariant.Bool(b)
    }
    if let f = decoder.readFloat32() {
      return MsgPackVariant.Float32(f)
    }
    if let f = decoder.readFloat64() {
      return MsgPackVariant.Float64(f)
    }
    if let s = decoder.readString() {
      return MsgPackVariant.String(s)
    }
    if let dataBytes = decoder.readBinary() {
      return MsgPackVariant.Binary(dataBytes)
    }
    if let count = decoder.readBeginArray() {
      var array: [MsgPackVariant] = []
      array.reserveCapacity(count)
      for i in 0..<count {
        let maybeValue = MsgPackVariant._deserializeFrom(&decoder)
        if let value = maybeValue {
          array.append(value)
        } else {
          return nil
        }
      }
      return .Array(MsgPackVariantArray(array))

    }
    if let count = decoder.readBeginMap() {
      var map: [(MsgPackVariant, MsgPackVariant)] = []
      map.reserveCapacity(count)
      for i in 0..<count {
        let maybeKey = MsgPackVariant._deserializeFrom(&decoder)
        let maybeValue = MsgPackVariant._deserializeFrom(&decoder)
        if let key = maybeKey, value = maybeValue {
          let keyValue = (key, value)
          map.append(key, value)
        } else {
          return nil
        }
      }
      return .Map(MsgPackVariantMap(map))
    }
    if let (type, data) = decoder.readExtended() {
      return MsgPackVariant.Extended(type: type, data: data)
    }
    return nil
  }

  public init?(bytes: [UInt8]) {
    var decoder = MsgPackDecoder(bytes)
    if let result = MsgPackVariant._deserializeFrom(&decoder) {
      self = result
    } else {
      return nil
    }
  }
}

