// RUN: %target-run-simple-swift

import SwiftPrivateSerialization
import StdlibUnittest

var MsgPackTestSuite = TestSuite("MsgPack")

MsgPackTestSuite.test("Serialize/Int64") {
  expectEqual(
    [ 0xd3,
      0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    MsgPackVariant.Int64(Int64.min).serialize())

  expectEqual(
    [ 0xd3,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff ],
    MsgPackVariant.Int64(-1).serialize())

  expectEqual(
    [ 0xd3,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    MsgPackVariant.Int64(0).serialize())

  expectEqual(
    [ 0xd3,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01 ],
    MsgPackVariant.Int64(1).serialize())

  expectEqual(
    [ 0xd3,
      0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf1 ],
    MsgPackVariant.Int64(0x1234_5678_9abc_def1).serialize())

  expectEqual(
    [ 0xd3,
      0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff ],
    MsgPackVariant.Int64(Int64.max).serialize())
}

MsgPackTestSuite.test("Serialize/UInt64") {
  expectEqual(
    [ 0xcf,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    MsgPackVariant.UInt64(0).serialize())

  expectEqual(
    [ 0xcf,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01 ],
    MsgPackVariant.UInt64(1).serialize())

  expectEqual(
    [ 0xcf,
      0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf1 ],
    MsgPackVariant.UInt64(0x1234_5678_9abc_def1).serialize())

  expectEqual(
    [ 0xcf,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff ],
    MsgPackVariant.UInt64(UInt64.max).serialize())
}

MsgPackTestSuite.test("Serialize/Nil") {
  expectEqual([ 0xc0 ], MsgPackVariant.Nil.serialize())
}

MsgPackTestSuite.test("Serialize/Bool") {
  expectEqual([ 0xc2 ], MsgPackVariant.Bool(false).serialize())
  expectEqual([ 0xc3 ], MsgPackVariant.Bool(true).serialize())
}

MsgPackTestSuite.test("Serialize/Float32") {
  expectEqual(
    [ 0xca,
      0x00, 0x00, 0x00, 0x00 ],
    MsgPackVariant.Float32(0.0).serialize())

  expectEqual(
    [ 0xca,
      0x3f, 0x80, 0x00, 0x00 ],
    MsgPackVariant.Float32(1.0).serialize())
}

MsgPackTestSuite.test("Serialize/Float64") {
  expectEqual(
    [ 0xcb,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    MsgPackVariant.Float64(0.0).serialize())

  expectEqual(
    [ 0xcb,
      0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    MsgPackVariant.Float64(1.0).serialize())
}

MsgPackTestSuite.test("Serialize/String/fixstr") {
  expectEqual(
    [ 0xa0 ],
    MsgPackVariant.String("").serialize())

  expectEqual(
    [ 0xa1,
      0x61 ],
    MsgPackVariant.String("a").serialize())

  expectEqual(
    [ 0xb0,
      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
      0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f ],
    MsgPackVariant.String("0123456789:;<=>?").serialize())

  expectEqual(
    [ 0xbf,
      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
      0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
      0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
      0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e ],
    MsgPackVariant.String("0123456789:;<=>?`abcdefghijklmn").serialize())

  expectEqual(
    [ 0xd9, 0x20,
      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
      0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
      0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
      0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f ],
    MsgPackVariant.String("0123456789:;<=>?`abcdefghijklmno").serialize())
}

func makeString(length: Int) -> String {
  var result = ""
  for i in 0..<(length / 10) {
    result += "0123456789"
  }
  result += String(Array("0123456789")[0 ..< length % 10])
  return result
}

MsgPackTestSuite.test("Serialize/String/str8") {
  if true {
    let str = makeString(0x20)
    expectEqual(
      [ 0xd9, 0x20 ] + Array(str.utf8),
      MsgPackVariant.String(str).serialize())
  }
  if true {
    let str = makeString(0xff)
    expectEqual(
      [ 0xd9, 0xff ] + Array(str.utf8),
      MsgPackVariant.String(str).serialize())
  }
}

MsgPackTestSuite.test("Serialize/String/str16") {
  if true {
    let str = makeString(0x100)
    expectEqual(
      [ 0xda, 0x01, 0x00 ] + Array(str.utf8),
      MsgPackVariant.String(str).serialize())
  }
  if true {
    let str = makeString(0xffff)
    expectEqual(
      [ 0xda, 0xff, 0xff ] + Array(str.utf8),
      MsgPackVariant.String(str).serialize())
  }
}

MsgPackTestSuite.test("Serialize/String/str32") {
  if true {
    let str = makeString(0x1_0000)
    expectEqual(
      [ 0xdb, 0x00, 0x01, 0x00, 0x00 ] + Array(str.utf8),
      MsgPackVariant.String(str).serialize())
  }
}

func makeByteBlob(length: Int) -> [UInt8] {
  var result: [UInt8] = []
  let pattern: [UInt8] = [
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
    0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f ]
  for i in 0..<(length / 16) {
    result += pattern
  }
  result += pattern[0..<(length % 16)]
  return result
}

MsgPackTestSuite.test("Serialize/Binary/bin8") {
  if true {
    let bytes = makeByteBlob(0)
    expectEqual(
      [ 0xc4, 0x00 ] + bytes,
      MsgPackVariant.Binary(bytes).serialize())
  }
  if true {
    let bytes = makeByteBlob(1)
    expectEqual(
      [ 0xc4, 0x01 ] + bytes,
      MsgPackVariant.Binary(bytes).serialize())
  }
  if true {
    let bytes = makeByteBlob(0x10)
    expectEqual(
      [ 0xc4, 0x10 ] + bytes,
      MsgPackVariant.Binary(bytes).serialize())
  }
  if true {
    let bytes = makeByteBlob(0xff)
    expectEqual(
      [ 0xc4, 0xff ] + bytes,
      MsgPackVariant.Binary(bytes).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Binary/bin16") {
  if true {
    let bytes = makeByteBlob(0x100)
    expectEqual(
      [ 0xc5, 0x01, 0x00 ] + bytes,
      MsgPackVariant.Binary(bytes).serialize())
  }
  if true {
    let bytes = makeByteBlob(0xffff)
    expectEqual(
      [ 0xc5, 0xff, 0xff ] + bytes,
      MsgPackVariant.Binary(bytes).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Binary/bin32") {
  if true {
    let bytes = makeByteBlob(0x1_0000)
    expectEqual(
      [ 0xc6, 0x00, 0x01, 0x00, 0x00 ] + bytes,
      MsgPackVariant.Binary(bytes).serialize())
  }
}

func makeMsgPackArray(length: Int) -> MsgPackVariantArray {
  return MsgPackVariantArray(
    [MsgPackVariant](count: length, repeatedValue: MsgPackVariant.Bool(true)))
}

func makeMsgPackArraySerialized(length: Int) -> [UInt8] {
  return [UInt8](count: length, repeatedValue: 0xc3)
}

MsgPackTestSuite.test("Serialize/Array/fixarray") {
  if true {
    let array = makeMsgPackArray(0x0)
    expectEqual(
      [ 0x90 ],
      MsgPackVariant.Array(array).serialize())
  }
  if true {
    let array = makeMsgPackArray(0x1)
    expectEqual(
      [ 0x91 ] + makeMsgPackArraySerialized(0x1),
      MsgPackVariant.Array(array).serialize())
  }
  if true {
    let array = makeMsgPackArray(15)
    expectEqual(
      [ 0x9f ] + makeMsgPackArraySerialized(0xf),
      MsgPackVariant.Array(array).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Array/array16") {
  if true {
    let array = makeMsgPackArray(0x10)
    expectEqual(
      [ 0xdc, 0x00, 0x10 ] + makeMsgPackArraySerialized(0x10),
      MsgPackVariant.Array(array).serialize())
  }
  if true {
    let array = makeMsgPackArray(0xffff)
    expectEqual(
      [ 0xdc, 0xff, 0xff ] + makeMsgPackArraySerialized(0xffff),
      MsgPackVariant.Array(array).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Array/array32") {
  if true {
    let array = makeMsgPackArray(0x1_0000)
    expectEqual(
      [ 0xdd, 0x00, 0x01, 0x00, 0x00 ] + makeMsgPackArraySerialized(0x1_0000),
      MsgPackVariant.Array(array).serialize())
  }
}

func makeMsgPackMap(length: Int) -> MsgPackVariantMap {
  var dict: [String: MsgPackVariant] = [:]
  for i in 0..<length {
    dict["\(i)"] = MsgPackVariant.Bool(true)
  }
  return MsgPackVariantMap(dict)
}

func makeMsgPackMapSerialized(map: MsgPackVariantMap) -> [UInt8] {
  var result: [UInt8] = []
  for (key, value) in map {
    key.serializeTo(&result)
    value.serializeTo(&result)
  }
  return result
}

MsgPackTestSuite.test("Serialize/Map/fixmap") {
  if true {
    let map = makeMsgPackMap(0)
    expectEqual(
      [ 0x80 ],
      MsgPackVariant.Map(map).serialize())
  }
  if true {
    let map = makeMsgPackMap(1)
    expectEqual(
      [ 0x81 ] + makeMsgPackMapSerialized(map),
      MsgPackVariant.Map(map).serialize())
  }
  if true {
    let map = makeMsgPackMap(15)
    expectEqual(
      [ 0x8f ] + makeMsgPackMapSerialized(map),
      MsgPackVariant.Map(map).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Map/map16") {
  if true {
    let map = makeMsgPackMap(16)
    expectEqual(
      [ 0xde, 0x00, 0x10 ] + makeMsgPackMapSerialized(map),
      MsgPackVariant.Map(map).serialize())
  }
  if true {
    let map = makeMsgPackMap(0xffff)
    expectEqual(
      [ 0xde, 0xff, 0xff ] + makeMsgPackMapSerialized(map),
      MsgPackVariant.Map(map).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Map/map32") {
  if true {
    let map = makeMsgPackMap(0x1_0000)
    expectEqual(
      [ 0xdf, 0x00, 0x01, 0x00, 0x00 ] + makeMsgPackMapSerialized(map),
      MsgPackVariant.Map(map).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Extension/fixext1") {
  let bytes = makeByteBlob(1)
  expectEqual(
    [ 0xd4, 0x42 ] + bytes,
    MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
}

MsgPackTestSuite.test("Serialize/Extension/fixext2") {
  let bytes = makeByteBlob(2)
  expectEqual(
    [ 0xd5, 0x42 ] + bytes,
    MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
}

MsgPackTestSuite.test("Serialize/Extension/fixext4") {
  let bytes = makeByteBlob(4)
  expectEqual(
    [ 0xd6, 0x42 ] + bytes,
    MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
}

MsgPackTestSuite.test("Serialize/Extension/fixext8") {
  let bytes = makeByteBlob(8)
  expectEqual(
    [ 0xd7, 0x42 ] + bytes,
    MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
}

MsgPackTestSuite.test("Serialize/Extension/fixext16") {
  let bytes = makeByteBlob(16)
  expectEqual(
    [ 0xd8, 0x42 ] + bytes,
    MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
}

MsgPackTestSuite.test("Serialize/Extension/ext8") {
  if true {
    let bytes = makeByteBlob(0)
    expectEqual(
      [ 0xc7, 0x00, 0x42 ] + bytes,
      MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
  }
  if true {
    let bytes = makeByteBlob(3)
    expectEqual(
      [ 0xc7, 0x03, 0x42 ] + bytes,
      MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
  }
  if true {
    let bytes = makeByteBlob(5)
    expectEqual(
      [ 0xc7, 0x05, 0x42 ] + bytes,
      MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
  }
  if true {
    let bytes = makeByteBlob(0xff)
    expectEqual(
      [ 0xc7, 0xff, 0x42 ] + bytes,
      MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Extension/ext16") {
  if true {
    let bytes = makeByteBlob(0x100)
    expectEqual(
      [ 0xc8, 0x01, 0x00, 0x42 ] + bytes,
      MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
  }
  if true {
    let bytes = makeByteBlob(0xffff)
    expectEqual(
      [ 0xc8, 0xff, 0xff, 0x42 ] + bytes,
      MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
  }
}

MsgPackTestSuite.test("Serialize/Extension/ext32") {
  if true {
    let bytes = makeByteBlob(0x1_0000)
    expectEqual(
      [ 0xc9, 0x00, 0x01, 0x00, 0x00, 0x42 ] + bytes,
      MsgPackVariant.Extended(type: 0x42, data: bytes).serialize())
  }
}

MsgPackTestSuite.test("Deserialize/Int64") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder(
      [ 0xd3,
        0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectOptionalEqual(Int64.min, d.readInt64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xd3,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff ])
    expectOptionalEqual(-1, d.readInt64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xd3,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectOptionalEqual(0, d.readInt64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xd3,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01 ])
    expectOptionalEqual(1, d.readInt64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xd3,
        0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf1 ])
    expectOptionalEqual(0x1234_5678_9abc_def1, d.readInt64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xd3,
        0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff ])
    expectOptionalEqual(Int64.max, d.readInt64())
    expectEqual(9, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte == 0xd3 {
      continue
    }
    var d = MsgPackDecoder(
      [ UInt8(startByte),
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectEmpty(d.readInt64())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readInt64())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xd3 ])
    expectEmpty(d.readInt64())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xd3,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectEmpty(d.readInt64())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/UInt64") {
  // Valid inputs.
  if true {
    var d = MsgPackDecoder(
      [ 0xcf,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectOptionalEqual(0, d.readUInt64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xcf,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01 ])
    expectOptionalEqual(1, d.readUInt64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xcf,
        0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf1 ])
    expectOptionalEqual(0x1234_5678_9abc_def1, d.readUInt64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xcf,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff ])
    expectOptionalEqual(UInt64.max, d.readUInt64())
    expectEqual(9, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte == 0xcf {
      continue
    }
    var d = MsgPackDecoder(
      [ UInt8(startByte),
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectEmpty(d.readUInt64())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readUInt64())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xcf ])
    expectEmpty(d.readUInt64())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xcf,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectEmpty(d.readUInt64())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Nil") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0xc0 ])
    expectTrue(d.readNil())
    expectEqual(1, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte == 0xc0 {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte) ])
    expectFalse(d.readNil())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectFalse(d.readNil())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Bool") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0xc2 ])
    expectOptionalEqual(false, d.readBool())
    expectEqual(1, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xc3 ])
    expectOptionalEqual(true, d.readBool())
    expectEqual(1, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte == 0xc2 || startByte == 0xc3 {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte) ])
    expectEmpty(d.readBool())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readBool())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Float32") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder(
      [ 0xca,
        0x00, 0x00, 0x00, 0x00 ])
    expectOptionalEqual(0.0, d.readFloat32())
    expectEqual(5, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xca,
        0x3f, 0x80, 0x00, 0x00 ])
    expectOptionalEqual(1.0, d.readFloat32())
    expectEqual(5, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte == 0xca {
      continue
    }
    var d = MsgPackDecoder(
      [ UInt8(startByte),
        0x00, 0x00, 0x00, 0x00 ])
    expectEmpty(d.readFloat32())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readFloat32())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xca ])
    expectEmpty(d.readFloat32())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xca,
        0x00, 0x00, 0x00 ])
    expectEmpty(d.readFloat32())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Float64") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder(
      [ 0xcb,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectOptionalEqual(0, d.readFloat64())
    expectEqual(9, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xcb,
        0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectOptionalEqual(1.0, d.readFloat64())
    expectEqual(9, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte == 0xcb {
      continue
    }
    var d = MsgPackDecoder(
      [ UInt8(startByte),
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectEmpty(d.readFloat64())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readFloat64())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xcb ])
    expectEmpty(d.readFloat64())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xcb,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
    expectEmpty(d.readFloat64())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/String/fixstr") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0xa0 ])
    expectOptionalEqual("", d.readString())
    expectEqual(1, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xa1,
        0x61 ])
    expectOptionalEqual("a", d.readString())
    expectEqual(2, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xb0,
        0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f ])
    expectOptionalEqual("0123456789:;<=>?", d.readString())
    expectEqual(17, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder(
      [ 0xbf,
        0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
        0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
        0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e ])
    expectOptionalEqual("0123456789:;<=>?`abcdefghijklmn", d.readString())
    expectEqual(32, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte >= 0xa0 && startByte <= 0xbf {
      continue
    }
    var d = MsgPackDecoder(
      [ UInt8(startByte),
        0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
        0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
        0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e ])
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }
  for length in 0x01...0x1f {
    if true {
      var d = MsgPackDecoder([ 0xa0 | UInt8(length) ])
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
    if true {
      var d = MsgPackDecoder(
        [ 0xa0 | UInt8(length) ] +
        [UInt8](count: length - 1, repeatedValue: 0x30))
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
  }
}

MsgPackTestSuite.test("Deserialize/String/str8") {
  //
  // Valid inputs.
  //
  if true {
    let str = makeString(0x20)
    var d = MsgPackDecoder([ 0xd9, 0x20 ] + Array(str.utf8))
    expectOptionalEqual(str, d.readString())
    expectEqual(0x22, d.consumedCount)
  }
  if true {
    let str = makeString(0xff)
    var d = MsgPackDecoder([ 0xd9, 0xff ] + Array(str.utf8))
    expectOptionalEqual(str, d.readString())
    expectEqual(0x101, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  let str_20 = makeString(0x20)
  let str_20_utf8 = Array(str_20.utf8)

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if (startByte >= 0xa0 && startByte <= 0xbf) ||
      startByte == 0xd9 || startByte == 0xda || startByte == 0xdb {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte), 0x20 ] + str_20_utf8)
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xd9 ])
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }
  for length in 0x01...0xff {
    if true {
      var d = MsgPackDecoder([ 0xd9, UInt8(length) ])
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
    if true {
      var d = MsgPackDecoder(
        [ 0xd9, UInt8(length) ] +
        [UInt8](count: length - 1, repeatedValue: 0x30))
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
  }

  // Overlong encoding.
  for length in 0x00...0x1f {
    var d = MsgPackDecoder([ 0xd9, UInt8(length) ] + str_20_utf8)
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/String/str16") {
  //
  // Valid inputs.
  //
  for length in [ 0x100, 0x1000, 0xffff ] {
    let str = makeString(length)
    var d = MsgPackDecoder([
      0xda,
      UInt8(truncatingBitPattern: length >> 8),
      UInt8(truncatingBitPattern: length)
    ] + Array(str.utf8))
    expectOptionalEqual(str, d.readString())
    expectEqual(length + 3, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  let str_100 = makeString(0x100)
  let str_100_utf8 = Array(str_100.utf8)

  // Wrong start byte.
  if true {
    var data = [ 0xff, 0x01, 0x00 ] + str_100_utf8
    for startByte in 0x00...0xff {
      if (startByte >= 0xa0 && startByte <= 0xbf) ||
        startByte == 0xd9 || startByte == 0xda || startByte == 0xdb {
        continue
      }
      data[0] = UInt8(startByte)
      var d = MsgPackDecoder(data)
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xda ])
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }
  for length in [ 0x100, 0x1000, 0xffff ] {
    if true {
      var d = MsgPackDecoder([ 0xda, UInt8(length >> 8) ])
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
    if true {
      var d = MsgPackDecoder(
        [ 0xda, UInt8(length >> 8), UInt8(truncatingBitPattern: length) ] +
        [UInt8](count: length - 1, repeatedValue: 0x30))
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
  }

  // Overlong encoding.
  for length in [ 0, 0x1f, 0x20, 0xff ] {
    var d = MsgPackDecoder(
      [ 0xda, UInt8(length >> 8), UInt8(truncatingBitPattern: length) ] +
      str_100_utf8)
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/String/str32") {
  //
  // Valid inputs.
  //
  if true {
    let str = makeString(0x1_0000)
    var d = MsgPackDecoder([ 0xdb, 0x00, 0x01, 0x00, 0x00 ] + Array(str.utf8))
    expectOptionalEqual(str, d.readString())
    expectEqual(0x1_0005, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  let str_10000 = makeString(0x1_0000)
  let str_10000_utf8 = Array(str_10000.utf8)

  // Wrong start byte.
  if true {
    var data = [ 0xff, 0x00, 0x01, 0x00, 0x00 ] + str_10000_utf8
    for startByte in 0x00...0xff {
      if (startByte >= 0xa0 && startByte <= 0xbf) ||
        startByte == 0xd9 || startByte == 0xda || startByte == 0xdb {
        continue
      }
      data[0] = UInt8(startByte)
      var d = MsgPackDecoder(data)
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xdb ])
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }
  for length in [ 0x1_0000 ] {
    if true {
      var d = MsgPackDecoder(
        [ 0xdb,
          UInt8(truncatingBitPattern: length >> 24),
          UInt8(truncatingBitPattern: length >> 16),
          UInt8(truncatingBitPattern: length >> 8),
          UInt8(truncatingBitPattern: length) ])
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
    if true {
      var d = MsgPackDecoder(
        [ 0xdb,
          UInt8(truncatingBitPattern: length >> 24),
          UInt8(truncatingBitPattern: length >> 16),
          UInt8(truncatingBitPattern: length >> 8),
          UInt8(truncatingBitPattern: length) ] +
        [UInt8](count: length - 1, repeatedValue: 0x30))
      expectEmpty(d.readString())
      expectEqual(0, d.consumedCount)
    }
  }

  // Overlong encoding.
  for length in [ 0, 0x1f, 0x20, 0xff, 0x100, 0xffff ] {
    var d = MsgPackDecoder(
      [ 0xdb,
        UInt8(truncatingBitPattern: length >> 24),
        UInt8(truncatingBitPattern: length >> 16),
        UInt8(truncatingBitPattern: length >> 8),
        UInt8(truncatingBitPattern: length) ] +
      str_10000_utf8)
    expectEmpty(d.readString())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Binary/bin8") {

  #if arch(arm64)
  // FIXME: rdar://20448082 miscompile in MsgPackDecoder.readBinary()
  #else

  //
  // Valid inputs.
  //
  if true {
    let bytes = makeByteBlob(0)
    var d = MsgPackDecoder([ 0xc4, 0x00 ] + bytes)
    expectOptionalEqual(bytes, d.readBinary())
    expectEqual(0x2, d.consumedCount)
  }
  if true {
    let bytes = makeByteBlob(1)
    var d = MsgPackDecoder([ 0xc4, 0x01 ] + bytes)
    expectOptionalEqual(bytes, d.readBinary())
    expectEqual(0x3, d.consumedCount)
  }
  if true {
    let bytes = makeByteBlob(0x10)
    var d = MsgPackDecoder([ 0xc4, 0x10 ] + bytes)
    expectOptionalEqual(bytes, d.readBinary())
    expectEqual(0x12, d.consumedCount)
  }
  if true {
    let bytes = makeByteBlob(0xff)
    var d = MsgPackDecoder([ 0xc4, 0xff ] + bytes)
    expectOptionalEqual(bytes, d.readBinary())
    expectEqual(0x101, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  if true {
    var data: [UInt8] = [ 0xff, 0x01, 0x30 ]
    for startByte in 0x00...0xff {
      if startByte == 0xc4 {
        continue
      }
      data[0] = UInt8(startByte)
      var d = MsgPackDecoder(data)
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readBinary())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xc4 ])
    expectEmpty(d.readBinary())
    expectEqual(0, d.consumedCount)
  }
  for length in 0x01...0xff {
    if true {
      var d = MsgPackDecoder([ 0xc4, UInt8(length) ])
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
    if true {
      var d = MsgPackDecoder(
        [ 0xc4, UInt8(length) ] +
        [UInt8](count: length - 1, repeatedValue: 0x30))
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
  }

  // not arm64
  #endif
}

MsgPackTestSuite.test("Deserialize/Binary/bin16") {
  //
  // Valid inputs.
  //
  if true {
    let bytes = makeByteBlob(0x100)
    var d = MsgPackDecoder([ 0xc5, 0x01, 0x00 ] + bytes)
    expectOptionalEqual(bytes, d.readBinary())
    expectEqual(0x103, d.consumedCount)
  }
  if true {
    let bytes = makeByteBlob(0xffff)
    var d = MsgPackDecoder([ 0xc5, 0xff, 0xff ] + bytes)
    expectOptionalEqual(bytes, d.readBinary())
    expectEqual(0x10002, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  let bytes_100 = makeByteBlob(0x100)

  // Wrong start byte.
  if true {
    var data = [ 0xff, 0x01, 0x00 ] + bytes_100
    for startByte in 0x00...0xff {
      if startByte == 0xc4 || startByte == 0xc5 {
        continue
      }
      data[0] = UInt8(startByte)
      var d = MsgPackDecoder(data)
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xc5 ])
    expectEmpty(d.readBinary())
    expectEqual(0, d.consumedCount)
  }
  for length in [ 0x100, 0x1000, 0xffff ] {
    if true {
      var d = MsgPackDecoder([ 0xc5, UInt8(length >> 8) ])
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
    if true {
      var d = MsgPackDecoder(
        [ 0xc5, UInt8(length >> 8), UInt8(truncatingBitPattern: length) ] +
        [UInt8](count: length - 1, repeatedValue: 0x30))
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
  }

  // Overlong encoding.
  for length in [ 0, 0x7f, 0xff ] {
    var d = MsgPackDecoder(
      [ 0xc5, UInt8(length >> 8), UInt8(truncatingBitPattern: length) ] +
      [UInt8](count: length, repeatedValue: 0x30))
    expectEmpty(d.readBinary())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Binary/bin32") {
  //
  // Valid inputs.
  //
  if true {
    let bytes = makeByteBlob(0x1_0000)
    var d = MsgPackDecoder([ 0xc6, 0x00, 0x01, 0x00, 0x00 ] + bytes)
    expectOptionalEqual(bytes, d.readBinary())
    expectEqual(0x1_0005, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  let bytes_10000 = makeByteBlob(0x1_0000)

  // Wrong start byte.
  if true {
    var data = [ 0xff, 0x00, 0x01, 0x00, 0x00 ] + bytes_10000
    for startByte in 0x00...0xff {
      if startByte == 0xc4 || startByte == 0xc5 || startByte == 0xc6 {
        continue
      }
      data[0] = UInt8(startByte)
      var d = MsgPackDecoder(data)
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xc6 ])
    expectEmpty(d.readBinary())
    expectEqual(0, d.consumedCount)
  }
  for length in [ 0x1_0000 ] {
    if true {
      var d = MsgPackDecoder(
        [ 0xc6,
          UInt8(truncatingBitPattern: length >> 24),
          UInt8(truncatingBitPattern: length >> 16),
          UInt8(truncatingBitPattern: length >> 8),
          UInt8(truncatingBitPattern: length) ])
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
    if true {
      var d = MsgPackDecoder(
        [ 0xc6,
          UInt8(truncatingBitPattern: length >> 24),
          UInt8(truncatingBitPattern: length >> 16),
          UInt8(truncatingBitPattern: length >> 8),
          UInt8(truncatingBitPattern: length) ] +
        [UInt8](count: length - 1, repeatedValue: 0x30))
      expectEmpty(d.readBinary())
      expectEqual(0, d.consumedCount)
    }
  }

  // Overlong encoding.
  for length in [ 0, 0x7f, 0xff, 0x100, 0xf000, 0xffff ] {
    var d = MsgPackDecoder(
      [ 0xc6,
        UInt8(truncatingBitPattern: length >> 24),
        UInt8(truncatingBitPattern: length >> 16),
        UInt8(truncatingBitPattern: length >> 8),
        UInt8(truncatingBitPattern: length) ] +
      [UInt8](count: length, repeatedValue: 0x30))
    expectEmpty(d.readBinary())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Array/fixarray") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0x90 ])
    expectOptionalEqual(0x0, d.readBeginArray())
    expectEqual(1, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0x91 ])
    expectOptionalEqual(0x1, d.readBeginArray())
    expectEqual(1, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0x9f ])
    expectOptionalEqual(0xf, d.readBeginArray())
    expectEqual(1, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte >= 0x90 && startByte <= 0x9f {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte) ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Array/array16") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0xdc, 0x00, 0x10 ])
    expectOptionalEqual(0x10, d.readBeginArray())
    expectEqual(3, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdc, 0x01, 0x00 ])
    expectOptionalEqual(0x100, d.readBeginArray())
    expectEqual(3, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdc, 0xff, 0xff ])
    expectOptionalEqual(0xffff, d.readBeginArray())
    expectEqual(3, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  // Tested in the fixarray testcase.

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xdc ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdc, 0x00 ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdc, 0x01 ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }

  // Overlong encoding.
  for length in [ 0, 0x9, 0xf ] {
    var d = MsgPackDecoder([ 0xdc, 0x00, UInt8(length) ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Array/array32") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0xdd, 0x00, 0x01, 0x00, 0x00 ])
    expectOptionalEqual(0x1_0000, d.readBeginArray())
    expectEqual(5, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdd, 0xff, 0xff, 0xff, 0xff ])
#if arch(i386) || arch(arm)
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
#elseif arch(x86_64) || arch(arm64)
    expectOptionalEqual(0xffff_ffff, d.readBeginArray())
    expectEqual(5, d.consumedCount)
#else
  fatalError("unimplemented")
#endif
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  // Tested in the fixarray testcase.

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xdd ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdd, 0x00 ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdd, 0x00, 0x01, 0x00 ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }

  // Overlong encoding.
  for length in [ 0, 0x9, 0xf, 0x10, 0xff, 0x100, 0xffff ] {
    var d = MsgPackDecoder(
      [ 0xdd,
        UInt8(truncatingBitPattern: length >> 24),
        UInt8(truncatingBitPattern: length >> 16),
        UInt8(truncatingBitPattern: length >> 8),
        UInt8(truncatingBitPattern: length) ])
    expectEmpty(d.readBeginArray())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Map/fixmap") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0x80 ])
    expectOptionalEqual(0x0, d.readBeginMap())
    expectEqual(1, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0x81 ])
    expectOptionalEqual(0x1, d.readBeginMap())
    expectEqual(1, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0x8f ])
    expectOptionalEqual(0xf, d.readBeginMap())
    expectEqual(1, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte >= 0x80 && startByte <= 0x8f {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte) ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    var d = MsgPackDecoder([])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Map/map16") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0xde, 0x00, 0x10 ])
    expectOptionalEqual(0x10, d.readBeginMap())
    expectEqual(3, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xde, 0x01, 0x00 ])
    expectOptionalEqual(0x100, d.readBeginMap())
    expectEqual(3, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xde, 0xff, 0xff ])
    expectOptionalEqual(0xffff, d.readBeginMap())
    expectEqual(3, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  // Tested in the fixmap testcase.

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xde ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xde, 0x00 ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xde, 0x01 ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }

  // Overlong encoding.
  for length in [ 0, 0x9, 0xf ] {
    var d = MsgPackDecoder([ 0xde, 0x00, UInt8(length) ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Map/map32") {
  //
  // Valid inputs.
  //
  if true {
    var d = MsgPackDecoder([ 0xdf, 0x00, 0x01, 0x00, 0x00 ])
    expectOptionalEqual(0x1_0000, d.readBeginMap())
    expectEqual(5, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdf, 0xff, 0xff, 0xff, 0xff ])
#if arch(i386) || arch(arm)
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
#elseif arch(x86_64) || arch(arm64)
    expectOptionalEqual(0xffff_ffff, d.readBeginMap())
    expectEqual(5, d.consumedCount)
#else
  fatalError("unimplemented")
#endif
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  // Tested in the fixmap testcase.

  // The input is too short.
  if true {
    var d = MsgPackDecoder([ 0xdf ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdf, 0x00 ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }
  if true {
    var d = MsgPackDecoder([ 0xdf, 0x00, 0x01, 0x00 ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }

  // Overlong encoding.
  for length in [ 0, 0x9, 0xf, 0x10, 0xff, 0x100, 0xffff ] {
    var d = MsgPackDecoder(
      [ 0xdf,
        UInt8(truncatingBitPattern: length >> 24),
        UInt8(truncatingBitPattern: length >> 16),
        UInt8(truncatingBitPattern: length >> 8),
        UInt8(truncatingBitPattern: length) ])
    expectEmpty(d.readBeginMap())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Extension/fixext1") {
  //
  // Valid inputs.
  //
  let payload = makeByteBlob(1)
  if true {
    var d = MsgPackDecoder([ 0xd4, 0x42 ] + payload)
    let extended = d.readExtended()
    expectNotEmpty(extended)
    expectEqual(0x42, extended!.type)
    expectEqual(payload, extended!.data)
    expectEqual(3, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte == 0xd4 {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte), 0x42 ] + payload)
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    let data: [UInt8] = [ 0xd4, 0x42 ]
    for length in 0..<data.count() {
      var d = MsgPackDecoder(Array(data[0..<length]))
      expectEmpty(d.readExtended())
      expectEqual(0, d.consumedCount)
    }
  }
}

MsgPackTestSuite.test("Deserialize/Extension/fixext2") {
  //
  // Valid inputs.
  //
  let payload = makeByteBlob(2)
  if true {
    var d = MsgPackDecoder([ 0xd5, 0x42 ] + payload)
    let extended = d.readExtended()
    expectNotEmpty(extended)
    expectEqual(0x42, extended!.type)
    expectEqual(payload, extended!.data)
    expectEqual(4, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte >= 0xd4 && startByte <= 0xd5 {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte), 0x42 ] + payload)
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    let data: [UInt8] = [ 0xd5, 0x42, 0x00 ]
    for length in 0..<data.count() {
      var d = MsgPackDecoder(Array(data[0..<length]))
      expectEmpty(d.readExtended())
      expectEqual(0, d.consumedCount)
    }
  }
}

MsgPackTestSuite.test("Deserialize/Extension/fixext4") {
  //
  // Valid inputs.
  //
  let payload = makeByteBlob(4)
  if true {
    var d = MsgPackDecoder([ 0xd6, 0x42 ] + payload)
    let extended = d.readExtended()
    expectNotEmpty(extended)
    expectEqual(0x42, extended!.type)
    expectEqual(payload, extended!.data)
    expectEqual(6, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte >= 0xd4 && startByte <= 0xd6 {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte), 0x42 ] + payload)
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    let data: [UInt8] = [ 0xd6, 0x42, 0x00, 0x00, 0x00 ]
    for length in 0..<data.count() {
      var d = MsgPackDecoder(Array(data[0..<length]))
      expectEmpty(d.readExtended())
      expectEqual(0, d.consumedCount)
    }
  }
}

MsgPackTestSuite.test("Deserialize/Extension/fixext8") {
  //
  // Valid inputs.
  //
  let payload = makeByteBlob(8)
  if true {
    var d = MsgPackDecoder([ 0xd7, 0x42 ] + payload)
    let extended = d.readExtended()
    expectNotEmpty(extended)
    expectEqual(0x42, extended!.type)
    expectEqual(payload, extended!.data)
    expectEqual(10, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte >= 0xd4 && startByte <= 0xd7 {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte), 0x42 ] + payload)
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    let data: [UInt8] = [
      0xd7, 0x42,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ]
    for length in 0..<data.count() {
      var d = MsgPackDecoder(Array(data[0..<length]))
      expectEmpty(d.readExtended())
      expectEqual(0, d.consumedCount)
    }
  }
}

MsgPackTestSuite.test("Deserialize/Extension/fixext16") {
  //
  // Valid inputs.
  //
  let payload = makeByteBlob(16)
  if true {
    var d = MsgPackDecoder([ 0xd8, 0x42 ] + payload)
    let extended = d.readExtended()
    expectNotEmpty(extended)
    expectEqual(0x42, extended!.type)
    expectEqual(payload, extended!.data)
    expectEqual(18, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if startByte >= 0xd4 && startByte <= 0xd8 {
      continue
    }
    var d = MsgPackDecoder([ UInt8(startByte), 0x42 ] + payload)
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  if true {
    let data: [UInt8] = [
      0xd8, 0x42,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ]
    for length in 0..<data.count() {
      var d = MsgPackDecoder(Array(data[0..<length]))
      expectEmpty(d.readExtended())
      expectEqual(0, d.consumedCount)
    }
  }
}

MsgPackTestSuite.test("Deserialize/Extension/ext8") {
  //
  // Valid inputs.
  //
  for length in [ 0, 3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 17, 0x7f, 0xff ] {
    let payload = makeByteBlob(length)
    var d = MsgPackDecoder([ 0xc7, UInt8(length), 0x42 ] + payload)
    let extended = d.readExtended()
    expectNotEmpty(extended)
    expectEqual(0x42, extended!.type)
    expectEqual(payload, extended!.data)
    expectEqual(length + 3, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  for startByte in 0x00...0xff {
    if (startByte >= 0xd4 && startByte <= 0xd6) || startByte == 0xc7 {
      continue
    }
    var d = MsgPackDecoder(
      [ UInt8(startByte), 0x03, 0x42 ] + makeByteBlob(3))
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // The input is too short.
  for length in 1...0xff {
    var d = MsgPackDecoder(
      [ 0xc7, UInt8(length), 0x42 ] + makeByteBlob(length - 1))
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // Overlong encoding.
  for length in [ 1, 2, 4, 8, 16 ] {
    var d = MsgPackDecoder(
      [ 0xc7, UInt8(length), 0x42 ] + makeByteBlob(length))
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Extension/ext16") {
  //
  // Valid inputs.
  //
  for length in [ 0x100, 0x1000, 0xffff ] {
    let payload = makeByteBlob(length)
    var d = MsgPackDecoder([
      0xc8,
      UInt8(truncatingBitPattern: length >> 8),
      UInt8(truncatingBitPattern: length),
      0x42
    ] + payload)
    let extended = d.readExtended()
    expectNotEmpty(extended)
    expectEqual(0x42, extended!.type)
    expectEqual(payload, extended!.data)
    expectEqual(length + 4, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  if true {
    var data = [ 0xff, 0x01, 0x00, 0x42 ] + makeByteBlob(0x100)
    for startByte in 0x00...0xff {
      if (startByte >= 0xd4 && startByte <= 0xd8) ||
        (startByte >= 0xc7 && startByte <= 0xc8) {
        continue
      }
      data[0] = UInt8(startByte)
      var d = MsgPackDecoder(data)
      expectEmpty(d.readExtended())
      expectEqual(0, d.consumedCount)
    }
  }

  // The input is too short.
  for length in [ 0x100, 0x1000, 0xffff ] {
    var d = MsgPackDecoder([
      0xc8,
      UInt8(truncatingBitPattern: length >> 8),
      UInt8(truncatingBitPattern: length),
      0x42
    ] + makeByteBlob(length - 1))
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // Overlong encoding.
  for length in [ 1, 2, 4, 8, 16, 3, 17, 0xff ] {
    var d = MsgPackDecoder([
      0xc8,
      UInt8(truncatingBitPattern: length >> 8),
      UInt8(truncatingBitPattern: length),
      0x42
    ] + makeByteBlob(length))
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }
}

MsgPackTestSuite.test("Deserialize/Extension/ext32") {
  //
  // Valid inputs.
  //
  for length in [ 0x1_0000 ] {
    let payload = makeByteBlob(length)
    var d = MsgPackDecoder([
      0xc9,
      UInt8(truncatingBitPattern: length >> 24),
      UInt8(truncatingBitPattern: length >> 16),
      UInt8(truncatingBitPattern: length >> 8),
      UInt8(truncatingBitPattern: length),
      0x42
    ] + payload)
    let extended = d.readExtended()
    expectNotEmpty(extended)
    expectEqual(0x42, extended!.type)
    expectEqual(payload, extended!.data)
    expectEqual(length + 6, d.consumedCount)
  }

  //
  // Invalid inputs.
  //

  // Wrong start byte.
  if true {
    var data = [ 0xff, 0x00, 0x01, 0x00, 0x00, 0x42 ] + makeByteBlob(0x1_0000)
    for startByte in 0x00...0xff {
      if (startByte >= 0xd4 && startByte <= 0xd8) ||
        (startByte >= 0xc7 && startByte <= 0xc9) {
        continue
      }
      data[0] = UInt8(startByte)
      var d = MsgPackDecoder(data)
      expectEmpty(d.readExtended())
      expectEqual(0, d.consumedCount)
    }
  }

  // The input is too short.
  for length in [ 0x1_0000 ] {
    var d = MsgPackDecoder([
      0xc9,
      UInt8(truncatingBitPattern: length >> 24),
      UInt8(truncatingBitPattern: length >> 16),
      UInt8(truncatingBitPattern: length >> 8),
      UInt8(truncatingBitPattern: length),
      0x42
    ] + makeByteBlob(length - 1))
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }

  // Overlong encoding.
  for length in [ 1, 2, 4, 8, 16, 3, 17, 0xff, 0x100, 0xffff ] {
    var d = MsgPackDecoder([
      0xc9,
      UInt8(truncatingBitPattern: length >> 24),
      UInt8(truncatingBitPattern: length >> 16),
      UInt8(truncatingBitPattern: length >> 8),
      UInt8(truncatingBitPattern: length),
      0x42
    ] + makeByteBlob(length))
    expectEmpty(d.readExtended())
    expectEqual(0, d.consumedCount)
  }
}

runAllTests()

