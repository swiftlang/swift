// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import SwiftPrivateSerialization
import StdlibUnittest

var SerializationTestSuite = TestSuite("Serialization")

struct SimpleSerializableType : SerializableFixedDictionaryType {
  var i: Int

  init(i: Int) {
    self.i = i
  }

  init(forDeserialization: ()) {
    self.i = 0
  }

  mutating func serializeDeserialize<
    Serializer : FixedDictionarySerializerType
  >(inout s: Serializer) {
    s.addKey("i", value: &self.i)
  }
}

SerializationTestSuite.test("Smoketest") {
  var input = SimpleSerializableType(i: 42)

  var bytes = MsgPackSerializer.serialize(input)
  expectType([UInt8].self, &bytes)

  var deserialized =
    MsgPackDeserializer.deserialize(bytes) as SimpleSerializableType

  expectEqual(42, deserialized.i)
}

struct SerializableTypeWithScalars : SerializableFixedDictionaryType {
  var i01: Int = 0
  var i02: UInt = 0
  var i03: Int8 = 0
  var i04: Int16 = 0
  var i05: Int32 = 0
  var i06: Int64 = 0
  var i07: UInt8 = 0
  var i08: UInt16 = 0
  var i09: UInt32 = 0
  var i10: UInt64 = 0

  var f01: Float = 0.0
  var f02: Double = 0.0

  var b01: Bool = false

  var s01: String = ""

  init() {}

  init(forDeserialization: ()) {}

  mutating func serializeDeserialize<
    Serializer : FixedDictionarySerializerType
  >(inout s: Serializer) {
    s.addKey("i01", value: &self.i01)
    s.addKey("i02", value: &self.i02)
    s.addKey("i03", value: &self.i03)
    s.addKey("i04", value: &self.i04)
    s.addKey("i05", value: &self.i05)
    s.addKey("i06", value: &self.i06)
    s.addKey("i07", value: &self.i07)
    s.addKey("i08", value: &self.i08)
    s.addKey("i09", value: &self.i09)
    s.addKey("i10", value: &self.i10)

    s.addKey("f01", value: &self.f01)
    s.addKey("f02", value: &self.f02)

    s.addKey("b01", value: &self.b01)

    s.addKey("s01", value: &self.s01)
  }
}

SerializationTestSuite.test("Scalars") {
  var input = SerializableTypeWithScalars()

  input.i01 = 1
  input.i02 = 2
  input.i03 = 3
  input.i04 = 4
  input.i05 = 5
  input.i06 = 6
  input.i07 = 7
  input.i08 = 8
  input.i09 = 9
  input.i10 = 10

  input.f01 = 0.25
  input.f02 = 0.75

  input.b01 = true

  input.s01 = "hello"

  var bytes = MsgPackSerializer.serialize(input)
  expectType([UInt8].self, &bytes)

  var deserialized =
    MsgPackDeserializer.deserialize(bytes) as SerializableTypeWithScalars

  expectEqual(1, deserialized.i01)
  expectEqual(2, deserialized.i02)
  expectEqual(3, deserialized.i03)
  expectEqual(4, deserialized.i04)
  expectEqual(5, deserialized.i05)
  expectEqual(6, deserialized.i06)
  expectEqual(7, deserialized.i07)
  expectEqual(8, deserialized.i08)
  expectEqual(9, deserialized.i09)
  expectEqual(10, deserialized.i10)

  expectEqual(0.25, deserialized.f01)
  expectEqual(0.75, deserialized.f02)

  expectEqual(true, deserialized.b01)

  expectEqual("hello", deserialized.s01)
}

struct SerializableTypeWithArraysOfScalars :
  SerializableFixedDictionaryType {

  var i01: [Int] = []
  var i02: [UInt] = []
  var i03: [Int8] = []
  var i04: [Int16] = []
  var i05: [Int32] = []
  var i06: [Int64] = []
  var i07: [UInt8] = []
  var i08: [UInt16] = []
  var i09: [UInt32] = []
  var i10: [UInt64] = []

  var f01: [Float] = []
  var f02: [Double] = []

  var b01: [Bool] = []

  var s01: [String] = []

  init() {}

  init(forDeserialization: ()) {}

  mutating func serializeDeserialize<
    Serializer : FixedDictionarySerializerType
  >(inout s: Serializer) {
    s.addKey("i01", value: &self.i01)
    s.addKey("i02", value: &self.i02)
    s.addKey("i03", value: &self.i03)
    s.addKey("i04", value: &self.i04)
    s.addKey("i05", value: &self.i05)
    s.addKey("i06", value: &self.i06)
    s.addKey("i07", value: &self.i07)
    s.addKey("i08", value: &self.i08)
    s.addKey("i09", value: &self.i09)
    s.addKey("i10", value: &self.i10)

    s.addKey("f01", value: &self.f01)
    s.addKey("f02", value: &self.f02)

    s.addKey("b01", value: &self.b01)

    s.addKey("s01", value: &self.s01)
  }
}

SerializationTestSuite.test("ArraysOfScalars") {
  var input = SerializableTypeWithArraysOfScalars()

  input.i01 = [ 1 ]
  input.i02 = [ 2 ]
  input.i03 = [ 3 ]
  input.i04 = [ 4 ]
  input.i05 = [ 5 ]
  input.i06 = [ 6 ]
  input.i07 = [ 7 ]
  input.i08 = [ 8 ]
  input.i09 = [ 9 ]
  input.i10 = [ 10 ]

  input.f01 = [ 0.25 ]
  input.f02 = [ 0.75 ]

  input.b01 = [ true ]

  input.s01 = [ "hello" ]

  var bytes = MsgPackSerializer.serialize(input)
  expectType([UInt8].self, &bytes)

  var deserialized =
    MsgPackDeserializer.deserialize(bytes) as
      SerializableTypeWithArraysOfScalars

  expectEqual([ 1 ], deserialized.i01)
  expectEqual([ 2 ], deserialized.i02)
  expectEqual([ 3 ], deserialized.i03)
  expectEqual([ 4 ], deserialized.i04)
  expectEqual([ 5 ], deserialized.i05)
  expectEqual([ 6 ], deserialized.i06)
  expectEqual([ 7 ], deserialized.i07)
  expectEqual([ 8 ], deserialized.i08)
  expectEqual([ 9 ], deserialized.i09)
  expectEqual([ 10 ], deserialized.i10)

  expectEqual([ 0.25 ], deserialized.f01)
  expectEqual([ 0.75 ], deserialized.f02)

  expectEqual([ true ], deserialized.b01)

  expectEqual([ "hello" ], deserialized.s01)
}

struct SerializableTypeWithDictionariesOfScalars :
  SerializableFixedDictionaryType {

  var i01: [String : Int] = [:]
  var i02: [String : UInt] = [:]
  var i03: [String : Int8] = [:]
  var i04: [String : Int16] = [:]
  var i05: [String : Int32] = [:]
  var i06: [String : Int64] = [:]
  var i07: [String : UInt8] = [:]
  var i08: [String : UInt16] = [:]
  var i09: [String : UInt32] = [:]
  var i10: [String : UInt64] = [:]

  var f01: [String : Float] = [:]
  var f02: [String : Double] = [:]

  var b01: [String : Bool] = [:]

  var s01: [String : String] = [:]

  init() {}

  init(forDeserialization: ()) {}

  mutating func serializeDeserialize<
    Serializer : FixedDictionarySerializerType
  >(inout s: Serializer) {
    s.addKey("i01", value: &self.i01)
    s.addKey("i02", value: &self.i02)
    s.addKey("i03", value: &self.i03)
    s.addKey("i04", value: &self.i04)
    s.addKey("i05", value: &self.i05)
    s.addKey("i06", value: &self.i06)
    s.addKey("i07", value: &self.i07)
    s.addKey("i08", value: &self.i08)
    s.addKey("i09", value: &self.i09)
    s.addKey("i10", value: &self.i10)

    s.addKey("f01", value: &self.f01)
    s.addKey("f02", value: &self.f02)

    s.addKey("b01", value: &self.b01)

    s.addKey("s01", value: &self.s01)
  }
}

SerializationTestSuite.test("DictionariesOfScalars") {
  var input = SerializableTypeWithDictionariesOfScalars()

  input.i01 = [ "a": 1 ]
  input.i02 = [ "a": 2 ]
  input.i03 = [ "a": 3 ]
  input.i04 = [ "a": 4 ]
  input.i05 = [ "a": 5 ]
  input.i06 = [ "a": 6 ]
  input.i07 = [ "a": 7 ]
  input.i08 = [ "a": 8 ]
  input.i09 = [ "a": 9 ]
  input.i10 = [ "a": 10 ]

  input.f01 = [ "a": 0.25 ]
  input.f02 = [ "a": 0.75 ]

  input.b01 = [ "a": true ]

  input.s01 = [ "a": "hello" ]

  var bytes = MsgPackSerializer.serialize(input)
  expectType([UInt8].self, &bytes)

  var deserialized =
    MsgPackDeserializer.deserialize(bytes) as
      SerializableTypeWithDictionariesOfScalars

  expectEqual([ "a": 1 ], deserialized.i01)
  expectEqual([ "a": 2 ], deserialized.i02)
  expectEqual([ "a": 3 ], deserialized.i03)
  expectEqual([ "a": 4 ], deserialized.i04)
  expectEqual([ "a": 5 ], deserialized.i05)
  expectEqual([ "a": 6 ], deserialized.i06)
  expectEqual([ "a": 7 ], deserialized.i07)
  expectEqual([ "a": 8 ], deserialized.i08)
  expectEqual([ "a": 9 ], deserialized.i09)
  expectEqual([ "a": 10 ], deserialized.i10)

  expectEqual([ "a": 0.25 ], deserialized.f01)
  expectEqual([ "a": 0.75 ], deserialized.f02)

  expectEqual([ "a": true ], deserialized.b01)

  expectEqual([ "a": "hello" ], deserialized.s01)
}

struct SerializableTypeWithFixedDictionaries :
  SerializableFixedDictionaryType {

  var fd01: SimpleSerializableType = SimpleSerializableType(i: 0)

  init() {}

  init(forDeserialization: ()) {}

  mutating func serializeDeserialize<
    Serializer : FixedDictionarySerializerType
  >(inout s: Serializer) {
    s.addKey("fd01", value: &self.fd01)
  }
}

SerializationTestSuite.test("FixedDictionaries") {
  var input = SerializableTypeWithFixedDictionaries()

  input.fd01 = SimpleSerializableType(i: 42)

  var bytes = MsgPackSerializer.serialize(input)
  expectType([UInt8].self, &bytes)

  var deserialized =
    MsgPackDeserializer.deserialize(bytes) as
      SerializableTypeWithFixedDictionaries

  expectEqual(42, deserialized.fd01.i)
}

runAllTests()

