// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Swift
import Foundation

// MARK: - Test Suite

#if FOUNDATION_XCTEST
import XCTest
class TestJSONEncoderSuper : XCTestCase { }
#else
import StdlibUnittest
class TestJSONEncoderSuper { }
#endif

class TestJSONEncoder : TestJSONEncoderSuper {
  // MARK: - Encoding Top-Level Empty Types
  func testEncodingTopLevelEmptyStruct() {
    let empty = EmptyStruct()
    _testRoundTrip(of: empty, expectedJSON: _jsonEmptyDictionary)
  }

  func testEncodingTopLevelEmptyClass() {
    let empty = EmptyClass()
    _testRoundTrip(of: empty, expectedJSON: _jsonEmptyDictionary)
  }

  // MARK: - Encoding Top-Level Single-Value Types
  func testEncodingTopLevelSingleValueEnum() {
    _testEncodeFailure(of: Switch.off)
    _testEncodeFailure(of: Switch.on)
  }

  func testEncodingTopLevelSingleValueStruct() {
    _testEncodeFailure(of: Timestamp(3141592653))
  }

  func testEncodingTopLevelSingleValueClass() {
    _testEncodeFailure(of: Counter())
  }

  // MARK: - Encoding Top-Level Structured Types
  func testEncodingTopLevelStructuredStruct() {
    // Address is a struct type with multiple fields.
    let address = Address.testValue
    _testRoundTrip(of: address)
  }

  func testEncodingTopLevelStructuredClass() {
    // Person is a class with multiple fields.
    let person = Person.testValue
    _testRoundTrip(of: person)
  }

  func testEncodingTopLevelDeepStructuredType() {
    // Company is a type with fields which are Codable themselves.
    let company = Company.testValue
    _testRoundTrip(of: company)
  }

  // MARK: - Date Strategy Tests
  func testEncodingDate() {
    // We can't encode a top-level Date, so it'll be wrapped in an array.
    _testRoundTrip(of: TopLevelWrapper(Date()))
  }

  func testEncodingDateSecondsSince1970() {
    // Cannot encode an arbitrary number of seconds since we've lost precision since 1970.
    let seconds = 1000.0
    let expectedJSON = "[1000]".data(using: .utf8)!

    // We can't encode a top-level Date, so it'll be wrapped in an array.
    _testRoundTrip(of: TopLevelWrapper(Date(timeIntervalSince1970: seconds)),
                   expectedJSON: expectedJSON,
                   dateEncodingStrategy: .secondsSince1970,
                   dateDecodingStrategy: .secondsSince1970)
  }

  func testEncodingDateMillisecondsSince1970() {
    // Cannot encode an arbitrary number of seconds since we've lost precision since 1970.
    let seconds = 1000.0
    let expectedJSON = "[1000000]".data(using: .utf8)!

    // We can't encode a top-level Date, so it'll be wrapped in an array.
    _testRoundTrip(of: TopLevelWrapper(Date(timeIntervalSince1970: seconds)),
                   expectedJSON: expectedJSON,
                   dateEncodingStrategy: .millisecondsSince1970,
                   dateDecodingStrategy: .millisecondsSince1970)
  }

  func testEncodingDateISO8601() {
    if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
      let formatter = ISO8601DateFormatter()
      formatter.formatOptions = .withInternetDateTime

      let timestamp = Date(timeIntervalSince1970: 1000)
      let expectedJSON = "[\"\(formatter.string(from: timestamp))\"]".data(using: .utf8)!

      // We can't encode a top-level Date, so it'll be wrapped in an array.
      _testRoundTrip(of: TopLevelWrapper(timestamp),
                     expectedJSON: expectedJSON,
                     dateEncodingStrategy: .iso8601,
                     dateDecodingStrategy: .iso8601)
    }
  }

  func testEncodingDateFormatted() {
    let formatter = DateFormatter()
    formatter.dateStyle = .full
    formatter.timeStyle = .full

    let timestamp = Date(timeIntervalSince1970: 1000)
    let expectedJSON = "[\"\(formatter.string(from: timestamp))\"]".data(using: .utf8)!

    // We can't encode a top-level Date, so it'll be wrapped in an array.
    _testRoundTrip(of: TopLevelWrapper(timestamp),
                   expectedJSON: expectedJSON,
                   dateEncodingStrategy: .formatted(formatter),
                   dateDecodingStrategy: .formatted(formatter))
  }

  func testEncodingDateCustom() {
    let timestamp = Date()

    // We'll encode a number instead of a date.
    let encode = { (_ data: Date, _ encoder: Encoder) throws -> Void in
      var container = encoder.singleValueContainer()
      try container.encode(42)
    }
    let decode = { (_: Decoder) throws -> Date in return timestamp }

    // We can't encode a top-level Date, so it'll be wrapped in an array.
    let expectedJSON = "[42]".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(timestamp),
                   expectedJSON: expectedJSON,
                   dateEncodingStrategy: .custom(encode),
                   dateDecodingStrategy: .custom(decode))
  }

  func testEncodingDateCustomEmpty() {
    let timestamp = Date()

    // Encoding nothing should encode an empty keyed container ({}).
    let encode = { (_: Date, _: Encoder) throws -> Void in }
    let decode = { (_: Decoder) throws -> Date in return timestamp }

    // We can't encode a top-level Date, so it'll be wrapped in an array.
    let expectedJSON = "[{}]".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(timestamp),
                   expectedJSON: expectedJSON,
                   dateEncodingStrategy: .custom(encode),
                   dateDecodingStrategy: .custom(decode))
  }

  // MARK: - Data Strategy Tests
  func testEncodingBase64Data() {
    let data = Data(bytes: [0xDE, 0xAD, 0xBE, 0xEF])

    // We can't encode a top-level Data, so it'll be wrapped in an array.
    let expectedJSON = "[\"3q2+7w==\"]".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(data), expectedJSON: expectedJSON)
  }

  func testEncodingCustomData() {
    // We'll encode a number instead of data.
    let encode = { (_ data: Data, _ encoder: Encoder) throws -> Void in
      var container = encoder.singleValueContainer()
      try container.encode(42)
    }
    let decode = { (_: Decoder) throws -> Data in return Data() }

    // We can't encode a top-level Data, so it'll be wrapped in an array.
    let expectedJSON = "[42]".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(Data()),
                   expectedJSON: expectedJSON,
                   dataEncodingStrategy: .custom(encode),
                   dataDecodingStrategy: .custom(decode))
  }

  func testEncodingCustomDataEmpty() {
    // Encoding nothing should encode an empty keyed container ({}).
    let encode = { (_: Data, _: Encoder) throws -> Void in }
    let decode = { (_: Decoder) throws -> Data in return Data() }

    // We can't encode a top-level Data, so it'll be wrapped in an array.
    let expectedJSON = "[{}]".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(Data()),
                   expectedJSON: expectedJSON,
                   dataEncodingStrategy: .custom(encode),
                   dataDecodingStrategy: .custom(decode))
  }

  // MARK: - Non-Conforming Floating Point Strategy Tests
  func testEncodingNonConformingFloats() {
    _testEncodeFailure(of: TopLevelWrapper(Float.infinity))
    _testEncodeFailure(of: TopLevelWrapper(-Float.infinity))
    _testEncodeFailure(of: TopLevelWrapper(Float.nan))

    _testEncodeFailure(of: TopLevelWrapper(Double.infinity))
    _testEncodeFailure(of: TopLevelWrapper(-Double.infinity))
    _testEncodeFailure(of: TopLevelWrapper(Double.nan))
  }

  func testEncodingNonConformingFloatStrings() {
    let encodingStrategy: JSONEncoder.NonConformingFloatEncodingStrategy = .convertToString(positiveInfinity: "INF", negativeInfinity: "-INF", nan: "NaN")
    let decodingStrategy: JSONDecoder.NonConformingFloatDecodingStrategy = .convertFromString(positiveInfinity: "INF", negativeInfinity: "-INF", nan: "NaN")


    _testRoundTrip(of: TopLevelWrapper(Float.infinity),
                   expectedJSON: "[\"INF\"]".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)
    _testRoundTrip(of: TopLevelWrapper(-Float.infinity),
                   expectedJSON: "[\"-INF\"]".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)

    // Since Float.nan != Float.nan, we have to use a placeholder that'll encode NaN but actually round-trip.
    _testRoundTrip(of: TopLevelWrapper(FloatNaNPlaceholder()),
                   expectedJSON: "[\"NaN\"]".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)

    _testRoundTrip(of: TopLevelWrapper(Double.infinity),
                   expectedJSON: "[\"INF\"]".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)
    _testRoundTrip(of: TopLevelWrapper(-Double.infinity),
                   expectedJSON: "[\"-INF\"]".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)

    // Since Double.nan != Double.nan, we have to use a placeholder that'll encode NaN but actually round-trip.
    _testRoundTrip(of: TopLevelWrapper(DoubleNaNPlaceholder()),
                   expectedJSON: "[\"NaN\"]".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)
  }

  // MARK: - Helper Functions
  private var _jsonEmptyDictionary: Data {
    return "{}".data(using: .utf8)!
  }

  private func _testEncodeFailure<T : Encodable>(of value: T) {
    do {
      let _ = try JSONEncoder().encode(value)
      expectUnreachable("Encode of top-level \(T.self) was expected to fail.")
    } catch {}
  }

  private func _testRoundTrip<T>(of value: T,
                                 expectedJSON json: Data? = nil,
                                 dateEncodingStrategy: JSONEncoder.DateEncodingStrategy = .deferredToDate,
                                 dateDecodingStrategy: JSONDecoder.DateDecodingStrategy = .deferredToDate,
                                 dataEncodingStrategy: JSONEncoder.DataEncodingStrategy = .base64Encode,
                                 dataDecodingStrategy: JSONDecoder.DataDecodingStrategy = .base64Decode,
                                 nonConformingFloatEncodingStrategy: JSONEncoder.NonConformingFloatEncodingStrategy = .throw,
                                 nonConformingFloatDecodingStrategy: JSONDecoder.NonConformingFloatDecodingStrategy = .throw) where T : Codable, T : Equatable {
    var payload: Data! = nil
    do {
      let encoder = JSONEncoder()
      encoder.dateEncodingStrategy = dateEncodingStrategy
      encoder.dataEncodingStrategy = dataEncodingStrategy
      encoder.nonConformingFloatEncodingStrategy = nonConformingFloatEncodingStrategy
      payload = try encoder.encode(value)
    } catch {
      expectUnreachable("Failed to encode \(T.self) to JSON.")
    }

    if let expectedJSON = json {
        expectEqual(expectedJSON, payload, "Produced JSON not identical to expected JSON.")
    }

    do {
      let decoder = JSONDecoder()
      decoder.dateDecodingStrategy = dateDecodingStrategy
      decoder.dataDecodingStrategy = dataDecodingStrategy
      decoder.nonConformingFloatDecodingStrategy = nonConformingFloatDecodingStrategy
      let decoded = try decoder.decode(T.self, from: payload)
      expectEqual(decoded, value, "\(T.self) did not round-trip to an equal value.")
    } catch {
      expectUnreachable("Failed to decode \(T.self) from JSON.")
    }
  }
}

// MARK: - Test Types
/* FIXME: Import from %S/Inputs/Coding/SharedTypes.swift somehow. */

// MARK: - Empty Types
fileprivate struct EmptyStruct : Codable, Equatable {
  static func ==(_ lhs: EmptyStruct, _ rhs: EmptyStruct) -> Bool {
    return true
  }
}

fileprivate class EmptyClass : Codable, Equatable {
  static func ==(_ lhs: EmptyClass, _ rhs: EmptyClass) -> Bool {
    return true
  }
}

// MARK: - Single-Value Types
/// A simple on-off switch type that encodes as a single Bool value.
fileprivate enum Switch : Codable {
  case off
  case on

  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    switch try container.decode(Bool.self) {
    case false: self = .off
    case true:  self = .on
    }
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    switch self {
    case .off: try container.encode(false)
    case .on:  try container.encode(true)
    }
  }
}

/// A simple timestamp type that encodes as a single Double value.
fileprivate struct Timestamp : Codable {
  let value: Double

  init(_ value: Double) {
    self.value = value
  }

  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    value = try container.decode(Double.self)
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(self.value)
  }
}

/// A simple referential counter type that encodes as a single Int value.
fileprivate final class Counter : Codable {
  var count: Int = 0

  init() {}

  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    count = try container.decode(Int.self)
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(self.count)
  }
}

// MARK: - Structured Types
/// A simple address type that encodes as a dictionary of values.
fileprivate struct Address : Codable, Equatable {
  let street: String
  let city: String
  let state: String
  let zipCode: Int
  let country: String

  init(street: String, city: String, state: String, zipCode: Int, country: String) {
    self.street = street
    self.city = city
    self.state = state
    self.zipCode = zipCode
    self.country = country
  }

  static func ==(_ lhs: Address, _ rhs: Address) -> Bool {
    return lhs.street == rhs.street &&
           lhs.city == rhs.city &&
           lhs.state == rhs.state &&
           lhs.zipCode == rhs.zipCode &&
           lhs.country == rhs.country
  }

  static var testValue: Address {
    return Address(street: "1 Infinite Loop",
                   city: "Cupertino",
                   state: "CA",
                   zipCode: 95014,
                   country: "United States")
  }
}

/// A simple person class that encodes as a dictionary of values.
fileprivate class Person : Codable, Equatable {
  let name: String
  let email: String

  init(name: String, email: String) {
    self.name = name
    self.email = email
  }

  static func ==(_ lhs: Person, _ rhs: Person) -> Bool {
    return lhs.name == rhs.name && lhs.email == rhs.email
  }

  static var testValue: Person {
    return Person(name: "Johnny Appleseed", email: "appleseed@apple.com")
  }
}

/// A simple company struct which encodes as a dictionary of nested values.
fileprivate struct Company : Codable, Equatable {
  let address: Address
  var employees: [Person]

  init(address: Address, employees: [Person]) {
    self.address = address
    self.employees = employees
  }

  static func ==(_ lhs: Company, _ rhs: Company) -> Bool {
    return lhs.address == rhs.address && lhs.employees == rhs.employees
  }

  static var testValue: Company {
    return Company(address: Address.testValue, employees: [Person.testValue])
  }
}

// MARK: - Helper Types

/// Wraps a type T so that it can be encoded at the top level of a payload.
fileprivate struct TopLevelWrapper<T> : Codable, Equatable where T : Codable, T : Equatable {
  let value: T

  init(_ value: T) {
    self.value = value
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(value)
  }

  init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    value = try container.decode(T.self)
    assert(container.isAtEnd)
  }

  static func ==(_ lhs: TopLevelWrapper<T>, _ rhs: TopLevelWrapper<T>) -> Bool {
    return lhs.value == rhs.value
  }
}

fileprivate struct FloatNaNPlaceholder : Codable, Equatable {
  init() {}

  func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(Float.nan)
  }

  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    let float = try container.decode(Float.self)
    if !float.isNaN {
      throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath, debugDescription: "Couldn't decode NaN."))
    }
  }

  static func ==(_ lhs: FloatNaNPlaceholder, _ rhs: FloatNaNPlaceholder) -> Bool {
    return true
  }
}

fileprivate struct DoubleNaNPlaceholder : Codable, Equatable {
  init() {}

  func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(Double.nan)
  }

  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    let double = try container.decode(Double.self)
    if !double.isNaN {
      throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath, debugDescription: "Couldn't decode NaN."))
    }
  }

  static func ==(_ lhs: DoubleNaNPlaceholder, _ rhs: DoubleNaNPlaceholder) -> Bool {
    return true
  }
}

// MARK: - Run Tests

#if !FOUNDATION_XCTEST
var JSONEncoderTests = TestSuite("TestJSONEncoder")
JSONEncoderTests.test("testEncodingTopLevelEmptyStruct")        { TestJSONEncoder().testEncodingTopLevelEmptyStruct()       }
JSONEncoderTests.test("testEncodingTopLevelEmptyClass")         { TestJSONEncoder().testEncodingTopLevelEmptyClass()        }
JSONEncoderTests.test("testEncodingTopLevelSingleValueEnum")    { TestJSONEncoder().testEncodingTopLevelSingleValueEnum()   }
JSONEncoderTests.test("testEncodingTopLevelSingleValueStruct")  { TestJSONEncoder().testEncodingTopLevelSingleValueStruct() }
JSONEncoderTests.test("testEncodingTopLevelSingleValueClass")   { TestJSONEncoder().testEncodingTopLevelSingleValueClass()  }
JSONEncoderTests.test("testEncodingTopLevelStructuredStruct")   { TestJSONEncoder().testEncodingTopLevelStructuredStruct()  }
JSONEncoderTests.test("testEncodingTopLevelStructuredClass")    { TestJSONEncoder().testEncodingTopLevelStructuredClass()   }
JSONEncoderTests.test("testEncodingTopLevelStructuredClass")    { TestJSONEncoder().testEncodingTopLevelStructuredClass()   }
JSONEncoderTests.test("testEncodingTopLevelDeepStructuredType") { TestJSONEncoder().testEncodingTopLevelDeepStructuredType()}
JSONEncoderTests.test("testEncodingDate")                       { TestJSONEncoder().testEncodingDate()                      }
JSONEncoderTests.test("testEncodingDateSecondsSince1970")       { TestJSONEncoder().testEncodingDateSecondsSince1970()      }
JSONEncoderTests.test("testEncodingDateMillisecondsSince1970")  { TestJSONEncoder().testEncodingDateMillisecondsSince1970() }
JSONEncoderTests.test("testEncodingDateISO8601")                { TestJSONEncoder().testEncodingDateISO8601()               }
JSONEncoderTests.test("testEncodingDateFormatted")              { TestJSONEncoder().testEncodingDateFormatted()             }
JSONEncoderTests.test("testEncodingDateCustom")                 { TestJSONEncoder().testEncodingDateCustom()                }
JSONEncoderTests.test("testEncodingDateCustomEmpty")            { TestJSONEncoder().testEncodingDateCustomEmpty()           }
JSONEncoderTests.test("testEncodingBase64Data")                 { TestJSONEncoder().testEncodingBase64Data()                }
JSONEncoderTests.test("testEncodingCustomData")                 { TestJSONEncoder().testEncodingCustomData()                }
JSONEncoderTests.test("testEncodingCustomDataEmpty")            { TestJSONEncoder().testEncodingCustomDataEmpty()           }
JSONEncoderTests.test("testEncodingNonConformingFloats")        { TestJSONEncoder().testEncodingNonConformingFloats()       }
JSONEncoderTests.test("testEncodingNonConformingFloatStrings")  { TestJSONEncoder().testEncodingNonConformingFloatStrings() }
runAllTests()
#endif
