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

    _testRoundTrip(of: TopLevelWrapper(Switch.off))
    _testRoundTrip(of: TopLevelWrapper(Switch.on))
  }

  func testEncodingTopLevelSingleValueStruct() {
    _testEncodeFailure(of: Timestamp(3141592653))
    _testRoundTrip(of: TopLevelWrapper(Timestamp(3141592653)))
  }

  func testEncodingTopLevelSingleValueClass() {
    _testEncodeFailure(of: Counter())
    _testRoundTrip(of: TopLevelWrapper(Counter()))
  }

  // MARK: - Encoding Top-Level Structured Types
  func testEncodingTopLevelStructuredStruct() {
    // Address is a struct type with multiple fields.
    let address = Address.testValue
    _testRoundTrip(of: address)
  }

  func testEncodingTopLevelStructuredClass() {
    // Person is a class with multiple fields.
    let expectedJSON = "{\"name\":\"Johnny Appleseed\",\"email\":\"appleseed@apple.com\"}".data(using: .utf8)!
    let person = Person.testValue
    _testRoundTrip(of: person, expectedJSON: expectedJSON)
  }

  func testEncodingTopLevelStructuredSingleStruct() {
    // Numbers is a struct which encodes as an array through a single value container.
    let numbers = Numbers.testValue
    _testRoundTrip(of: numbers)
  }

  func testEncodingTopLevelStructuredSingleClass() {
    // Mapping is a class which encodes as a dictionary through a single value container.
    let mapping = Mapping.testValue
    _testRoundTrip(of: mapping)
  }

  func testEncodingTopLevelDeepStructuredType() {
    // Company is a type with fields which are Codable themselves.
    let company = Company.testValue
    _testRoundTrip(of: company)
  }

  func testEncodingClassWhichSharesEncoderWithSuper() {
    // Employee is a type which shares its encoder & decoder with its superclass, Person.
    let employee = Employee.testValue
    _testRoundTrip(of: employee)
  }

  func testEncodingTopLevelNullableType() {
    // EnhancedBool is a type which encodes either as a Bool or as nil.
    _testEncodeFailure(of: EnhancedBool.true)
    _testEncodeFailure(of: EnhancedBool.false)
    _testEncodeFailure(of: EnhancedBool.fileNotFound)

    _testRoundTrip(of: TopLevelWrapper(EnhancedBool.true), expectedJSON: "{\"value\":true}".data(using: .utf8)!)
    _testRoundTrip(of: TopLevelWrapper(EnhancedBool.false), expectedJSON: "{\"value\":false}".data(using: .utf8)!)
    _testRoundTrip(of: TopLevelWrapper(EnhancedBool.fileNotFound), expectedJSON: "{\"value\":null}".data(using: .utf8)!)
  }

  // MARK: - Output Formatting Tests
  func testEncodingOutputFormattingDefault() {
    let expectedJSON = "{\"name\":\"Johnny Appleseed\",\"email\":\"appleseed@apple.com\"}".data(using: .utf8)!
    let person = Person.testValue
    _testRoundTrip(of: person, expectedJSON: expectedJSON)
  }

  func testEncodingOutputFormattingPrettyPrinted() {
    let expectedJSON = "{\n  \"name\" : \"Johnny Appleseed\",\n  \"email\" : \"appleseed@apple.com\"\n}".data(using: .utf8)!
    let person = Person.testValue
    _testRoundTrip(of: person, expectedJSON: expectedJSON, outputFormatting: [.prettyPrinted])
  }

  func testEncodingOutputFormattingSortedKeys() {
    if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
      let expectedJSON = "{\"email\":\"appleseed@apple.com\",\"name\":\"Johnny Appleseed\"}".data(using: .utf8)!
      let person = Person.testValue
      _testRoundTrip(of: person, expectedJSON: expectedJSON, outputFormatting: [.sortedKeys])
    }
  }

  func testEncodingOutputFormattingPrettyPrintedSortedKeys() {
    if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
      let expectedJSON = "{\n  \"email\" : \"appleseed@apple.com\",\n  \"name\" : \"Johnny Appleseed\"\n}".data(using: .utf8)!
      let person = Person.testValue
      _testRoundTrip(of: person, expectedJSON: expectedJSON, outputFormatting: [.prettyPrinted, .sortedKeys])
    }
  }

  // MARK: - Date Strategy Tests
  func testEncodingDate() {
    // We can't encode a top-level Date, so it'll be wrapped in a dictionary.
    _testRoundTrip(of: TopLevelWrapper(Date()))
  }

  func testEncodingDateSecondsSince1970() {
    // Cannot encode an arbitrary number of seconds since we've lost precision since 1970.
    let seconds = 1000.0
    let expectedJSON = "{\"value\":1000}".data(using: .utf8)!

    // We can't encode a top-level Date, so it'll be wrapped in a dictionary.
    _testRoundTrip(of: TopLevelWrapper(Date(timeIntervalSince1970: seconds)),
                   expectedJSON: expectedJSON,
                   dateEncodingStrategy: .secondsSince1970,
                   dateDecodingStrategy: .secondsSince1970)
  }

  func testEncodingDateMillisecondsSince1970() {
    // Cannot encode an arbitrary number of seconds since we've lost precision since 1970.
    let seconds = 1000.0
    let expectedJSON = "{\"value\":1000000}".data(using: .utf8)!

    // We can't encode a top-level Date, so it'll be wrapped in a dictionary.
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
      let expectedJSON = "{\"value\":\"\(formatter.string(from: timestamp))\"}".data(using: .utf8)!

      // We can't encode a top-level Date, so it'll be wrapped in a dictionary.
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
    let expectedJSON = "{\"value\":\"\(formatter.string(from: timestamp))\"}".data(using: .utf8)!

    // We can't encode a top-level Date, so it'll be wrapped in a dictionary.
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

    // We can't encode a top-level Date, so it'll be wrapped in a dictionary.
    let expectedJSON = "{\"value\":42}".data(using: .utf8)!
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

    // We can't encode a top-level Date, so it'll be wrapped in a dictionary.
    let expectedJSON = "{\"value\":{}}".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(timestamp),
                   expectedJSON: expectedJSON,
                   dateEncodingStrategy: .custom(encode),
                   dateDecodingStrategy: .custom(decode))
  }

  // MARK: - Data Strategy Tests
  func testEncodingData() {
    let data = Data(bytes: [0xDE, 0xAD, 0xBE, 0xEF])

    // We can't encode a top-level Data, so it'll be wrapped in a dictionary.
    let expectedJSON = "{\"value\":[222,173,190,239]}".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(data),
                   expectedJSON: expectedJSON,
                   dataEncodingStrategy: .deferredToData,
                   dataDecodingStrategy: .deferredToData)
  }

  func testEncodingBase64Data() {
    let data = Data(bytes: [0xDE, 0xAD, 0xBE, 0xEF])

    // We can't encode a top-level Data, so it'll be wrapped in a dictionary.
    let expectedJSON = "{\"value\":\"3q2+7w==\"}".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(data), expectedJSON: expectedJSON)
  }

  func testEncodingCustomData() {
    // We'll encode a number instead of data.
    let encode = { (_ data: Data, _ encoder: Encoder) throws -> Void in
      var container = encoder.singleValueContainer()
      try container.encode(42)
    }
    let decode = { (_: Decoder) throws -> Data in return Data() }

    // We can't encode a top-level Data, so it'll be wrapped in a dictionary.
    let expectedJSON = "{\"value\":42}".data(using: .utf8)!
    _testRoundTrip(of: TopLevelWrapper(Data()),
                   expectedJSON: expectedJSON,
                   dataEncodingStrategy: .custom(encode),
                   dataDecodingStrategy: .custom(decode))
  }

  func testEncodingCustomDataEmpty() {
    // Encoding nothing should encode an empty keyed container ({}).
    let encode = { (_: Data, _: Encoder) throws -> Void in }
    let decode = { (_: Decoder) throws -> Data in return Data() }

    // We can't encode a top-level Data, so it'll be wrapped in a dictionary.
    let expectedJSON = "{\"value\":{}}".data(using: .utf8)!
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
                   expectedJSON: "{\"value\":\"INF\"}".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)
    _testRoundTrip(of: TopLevelWrapper(-Float.infinity),
                   expectedJSON: "{\"value\":\"-INF\"}".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)

    // Since Float.nan != Float.nan, we have to use a placeholder that'll encode NaN but actually round-trip.
    _testRoundTrip(of: TopLevelWrapper(FloatNaNPlaceholder()),
                   expectedJSON: "{\"value\":\"NaN\"}".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)

    _testRoundTrip(of: TopLevelWrapper(Double.infinity),
                   expectedJSON: "{\"value\":\"INF\"}".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)
    _testRoundTrip(of: TopLevelWrapper(-Double.infinity),
                   expectedJSON: "{\"value\":\"-INF\"}".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)

    // Since Double.nan != Double.nan, we have to use a placeholder that'll encode NaN but actually round-trip.
    _testRoundTrip(of: TopLevelWrapper(DoubleNaNPlaceholder()),
                   expectedJSON: "{\"value\":\"NaN\"}".data(using: .utf8)!,
                   nonConformingFloatEncodingStrategy: encodingStrategy,
                   nonConformingFloatDecodingStrategy: decodingStrategy)
  }

  // MARK: - Encoder Features
  func testNestedContainerCodingPaths() {
    let encoder = JSONEncoder()
    do {
      let _ = try encoder.encode(NestedContainersTestType())
    } catch let error as NSError {
      expectUnreachable("Caught error during encoding nested container types: \(error)")
    }
  }

  func testSuperEncoderCodingPaths() {
    let encoder = JSONEncoder()
    do {
      let _ = try encoder.encode(NestedContainersTestType(testSuperEncoder: true))
    } catch let error as NSError {
      expectUnreachable("Caught error during encoding nested container types: \(error)")
    }
  }

  func testInterceptDecimal() {
    let expectedJSON = "{\"value\":10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000}".data(using: .utf8)!

    // Want to make sure we write out a JSON number, not the keyed encoding here.
    // 1e127 is too big to fit natively in a Double, too, so want to make sure it's encoded as a Decimal.
    let decimal = Decimal(sign: .plus, exponent: 127, significand: Decimal(1))
    _testRoundTrip(of: TopLevelWrapper(decimal), expectedJSON: expectedJSON)
  }

  func testInterceptURL() {
    // Want to make sure JSONEncoder writes out single-value URLs, not the keyed encoding.
    let expectedJSON = "{\"value\":\"http:\\/\\/swift.org\"}".data(using: .utf8)!
    let url = URL(string: "http://swift.org")!
    _testRoundTrip(of: TopLevelWrapper(url), expectedJSON: expectedJSON)
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
                                 outputFormatting: JSONEncoder.OutputFormatting = [],
                                 dateEncodingStrategy: JSONEncoder.DateEncodingStrategy = .deferredToDate,
                                 dateDecodingStrategy: JSONDecoder.DateDecodingStrategy = .deferredToDate,
                                 dataEncodingStrategy: JSONEncoder.DataEncodingStrategy = .base64,
                                 dataDecodingStrategy: JSONDecoder.DataDecodingStrategy = .base64,
                                 nonConformingFloatEncodingStrategy: JSONEncoder.NonConformingFloatEncodingStrategy = .throw,
                                 nonConformingFloatDecodingStrategy: JSONDecoder.NonConformingFloatDecodingStrategy = .throw) where T : Codable, T : Equatable {
    var payload: Data! = nil
    do {
      let encoder = JSONEncoder()
      encoder.outputFormatting = outputFormatting
      encoder.dateEncodingStrategy = dateEncodingStrategy
      encoder.dataEncodingStrategy = dataEncodingStrategy
      encoder.nonConformingFloatEncodingStrategy = nonConformingFloatEncodingStrategy
      payload = try encoder.encode(value)
    } catch {
      expectUnreachable("Failed to encode \(T.self) to JSON: \(error)")
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
      expectUnreachable("Failed to decode \(T.self) from JSON: \(error)")
    }
  }
}

// MARK: - Helper Global Functions
func expectEqualPaths(_ lhs: [CodingKey?], _ rhs: [CodingKey?], _ prefix: String) {
  if lhs.count != rhs.count {
    expectUnreachable("\(prefix) [CodingKey?].count mismatch: \(lhs.count) != \(rhs.count)")
    return
  }

  for (k1, k2) in zip(lhs, rhs) {
    switch (k1, k2) {
    case (.none, .none): continue
    case (.some(let _k1), .none):
      expectUnreachable("\(prefix) CodingKey mismatch: \(type(of: _k1)) != nil")
      return
    case (.none, .some(let _k2)):
      expectUnreachable("\(prefix) CodingKey mismatch: nil != \(type(of: _k2))")
      return
    default: break
    }

    let key1 = k1!
    let key2 = k2!

    switch (key1.intValue, key2.intValue) {
    case (.none, .none): break
    case (.some(let i1), .none):
      expectUnreachable("\(prefix) CodingKey.intValue mismatch: \(type(of: key1))(\(i1)) != nil")
      return
    case (.none, .some(let i2)):
      expectUnreachable("\(prefix) CodingKey.intValue mismatch: nil != \(type(of: key2))(\(i2))")
      return
    case (.some(let i1), .some(let i2)):
        guard i1 == i2 else {
            expectUnreachable("\(prefix) CodingKey.intValue mismatch: \(type(of: key1))(\(i1)) != \(type(of: key2))(\(i2))")
            return
        }

        break
    }

    expectEqual(key1.stringValue, key2.stringValue, "\(prefix) CodingKey.stringValue mismatch: \(type(of: key1))('\(key1.stringValue)') != \(type(of: key2))('\(key2.stringValue)')")
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
fileprivate struct Timestamp : Codable, Equatable {
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

  static func ==(_ lhs: Timestamp, _ rhs: Timestamp) -> Bool {
    return lhs.value == rhs.value
  }
}

/// A simple referential counter type that encodes as a single Int value.
fileprivate final class Counter : Codable, Equatable {
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

  static func ==(_ lhs: Counter, _ rhs: Counter) -> Bool {
    return lhs === rhs || lhs.count == rhs.count
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
  let website: URL?

  init(name: String, email: String, website: URL? = nil) {
    self.name = name
    self.email = email
    self.website = website
  }

  private enum CodingKeys : String, CodingKey {
    case name
    case email
    case website
  }

  // FIXME: Remove when subclasses (Employee) are able to override synthesized conformance.
  required init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    name = try container.decode(String.self, forKey: .name)
    email = try container.decode(String.self, forKey: .email)
    website = try container.decodeIfPresent(URL.self, forKey: .website)
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(name, forKey: .name)
    try container.encode(email, forKey: .email)
    try container.encodeIfPresent(website, forKey: .website)
  }

  func isEqual(_ other: Person) -> Bool {
    return self.name == other.name &&
           self.email == other.email &&
           self.website == other.website
  }

  static func ==(_ lhs: Person, _ rhs: Person) -> Bool {
    return lhs.isEqual(rhs)
  }

  class var testValue: Person {
    return Person(name: "Johnny Appleseed", email: "appleseed@apple.com")
  }
}

/// A class which shares its encoder and decoder with its superclass.
fileprivate class Employee : Person {
  let id: Int

  init(name: String, email: String, website: URL? = nil, id: Int) {
    self.id = id
    super.init(name: name, email: email, website: website)
  }

  enum CodingKeys : String, CodingKey {
    case id
  }

  required init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    id = try container.decode(Int.self, forKey: .id)
    try super.init(from: decoder)
  }

  override func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(id, forKey: .id)
    try super.encode(to: encoder)
  }

  override func isEqual(_ other: Person) -> Bool {
    if let employee = other as? Employee {
      guard self.id == employee.id else { return false }
    }

    return super.isEqual(other)
  }

  override class var testValue: Employee {
    return Employee(name: "Johnny Appleseed", email: "appleseed@apple.com", id: 42)
  }
}

/// A simple company struct which encodes as a dictionary of nested values.
fileprivate struct Company : Codable, Equatable {
  let address: Address
  var employees: [Employee]

  init(address: Address, employees: [Employee]) {
    self.address = address
    self.employees = employees
  }

  static func ==(_ lhs: Company, _ rhs: Company) -> Bool {
    return lhs.address == rhs.address && lhs.employees == rhs.employees
  }

  static var testValue: Company {
    return Company(address: Address.testValue, employees: [Employee.testValue])
  }
}

/// An enum type which decodes from Bool?.
fileprivate enum EnhancedBool : Codable {
  case `true`
  case `false`
  case fileNotFound

  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    if container.decodeNil() {
      self = .fileNotFound
    } else {
      let value = try container.decode(Bool.self)
      self = value ? .true : .false
    }
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    switch self {
    case .true: try container.encode(true)
    case .false: try container.encode(false)
    case .fileNotFound: try container.encodeNil()
    }
  }
}

/// A type which encodes as an array directly through a single value container.
struct Numbers : Codable, Equatable {
  let values = [4, 8, 15, 16, 23, 42]

  init() {}

  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    let decodedValues = try container.decode([Int].self)
    guard decodedValues == values else {
      throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath, debugDescription: "The Numbers are wrong!"))
    }
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(values)
  }

  static func ==(_ lhs: Numbers, _ rhs: Numbers) -> Bool {
    return lhs.values == rhs.values
  }

  static var testValue: Numbers {
    return Numbers()
  }
}

/// A type which encodes as a dictionary directly through a single value container.
fileprivate final class Mapping : Codable, Equatable {
  let values: [String : URL]

  init(values: [String : URL]) {
    self.values = values
  }

  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    values = try container.decode([String : URL].self)
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(values)
  }

  static func ==(_ lhs: Mapping, _ rhs: Mapping) -> Bool {
    return lhs === rhs || lhs.values == rhs.values
  }

  static var testValue: Mapping {
    return Mapping(values: ["Apple": URL(string: "http://apple.com")!,
                            "localhost": URL(string: "http://127.0.0.1")!])
  }
}

struct NestedContainersTestType : Encodable {
  let testSuperEncoder: Bool

  init(testSuperEncoder: Bool = false) {
    self.testSuperEncoder = testSuperEncoder
  }

  enum TopLevelCodingKeys : Int, CodingKey {
    case a
    case b
    case c
  }

  enum IntermediateCodingKeys : Int, CodingKey {
      case one
      case two
  }

  func encode(to encoder: Encoder) throws {
    if self.testSuperEncoder {
      var topLevelContainer = encoder.container(keyedBy: TopLevelCodingKeys.self)
      expectEqualPaths(encoder.codingPath, [], "Top-level Encoder's codingPath changed.")
      expectEqualPaths(topLevelContainer.codingPath, [], "New first-level keyed container has non-empty codingPath.")

      let superEncoder = topLevelContainer.superEncoder(forKey: .a)
      expectEqualPaths(encoder.codingPath, [], "Top-level Encoder's codingPath changed.")
      expectEqualPaths(topLevelContainer.codingPath, [], "First-level keyed container's codingPath changed.")
      expectEqualPaths(superEncoder.codingPath, [TopLevelCodingKeys.a], "New superEncoder had unexpected codingPath.")
      _testNestedContainers(in: superEncoder, baseCodingPath: [TopLevelCodingKeys.a])
    } else {
      _testNestedContainers(in: encoder, baseCodingPath: [])
    }
  }

  func _testNestedContainers(in encoder: Encoder, baseCodingPath: [CodingKey?]) {
    expectEqualPaths(encoder.codingPath, baseCodingPath, "New encoder has non-empty codingPath.")

    // codingPath should not change upon fetching a non-nested container.
    var firstLevelContainer = encoder.container(keyedBy: TopLevelCodingKeys.self)
    expectEqualPaths(encoder.codingPath, baseCodingPath, "Top-level Encoder's codingPath changed.")
    expectEqualPaths(firstLevelContainer.codingPath, baseCodingPath, "New first-level keyed container has non-empty codingPath.")

    // Nested Keyed Container
    do {
      // Nested container for key should have a new key pushed on.
      var secondLevelContainer = firstLevelContainer.nestedContainer(keyedBy: IntermediateCodingKeys.self, forKey: .a)
      expectEqualPaths(encoder.codingPath, baseCodingPath, "Top-level Encoder's codingPath changed.")
      expectEqualPaths(firstLevelContainer.codingPath, baseCodingPath, "First-level keyed container's codingPath changed.")
      expectEqualPaths(secondLevelContainer.codingPath, baseCodingPath + [TopLevelCodingKeys.a], "New second-level keyed container had unexpected codingPath.")

      // Inserting a keyed container should not change existing coding paths.
      let thirdLevelContainerKeyed = secondLevelContainer.nestedContainer(keyedBy: IntermediateCodingKeys.self, forKey: .one)
      expectEqualPaths(encoder.codingPath, baseCodingPath, "Top-level Encoder's codingPath changed.")
      expectEqualPaths(firstLevelContainer.codingPath, baseCodingPath, "First-level keyed container's codingPath changed.")
      expectEqualPaths(secondLevelContainer.codingPath, baseCodingPath + [TopLevelCodingKeys.a], "Second-level keyed container's codingPath changed.")
      expectEqualPaths(thirdLevelContainerKeyed.codingPath, baseCodingPath + [TopLevelCodingKeys.a, IntermediateCodingKeys.one], "New third-level keyed container had unexpected codingPath.")

      // Inserting an unkeyed container should not change existing coding paths.
      let thirdLevelContainerUnkeyed = secondLevelContainer.nestedUnkeyedContainer(forKey: .two)
      expectEqualPaths(encoder.codingPath, baseCodingPath + [], "Top-level Encoder's codingPath changed.")
      expectEqualPaths(firstLevelContainer.codingPath, baseCodingPath + [], "First-level keyed container's codingPath changed.")
      expectEqualPaths(secondLevelContainer.codingPath, baseCodingPath + [TopLevelCodingKeys.a], "Second-level keyed container's codingPath changed.")
      expectEqualPaths(thirdLevelContainerUnkeyed.codingPath, baseCodingPath + [TopLevelCodingKeys.a, IntermediateCodingKeys.two], "New third-level unkeyed container had unexpected codingPath.")
    }

    // Nested Unkeyed Container
    do {
      // Nested container for key should have a new key pushed on.
      var secondLevelContainer = firstLevelContainer.nestedUnkeyedContainer(forKey: .a)
      expectEqualPaths(encoder.codingPath, baseCodingPath, "Top-level Encoder's codingPath changed.")
      expectEqualPaths(firstLevelContainer.codingPath, baseCodingPath, "First-level keyed container's codingPath changed.")
      expectEqualPaths(secondLevelContainer.codingPath, baseCodingPath + [TopLevelCodingKeys.a], "New second-level keyed container had unexpected codingPath.")

      // Appending a keyed container should not change existing coding paths.
      let thirdLevelContainerKeyed = secondLevelContainer.nestedContainer(keyedBy: IntermediateCodingKeys.self)
      expectEqualPaths(encoder.codingPath, baseCodingPath, "Top-level Encoder's codingPath changed.")
      expectEqualPaths(firstLevelContainer.codingPath, baseCodingPath, "First-level keyed container's codingPath changed.")
      expectEqualPaths(secondLevelContainer.codingPath, baseCodingPath + [TopLevelCodingKeys.a], "Second-level unkeyed container's codingPath changed.")
      expectEqualPaths(thirdLevelContainerKeyed.codingPath, baseCodingPath + [TopLevelCodingKeys.a, nil], "New third-level keyed container had unexpected codingPath.")

      // Appending an unkeyed container should not change existing coding paths.
      let thirdLevelContainerUnkeyed = secondLevelContainer.nestedUnkeyedContainer()
      expectEqualPaths(encoder.codingPath, baseCodingPath, "Top-level Encoder's codingPath changed.")
      expectEqualPaths(firstLevelContainer.codingPath, baseCodingPath, "First-level keyed container's codingPath changed.")
      expectEqualPaths(secondLevelContainer.codingPath, baseCodingPath + [TopLevelCodingKeys.a], "Second-level unkeyed container's codingPath changed.")
      expectEqualPaths(thirdLevelContainerUnkeyed.codingPath, baseCodingPath + [TopLevelCodingKeys.a, nil], "New third-level unkeyed container had unexpected codingPath.")
    }
  }
}

// MARK: - Helper Types

/// Wraps a type T so that it can be encoded at the top level of a payload.
fileprivate struct TopLevelWrapper<T> : Codable, Equatable where T : Codable, T : Equatable {
  let value: T

  init(_ value: T) {
    self.value = value
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
JSONEncoderTests.test("testEncodingTopLevelEmptyStruct") { TestJSONEncoder().testEncodingTopLevelEmptyStruct() }
JSONEncoderTests.test("testEncodingTopLevelEmptyClass") { TestJSONEncoder().testEncodingTopLevelEmptyClass() }
JSONEncoderTests.test("testEncodingTopLevelSingleValueEnum") { TestJSONEncoder().testEncodingTopLevelSingleValueEnum() }
JSONEncoderTests.test("testEncodingTopLevelSingleValueStruct") { TestJSONEncoder().testEncodingTopLevelSingleValueStruct() }
JSONEncoderTests.test("testEncodingTopLevelSingleValueClass") { TestJSONEncoder().testEncodingTopLevelSingleValueClass() }
JSONEncoderTests.test("testEncodingTopLevelStructuredStruct") { TestJSONEncoder().testEncodingTopLevelStructuredStruct() }
JSONEncoderTests.test("testEncodingTopLevelStructuredClass") { TestJSONEncoder().testEncodingTopLevelStructuredClass() }
JSONEncoderTests.test("testEncodingTopLevelStructuredSingleStruct") { TestJSONEncoder().testEncodingTopLevelStructuredSingleStruct() }
JSONEncoderTests.test("testEncodingTopLevelStructuredSingleClass") { TestJSONEncoder().testEncodingTopLevelStructuredSingleClass() }
JSONEncoderTests.test("testEncodingTopLevelDeepStructuredType") { TestJSONEncoder().testEncodingTopLevelDeepStructuredType()}
JSONEncoderTests.test("testEncodingClassWhichSharesEncoderWithSuper") { TestJSONEncoder().testEncodingClassWhichSharesEncoderWithSuper() }
JSONEncoderTests.test("testEncodingTopLevelNullableType") { TestJSONEncoder().testEncodingTopLevelNullableType() }
JSONEncoderTests.test("testEncodingOutputFormattingDefault") { TestJSONEncoder().testEncodingOutputFormattingDefault() }
JSONEncoderTests.test("testEncodingOutputFormattingPrettyPrinted") { TestJSONEncoder().testEncodingOutputFormattingPrettyPrinted() }
JSONEncoderTests.test("testEncodingOutputFormattingSortedKeys") { TestJSONEncoder().testEncodingOutputFormattingSortedKeys() }
JSONEncoderTests.test("testEncodingOutputFormattingPrettyPrintedSortedKeys") { TestJSONEncoder().testEncodingOutputFormattingPrettyPrintedSortedKeys() }
JSONEncoderTests.test("testEncodingDate") { TestJSONEncoder().testEncodingDate() }
JSONEncoderTests.test("testEncodingDateSecondsSince1970") { TestJSONEncoder().testEncodingDateSecondsSince1970() }
JSONEncoderTests.test("testEncodingDateMillisecondsSince1970") { TestJSONEncoder().testEncodingDateMillisecondsSince1970() }
JSONEncoderTests.test("testEncodingDateISO8601") { TestJSONEncoder().testEncodingDateISO8601() }
JSONEncoderTests.test("testEncodingDateFormatted") { TestJSONEncoder().testEncodingDateFormatted() }
JSONEncoderTests.test("testEncodingDateCustom") { TestJSONEncoder().testEncodingDateCustom() }
JSONEncoderTests.test("testEncodingDateCustomEmpty") { TestJSONEncoder().testEncodingDateCustomEmpty() }
JSONEncoderTests.test("testEncodingData") { TestJSONEncoder().testEncodingData() }
JSONEncoderTests.test("testEncodingBase64Data") { TestJSONEncoder().testEncodingBase64Data() }
JSONEncoderTests.test("testEncodingCustomData") { TestJSONEncoder().testEncodingCustomData() }
JSONEncoderTests.test("testEncodingCustomDataEmpty") { TestJSONEncoder().testEncodingCustomDataEmpty() }
JSONEncoderTests.test("testEncodingNonConformingFloats") { TestJSONEncoder().testEncodingNonConformingFloats() }
JSONEncoderTests.test("testEncodingNonConformingFloatStrings") { TestJSONEncoder().testEncodingNonConformingFloatStrings() }
JSONEncoderTests.test("testNestedContainerCodingPaths") { TestJSONEncoder().testNestedContainerCodingPaths() }
JSONEncoderTests.test("testSuperEncoderCodingPaths") { TestJSONEncoder().testSuperEncoderCodingPaths() }
JSONEncoderTests.test("testInterceptDecimal") { TestJSONEncoder().testInterceptDecimal() }
JSONEncoderTests.test("testInterceptURL") { TestJSONEncoder().testInterceptURL() }
runAllTests()
#endif
