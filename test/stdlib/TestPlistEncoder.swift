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
class TestPropertyListEncoderSuper : XCTestCase { }
#else
import StdlibUnittest
class TestPropertyListEncoderSuper { }
#endif

class TestPropertyListEncoder : TestPropertyListEncoderSuper {
  // MARK: - Encoding Top-Level Empty Types
  func testEncodingTopLevelEmptyStruct() {
    let empty = EmptyStruct()
    _testRoundTrip(of: empty, in: .binary, expectedPlist: _plistEmptyDictionaryBinary)
    _testRoundTrip(of: empty, in: .xml, expectedPlist: _plistEmptyDictionaryXML)
  }

  func testEncodingTopLevelEmptyClass() {
    let empty = EmptyClass()
    _testRoundTrip(of: empty, in: .binary, expectedPlist: _plistEmptyDictionaryBinary)
    _testRoundTrip(of: empty, in: .xml, expectedPlist: _plistEmptyDictionaryXML)
  }

  // MARK: - Encoding Top-Level Single-Value Types
  func testEncodingTopLevelSingleValueEnum() {
    let s1 = Switch.off
    _testEncodeFailure(of: s1, in: .binary)
    _testEncodeFailure(of: s1, in: .xml)
    _testRoundTrip(of: TopLevelWrapper(s1), in: .binary)
    _testRoundTrip(of: TopLevelWrapper(s1), in: .xml)

    let s2 = Switch.on
    _testEncodeFailure(of: s2, in: .binary)
    _testEncodeFailure(of: s2, in: .xml)
    _testRoundTrip(of: TopLevelWrapper(s2), in: .binary)
    _testRoundTrip(of: TopLevelWrapper(s2), in: .xml)
  }

  func testEncodingTopLevelSingleValueStruct() {
    let t = Timestamp(3141592653)
    _testEncodeFailure(of: t, in: .binary)
    _testEncodeFailure(of: t, in: .xml)
    _testRoundTrip(of: TopLevelWrapper(t), in: .binary)
    _testRoundTrip(of: TopLevelWrapper(t), in: .xml)
  }

  func testEncodingTopLevelSingleValueClass() {
    let c = Counter()
    _testEncodeFailure(of: c, in: .binary)
    _testEncodeFailure(of: c, in: .xml)
    _testRoundTrip(of: TopLevelWrapper(c), in: .binary)
    _testRoundTrip(of: TopLevelWrapper(c), in: .xml)
  }

  // MARK: - Encoding Top-Level Structured Types
  func testEncodingTopLevelStructuredStruct() {
    // Address is a struct type with multiple fields.
    let address = Address.testValue
    _testRoundTrip(of: address, in: .binary)
    _testRoundTrip(of: address, in: .xml)
  }

  func testEncodingTopLevelStructuredClass() {
    // Person is a class with multiple fields.
    let person = Person.testValue
    _testRoundTrip(of: person, in: .binary)
    _testRoundTrip(of: person, in: .xml)
  }

  func testEncodingTopLevelStructuredSingleStruct() {
    // Numbers is a struct which encodes as an array through a single value container.
    let numbers = Numbers.testValue
    _testRoundTrip(of: numbers, in: .binary)
    _testRoundTrip(of: numbers, in: .xml)
  }

  func testEncodingTopLevelStructuredSingleClass() {
    // Mapping is a class which encodes as a dictionary through a single value container.
    let mapping = Mapping.testValue
    _testRoundTrip(of: mapping, in: .binary)
    _testRoundTrip(of: mapping, in: .xml)
  }

  func testEncodingTopLevelDeepStructuredType() {
    // Company is a type with fields which are Codable themselves.
    let company = Company.testValue
    _testRoundTrip(of: company, in: .binary)
    _testRoundTrip(of: company, in: .xml)
  }

  func testEncodingClassWhichSharesEncoderWithSuper() {
    // Employee is a type which shares its encoder & decoder with its superclass, Person.
    let employee = Employee.testValue
    _testRoundTrip(of: employee, in: .binary)
    _testRoundTrip(of: employee, in: .xml)
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

  // MARK: - Helper Functions
  private var _plistEmptyDictionaryBinary: Data {
    return Data(base64Encoded: "YnBsaXN0MDDQCAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAJ")!
  }

  private var _plistEmptyDictionaryXML: Data {
    return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n<plist version=\"1.0\">\n<dict/>\n</plist>\n".data(using: .utf8)!
  }

  private func _testEncodeFailure<T : Encodable>(of value: T, in format: PropertyListSerialization.PropertyListFormat) {
    do {
      let encoder = PropertyListEncoder()
      encoder.outputFormat = format
      let _ = try encoder.encode(value)
      expectUnreachable("Encode of top-level \(T.self) was expected to fail.")
    } catch {}
  }

  private func _testRoundTrip<T>(of value: T, in format: PropertyListSerialization.PropertyListFormat, expectedPlist plist: Data? = nil) where T : Codable, T : Equatable {
    var payload: Data! = nil
    do {
      let encoder = PropertyListEncoder()
      encoder.outputFormat = format
      payload = try encoder.encode(value)
    } catch {
      expectUnreachable("Failed to encode \(T.self) to plist: \(error)")
    }

    if let expectedPlist = plist {
      expectEqual(expectedPlist, payload, "Produced plist not identical to expected plist.")
    }

    do {
      var decodedFormat: PropertyListSerialization.PropertyListFormat = .xml
      let decoded = try PropertyListDecoder().decode(T.self, from: payload, format: &decodedFormat)
      expectEqual(format, decodedFormat, "Encountered plist format differed from requested format.")
      expectEqual(decoded, value, "\(T.self) did not round-trip to an equal value.")
    } catch {
      expectUnreachable("Failed to decode \(T.self) from plist: \(error)")
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

// MARK: - Run Tests

#if !FOUNDATION_XCTEST
var PropertyListEncoderTests = TestSuite("TestPropertyListEncoder")
PropertyListEncoderTests.test("testEncodingTopLevelEmptyStruct") { TestPropertyListEncoder().testEncodingTopLevelEmptyStruct() }
PropertyListEncoderTests.test("testEncodingTopLevelEmptyClass") { TestPropertyListEncoder().testEncodingTopLevelEmptyClass() }
PropertyListEncoderTests.test("testEncodingTopLevelSingleValueEnum") { TestPropertyListEncoder().testEncodingTopLevelSingleValueEnum() }
PropertyListEncoderTests.test("testEncodingTopLevelSingleValueStruct") { TestPropertyListEncoder().testEncodingTopLevelSingleValueStruct() }
PropertyListEncoderTests.test("testEncodingTopLevelSingleValueClass") { TestPropertyListEncoder().testEncodingTopLevelSingleValueClass() }
PropertyListEncoderTests.test("testEncodingTopLevelStructuredStruct") { TestPropertyListEncoder().testEncodingTopLevelStructuredStruct() }
PropertyListEncoderTests.test("testEncodingTopLevelStructuredClass") { TestPropertyListEncoder().testEncodingTopLevelStructuredClass() }
PropertyListEncoderTests.test("testEncodingTopLevelStructuredSingleStruct") { TestPropertyListEncoder().testEncodingTopLevelStructuredSingleStruct() }
PropertyListEncoderTests.test("testEncodingTopLevelStructuredSingleClass") { TestPropertyListEncoder().testEncodingTopLevelStructuredSingleClass() }
PropertyListEncoderTests.test("testEncodingTopLevelDeepStructuredType") { TestPropertyListEncoder().testEncodingTopLevelDeepStructuredType() }
PropertyListEncoderTests.test("testEncodingClassWhichSharesEncoderWithSuper") { TestPropertyListEncoder().testEncodingClassWhichSharesEncoderWithSuper() }
PropertyListEncoderTests.test("testNestedContainerCodingPaths") { TestPropertyListEncoder().testNestedContainerCodingPaths() }
PropertyListEncoderTests.test("testSuperEncoderCodingPaths") { TestPropertyListEncoder().testSuperEncoderCodingPaths() }
runAllTests()
#endif
