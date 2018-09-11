
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
import XPC
import Foundation
import StdlibUnittest

class TestXPCEncoder {
    // MARK: - Encoding Top-Level Empty Types
    func testEncodingTopLevelEmptyStruct() {
        let empty = EmptyStruct()
        _testRoundTrip(of: empty)
    }

    func testEncodingTopLevelEmptyClass() {
        let empty = EmptyClass()
        _testRoundTrip(of: empty)
    }

    // MARK: - Encoding Top-Level Single-Value Types
    func testEncodingTopLevelSingleValueEnum() {
        _testRoundTrip(of: Switch.off)
        _testRoundTrip(of: Switch.on)
    }

    func testEncodingTopLevelSingleValueStruct() {
        _testRoundTrip(of: Timestamp(3141592653))
    }

    func testEncodingTopLevelSingleValueClass() {
        _testRoundTrip(of: Counter())
    }

    // MARK: - Ecoding Top-Level Structured Types
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

    func testEncodingDerivedClass() {
        let programmer = Programmer.testValue
        _testRoundTrip(of: programmer)
    }

    func testEncodingClassWhichSharesEncoderWithSuper() {
        // Employee is a type which shares its encoder & decoder with its superclass, Person.
        let employee = Employee.testValue
        _testRoundTrip(of: employee)
    }

    func testEncodingTopLevelDeepStructuredType() {
        // Company is a type with fields which are Codable themselves.
        let company = Company.testValue
        _testRoundTrip(of: company)
    }

    func testEncodingTopLevelNullableType() {
        _testRoundTrip(of: EnhancedBool.true)
        _testRoundTrip(of: EnhancedBool.false)
        _testRoundTrip(of: EnhancedBool.fileNotFound)
    }

    func testEncodingDictionaryFailureKeyPath() {
        let toEncode: [String: EncodeFailure] = ["key": EncodeFailure(someValue: 3.14)]

        do {
            _ = try XPCEncoder.encode(toEncode)
        } catch EncodingError.invalidValue(let (_, context)) {
            expectEqual(1, context.codingPath.count)
            expectEqual("key", context.codingPath[0].stringValue)
        } catch {
            expectUnreachable("Unexpected error: \(String(describing: error))")
        }
    }

    func testEncodingDictionaryFailureKeyPathNested() {
        let toEncode: [String: [String: EncodeFailureNested]] = ["key": ["sub_key": EncodeFailureNested(nestedValue: EncodeFailure(someValue: 3.14))]]

        do {
            _ = try XPCEncoder.encode(toEncode)
        } catch EncodingError.invalidValue(let (_, context)) {
            expectEqual(3, context.codingPath.count)
            expectEqual("key", context.codingPath[0].stringValue)
            expectEqual("sub_key", context.codingPath[1].stringValue)
            expectEqual("nestedValue", context.codingPath[2].stringValue)
        } catch {
            expectUnreachable("Unexpected error: \(String(describing: error))")
        }
    }

    func testDecodingDictionaryFailureKeyPath() {
        let input = xpc_dictionary_create(nil, nil, 0)
        let key = "intValue"
        let value = "not an integer"
        var xpcValue: xpc_object_t!
        value.withCString({ xpcValue = xpc_string_create($0) })
        key.withCString({ xpc_dictionary_set_value(input, $0, xpcValue) })

        do {
            _ = try XPCDecoder.decode(DecodeFailure.self, message: input)
        } catch DecodingError.typeMismatch(let (_, context)) {
            expectEqual(1, context.codingPath.count)
            expectEqual("intValue", context.codingPath[0].stringValue)
        } catch {
            expectUnreachable("Unexpected error: \(String(describing: error))")
        }
    }

    func testDecodingDictionaryFailureKeyPathNested() {
        let input = xpc_dictionary_create(nil, nil, 0)
        let subInput = xpc_dictionary_create(nil, nil, 0)
        let nestedKey = "nestedValue"
        let intKey = "intValue"
        let value = "not an integer"
        var xpcValue: xpc_object_t!
        value.withCString({ xpcValue = xpc_string_create($0) })
        intKey.withCString({ xpc_dictionary_set_value(subInput, $0, xpcValue) })
        nestedKey.withCString({ xpc_dictionary_set_value(input, $0, subInput) })

        do {
            _ = try XPCDecoder.decode(DecodeFailureNested.self, message: input)
        } catch DecodingError.typeMismatch(let (_, context)) {
            expectEqual(2, context.codingPath.count)
            expectEqual("intValue", context.codingPath[1].stringValue)
        } catch {
            expectUnreachable("Unexpected error: \(String(describing: error))")
        }
    }

    // MARK: - Testing helpers
    private func _testRoundTrip<T>(of value: T) where T : Codable, T: Equatable {
        var payload: xpc_object_t! = nil
        do {
            payload = try XPCEncoder.encode(value)
        } catch {
            expectUnreachable("Failed to encode \(T.self) to xpc_object_t: \(error)")
        }

        do {
            let decoded = try XPCDecoder.decode(T.self, message: payload)
            expectEqual(decoded, value, "\(T.self) did not round-trip to an equal value.")
        } catch {
            expectUnreachable("Failed to decode \(T.self) from xpc_object_t: \(error)")
        }
    }
}

// MARK: - Test Types
/* FIXME: Import from %S/Inputs/Coding/SharedTypes.swift somehow. */

// MARK: - Empty helper types for testing
fileprivate struct EmptyStruct : Codable, Equatable {}
fileprivate struct EmptyClass : Codable, Equatable {}

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

/// A derived class
fileprivate class Programmer : Person {
    let favoriteIDE: String

    init(name: String, email: String, website: URL? = nil, favoriteIDE: String) {
        self.favoriteIDE = favoriteIDE
        super.init(name: name, email: email, website: website)
    }

    required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        favoriteIDE = try container.decode(String.self, forKey: .favoriteIDE)
        try super.init(from: container.superDecoder())
    }

    override func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(favoriteIDE, forKey: .favoriteIDE)
        try super.encode(to: container.superEncoder())
    }

    enum CodingKeys : String, CodingKey {
        case favoriteIDE
    }

    override func isEqual(_ other: Person) -> Bool {
        if let programmer = other as? Programmer {
            guard self.favoriteIDE == programmer.favoriteIDE else { return false }
        }

        return super.isEqual(other)
    }

    override class var testValue: Programmer {
        return Programmer(name: "Johnny Appleseed", email: "appleseed@apple.com", favoriteIDE: "XCode")
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

fileprivate struct EncodeFailure : Encodable {
    enum Failure: Error {
        case Failure
    }

    var someValue: Double
    func encode(to encoder: Encoder) throws {
        throw Failure.Failure
    }
}

fileprivate struct EncodeFailureNested : Encodable {
    var nestedValue: EncodeFailure
}

private struct DecodeFailure : Decodable {
    var intValue: Int
}

private struct DecodeFailureNested : Decodable {
    var nestedValue: DecodeFailure
}

// MARK: - Run the the tests!
var XPCEncoderTests = TestSuite("TestXPCEncoder")
XPCEncoderTests.test("testEncodingTopLevelEmptyStruct") { TestXPCEncoder().testEncodingTopLevelEmptyStruct() }
XPCEncoderTests.test("testEncodingTopLevelEmptyClass") { TestXPCEncoder().testEncodingTopLevelEmptyClass() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueEnum") { TestXPCEncoder().testEncodingTopLevelSingleValueEnum() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueStruct") { TestXPCEncoder().testEncodingTopLevelSingleValueStruct() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueClass") { TestXPCEncoder().testEncodingTopLevelSingleValueClass() }
XPCEncoderTests.test("testEncodingTopLevelStructuredStruct") { TestXPCEncoder().testEncodingTopLevelStructuredStruct() }
XPCEncoderTests.test("testEncodingTopLevelStructuredClass") { TestXPCEncoder().testEncodingTopLevelStructuredClass() }
XPCEncoderTests.test("testEncodingTopLevelStructuredSingleStruct") { TestXPCEncoder().testEncodingTopLevelStructuredSingleStruct() }
XPCEncoderTests.test("testEncodingTopLevelStructuredSingleClass") { TestXPCEncoder().testEncodingTopLevelStructuredSingleClass() }
XPCEncoderTests.test("testEncodingDerivedClass") { TestXPCEncoder().testEncodingDerivedClass() }
XPCEncoderTests.test("testEncodingClassWhichSharesEncoderWithSuper") { TestXPCEncoder().testEncodingClassWhichSharesEncoderWithSuper() }
XPCEncoderTests.test("testEncodingTopLevelDeepStructuredType") { TestXPCEncoder().testEncodingTopLevelDeepStructuredType() }
XPCEncoderTests.test("testEncodingTopLevelNullableType") { TestXPCEncoder().testEncodingTopLevelNullableType() }
XPCEncoderTests.test("testEncodingDictionaryFailureKeyPath") { TestXPCEncoder().testEncodingDictionaryFailureKeyPath() }
XPCEncoderTests.test("testEncodingDictionaryFailureKeyPathNested") { TestXPCEncoder().testEncodingDictionaryFailureKeyPathNested() }
XPCEncoderTests.test("testDecodingDictionaryFailureKeyPath") { TestXPCEncoder().testDecodingDictionaryFailureKeyPath() }
XPCEncoderTests.test("testDecodingDictionaryFailureKeyPathNested") { TestXPCEncoder().testDecodingDictionaryFailureKeyPathNested() }
runAllTests()
