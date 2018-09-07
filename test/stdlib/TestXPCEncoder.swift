
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

// MARK: - Run the the tests!
var XPCEncoderTests = TestSuite("TestXPCEncoder")
XPCEncoderTests.test("testEncodingTopLevelEmptyStruct") { TestXPCEncoder().testEncodingTopLevelEmptyStruct() }
XPCEncoderTests.test("testEncodingTopLevelEmptyClass") { TestXPCEncoder().testEncodingTopLevelEmptyClass() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueEnum") { TestXPCEncoder().testEncodingTopLevelSingleValueEnum() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueStruct") { TestXPCEncoder().testEncodingTopLevelSingleValueStruct() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueClass") { TestXPCEncoder().testEncodingTopLevelSingleValueClass() }
XPCEncoderTests.test("testEncodingTopLevelStructuredStruct") { TestXPCEncoder().testEncodingTopLevelStructuredStruct() }
XPCEncoderTests.test("testEncodingTopLevelStructuredClass") { TestXPCEncoder().testEncodingTopLevelStructuredClass() }
runAllTests()
