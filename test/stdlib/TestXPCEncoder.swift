
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
        expectEqual(true, false, "Test")
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

// MARK: - Run the the tests!
var XPCEncoderTests = TestSuite("TestXPCEncoder")
XPCEncoderTests.test("testEncodingTopLevelEmptyStruct") { TestXPCEncoder().testEncodingTopLevelEmptyStruct() }
XPCEncoderTests.test("testEncodingTopLevelEmptyClass") { TestXPCEncoder().testEncodingTopLevelEmptyClass() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueEnum") { TestXPCEncoder().testEncodingTopLevelSingleValueEnum() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueStruct") { TestXPCEncoder().testEncodingTopLevelSingleValueStruct() }
XPCEncoderTests.test("testEncodingTopLevelSingleValueClass") { TestXPCEncoder().testEncodingTopLevelSingleValueClass() }
runAllTests()
