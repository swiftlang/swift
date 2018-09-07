
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

// MARK: - Run the the tests!
var XPCEncoderTests = TestSuite("TestXPCEncoder")
XPCEncoderTests.test("testEncodingTopLevelEmptyStruct") { TestXPCEncoder().testEncodingTopLevelEmptyStruct() }
XPCEncoderTests.test("testEncodingTopLevelEmptyClass") { TestXPCEncoder().testEncodingTopLevelEmptyClass() }
runAllTests()
