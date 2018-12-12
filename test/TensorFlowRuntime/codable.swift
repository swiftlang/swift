// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// REQUIRES: objc_interop

import TensorFlow
import Foundation
import TensorFlowUnittest
import StdlibUnittest

var CodableTests = TestSuite("TensorFlowCodable")

// `testRoundTrip` adapted from test/stdlib/TestJSONEncoder.swift.
private func _testRoundTrip<T>(
  of value: T, expectedJSON json: Data? = nil
) where T : Codable, T : Equatable {
  var payload: Data! = nil
  do {
    let encoder = JSONEncoder()
    payload = try encoder.encode(value)
  } catch {
    expectUnreachable("Failed to encode \(T.self) to JSON: \(error)")
  }

  if let expectedJSON = json {
    expectEqual(expectedJSON, payload, "Produced JSON not identical to expected JSON.")
  }

  do {
    let decoder = JSONDecoder()
    let decoded = try decoder.decode(T.self, from: payload)

    // NOTE: `expectEqual` cannot be called with `Tensor` arguments because it
    // is not @inlinable and defined in another module. Calling it with
    // `Tensor` arguments results in a crash:
    // !!! Compiler bug -- Tensor op builtin __tfop_Equal,$in,$in,T cannot be lowered to LLVM IR !!!
    // This bug is a challenge for graph program extraction and will surface
    // in cross-module code (when an inlinable `Tensor` op is called from an
    // opaque, non-inlinable context).
    // expectEqual(decoded, value, "\(T.self) did not round-trip to an equal value.")

    expectTrue(decoded == value, "\(T.self) did not round-trip to an equal value.")
  } catch {
    expectUnreachable("Failed to decode \(T.self) from JSON: \(error)")
  }
}

CodableTests.testAllBackends("Tensor") {
  let tensor = Tensor<Int32>(shape: [2, 3], scalars: Array(1...6))
  let expectedJSON = "{\"shape\":[2,3],\"scalars\":[1,2,3,4,5,6]}".data(using: .utf8)!
  _testRoundTrip(of: tensor, expectedJSON: expectedJSON)
}

CodableTests.testAllBackends("ShapedArray") {
  let array = ShapedArray(shape: [2, 3], scalars: Array(1...6))
  let expectedJSON = "{\"shape\":[2,3],\"scalars\":[1,2,3,4,5,6]}".data(using: .utf8)!
  _testRoundTrip(of: array, expectedJSON: expectedJSON)
}

CodableTests.testAllBackends("TensorShape") {
  let shape: TensorShape = [2, 3, 4]
  _testRoundTrip(of: shape)
}

runAllTests()
