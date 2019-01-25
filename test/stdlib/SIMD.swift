// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

let SIMDCodableTests = TestSuite("SIMDCodable")

func testRoundTrip<T>(_ for: T.Type)
where T : SIMD, T.Scalar : FixedWidthInteger {
  let input = T.random(in: T.Scalar.min ... T.Scalar.max)
  let encoder = JSONEncoder()
  let decoder = JSONDecoder()
  do {
    let data = try encoder.encode(input)
    let output = try decoder.decode(T.self, from: data)
    assertEqual(input, output)
  }
  catch {
    expectUnreachableCatch(error)
  }
}

func testRoundTrip<T>(_ for: T.Type)
where T : SIMD, T.Scalar : BinaryFloatingPoint {
  let input = T.random(in: -16 ... (16 as T))
  let encoder = JSONEncoder()
  let decoder = JSONDecoder()
  do {
    let data = try encoder.encode(input)
    let output = try decoder.decode(T.self, from: data)
    assertEqual(input, output)
  }
  catch {
    expectUnreachableCatch(error)
  }
}

// Very basic round-trip test. We can be much more sophisticated in the future,
// but we want to at least exercise the API. Also need to add some negative
// tests for the error paths, and test a more substantial set of types.
SIMDCodableTests.test("roundTrip") {
  testRoundTrip(SIMD2<Int8>.self)
  testRoundTrip(SIMD3<Int8>.self)
  testRoundTrip(SIMD4<Int8>.self)
  testRoundTrip(SIMD2<UInt8>.self)
  testRoundTrip(SIMD3<UInt8>.self)
  testRoundTrip(SIMD4<UInt8>.self)
  testRoundTrip(SIMD2<Int32>.self)
  testRoundTrip(SIMD3<Int32>.self)
  testRoundTrip(SIMD4<Int32>.self)
  testRoundTrip(SIMD2<UInt32>.self)
  testRoundTrip(SIMD3<UInt32>.self)
  testRoundTrip(SIMD4<UInt32>.self)
  testRoundTrip(SIMD2<Int>.self)
  testRoundTrip(SIMD3<Int>.self)
  testRoundTrip(SIMD4<Int>.self)
  testRoundTrip(SIMD2<UInt>.self)
  testRoundTrip(SIMD3<UInt>.self)
  testRoundTrip(SIMD4<UInt>.self)
  testRoundTrip(SIMD2<Float>.self)
  testRoundTrip(SIMD3<Float>.self)
  testRoundTrip(SIMD4<Float>.self)
  testRoundTrip(SIMD2<Double>.self)
  testRoundTrip(SIMD3<Double>.self)
  testRoundTrip(SIMD4<Double>.self)
}

runAllTests()
