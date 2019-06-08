// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// We don't really want to excerise actual demangling here, but rather just that
// the stdlib demangle function actually works as intended.

import Swift
import StdlibUnittest

var DemangleTests = TestSuite("Demangle")

DemangleTests.test("basic string return API") {
  // First, test that we get back the mangled name with invalid input.
  expectEqual(demangle("abc123"), nil)
  expectEqual(demangle("Si"), nil)
  expectEqual(demangle("Swift is super cool!"), nil)

  // Test that correct symbols are demangled. (Test all documented prefixes)
  expectEqual(demangle("_TSb"), "Swift.Bool")
  expectEqual(demangle("_T0Si"), "Swift.Int")
  expectEqual(demangle("$SSdXSaXSq"), "[Swift.Double]?")
  expectEqual(demangle("_$S8Demangle4main4argc4argvs5Int32VAF_SpySpys4Int8VGSgGtF"), "Demangle.main(argc: Swift.Int32, argv: Swift.UnsafeMutablePointer<Swift.Optional<Swift.UnsafeMutablePointer<Swift.Int8>>>) -> Swift.Int32")
  expectEqual(demangle("$sSG"), "Swift.RandomNumberGenerator")
  expectEqual(demangle("_$sSS7cStringSSSPys4Int8VG_tcfC"), "Swift.String.init(cString: Swift.UnsafePointer<Swift.Int8>) -> Swift.String")
}

DemangleTests.test("buffer API") {
  let buffer = UnsafeMutableBufferPointer<Int8>.allocate(capacity: 140)

  defer { buffer.deallocate() }

  buffer[0] = 0 // Ensure that when we do String(cString: ptr) it halts at first byte.
  let ptr = buffer.baseAddress!

  // First, test that the buffer is still empty after failed demanglings.
  expectEqual(demangle("abc123", into: buffer), .invalidSymbol)
  expectEqual(String(cString: ptr), "")

  expectEqual(demangle("Si", into: buffer), .invalidSymbol)
  expectEqual(String(cString: ptr), "")

  expectEqual(demangle("Swift is super cool!", into: buffer), .invalidSymbol)
  expectEqual(String(cString: ptr), "")

  // Test that correct symbols are demangled. (Test all documented prefixes)
  expectEqual(demangle("_TSb", into: buffer), .success)
  expectEqual(String(cString: ptr), "Swift.Bool")

  expectEqual(demangle("_T0Si", into: buffer), .success)
  expectEqual(String(cString: ptr), "Swift.Int")

  expectEqual(demangle("$SSdXSaXSq", into: buffer), .success)
  expectEqual(String(cString: ptr), "[Swift.Double]?")

  expectEqual(demangle("_$S8Demangle4main4argc4argvs5Int32VAF_SpySpys4Int8VGSgGtF", into: buffer), .success)
  expectEqual(String(cString: ptr), "Demangle.main(argc: Swift.Int32, argv: Swift.UnsafeMutablePointer<Swift.Optional<Swift.UnsafeMutablePointer<Swift.Int8>>>) -> Swift.Int32")

  expectEqual(demangle("$sSG", into: buffer), .success)
  expectEqual(String(cString: ptr), "Swift.RandomNumberGenerator")

  expectEqual(demangle("_$sSS7cStringSSSPys4Int8VG_tcfC", into: buffer), .success)
  expectEqual(String(cString: ptr), "Swift.String.init(cString: Swift.UnsafePointer<Swift.Int8>) -> Swift.String")

  // Test the return of demangle into with a smaller buffer.
  // Swift.Int requires 10 bytes, give this 9
  let smolBuffer = UnsafeMutableBufferPointer<Int8>.allocate(capacity: 9)

  defer { smolBuffer.deallocate() }

  let smolPtr = smolBuffer.baseAddress!

  let fail = demangle("$sSi", into: smolBuffer)
  expectEqual(fail, .truncated(10))
  expectEqual(String(cString: smolPtr), "Swift.In")

  // Test nil return on successful demangle.
  let success = demangle("$s4Smol3IntV", into: smolBuffer)
  expectEqual(success, .success)
  expectEqual(String(cString: smolPtr), "Smol.Int")
}

runAllTests()
