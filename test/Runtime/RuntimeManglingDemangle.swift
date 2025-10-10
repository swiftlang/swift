// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu

import Swift
import Runtime
import StdlibUnittest

var DemangleTests = TestSuite("Demangle")

if #available(SwiftStdlib 6.3, *) {
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

  DemangleTests.test("Span API") {
    let buffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 140)
    var outputSpan = buffer.mutableSpan

    defer {
      buffer.deallocate()
    }

    buffer[0] = 0 // Ensure that when we do String(cString: ptr) it halts at first byte.
    let ptr = buffer.baseAddress!

    // First, test that the buffer is still empty after failed demanglings.
    var res = demangle("abc123".utf8Span.span, into: &outputSpan)
    expectEqual(res, .invalidSymbol)
    expectEqual(String(cString: ptr), "")

    res = demangle("Si".utf8Span.span, into: &outputSpan)
    expectEqual(res, .invalidSymbol)
    expectEqual(String(cString: ptr), "")

    res = demangle("Swift is super cool!".utf8Span.span, into: &outputSpan)
    expectEqual(res, .invalidSymbol)
    expectEqual(String(cString: ptr), "")

    // Test that correct symbols are demangled. (Test all documented prefixes)
    res = demangle("_TSb".utf8Span.span, into: &outputSpan)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Swift.Bool")

    res = demangle("_T0Si".utf8Span.span, into: &outputSpan)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Swift.Int")

    res = demangle("$SSdXSaXSq".utf8Span.span, into: &outputSpan)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "[Swift.Double]?")

    res = demangle("_$S8Demangle4main4argc4argvs5Int32VAF_SpySpys4Int8VGSgGtF".utf8Span.span, into: &outputSpan)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Demangle.main(argc: Swift.Int32, argv: Swift.UnsafeMutablePointer<Swift.Optional<Swift.UnsafeMutablePointer<Swift.Int8>>>) -> Swift.Int32")

    res = demangle("$sSG".utf8Span.span, into: &outputSpan)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Swift.RandomNumberGenerator")

    res = demangle("_$sSS7cStringSSSPys4Int8VG_tcfC".utf8Span.span, into: &outputSpan)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Swift.String.init(cString: Swift.UnsafePointer<Swift.Int8>) -> Swift.String")
  }

  DemangleTests.test("buffer API") {
    let buffer = UnsafeMutableBufferPointer<Int8>.allocate(capacity: 140)

    defer {
      buffer.deallocate()
    }

    buffer[0] = 0 // Ensure that when we do String(cString: ptr) it halts at first byte.
    let ptr = buffer.baseAddress!

    // First, test that the buffer is still empty after failed demanglings.
    var res = demangle("abc123", into: buffer)
    expectEqual(res, .invalidSymbol)
    expectEqual(String(cString: ptr), "")

    res = demangle("Si", into: buffer)
    expectEqual(res, .invalidSymbol)
    expectEqual(String(cString: ptr), "")

    res = demangle("Swift is super cool!", into: buffer)
    expectEqual(res, .invalidSymbol)
    expectEqual(String(cString: ptr), "")

    // Test that correct symbols are demangled. (Test all documented prefixes)
    res = demangle("_TSb", into: buffer)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Swift.Bool")

    res = demangle("_T0Si", into: buffer)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Swift.Int")

    res = demangle("$SSdXSaXSq", into: buffer)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "[Swift.Double]?")

    res = demangle("_$S8Demangle4main4argc4argvs5Int32VAF_SpySpys4Int8VGSgGtF", into: buffer)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Demangle.main(argc: Swift.Int32, argv: Swift.UnsafeMutablePointer<Swift.Optional<Swift.UnsafeMutablePointer<Swift.Int8>>>) -> Swift.Int32")

    res = demangle("$sSG", into: buffer)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Swift.RandomNumberGenerator")

    res = demangle("_$sSS7cStringSSSPys4Int8VG_tcfC", into: buffer)
    print("res = \(String(cString: ptr)) len:\(String(cString: ptr).count)")
    expectEqual(res, .success)
    expectEqual(String(cString: ptr), "Swift.String.init(cString: Swift.UnsafePointer<Swift.Int8>) -> Swift.String")
  }

  DemangleTests.test("buffer API - too small buffer") {
    // Test the return of demangle into with a smaller buffer.
    // Swift.Int requires 10 bytes, give this 9
    let smolBuffer = UnsafeMutableBufferPointer<Int8>.allocate(capacity: 9)
    defer { smolBuffer.deallocate() }

    let smolPtr = smolBuffer.baseAddress!

    let fail = demangle("$sSi", into: smolBuffer)
    expectEqual(fail, .truncated(10))
    expectEqual(String(cString: smolPtr), "Swift.In")
  }

  DemangleTests.test("buffer API - small buffer, success") {
    let smolBuffer = UnsafeMutableBufferPointer<Int8>.allocate(capacity: 9)
    defer { smolBuffer.deallocate() }

    let smolPtr = smolBuffer.baseAddress!
    // Test nil return on successful demangle.
    let success = demangle("$s4Smol3IntV", into: smolBuffer)
    expectEqual(success, .success)
    expectEqual(String(cString: smolPtr), "Smol.Int")
  }
}

runAllTests()