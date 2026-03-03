// RUN: %target-run-simple-swift


// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: runtime_module

import Swift
import Runtime
import StdlibUnittest

var DemangleTests = TestSuite("Demangle")

if #available(SwiftStdlib 6.3, *) {
  DemangleTests.test("basic string return API") {
    // First, test that we get back 'nil' with invalid input.
    expectEqual(try? demangle("abc123"), nil)
    expectEqual(try? demangle("Si"), nil)
    expectEqual(try? demangle("Swift is super cool!"), nil)

    // Test that correct symbols are demangled. (Test all documented prefixes)
    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) // legacy mangling is not supported on Linux
    expectEqual(try? demangle("_TSb"), "Swift.Bool")
    expectEqual(try? demangle("_T0Si"), "Swift.Int")
    #endif
    expectEqual(try? demangle("$SSdXSaXSq"), "[Swift.Double]?")
    expectEqual(try? demangle("_$S8Demangle4main4argc4argvs5Int32VAF_SpySpys4Int8VGSgGtF"), "Demangle.main(argc: Swift.Int32, argv: Swift.UnsafeMutablePointer<Swift.Optional<Swift.UnsafeMutablePointer<Swift.Int8>>>) -> Swift.Int32")
    expectEqual(try? demangle("$sSG"), "Swift.RandomNumberGenerator")
    expectEqual(try? demangle("_$sSS7cStringSSSPys4Int8VG_tcfC"), "Swift.String.init(cString: Swift.UnsafePointer<Swift.Int8>) -> Swift.String")
  }

  func test(body: (inout OutputSpan<UTF8.CodeUnit>) -> ()) {
    let buffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 140)
    var outputSpan = OutputSpan<UTF8.CodeUnit>(buffer: buffer, initializedCount: 0)

    defer { buffer.deallocate() }

    body(&outputSpan)
  }

  DemangleTests.test("Span API") {
    // First, test that the buffer is still empty after failed demanglings.
    test { outputSpan in
      do {
        try demangle("abc123".utf8Span, into: &outputSpan)
        expectTrue(false, "Expected demangle to throw")
      } catch DemanglingError.invalidSymbol {
        expectEqual(outputSpan.count, 0)
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }

    test { outputSpan in
      do {
        try demangle("Si".utf8Span, into: &outputSpan)
        expectTrue(false, "Expected demangle to throw")
      } catch DemanglingError.invalidSymbol {
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }

    test { outputSpan in
      do {
        try demangle("Swift is super cool!".utf8Span, into: &outputSpan)
        expectTrue(false, "Expected demangle to throw")
      } catch DemanglingError.invalidSymbol {
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }

    // Test that correct symbols are demangled. (Test all documented prefixes)
    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) // legacy mangling is not supported on Linux
    test { outputSpan in
      do {
        try demangle("_TSb".utf8Span, into: &outputSpan)
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "Swift.Bool")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }
    #endif

    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) // legacy mangling is not supported on Linux
    test { outputSpan in
      do {
        try demangle("_T0Si".utf8Span, into: &outputSpan)
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "Swift.Int")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }
    #endif

    test { outputSpan in
      do {
        try demangle("$SSdXSaXSq".utf8Span, into: &outputSpan)
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "[Swift.Double]?")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }

    test { outputSpan in
      do {
        try demangle("_$S8Demangle4main4argc4argvs5Int32VAF_SpySpys4Int8VGSgGtF".utf8Span, into: &outputSpan)
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "Demangle.main(argc: Swift.Int32, argv: Swift.UnsafeMutablePointer<Swift.Optional<Swift.UnsafeMutablePointer<Swift.Int8>>>) -> Swift.Int32")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }

    test { outputSpan in
      do {
        try demangle("$sSG".utf8Span, into: &outputSpan)
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "Swift.RandomNumberGenerator")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }

    test { outputSpan in
      do {
        try demangle("_$sSS7cStringSSSPys4Int8VG_tcfC".utf8Span, into: &outputSpan)
        let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
        expectEqual(demangled, "Swift.String.init(cString: Swift.UnsafePointer<Swift.Int8>) -> Swift.String")
      } catch {
        expectTrue(false, "Got unexpected error: \(error)")
      }
    }
  }

  DemangleTests.test("Span API - too small buffer") {
    // Test the return of demangle into with a smaller buffer.
    // Swift.Int requires 9 bytes, give this 8
    let smolBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 8)
    var smolOutputSpan = OutputSpan<UTF8.CodeUnit>(buffer: smolBuffer, initializedCount: 0)
    defer { smolBuffer.deallocate() }

    do {
      try demangle("$sSi".utf8Span, into: &smolOutputSpan)
      expectTrue(false, "Expected demangle to throw")
    } catch DemanglingError.truncated(let requiredBufferSize) {
      expectEqual(requiredBufferSize, 9)
      expectEqual(String(copying: try! UTF8Span(validating: smolOutputSpan.span)), "Swift.In")
    } catch {
      expectTrue(false, "Got unexpected error: \(error)")
    }
  }

  DemangleTests.test("Span API - small buffer, success") {
    let smolBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 9)
    var smolOutputSpan = OutputSpan<UTF8.CodeUnit>(buffer: smolBuffer, initializedCount: 0)
    defer { smolBuffer.deallocate() }

    do {
      try demangle("$s4Smol3IntV".utf8Span, into: &smolOutputSpan)
      expectEqual(String(copying: try! UTF8Span(validating: smolOutputSpan.span)), "Smol.Int")
    } catch {
      expectTrue(false, "Got unexpected error: \(error)")
    }
  }

  DemangleTests.test("Span API - don't cut off an UTF8 codepoint in the middle (length 3)") {
    let smolBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 3)
    var smolOutputSpan = OutputSpan<UTF8.CodeUnit>(buffer: smolBuffer, initializedCount: 0)
    defer { smolBuffer.deallocate() }

    // Test nil return on successful demangle.
    do {
      try demangle("$s4ąą3IntV".utf8Span, into: &smolOutputSpan)
      expectTrue(false, "Expected demangle to throw")
    } catch DemanglingError.truncated(let requiredBufferSize) {
      expectEqual(requiredBufferSize, 8)
      // ą is two bytes, so the 3 bytes won't wit the "ąą" but only "ą" and half of the "ą"
      // therefore, the output must properly truncate to just a single "ą"
      //
      // 2 bytes - "ą"
      expectEqual(String(copying: try! UTF8Span(validating: smolOutputSpan.span)), "ą")
    } catch {
      expectTrue(false, "Got unexpected error: \(error)")
    }
  }

  DemangleTests.test("Span API - don't cut off an UTF8 codepoint in the middle (length 4)") {
    let smolBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 4)
    var smolOutputSpan = OutputSpan<UTF8.CodeUnit>(buffer: smolBuffer, initializedCount: 0)
    defer { smolBuffer.deallocate() }

    // Test nil return on successful demangle.
    do {
      try demangle("$s4ąą3IntV".utf8Span, into: &smolOutputSpan)
      expectTrue(false, "Expected demangle to throw")
    } catch DemanglingError.truncated(let requiredBufferSize) {
      expectEqual(requiredBufferSize, 8)
      // 2 bytes - "ą"
      // 2 bytes - "ą"
      expectEqual(String(copying: try! UTF8Span(validating: smolOutputSpan.span)), "ąą")
    } catch {
      expectTrue(false, "Got unexpected error: \(error)")
    }
  }

}

runAllTests()
