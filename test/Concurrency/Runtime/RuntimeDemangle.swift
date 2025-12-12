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
    // First, test that we get back 'nil' with invalid input.
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

  func test(body: (inout OutputSpan<UTF8.CodeUnit>) -> ()) {
    let buffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 140)
    var outputSpan = OutputSpan<UTF8.CodeUnit>(buffer: buffer, initializedCount: 0)

    defer { buffer.deallocate() }

    body(&outputSpan)
  }

  DemangleTests.test("Span API") {
    // First, test that the buffer is still empty after failed demanglings.
    test { outputSpan in 
      let res = demangle("abc123".utf8Span, into: &outputSpan)
      expectEqual(res, .invalidSymbol)
      expectEqual(outputSpan.count, 0)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      expectEqual(demangled, "")
    }

    test { outputSpan in 
      let res = demangle("Si".utf8Span, into: &outputSpan)
      expectEqual(res, .invalidSymbol)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      expectEqual(demangled, "")
    }

    test { outputSpan in 
      let res = demangle("Swift is super cool!".utf8Span, into: &outputSpan)
      expectEqual(res, .invalidSymbol)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      expectEqual(demangled, "")
    }

    // Test that correct symbols are demangled. (Test all documented prefixes)
    test { outputSpan in 
      let res = demangle("_TSb".utf8Span, into: &outputSpan)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      print("res = \(demangled) len:\(demangled.count)")
      expectEqual(res, .success)
      expectEqual(demangled, "Swift.Bool")
    }

    test { outputSpan in 
      let res = demangle("_T0Si".utf8Span, into: &outputSpan)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      print("res = \(demangled) len:\(demangled.count)")
      expectEqual(res, .success)
      expectEqual(demangled, "Swift.Int")
    }

    test { outputSpan in 
      let res = demangle("$SSdXSaXSq".utf8Span, into: &outputSpan)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      print("res = \(demangled) len:\(demangled.count)")
      expectEqual(res, .success)
      expectEqual(demangled, "[Swift.Double]?")
    }
    
    test { outputSpan in 
      let res = demangle("_$S8Demangle4main4argc4argvs5Int32VAF_SpySpys4Int8VGSgGtF".utf8Span, into: &outputSpan)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      print("res = \(demangled) len:\(demangled.count)")
      expectEqual(res, .success)
      expectEqual(demangled, "Demangle.main(argc: Swift.Int32, argv: Swift.UnsafeMutablePointer<Swift.Optional<Swift.UnsafeMutablePointer<Swift.Int8>>>) -> Swift.Int32")
    }
    
    test { outputSpan in 
      let res = demangle("$sSG".utf8Span, into: &outputSpan)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      print("res = \(demangled) len:\(demangled.count)")
      expectEqual(res, .success)
      expectEqual(demangled, "Swift.RandomNumberGenerator")
    }

    test { outputSpan in 
      let res = demangle("_$sSS7cStringSSSPys4Int8VG_tcfC".utf8Span, into: &outputSpan)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      print("res = \(demangled) len:\(demangled.count)")
      expectEqual(res, .success)
      expectEqual(demangled, "Swift.String.init(cString: Swift.UnsafePointer<Swift.Int8>) -> Swift.String")
    }
  }

  DemangleTests.test("Span API - too small buffer") {
    // Test the return of demangle into with a smaller buffer.
    // Swift.Int requires 9 bytes, give this 8
    let smolBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 8)
    var smolOutputSpan = OutputSpan<UTF8.CodeUnit>(buffer: smolBuffer, initializedCount: 0)
    defer { smolBuffer.deallocate() }

    let fail = demangle("$sSi".utf8Span, into: &smolOutputSpan)
    expectEqual(fail, .truncated(9))
    expectEqual(String(copying: try! UTF8Span(validating: smolOutputSpan.span)), "Swift.In")
  }

  DemangleTests.test("Span API - small buffer, success") {
    let smolBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 9)
    var smolOutputSpan = OutputSpan<UTF8.CodeUnit>(buffer: smolBuffer, initializedCount: 0)
    defer { smolBuffer.deallocate() }

    // Test nil return on successful demangle.
    let success = demangle("$s4Smol3IntV".utf8Span, into: &smolOutputSpan)
    expectEqual(success, .success)
    expectEqual(String(copying: try! UTF8Span(validating: smolOutputSpan.span)), "Smol.Int")
  }

  DemangleTests.test("String API - demangle C++ symbols") {
    expectEqual(demangle("_ZN7MyClass6methodEi"), "MyClass::method(int)")
    expectEqual(demangle("_ZNKSt6vectorIiSaIiEE4sizeEv"), "std::vector<int, std::allocator<int>>::size() const")
  }

  DemangleTests.test("String API - demangle C++ symbols, illegal") {
    expectEqual(demangle("_ZNKSt6vectorIiSaIiEEv"), nil)
  }
  
  DemangleTests.test("Span API - demangle C++ symbols") {
    test { outputSpan in 
      let res = demangle("_ZN7MyClass6methodEi".utf8Span, into: &outputSpan)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      print("res = \(demangled) len:\(demangled.count)")
      expectEqual(res, .success)
      expectEqual(demangled, "MyClass::method(int)")
    }
    test { outputSpan in 
      let res = demangle("_ZNKSt6vectorIiSaIiEE4sizeEv".utf8Span, into: &outputSpan)
      let demangled = String(copying: try! UTF8Span(validating: outputSpan.span))
      print("res = \(demangled) len:\(demangled.count)")
      expectEqual(res, .success)
      expectEqual(demangled, "std::vector<int, std::allocator<int>>::size() const")
    }
  }

  DemangleTests.test("Span API - demangle C++ symbols, too small buffer") {
    let smolBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 12)
    var smolOutputSpan = OutputSpan<UTF8.CodeUnit>(buffer: smolBuffer, initializedCount: 0)
    defer { smolBuffer.deallocate() }

    let fail = demangle("_ZN7MyClass6methodEi".utf8Span, into: &smolOutputSpan)
    expectEqual(fail, .truncated(20))
    expectEqual(String(copying: try! UTF8Span(validating: smolOutputSpan.span)), "MyClass::met")
  }

}

runAllTests()
