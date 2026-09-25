// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -I %S/Inputs)
// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -I %S/Inputs -O)

// REQUIRES: executable_test

@_spi(CxxExceptionBridging) import Cxx
import StdlibUnittest

var CxxExceptionTests = TestSuite("CxxException")

func requireSendable<T: Sendable>(_: T) {}

CxxExceptionTests.test("NoException") {
  do throws(CxxException) {
    let result = try _withCxxExceptionCapture { _, _ in 42 }
    expectEqual(42, result)
    try _withCxxExceptionCapture { _, _ in }
  } catch {
    expectUnreachable("unexpected exception: \(error.message)")
  }
}

CxxExceptionTests.test("CopiesMessageBeforeCallbackReturns") {
  do throws(CxxException) {
    try _withCxxExceptionCapture { context, callback in
      var bytes: [CChar] = [104, 101, 108, 108, 111, 0]
      bytes.withUnsafeBufferPointer { buffer in
        callback(context, buffer.baseAddress)
      }
      bytes = [0]
    }
    expectUnreachable("expected the captured exception")
  } catch {
    requireSendable(error)
    expectEqual("hello", error.message)
  }
}

CxxExceptionTests.test("RepairsInvalidUTF8") {
  do throws(CxxException) {
    try _withCxxExceptionCapture { context, callback in
      let bytes: [CChar] = [67, -61, 0]
      bytes.withUnsafeBufferPointer { buffer in
        callback(context, buffer.baseAddress)
      }
    }
    expectUnreachable("expected the captured exception")
  } catch {
    expectEqual("C\u{FFFD}", error.message)
  }
}

CxxExceptionTests.test("UnknownException") {
  do throws(CxxException) {
    try _withCxxExceptionCapture { context, callback in
      callback(context, nil)
    }
    expectUnreachable("expected the captured exception")
  } catch {
    expectEqual("Unknown C++ exception", error.message)
  }
}

CxxExceptionTests.test("DestroysUnusedResultAfterException") {
  final class Result {}
  weak var weakResult: Result?
  do throws(CxxException) {
    let _ = try _withCxxExceptionCapture { context, callback in
      let result = Result()
      weakResult = result
      "message".withCString { message in
        callback(context, message)
      }
      return result
    }
    expectUnreachable("expected the captured exception")
  } catch {
    expectEqual("message", error.message)
  }
  expectNil(weakResult)
}

CxxExceptionTests.test("NestedCaptures") {
  do throws(CxxException) {
    let result = try _withCxxExceptionCapture { outerContext, outerCallback in
      do throws(CxxException) {
        try _withCxxExceptionCapture { innerContext, innerCallback in
          "inner".withCString { message in
            innerCallback(innerContext, message)
          }
        }
        expectUnreachable("expected the inner exception")
      } catch {
        expectEqual("inner", error.message)
      }
      "outer".withCString { message in
        outerCallback(outerContext, message)
      }
      return 1
    }
    expectUnreachable("expected the outer exception, received \(result)")
  } catch {
    expectEqual("outer", error.message)
  }
}

// The support module is only available with the libc++abi and libstdc++
// runtimes. Android is not tested yet.
#if canImport(Darwin) || os(Linux)
import ExceptionSupportReporter

// These cases import _SwiftCxxExceptionSupport through ClangImporter and report
// exceptions from a real C++ catch handler.
CxxExceptionTests.test("ReportsStdException") {
  do throws(CxxException) {
    let result = try _withCxxExceptionCapture { context, callback in
      catchAndReport(1, context, callback)
    }
    expectUnreachable("expected the captured exception, received \(result)")
  } catch {
    expectEqual("runtime error", error.message)
  }
}

CxxExceptionTests.test("ReportsUnknownException") {
  do throws(CxxException) {
    let result = try _withCxxExceptionCapture { context, callback in
      catchAndReport(2, context, callback)
    }
    expectUnreachable("expected the captured exception, received \(result)")
  } catch {
    expectEqual("Unknown C++ exception", error.message)
  }
}

CxxExceptionTests.test("ReportsNothingWithoutException") {
  do throws(CxxException) {
    let result = try _withCxxExceptionCapture { context, callback in
      catchAndReport(0, context, callback)
    }
    expectEqual(7, result)
  } catch {
    expectUnreachable("unexpected exception: \(error.message)")
  }
}
#endif

runAllTests()
