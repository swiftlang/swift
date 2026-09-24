// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default)
// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -O)

// REQUIRES: executable_test

import Cxx
import StdlibUnittest

var CxxExceptionTests = TestSuite("CxxException")

func requireSendable<T: Sendable>(_: T) {}

CxxExceptionTests.test("Message") {
  let exception = CxxException(message: "failure")
  requireSendable(exception)
  expectEqual("failure", exception.message)
}

CxxExceptionTests.test("NoException") {
  do {
    let result = try _withCxxExceptionCapture { _, _ in 42 }
    expectEqual(42, result)
    try _withCxxExceptionCapture { _, _ in }
  } catch {
    expectUnreachable("unexpected exception")
  }
}

CxxExceptionTests.test("CopiesMessageBeforeCallbackReturns") {
  do {
    try _withCxxExceptionCapture { context, callback in
      var bytes: [CChar] = [104, 101, 108, 108, 111, 0]
      bytes.withUnsafeBufferPointer { buffer in
        callback(context, buffer.baseAddress)
      }
      bytes = [0]
    }
    expectUnreachable("expected the captured exception")
  } catch let exception as CxxException {
    expectEqual("hello", exception.message)
  } catch {
    expectUnreachable("unexpected error type")
  }
}

CxxExceptionTests.test("RepairsInvalidUTF8") {
  do {
    try _withCxxExceptionCapture { context, callback in
      let bytes: [CChar] = [67, -61, 0]
      bytes.withUnsafeBufferPointer { buffer in
        callback(context, buffer.baseAddress)
      }
    }
    expectUnreachable("expected the captured exception")
  } catch let exception as CxxException {
    expectEqual("C\u{FFFD}", exception.message)
  } catch {
    expectUnreachable("unexpected error type")
  }
}

CxxExceptionTests.test("UnknownException") {
  do {
    try _withCxxExceptionCapture { context, callback in
      callback(context, nil)
    }
    expectUnreachable("expected the captured exception")
  } catch let exception as CxxException {
    expectEqual("Unknown C++ exception", exception.message)
  } catch {
    expectUnreachable("unexpected error type")
  }
}

CxxExceptionTests.test("DestroysUnusedResultAfterException") {
  final class Result {}
  weak var weakResult: Result?
  do {
    let _ = try _withCxxExceptionCapture { context, callback in
      let result = Result()
      weakResult = result
      "message".withCString { message in
        callback(context, message)
      }
      return result
    }
    expectUnreachable("expected the captured exception")
  } catch let exception as CxxException {
    expectEqual("message", exception.message)
  } catch {
    expectUnreachable("unexpected error type")
  }
  expectNil(weakResult)
}

CxxExceptionTests.test("NestedCaptures") {
  do {
    let result = try _withCxxExceptionCapture { outerContext, outerCallback in
      do {
        try _withCxxExceptionCapture { innerContext, innerCallback in
          "inner".withCString { message in
            innerCallback(innerContext, message)
          }
        }
        expectUnreachable("expected the inner exception")
      } catch let exception as CxxException {
        expectEqual("inner", exception.message)
      } catch {
        expectUnreachable("unexpected error type")
      }
      "outer".withCString { message in
        outerCallback(outerContext, message)
      }
      return 1
    }
    expectUnreachable("expected the outer exception, received \(result)")
  } catch let exception as CxxException {
    expectEqual("outer", exception.message)
  } catch {
    expectUnreachable("unexpected error type")
  }
}

runAllTests()
