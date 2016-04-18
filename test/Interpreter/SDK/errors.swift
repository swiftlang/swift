// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/errors.h -o %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: objc_interop

//
// Tests for error handling.
//

import StdlibUnittest

struct Problem : ErrorProtocol {}

class ErrorImpl : NSObject, ErrorTest {
  func succeed() throws -> AnyObject { return self }
  func fail() throws -> AnyObject { throw Problem() }
}


var ErrorHandlingTests = TestSuite("ErrorHandling")

ErrorHandlingTests.test("succeed") {
  let obj = ErrorImpl()
  let result = testSucceed(obj)
  expectTrue(obj === result)
}

ErrorHandlingTests.test("succeedIgnoringError") {
  let obj = ErrorImpl()
  let result = testSucceedIgnoringError(obj)
  expectTrue(obj === result)
}

ErrorHandlingTests.test("fail") {
  let obj = ErrorImpl()
  let result = testFail(obj)
  expectEmpty(result)
}

ErrorHandlingTests.test("failIgnoringError") {
  let obj = ErrorImpl()
  let result = testFailIgnoringError(obj)
  expectEmpty(result)
}

runAllTests()
