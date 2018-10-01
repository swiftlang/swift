// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/errors.h -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: objc_interop

//
// Tests for error handling.
//

import StdlibUnittest

struct Problem : Error {}

class ErrorImpl : NSObject, ErrorTest {
  func succeed() throws -> Any { return self }
  func fail() throws -> Any { throw Problem() }
}


var ErrorHandlingTests = TestSuite("ErrorHandling")

func sameObject(_ x: Any?, _ y: Any?) -> Bool {
  return x.map { $0 as AnyObject } === y.map { $0 as AnyObject }
}

ErrorHandlingTests.test("succeed") {
  let obj = ErrorImpl()
  let result = testSucceed(obj)
  expectTrue(sameObject(obj, result))
}

ErrorHandlingTests.test("succeedIgnoringError") {
  let obj = ErrorImpl()
  let result = testSucceedIgnoringError(obj)
  expectTrue(sameObject(obj, result))
}

ErrorHandlingTests.test("fail") {
  let obj = ErrorImpl()
  let result = testFail(obj)
  expectNil(result)
}

ErrorHandlingTests.test("failIgnoringError") {
  let obj = ErrorImpl()
  let result = testFailIgnoringError(obj)
  expectNil(result)
}

runAllTests()
