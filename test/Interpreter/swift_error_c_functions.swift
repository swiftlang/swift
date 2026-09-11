// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/SwiftErrorCFunctions/SwiftErrorCFunctions.m -c -o %t/SwiftErrorCFunctions.o
// RUN: %target-build-swift -I %S/Inputs/SwiftErrorCFunctions/ %t/SwiftErrorCFunctions.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import SwiftErrorCFunctions
import Foundation
import StdlibUnittest

var SwiftErrorCFunctionTests = TestSuite("SwiftErrorCFunctions")

// --- zero_result ---

SwiftErrorCFunctionTests.test("zero_result/success") {
  do {
    try c_error_zero(false)
  } catch {
    expectUnreachableCatch(error)
  }
}

SwiftErrorCFunctionTests.test("zero_result/failure") {
  do {
    try c_error_zero(true)
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("TestDomain", error.domain)
    expectEqual(1, error.code)
  }
}

// --- nonzero_result ---

SwiftErrorCFunctionTests.test("nonzero_result/success") {
  do {
    try c_error_nonzero(0)
  } catch {
    expectUnreachableCatch(error)
  }
}

SwiftErrorCFunctionTests.test("nonzero_result/failure") {
  do {
    try c_error_nonzero(7)
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("TestDomain", error.domain)
    expectEqual(7, error.code)
  }
}

// --- nonnull_error ---

SwiftErrorCFunctionTests.test("nonnull_error/success") {
  do {
    try c_error_nonnull(false)
  } catch {
    expectUnreachableCatch(error)
  }
}

SwiftErrorCFunctionTests.test("nonnull_error/failure") {
  do {
    try c_error_nonnull(true)
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("TestDomain", error.domain)
    expectEqual(3, error.code)
  }
}

// --- null_result (NSError) ---

SwiftErrorCFunctionTests.test("null_result/success") {
  do {
    let ptr = try c_error_null(false)
    expectNotNil(ptr)
  } catch {
    expectUnreachableCatch(error)
  }
}

SwiftErrorCFunctionTests.test("null_result/failure") {
  do {
    _ = try c_error_null(true)
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("TestDomain", error.domain)
    expectEqual(4, error.code)
  }
}

// --- null_result (CFErrorRef) ---

SwiftErrorCFunctionTests.test("cf_null_result/success") {
  do {
    let ptr = try c_error_cf_null(false)
    expectNotNil(ptr)
  } catch {
    expectUnreachableCatch(error)
  }
}

SwiftErrorCFunctionTests.test("cf_null_result/failure") {
  do {
    _ = try c_error_cf_null(true)
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("TestDomain", error.domain)
    expectEqual(5, error.code)
  }
}

// --- nonnull_error (CFErrorRef) ---

SwiftErrorCFunctionTests.test("cf_nonnull_error/success") {
  do {
    try c_error_cf_nonnull(false)
  } catch {
    expectUnreachableCatch(error)
  }
}

SwiftErrorCFunctionTests.test("cf_nonnull_error/failure") {
  do {
    try c_error_cf_nonnull(true)
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("TestDomain", error.domain)
    expectEqual(6, error.code)
  }
}

// --- zero_result with blocks on both sides of error param ---

SwiftErrorCFunctionTests.test("blocks_both_sides/success") {
  var beforeArg: Int32 = 0
  var afterArg: Int32 = 0
  do {
    try c_error_blocks_both_sides(false,
                                  { beforeArg = $0 },
                                  { afterArg = $0 })
    expectEqual(10, beforeArg)
    expectEqual(20, afterArg)
  } catch {
    expectUnreachableCatch(error)
  }
}

SwiftErrorCFunctionTests.test("blocks_both_sides/failure") {
  var beforeArg: Int32 = 0
  var afterArg: Int32 = 0
  do {
    try c_error_blocks_both_sides(true,
                                  { beforeArg = $0 },
                                  { afterArg = $0 })
    expectUnreachable()
  } catch let error as NSError {
    expectEqual("TestDomain", error.domain)
    expectEqual(7, error.code)
    // Both blocks ran before the error was raised.
    expectEqual(10, beforeArg)
    expectEqual(20, afterArg)
  }
}

runAllTests()
