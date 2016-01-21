// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

// Currently it fails because a dylib cannot be found.
// TODO: Re-enable this test when rdar://problem/24222804 is fixed
// REQUIRES: FIXME

// watchOS 2.0 does not have a public XCTest module.
// XFAIL: OS=watchos

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

import XCTest

var XCTestTestSuite = TestSuite("XCTest")

// NOTE: When instantiating test cases for a particular test method using the
//       XCTestCase(selector:) initializer, those test methods must be marked
//       as dynamic. Objective-C XCTest uses runtime introspection to
//       instantiate an NSInvocation with the given selector.

XCTestTestSuite.test("exceptions") {
  class ExceptionTestCase: XCTestCase {
    dynamic func test_raises() {
      NSException(name: "XCTestTestSuiteException", reason: nil, userInfo: nil).raise()
    }
  }

  let testCase = ExceptionTestCase(selector: "test_raises")
  testCase.runTest()
  let testRun = testCase.testRun!

  expectEqual(1, testRun.testCaseCount)
  expectEqual(1, testRun.executionCount)
  expectEqual(0, testRun.failureCount)
  expectEqual(1, testRun.unexpectedExceptionCount)
  expectEqual(1, testRun.totalFailureCount)
  expectFalse(testRun.hasSucceeded)
}

XCTestTestSuite.test("XCTAssertEqual/Array<T>") {
  class AssertEqualArrayTestCase: XCTestCase {
    dynamic func test_whenArraysAreEqual_passes() {
      XCTAssertEqual(["foo", "bar", "baz"],
                     ["foo", "bar", "baz"])
    }

    dynamic func test_whenArraysAreNotEqual_fails() {
      XCTAssertEqual(["foo", "baz", "bar"],
                     ["foo", "bar", "baz"])
    }
  }

  let passingTestCase = AssertEqualArrayTestCase(selector: "test_whenArraysAreEqual_passes")
  passingTestCase.runTest()
  let passingTestRun = passingTestCase.testRun!
  expectEqual(1, passingTestRun.testCaseCount)
  expectEqual(1, passingTestRun.executionCount)
  expectEqual(0, passingTestRun.failureCount)
  expectEqual(0, passingTestRun.unexpectedExceptionCount)
  expectEqual(0, passingTestRun.totalFailureCount)
  expectTrue(passingTestRun.hasSucceeded)

  let failingTestCase = AssertEqualArrayTestCase(selector: "test_whenArraysAreNotEqual_fails")
  failingTestCase.runTest()
  let failingTestRun = failingTestCase.testRun!
  expectEqual(1, failingTestRun.testCaseCount)
  expectEqual(1, failingTestRun.executionCount)
  expectEqual(1, failingTestRun.failureCount)
  expectEqual(0, failingTestRun.unexpectedExceptionCount)
  expectEqual(1, failingTestRun.totalFailureCount)
  expectFalse(failingTestRun.hasSucceeded)
}

XCTestTestSuite.test("XCTAssertEqual/Dictionary<T, U>") {
  class AssertEqualDictionaryTestCase: XCTestCase {
    dynamic func test_whenDictionariesAreEqual_passes() {
      XCTAssertEqual(["foo": "bar", "baz": "flim"],
                     ["baz": "flim", "foo": "bar"])
    }

    dynamic func test_whenDictionariesAreNotEqual_fails() {
      XCTAssertEqual(["foo": ["bar": "baz"]],
                     ["foo": ["bar": "flim"]])
    }
  }

  let passingTestCase = AssertEqualDictionaryTestCase(selector: "test_whenDictionariesAreEqual_passes")
  passingTestCase.runTest()
  let passingTestRun = passingTestCase.testRun!
  expectEqual(1, passingTestRun.testCaseCount)
  expectEqual(1, passingTestRun.executionCount)
  expectEqual(0, passingTestRun.failureCount)
  expectEqual(0, passingTestRun.unexpectedExceptionCount)
  expectEqual(0, passingTestRun.totalFailureCount)
  expectTrue(passingTestRun.hasSucceeded)

  let failingTestCase = AssertEqualDictionaryTestCase(selector: "test_whenDictionariesAreNotEqual_fails")
  failingTestCase.runTest()
  let failingTestRun = failingTestCase.testRun!
  expectEqual(1, failingTestRun.testCaseCount)
  expectEqual(1, failingTestRun.executionCount)
  expectEqual(1, failingTestRun.failureCount)
  expectEqual(0, failingTestRun.unexpectedExceptionCount)
  expectEqual(1, failingTestRun.totalFailureCount)
  expectFalse(failingTestRun.hasSucceeded)
}

runAllTests()

