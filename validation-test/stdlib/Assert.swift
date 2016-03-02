// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Debug
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Release -O
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Unchecked -Ounchecked
//
// RUN: %target-run %t/Assert_Debug
// RUN: %target-run %t/Assert_Release
// RUN: %target-run %t/Assert_Unchecked
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif


//===---
// Tests.
//===---

func testTrapsAreNoreturn(i: Int) -> Int {
  // Don't need a return statement in 'case' statements because these functions
  // are @noreturn.
  switch i {
  case 2:
    preconditionFailure("cannot happen")
  case 3:
    _preconditionFailure("cannot happen")
  case 4:
    _stdlibAssertionFailure("cannot happen")
  case 5:
    _sanityCheckFailure("cannot happen")

  default:
    return 0
  }
}

var Assert = TestSuite("Assert")

Assert.test("assert")
  .xfail(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "assertions are disabled in Release and Unchecked mode"))
  .crashOutputMatches("this should fail")
  .code {
  var x = 2
  assert(x * 21 == 42, "should not fail")

  expectCrashLater()
  assert(x == 42, "this should fail")
}

Assert.test("assert/StringInterpolation")
  .xfail(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "assertions are disabled in Release and Unchecked mode"))
  .crashOutputMatches("this should fail")
  .code {
  var should = "should"
  var x = 2
  assert(x * 21 == 42, "\(should) not fail")

  expectCrashLater()
  assert(x == 42, "this \(should) fail")
}

Assert.test("assertionFailure")
  .skip(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  expectCrashLater()
  assertionFailure("this should fail")
}

Assert.test("assertionFailure/StringInterpolation")
  .skip(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  var should = "should"
  expectCrashLater()
  assertionFailure("this \(should) fail")
}

Assert.test("precondition")
  .xfail(.custom(
    { _isFastAssertConfiguration() },
    reason: "preconditions are disabled in Unchecked mode"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var x = 2
  precondition(x * 21 == 42, "should not fail")
  expectCrashLater()
  precondition(x == 42, "this should fail")
}

Assert.test("precondition/StringInterpolation")
  .xfail(.custom(
    { _isFastAssertConfiguration() },
    reason: "preconditions are disabled in Unchecked mode"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var should = "should"
  var x = 2
  precondition(x * 21 == 42, "\(should) not fail")
  expectCrashLater()
  precondition(x == 42, "this \(should) fail")
}

Assert.test("preconditionFailure")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  expectCrashLater()
  preconditionFailure("this should fail")
}

Assert.test("preconditionFailure/StringInterpolation")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var should = "should"
  expectCrashLater()
  preconditionFailure("this \(should) fail")
}

Assert.test("fatalError")
  .crashOutputMatches("this should fail")
  .code {
  expectCrashLater()
  fatalError("this should fail")
}

Assert.test("fatalError/StringInterpolation")
  .crashOutputMatches("this should fail")
  .code {
  var should = "should"
  expectCrashLater()
  fatalError("this \(should) fail")
}

Assert.test("_precondition")
  .xfail(.custom(
    { _isFastAssertConfiguration() },
    reason: "preconditions are disabled in Unchecked mode"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var x = 2
  _precondition(x * 21 == 42, "should not fail")
  expectCrashLater()
  _precondition(x == 42, "this should fail")
}

Assert.test("_preconditionFailure")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  expectCrashLater()
  _preconditionFailure("this should fail")
}

Assert.test("_stdlibAssert")
  .xfail(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "debug preconditions are disabled in Release and Unchecked mode"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var x = 2
  _stdlibAssert(x * 21 == 42, "should not fail")
  expectCrashLater()
  _stdlibAssert(x == 42, "this should fail")
}

Assert.test("_stdlibAssertionFailure")
  .skip(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  expectCrashLater()
  _stdlibAssertionFailure("this should fail")
}

Assert.test("_sanityCheck")
  .xfail(.custom(
    { !_isStdlibInternalChecksEnabled() },
    reason: "sanity checks are disabled in this build of stdlib"))
  .crashOutputMatches("this should fail")
  .code {
  var x = 2
  _sanityCheck(x * 21 == 42, "should not fail")
  expectCrashLater()
  _sanityCheck(x == 42, "this should fail")
}

Assert.test("_sanityCheckFailure")
  .skip(.custom(
    { !_isStdlibInternalChecksEnabled() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  expectCrashLater()
  _sanityCheckFailure("this should fail")
}

runAllTests()

