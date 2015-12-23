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
    requirementFailure("cannot happen")
  case 3:
    _requirementFailure("cannot happen")
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
  .xfail(.Custom(
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
  .xfail(.Custom(
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
  .skip(.Custom(
    { !_isDebugAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  expectCrashLater()
  assertionFailure("this should fail")
}

Assert.test("assertionFailure/StringInterpolation")
  .skip(.Custom(
    { !_isDebugAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  var should = "should"
  expectCrashLater()
  assertionFailure("this \(should) fail")
}

Assert.test("require")
  .xfail(.Custom(
    { _isFastAssertConfiguration() },
    reason: "preconditions are disabled in Unchecked mode"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var x = 2
  require(x * 21 == 42, "should not fail")
  expectCrashLater()
  require(x == 42, "this should fail")
}

Assert.test("require/StringInterpolation")
  .xfail(.Custom(
    { _isFastAssertConfiguration() },
    reason: "preconditions are disabled in Unchecked mode"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var should = "should"
  var x = 2
  require(x * 21 == 42, "\(should) not fail")
  expectCrashLater()
  require(x == 42, "this \(should) fail")
}

Assert.test("requirementFailure")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  expectCrashLater()
  requirementFailure("this should fail")
}

Assert.test("requirementFailure/StringInterpolation")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var should = "should"
  expectCrashLater()
  requirementFailure("this \(should) fail")
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

Assert.test("_require")
  .xfail(.Custom(
    { _isFastAssertConfiguration() },
    reason: "preconditions are disabled in Unchecked mode"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var x = 2
  _require(x * 21 == 42, "should not fail")
  expectCrashLater()
  _require(x == 42, "this should fail")
}

Assert.test("_requirementFailure")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  expectCrashLater()
  _requirementFailure("this should fail")
}

Assert.test("_stdlibAssert")
  .xfail(.Custom(
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
  .skip(.Custom(
    { !_isDebugAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  expectCrashLater()
  _stdlibAssertionFailure("this should fail")
}

Assert.test("_sanityCheck")
  .xfail(.Custom(
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
  .skip(.Custom(
    { !_isStdlibInternalChecksEnabled() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  expectCrashLater()
  _sanityCheckFailure("this should fail")
}

runAllTests()

