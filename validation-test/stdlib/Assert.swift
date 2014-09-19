// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Debug
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Release -O
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Unchecked -Ounchecked
//
// RUN: %target-run %t/Assert_Debug
// RUN: %target-run %t/Assert_Release
// RUN: %target-run %t/Assert_Unchecked

import StdlibUnittest

//===---
// Utilities.
//===---

func isDebugOrRelease() -> Bool {
  return !_isFastAssertConfiguration()
}

//===---
// Tests.
//===---

func testTrapsAreNoreturn(i: Int) -> Int {
  // Don't need a return statement in 'case' statements because these functions
  // are @noreturn.
  switch i {
  case 1:
    assertionFailure("can not happen")
  case 2:
    preconditionFailure("can not happen")
  case 3:
    _preconditionFailure("can not happen")
  case 4:
    _debugPreconditionFailure("can not happen")
  case 5:
    _sanityCheckFailure("can not happen")

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

Assert.test("precondition")
  .xfail(.Custom(
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
  .xfail(.Custom(
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
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  expectCrashLater()
  preconditionFailure("this should fail")
}

Assert.test("preconditionFailure/StringInterpolation")
  .skip(.Custom(
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
  .xfail(.Custom(
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
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  expectCrashLater()
  _preconditionFailure("this should fail")
}

Assert.test("_debugPrecondition")
  .xfail(.Custom(
    { !_isDebugAssertConfiguration() },
    reason: "debug preconditions are disabled in Release and Unchecked mode"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? "this should fail" : "")
  .code {
  var x = 2
  _debugPrecondition(x * 21 == 42, "should not fail")
  expectCrashLater()
  _debugPrecondition(x == 42, "this should fail")
}

Assert.test("_debugPreconditionFailure")
  .skip(.Custom(
    { !_isDebugAssertConfiguration() },
    reason: "optimizer assumes that the code path is unreachable"))
  .crashOutputMatches("this should fail")
  .code {
  expectCrashLater()
  _debugPreconditionFailure("this should fail")
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

