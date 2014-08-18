// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/Assert_Debug
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/Assert_Release -O
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/Assert_Unchecked -Ounchecked
//
// RUN: %target-run %t/Assert_Debug Debug Assert 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug AssertInterpolation 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug AssertBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug AssertionFailure 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug AssertionFailureInterpolation 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
//
// RUN: %target-run %t/Assert_Debug Debug Precondition 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug PreconditionInterpolation 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug PreconditionBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug PreconditionFailure 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug PreconditionFailureInterpolation 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
//
// RUN: %target-run %t/Assert_Debug Debug FatalError 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug FatalErrorInterpolation 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
//
// RUN: %target-run %t/Assert_Debug Debug StdlibPrecondition 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug StdlibPreconditionBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug StdlibPreconditionFailure 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
//
// RUN: %target-run %t/Assert_Debug Debug StdlibDebugPrecondition 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug StdlibDebugPreconditionBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug StdlibDebugPreconditionFailure 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
//
// RUN: %target-run %t/Assert_Debug Debug StdlibSanityCheck 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug StdlibSanityCheckBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Debug Debug StdlibSanityCheckFailure 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
//
//
// RUN: %target-run %t/Assert_Release Release Assert 2>&1 | FileCheck %s -check-prefix=CHECK_NO_TRAP
// RUN: %target-run %t/Assert_Release Release AssertInterpolation 2>&1 | FileCheck %s -check-prefix=CHECK_NO_TRAP
// RUN: %target-run %t/Assert_Release Release AssertBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_NO_TRAP
// Skip because the optimizer assumes that the code path is unreachable.
// SKIP:            %t/Assert_Release Release AssertionFailure
//
// RUN: %target-run %t/Assert_Release Release Precondition 2>&1 | FileCheck %s -check-prefix=CHECK_NO_MESSAGE
// RUN: %target-run %t/Assert_Release Release PreconditionInterpolation 2>&1 | FileCheck %s -check-prefix=CHECK_NO_MESSAGE
// RUN: %target-run %t/Assert_Release Release PreconditionBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_NO_MESSAGE
// RUN: %target-run %t/Assert_Release Release PreconditionFailure 2>&1 | FileCheck %s -check-prefix=CHECK_NO_MESSAGE
//
// RUN: %target-run %t/Assert_Release Release FatalError 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
//
// RUN: %target-run %t/Assert_Release Release StdlibPrecondition 2>&1 | FileCheck %s -check-prefix=CHECK_NO_MESSAGE
// RUN: %target-run %t/Assert_Release Release StdlibPreconditionBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_NO_MESSAGE
// RUN: %target-run %t/Assert_Release Release StdlibPreconditionFailure 2>&1 | FileCheck %s -check-prefix=CHECK_NO_MESSAGE
//
// RUN: %target-run %t/Assert_Release Release StdlibDebugPrecondition 2>&1 | FileCheck %s -check-prefix=CHECK_NO_TRAP
// RUN: %target-run %t/Assert_Release Release StdlibDebugPreconditionBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_NO_TRAP
// Skip because the optimizer assumes that the code path is unreachable.
// SKIP:            %t/Assert_Release Release StdlibDebugPreconditionFailure 2>&1 | FileCheck %s -check-prefix=CHECK_NO_MESSAGE
//
// RUN: %target-run %t/Assert_Release Release StdlibSanityCheck 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Release Release StdlibSanityCheckBooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
// RUN: %target-run %t/Assert_Release Release StdlibSanityCheckFailure 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE
//
//
// RUN: %target-run %t/Assert_Unchecked Unchecked FatalError 2>&1 | FileCheck %s -check-prefix=CHECK_MESSAGE

// REQUIRES: swift_stdlib_asserts

// CHECK_MESSAGE: OK
// CHECK_MESSAGE: this should fail
// CHECK_MESSAGE: CRASHED: SIG{{ILL|TRAP|ABRT}}

// CHECK_NO_MESSAGE: OK
// CHECK_NO_MESSAGE-NOT: this should fail
// CHECK_NO_MESSAGE: CRASHED: SIG{{ILL|TRAP|ABRT}}

// CHECK_NO_TRAP: did not trap

import Darwin

//===---
// Utilities.
//===---

struct Truthiness : BooleanType {
  init(_ value: Bool) { self.value = value }
  var boolValue: Bool { return value }

  var value: Bool
}
var falsie = Truthiness(false)
var truthie = Truthiness(true)

func isDebugAssertConfiguration() -> Bool {
  return Process.arguments[1] == "Debug"
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

func testAssert() {
  var x = 2
  assert(x * 21 == 42, "should not fail")
  println("OK")
  assert(x == 42, "this should fail")
}
if (Process.arguments[2] == "Assert") {
  testAssert()
  println("did not trap")
  if !isDebugAssertConfiguration() {
    exit(0)
  }
}

func testAssertInterpolation() {
  var should = "should"
  var x = 2
  assert(x * 21 == 42, "\(should) not fail")
  println("OK")
  assert(x == 42, "this \(should) fail")
}
if (Process.arguments[2] == "AssertInterpolation") {
  testAssertInterpolation()
  println("did not trap")
  if !isDebugAssertConfiguration() {
    exit(0)
  }
}

func testAssertBooleanType() {
  assert(truthie, "should not fail")
  println("OK")
  assert(falsie, "this should fail")
}
if (Process.arguments[2] == "AssertBooleanType") {
  testAssertBooleanType()
  println("did not trap")
  if !isDebugAssertConfiguration() {
    exit(0)
  }
}

func testAssertBooleanTypeInterpolation() {
  var should = "should"
  assert(truthie, "\(should) not fail")
  println("OK")
  assert(falsie, "this \(should) fail")
}
if (Process.arguments[2] == "AssertBooleanTypeInterpolation") {
  testAssertBooleanTypeInterpolation()
  println("did not trap")
  if !isDebugAssertConfiguration() {
    exit(0)
  }
}

func testAssertionFailure() {
  println("OK")
  assertionFailure("this should fail")
}
if (Process.arguments[2] == "AssertionFailure") {
  testAssertionFailure()
}

func testAssertionFailureInterpolation() {
  var should = "should"
  println("OK")
  assertionFailure("this \(should) fail")
}
if (Process.arguments[2] == "AssertionFailureInterpolation") {
  testAssertionFailureInterpolation()
}

func testPrecondition() {
  var x = 2
  precondition(x * 21 == 42, "should not fail")
  println("OK")
  precondition(x == 42, "this should fail")
}
if (Process.arguments[2] == "Precondition") {
  testPrecondition()
}

func testPreconditionInterpolation() {
  var should = "should"
  var x = 2
  precondition(x * 21 == 42, "\(should) not fail")
  println("OK")
  precondition(x == 42, "this \(should) fail")
}
if (Process.arguments[2] == "PreconditionInterpolation") {
  testPreconditionInterpolation()
}

func testPreconditionBooleanType() {
  precondition(truthie, "should not fail")
  println("OK")
  precondition(falsie, "this should fail")
}
if (Process.arguments[2] == "PreconditionBooleanType") {
  testPreconditionBooleanType()
}

func testPreconditionBooleanTypeInterpolation() {
  var should = "should"
  precondition(truthie, "\(should) not fail")
  println("OK")
  precondition(falsie, "this \(should) fail")
}
if (Process.arguments[2] == "PreconditionBooleanTypeInterpolation") {
  testPreconditionBooleanTypeInterpolation()
}

func testPreconditionFailure() {
  println("OK")
  preconditionFailure("this should fail")
}
if (Process.arguments[2] == "PreconditionFailure") {
  testPreconditionFailure()
}

func testPreconditionFailureInterpolation() {
  var should = "should"
  println("OK")
  preconditionFailure("this \(should) fail")
}
if (Process.arguments[2] == "PreconditionFailureInterpolation") {
  testPreconditionFailureInterpolation()
}

func testFatalError() {
  println("OK")
  fatalError("this should fail")
}
if (Process.arguments[2] == "FatalError") {
  testFatalError()
}

func testFatalErrorInterpolation() {
  var should = "should"
  println("OK")
  fatalError("this \(should) fail")
}
if (Process.arguments[2] == "FatalErrorInterpolation") {
  testFatalErrorInterpolation()
}

func testStdlibPrecondition() {
  var x = 2
  _precondition(x * 21 == 42, "should not fail")
  println("OK")
  _precondition(x == 42, "this should fail")
}
if (Process.arguments[2] == "StdlibPrecondition") {
  testStdlibPrecondition()
}

func testStdlibPreconditionBooleanType() {
  _precondition(truthie, "should not fail")
  println("OK")
  _precondition(falsie, "this should fail")
}
if (Process.arguments[2] == "StdlibPreconditionBooleanType") {
  testStdlibPreconditionBooleanType()
}

func testStdlibPreconditionFailure() {
  println("OK")
  _preconditionFailure("this should fail")
}
if (Process.arguments[2] == "StdlibPreconditionFailure") {
  testStdlibPreconditionFailure()
}

func testStdlibDebugPrecondition() {
  var x = 2
  _debugPrecondition(x * 21 == 42, "should not fail")
  println("OK")
  _debugPrecondition(x == 42, "this should fail")
}
if (Process.arguments[2] == "StdlibDebugPrecondition") {
  testStdlibDebugPrecondition()
  println("did not trap")
  if !isDebugAssertConfiguration() {
    exit(0)
  }
}

func testStdlibDebugPreconditionBooleanType() {
  _debugPrecondition(truthie, "should not fail")
  println("OK")
  _debugPrecondition(falsie, "this should fail")
}
if (Process.arguments[2] == "StdlibDebugPreconditionBooleanType") {
  testStdlibDebugPreconditionBooleanType()
  println("did not trap")
  if !isDebugAssertConfiguration() {
    exit(0)
  }
}

func testStdlibDebugPreconditionFailure() {
  println("OK")
  _debugPreconditionFailure("this should fail")
}
if (Process.arguments[2] == "StdlibDebugPreconditionFailure") {
  testStdlibDebugPreconditionFailure()
}

func testStdlibSanityCheck() {
  var x = 2
  _sanityCheck(x * 21 == 42, "should not fail")
  println("OK")
  _sanityCheck(x == 42, "this should fail")
}
if (Process.arguments[2] == "StdlibSanityCheck") {
  testStdlibSanityCheck()
}

func testStdlibSanityCheckBooleanType() {
  _sanityCheck(truthie, "should not fail")
  println("OK")
  _sanityCheck(falsie, "this should fail")
}
if (Process.arguments[2] == "StdlibSanityCheckBooleanType") {
  testStdlibSanityCheckBooleanType()
}

func testStdlibSanityCheckFailure() {
  println("OK")
  _sanityCheckFailure("this should fail")
}
if (Process.arguments[2] == "StdlibSanityCheckFailure") {
  testStdlibSanityCheckFailure()
}

println("BUSTED: should have crashed already")
exit(1)

