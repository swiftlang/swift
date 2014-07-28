// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/Assert_Debug
//
// RUN: %target-run %t/Assert_Debug fatal 2>&1 | FileCheck %s -check-prefix=CHECK_FATAL
// RUN: %target-run %t/Assert_Debug Bool 2>&1 | FileCheck %s -check-prefix=CHECK_BOOL
// RUN: %target-run %t/Assert_Debug BooleanType 2>&1 | FileCheck %s -check-prefix=CHECK_LOGICVALUE
// RUN: %target-run %t/Assert_Debug trap 2>&1 | FileCheck %s -check-prefix=CHECK_TRAP_DEBUG
//
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/Assert_Release -O
//
// RUN: %target-run %t/Assert_Release trap 2>&1 | FileCheck %s -check-prefix=CHECK_TRAP_RELEASE

// REQUIRES: swift_stdlib_asserts

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

//===---
// Tests.
//===---

func test_fatalIsNoreturn() {
  enum EA {
    case A(Bool)
    case B
  }
  func f(e: EA) -> Bool {
    switch e {
    case .A(let res):
      return res
    case .B:
      _preconditionFailure("can not happen")
      // Don't need a return statement here because fatal() is @noreturn.
    }
  }
}

func test_fatal() {
  _preconditionFailure("this should fail")
  // CHECK_FATAL: fatal error: this should fail
  // CHECK_FATAL: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "fatal") {
  test_fatal()
}


func test_securityCheckBool() {
  var x = 2
  _precondition(x * 21 == 42, "should not fail")
  println("OK")
  // CHECK_BOOL: OK
  _precondition(x == 42, "this should fail")
  // CHECK_BOOL-NEXT: fatal error: this should fail
  // CHECK_BOOL-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "Bool") {
  test_securityCheckBool()
}

func test_securityCheckLogicValue() {
  _precondition(truthie, "should not fail")
  println("OK")
  // CHECK_LOGICVALUE: OK
  _precondition(falsie, "this should fail")
  // CHECK_LOGICVALUE-NEXT: fatal error: this should fail
  // CHECK_LOGICVALUE-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "BooleanType") {
  test_securityCheckLogicValue()
}

func test_trop() {
  println("OK")
  // CHECK_TRAP_DEBUG: OK
  // CHECK_TRAP_RELEASE: OK
  trap("this should fail")
  // CHECK_TRAP_DEBUG-NEXT: fatal error: this should fail
  // CHECK_TRAP_DEBUG-NEXT: CRASHED: SIG{{ILL|TRAP}}
  // CHECK_TRAP_RELEASE-NEXT: CRASHED: SIG{{ILL|TRAP}}
}
if Process.arguments[1] == "trap" {
  test_trop()
}

println("BUSTED: should have crashed already")
exit(1)

