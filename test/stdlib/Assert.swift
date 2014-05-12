// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out fatal 2>&1 | FileCheck %s -check-prefix=CHECK_FATAL
// RUN: %target-run %t/a.out Bool 2>&1 | FileCheck %s -check-prefix=CHECK_BOOL
// RUN: %target-run %t/a.out LogicValue 2>&1 | FileCheck %s -check-prefix=CHECK_LOGICVALUE

// REQUIRES: swift_stdlib_asserts

import Darwin

//===---
// Utilities.
//===---

struct Truthiness : LogicValue {
  init(_ value: Bool) { self.value = value }
  func getLogicValue() -> Bool { return value }

  var value: Bool;
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
      fatal("can not happen")
      // Don't need a return statement here because fatal() is @noreturn.
    }
  }
}

func test_fatal() {
  fatal("this should fail")
  // CHECK_FATAL: fatal error: this should fail: file {{.*}}Assert.swift, line [[@LINE-1]]
  // CHECK_FATAL: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "fatal") {
  test_fatal()
}


func test_securityCheckBool() {
  var x = 2
  securityCheck(x * 21 == 42, "should not fail")
  println("OK")
  // CHECK_BOOL: OK
  securityCheck(x == 42, "this should fail")
  // CHECK_BOOL-NEXT: fatal error: this should fail: file {{.*}}Assert.swift, line [[@LINE-1]]
  // CHECK_BOOL-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "Bool") {
  test_securityCheckBool()
}

func test_securityCheckLogicValue() {
  securityCheck(truthie, "should not fail")
  println("OK")
  // CHECK_LOGICVALUE: OK
  securityCheck(falsie, "this should fail")
  // CHECK_LOGICVALUE-NEXT: fatal error: this should fail: file {{.*}}Assert.swift, line [[@LINE-1]]
  // CHECK_LOGICVALUE-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "LogicValue") {
  test_securityCheckLogicValue()
}

println("BUSTED: should have crashed already")
exit(1)
