// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
//
// RUN: %target-run %t/a.out assertBool 2>&1 | FileCheck %s -check-prefix=ASSERT_BOOL
// RUN: %target-run %t/a.out debugTrapBool 2>&1 | FileCheck %s -check-prefix=DEBUG_TRAP_BOOL
// RUN: %target-run %t/a.out assertLogicValue 2>&1 | FileCheck %s -check-prefix=ASSERT_LOGICVALUE
// RUN: %target-run %t/a.out debugTrapLogicValue 2>&1 | FileCheck %s -check-prefix=DEBUG_TRAP_LOGICVALUE

// REQUIRES: asserts
// REQUIRES: swift_stdlib_asserts

import Darwin

func test_assertBool() {
  var x = 2
  assert(x * 21 == 42, "should not fail")
  println("OK")
  // ASSERT_BOOL: OK
  assert(x == 42, "this should fail")
  // ASSERT_BOOL-NEXT: assertion failed: this should fail: file {{.*}}AssertDebug.swift, line [[@LINE-1]]
  // ASSERT_BOOL-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "assertBool") {
  test_assertBool()
}

func test_debugTrapBool() {
  var x = 2
  assert(x * 21 == 42, "should not fail")
  println("OK")
  // DEBUG_TRAP_BOOL: OK
  assert(x == 42, "this should fail")
  // DEBUG_TRAP_BOOL-NEXT: assertion failed: this should fail: file {{.*}}AssertDebug.swift, line [[@LINE-1]]
  // DEBUG_TRAP_BOOL-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "debugTrapBool") {
  test_debugTrapBool()
}

struct Truthiness : LogicValue {
  init(_ value: Bool) { self.value = value }
  func getLogicValue() -> Bool { return value }

  var value: Bool
}
var falsie = Truthiness(false)
var truthie = Truthiness(true)

func test_assertLogicValue() {
  assert(truthie, "should not fail")
  println("OK")
  // ASSERT_LOGICVALUE: OK
  assert(falsie, "this should fail")
  // ASSERT_LOGICVALUE-NEXT: assertion failed: this should fail: file {{.*}}AssertDebug.swift, line [[@LINE-1]]
  // ASSERT_LOGICVALUE-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "assertLogicValue") {
  test_assertLogicValue()
}

func test_debugTrapLogicValue() {
  assert(truthie, "should not fail")
  println("OK")
  // DEBUG_TRAP_LOGICVALUE: OK
  assert(falsie, "this should fail")
  // DEBUG_TRAP_LOGICVALUE-NEXT: assertion failed: this should fail: file {{.*}}AssertDebug.swift, line [[@LINE-1]]
  // DEBUG_TRAP_LOGICVALUE-NEXT: CRASHED: SIG{{ILL|TRAP}}
}

if (Process.arguments[1] == "debugTrapLogicValue") {
  test_debugTrapLogicValue()
}


println("BUSTED: should have crashed already")
exit(1)

