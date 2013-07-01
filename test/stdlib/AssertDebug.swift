// RUN: %swift -i %s --args assert 2>&1 | FileCheck %s -check-prefix=ASSERT
// RUN: %swift -i %s --args debugTrap 2>&1 | FileCheck %s -check-prefix=DEBUG_TRAP
// REQUIRES: asserts

func test_assert() {
  var x = 2
  assert(x * 21 == 42, "should not fail")
  con.write("OK")
  // ASSERT: OK
  assert(x == 42, "this should fail")
  // ASSERT: assertion failed: this should fail: file {{.*}}Assert.swift, line [[@LINE-1]]
}

if (CommandLineArguments.arguments[0] == "assert") {
  test_assert()
}

func test_debugTrap() {
  var x = 2
  debugTrap(x * 21 == 42, "should not fail")
  con.write("OK")
  // DEBUG_TRAP: OK
  debugTrap(x == 42, "this should fail")
  // DEBUG_TRAP: assertion failed: this should fail: file {{.*}}Assert.swift, line [[@LINE-1]]
}

if (CommandLineArguments.arguments[0] == "debugTrap") {
  test_debugTrap()
}

