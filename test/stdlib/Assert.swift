// RUN: %swift -i %s 2>&1 | FileCheck %s

func test_alwaysTrap() {
  var x = 2
  alwaysTrap(x * 21 == 42, "should not fail")
  con.write("OK")
  // CHECK: OK
  alwaysTrap(x == 42, "this should fail")
  // CHECK: assertion failed: this should fail: file {{.*}}Assert.swift, line [[@LINE-1]]
}

test_alwaysTrap()

