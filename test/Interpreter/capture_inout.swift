// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

func foo(inout x: Int) -> () -> Int {
  func bar() -> Int {
    x += 1
    return x
  }
  bar()
  return bar
}

var x = 219
var f = foo(&x)
print(x) // CHECK: 220
print(f()) // CHECK: 221
print(f()) // CHECK: 222
print(f()) // CHECK: 223
print(x) // CHECK: 220
