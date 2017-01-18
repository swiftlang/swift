// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

func foo(_ x: inout Int) {
  func bar() -> Int {
    x += 1
    return x
  }
  bar()
}

var x = 219
foo(&x)
print(x) // CHECK: 220
