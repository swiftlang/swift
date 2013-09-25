// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

func foo(x:[byref] Int) -> () -> Int {
  func bar() -> Int {
    x += 1
    return x
  }
  bar()
  return bar
}

var x = 219
var f = foo(&x)
println(x) // CHECK: 220
println(f()) // CHECK: 221
println(f()) // CHECK: 222
println(f()) // CHECK: 223
println(x) // CHECK: 220
