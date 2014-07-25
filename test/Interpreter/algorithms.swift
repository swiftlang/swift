// RUN: %target-run-simple-swift | FileCheck %s

func fib() {
  var (a, b) = (0, 1)
  while b < 10 {
    println(b)
    (a, b) = (b, a+b)
  }
}
fib()

// CHECK: 1
// CHECK: 1
// CHECK: 2
// CHECK: 3
// CHECK: 5
// CHECK: 8

// From: <rdar://problem/17796401>
let two_one = Array(lazy([1, 2, 3, 4]).reverse().filter { $0 % 2 == 0 }.map { $0 / 2 })
println(two_one)
// CHECK: [2, 1]
