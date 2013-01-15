// RUN: %swift -sil-i %s | FileCheck %s

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
