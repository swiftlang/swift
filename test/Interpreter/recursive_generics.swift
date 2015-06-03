// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// rdar://18067671
class List<T> {
  var value: T
  var next: List<T>?

  init(value: T) {
    self.value = value
  }
  init(value: T, next: List<T>) {
    self.value = value
    self.next = next
  }
}

let a = List(value: 0.0)
let b = List(value: 1.0, next: a)
let c = List(value: 2.0, next: b)
b.value = 4.0
a.value = 8.0

print("begin")
print(c.value)
print(c.next!.value)
print(c.next!.next!.value)
// CHECK:      begin
// CHECK-NEXT: 2.0
// CHECK-NEXT: 4.0
// CHECK-NEXT: 8.0
