// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// rdar://189854998

final class C {
  var x = 0
  deinit { print("bye") }
}

enum Box<T>: ~Copyable {
  case some(T)
}

func make<T>(_ value: T) -> Box<T>? { .some(value) }

func unwrapAndSwitch<T>(_ value: T) -> T {
  switch make(value)! {
  case .some(let value):
    return value
  }
}

// CHECK: start
print("start")

do {
  let c = unwrapAndSwitch(C())
  // CHECK-NEXT: got 0
  print("got", c.x)
  // CHECK-NEXT: bye
}

// CHECK-NEXT: end
print("end")
