// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s
// REQUIRES: executable_test

// Calls through the base class must honor the base's parameter ownership,
// even when the override changes it.

final class Tracked {
  let name: String
  init(_ name: String) { self.name = name }
  deinit { print("deinit \(name)") }
}

class Base {
  func borrowToConsume(_ x: borrowing Tracked) {}
  func consumeToBorrow(_ x: consuming Tracked) {}
  func generic<T>(_ x: borrowing T) {}
}

class Derived: Base {
  override func borrowToConsume(_ x: consuming Tracked) {}
  override func consumeToBorrow(_ x: borrowing Tracked) {}
  override func generic<T>(_ x: consuming T) {}
}

@inline(never)
func makeBase() -> Base { Derived() }

@inline(never)
func test(_ b: Base) {
  do {
    let t = Tracked("borrowed")
    b.borrowToConsume(t)
    b.borrowToConsume(t)
    print("still alive: \(t.name)")
  }

  b.consumeToBorrow(Tracked("consumed"))
  print("after consume")

  do {
    let t = Tracked("generic")
    b.generic(t)
    b.generic(t)
    print("still alive: \(t.name)")
  }
}

// CHECK:      still alive: borrowed
// CHECK-NEXT: deinit borrowed
// CHECK-NEXT: deinit consumed
// CHECK-NEXT: after consume
// CHECK-NEXT: still alive: generic
// CHECK-NEXT: deinit generic
// CHECK-NEXT: done
test(makeBase())
print("done")
