// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Check that subscripts and functions named subscript can exist side-by-side
struct Foo {
  subscript() -> String {
    return "subscript"
  }
  
  func `subscript`() -> String {
    return "func"
  }
}

let f = Foo()
print(f[]) // CHECK: subscript
print(f.subscript()) // CHECK: func

// SR-7418

protocol P {
  subscript<T : Y>(_: T) -> Int { get set }
}

struct Q : P {
  subscript<T : X>(_ idx: T) -> Int {
    get { return 0 } set { idx.foo() }
  }
}
protocol Y : X {}
protocol X { func foo() }

struct Idx : Y {
  func foo() { print("I survived") }
}

func foo<T : P>(_ t: inout T) {
  t[Idx()] += 1
}

var q = Q()
foo(&q) // CHECK: I survived
