// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol P {
  associatedtype T
  func foo(t: inout T)
}
struct S: P {
  func foo(t: inout () -> Void) {
    t()
    t = { print("new") }
  }
}

func doTheFoo<SomeP: P>(_ p: SomeP, _ value: SomeP.T) -> SomeP.T {
  var mutableValue = value
  p.foo(t: &mutableValue)
  return mutableValue
}

print("START")
let newClosure = doTheFoo(S(), { print("old") })
newClosure()
print("DONE")

// CHECK: START
// CHECK-NEXT: old
// CHECK-NEXT: new
// CHECK-NEXT: DONE
