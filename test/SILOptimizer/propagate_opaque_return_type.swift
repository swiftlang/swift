// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

protocol P {}
extension P {
  func foo() -> some Sequence<Int> {
    [1, 2, 3]
  }
}

struct B {
  let p: P

  @inline(never)
  func bar() {
    for x in p.foo() {
      print(x)
    }
  }
}


struct S : P {
  var x = 0
}


let b = B(p: S())

// CHECK:      1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
b.bar()

