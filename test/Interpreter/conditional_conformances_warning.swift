// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out 2>&1 | %FileCheck %s -check-prefix=CHECK_WARNINGS
// REQUIRES: executable_test
protocol P {
  func foo()
}

struct X : P {
  func foo() { print("X.P") }
}

struct Y<T> {
  var wrapped: T
}

extension Y: P where T: P {
  func foo() { wrapped.foo() }
}

func tryAsP(_ value: Any) {
  if let p = value as? P {
    p.foo()
  }
}

extension Dictionary: P where Value == (Key) -> Bool {
  func foo() { }
}


let yx = Y(wrapped: X())

// CHECK_WARNINGS: warning: Swift runtime does not yet support dynamically querying conditional conformance ('a.Y<a.X>': 'a.P')
tryAsP(yx)
