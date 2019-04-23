// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol P {
  associatedtype AT
  func foo() -> AT
}

struct Adapter<T: P>: P {
  var inner: T
  func foo() -> some P {
    return inner
  }
}

extension P {
  func foo() -> some P {
    return Adapter(inner: self)
  }
}

func getPAT<T: P>(_: T.Type) -> Any.Type {
  return T.AT.self
}

extension Int: P { }

// CHECK: Adapter<Int>
print(getPAT(Int.self))
