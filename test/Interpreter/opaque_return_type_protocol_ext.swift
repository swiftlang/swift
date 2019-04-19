// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol P {
  associatedtype AT
  func foo() -> AT
}

extension P {
  func foo() -> some P {
    return self
  }
}

func getPAT<T: P>(_: T.Type) -> Any.Type {
  return T.AT.self
}

extension Int: P { }

// CHECK: Int
print(getPAT(Int.self))
