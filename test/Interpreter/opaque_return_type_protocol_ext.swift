// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
protocol P {
  associatedtype AT
  func foo() -> AT
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
struct Adapter<T: P>: P {
  var inner: T

  @available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
  func foo() -> some P {
    return inner
  }
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
extension P {
  @available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
  func foo() -> some P {
    return Adapter(inner: self)
  }
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
func getPAT<T: P>(_: T.Type) -> Any.Type {
  return T.AT.self
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
extension Int: P { }

// CHECK: {{Adapter<Int>|too old}}
if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  print(getPAT(Int.self))
} else {
  print("i'm getting too old for this sh")
}
