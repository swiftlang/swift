// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
protocol P {
  associatedtype AT
  func foo() -> AT
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
struct Adapter<T: P>: P {
  var inner: T

  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  func foo() -> some P {
    return inner
  }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension P {
  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  func foo() -> some P {
    return Adapter(inner: self)
  }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
func getPAT<T: P>(_: T.Type) -> Any.Type {
  return T.AT.self
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension Int: P { }

// CHECK: {{Adapter<Int>|too old}}
if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
  print(getPAT(Int.self))
} else {
  print("i'm getting too old for this sh")
}
