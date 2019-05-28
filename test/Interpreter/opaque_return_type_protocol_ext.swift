// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

@available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
protocol P {
  associatedtype AT
  func foo() -> AT
}

@available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
struct Adapter<T: P>: P {
  var inner: T

  @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
  func foo() -> some P {
    return inner
  }
}

@available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
extension P {
  @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
  func foo() -> some P {
    return Adapter(inner: self)
  }
}

@available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
func getPAT<T: P>(_: T.Type) -> Any.Type {
  return T.AT.self
}

@available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
extension Int: P { }

// CHECK: {{Adapter<Int>|too old}}
if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
  print(getPAT(Int.self))
} else {
  print("i'm getting too old for this sh")
}
