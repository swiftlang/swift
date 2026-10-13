// RUN: %target-swift-frontend -O -emit-sil -sil-verify-all %s | %FileCheck %s

public protocol P {
  func method()
}

@inline(__always)
func receive<each T>(_ types: repeat (each T).Type)
    where repeat (each T).Type: P {
  for type in repeat each types {
    type.method()
  }
}

// CHECK-LABEL: sil {{.*}}forward
// CHECK-NOT: function_ref {{.*}}receive
// CHECK: witness_method {{.*}}#P.method
public func forward<each T>(_ types: repeat (each T).Type)
    where repeat (each T).Type: P {
  receive(repeat each types)
}
