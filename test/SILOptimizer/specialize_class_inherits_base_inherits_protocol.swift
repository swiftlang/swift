// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

protocol P { func p() -> Any.Type }
protocol Q: P { }

@inline(never) func sink<T>(_ x: T) {}

func p<T: Q>(_ x: T) { sink(x.p() as Any.Type) }

class Foo<T>: Q { func p() -> Any.Type { return T.self } }
class Bar<T>: Foo<T> {}

// CHECK-LABEL: sil @$s031specialize_class_inherits_base_C9_protocol3fooyyF
public func foo() {
  // CHECK: function_ref @$s031specialize_class_inherits_base_C9_protocol4sinkyyxlFypXp_Ttg5
  p(Bar<Int>())
}

